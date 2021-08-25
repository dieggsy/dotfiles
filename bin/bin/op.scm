#!/usr/bin/chicken-scheme
(import (chicken process)
        (chicken process-context)
        (only (chicken io) read-line)
        (only (chicken file posix) file-modification-time)
        (only (chicken file) file-exists?)
        (only (chicken time) current-seconds)
        (only (chicken pretty-print) pp)
        (only (chicken string) string-split ->string)
        (only (chicken sort) sort)
        (prefix medea medea:))

;; I'm not sure how much this actually helps - are we encrypting because
;; someone else has access to our system? At which point we have bigger
;; problems. I suppose if there was some malicious program just sending files
;; in bulk to a server, this would prevent access there...
(define encrypt-session-key #t)
(define encrypt-cache #t)

(define (encrypt-cmd file)
  (string-append "gpg --batch --yes -eaq --default-recipient-self -o " file))

(define (decrypt-cmd file)
  (string-append "gpg --batch --yes -qd " file))

(define args (command-line-arguments))

(define env-file "/tmp/op-env")
(define cache-file "/tmp/op-cache")

;; parse array as list
(medea:json-parsers (cons
                     (cons 'array (lambda (x) x))
                     (medea:json-parsers)))

;; Authenticate and cache session key
(when (or (not (file-exists? env-file))
          (< (file-modification-time env-file)
             (- (current-seconds)
                (* 29 60)))
          (member "-reauth" args))
  (let-values (((auth-out auth-in auth-pid)
                (process "rofi -dmenu -p password -password | op signin my")))
    (close-output-port auth-in)
    (let-values (((pid normal status) (process-wait auth-pid)))
      (unless (zero? status)
        (display "Authentication error.")
        (exit 1)))
    ((if encrypt-session-key
         (cut call-with-output-pipe (encrypt-cmd env-file) <>)
         (cut call-with-output-file ennv-file <>))
      (lambda (port) (display (read-line auth-out) port)))))

;; Get session key
(define op-env ((if encrypt-session-key
                    (cut call-with-input-pipe (decrypt-cmd env-file)  <>)
                    (cut call-with-input-file env-file <>))
                read-line))

(define (op args)
  (apply string-append op-env " && op " (intersperse (map ->string args) " ")))

;; Cache metadata
(when (or (not (file-exists? cache-file))
          (< (file-modification-time cache-file)
             (- (current-seconds)
                (* 24 60 60)))
          (member  "-recache" args))
  (let*-values (((vaults-out vaults-in vaults-pid)
                 (process (op '(list vaults))))
                ((templates-out templates-in templates-pid)
                 (process (op '(list templates))))
                ((items-out items-in items-pid)
                 (process (op '(list items))))
                ((vaults) (map
                           (lambda (vault)
                             (cons
                              (alist-ref 'uuid vault)
                              (alist-ref 'name vault)))
                           (medea:read-json vaults-out)))
                ((templates) (map
                              (lambda (template)
                                (cons
                                 (alist-ref 'uuid template)
                                 (alist-ref 'name template)))
                              (medea:read-json templates-out))))
    (let*-values (((pid normal vaults-status) (process-wait vaults-pid))
                  ((pid normal templates-status) (process-wait templates-pid))
                  ((pid normal items-status) (process-wait items-pid)))
      (unless (and (zero? vaults-status)
                   (zero? items-status)
                   (zero? templates-status))
        (display "Error getting vaults or items")
        (exit 1)))
    ((if encrypt-cache
         (cut call-with-output-pipe (encrypt-cmd cache-file) <>)
         (cut call-with-output-file cache-file <>))
      (lambda (port)
        (for-each
         (lambda (line) (display line port))
         (sort
          (map
           (lambda (item)
             (string-append (alist-ref 'title (alist-ref 'overview item))
                            "\u200b <span color=\"#7C6F64\">\u200b"
                            (alist-ref (alist-ref 'vaultUuid item) vaults equal?)
                            "\u200b </span><span color=\"#83A598\">\u200b"
                            (alist-ref (alist-ref 'templateUuid item) templates equal?)
                            "\u200b</span>\n"))
           (medea:read-json items-out))
          string<?))))))

(define-values (rofi-out rofi-in rofi-pid)
  (process "rofi -dmenu -i -p '1password' -markup-rows"))

;; Read metadata
((if encrypt-cache
     (cut call-with-input-pipe (decrypt-cmd cache-file) <>)
     (cut call-with-input-file cache-file <>))
  (lambda (port)
    (let loop ((line (read-line port)))
      (unless (eof-object? line)
        (display line rofi-in)
        (newline rofi-in)
        (loop (read-line port))))
    (close-output-port rofi-in)))

(define selection (read-line rofi-out))

(when (eof-object? selection)
  (exit))

(define vault/item (string-split selection "\u200b"))

(define vault (caddr vault/item))
(define item (car vault/item))

(define-values (xclip-out xclip-in xclip-pid) (process "xclip" '("-sel" "c")))

(define-values (op-out op-in op-pid)
  (process (op `(get item ,(string-append "'" item "'") --vault ,vault --fields password))))

(define password (read-line op-out))

(display password xclip-in)
(close-output-port xclip-in)

;; Clear if still on clipboard
(process-fork
 (lambda ()
   (sleep 90)
   (let-values (((xclip-out xclip-in xclip-pid) (process "xclip" '("-sel" "c" "-o"))))
     (let ((clipboard (read-line xclip-out)))
       (when (and (not (eof-object? clipboard))
                  (string=? password clipboard))
         (system "printf '' | xclip -sel c"))))))
