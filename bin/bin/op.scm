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
        (only utf8-srfi-13 string-pad-right)
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
       (write
        (sort
         (map
          (lambda (item)
            (list (alist-ref 'title (alist-ref 'overview item))
                  (alist-ref (alist-ref 'vaultUuid item) vaults equal?)
                  (alist-ref (alist-ref 'templateUuid item) templates equal?)))
          (medea:read-json items-out))
         (lambda (a b)
           (string<? (car a) (car b))))
        port)))))

(define-values (rofi-out rofi-in rofi-pid)
  (process "rofi -dmenu -i -p '1password' -markup-rows"))

;; Read metadata
((if encrypt-cache
     (cut call-with-input-pipe (decrypt-cmd cache-file) <>)
     (cut call-with-input-file cache-file <>))
 (lambda (port)
   (let* ((lines (read port))
          ;; Add two to account for zero width space and column space
          (item-pad (+ 2 (apply max (map (lambda (x) (string-length (car x))) lines))))
          (vault-pad (+ 2 (apply max (map (lambda (x) (string-length (cadr x))) lines))))
          (lines* (apply
                   string-append
                   (map (lambda (x)
                          (string-append
                           (string-pad-right (string-append (car x) "\u200b") item-pad)
                           "<span color=\"#7C6F64\">\u200b"
                           (string-pad-right (string-append (cadr x) "\u200b") vault-pad)
                           "</span><span color=\"#83A598\">\u200b"
                           (caddr x)
                           "\u200b</span>\n"))
                        lines))))
     (display lines* rofi-in))
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
