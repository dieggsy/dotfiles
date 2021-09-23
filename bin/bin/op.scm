#!/usr/bin/csi -s
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
        scsh-process
        (prefix medea medea:))

(define-syntax $ (syntax-rules () (($ arg) (car (run/strings arg)))))

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
  (receive (status success pid)
      (run (pipe (rofi -dmenu -p "1(password)" -password)
                 (op signin my --raw)
                 (gpg --batch --yes -eaq --default-recipient-self -o ,env-file)))
    (unless success
      (format (current-error-port) "Authentication error")
      (exit 1))))

;; Get session key
(set-environment-variable!
 "OP_SESSION_my"
 ($ (gpg --batch --yes -qd ,env-file)))

(define-syntax op
  (syntax-rules ()
    ((op . args)
     (process "op" (map ->string `args)))))

;; Cache metadata
(when (or (not (file-exists? cache-file))
          (< (file-modification-time cache-file)
             (- (current-seconds)
                (* 24 60 60)))
          (member  "-recache" args))
  (let* ((vaults (map (lambda (vault)
                        (cons
                         (alist-ref 'uuid vault)
                         (alist-ref 'name vault)))
                      (medea:read-json (run/port (op list vaults)))))
         (templates (map (lambda (template)
                           (cons
                            (alist-ref 'uuid template)
                            (alist-ref 'name template)))
                         (medea:read-json (run/port (op list templates)))))
         (items (medea:read-json (run/port (op list items)))))
    (receive (status success pid)
        (run (pipe (begin
                     (write (sort
                             (map (lambda (item)
                                    (list (alist-ref 'title (alist-ref 'overview item))
                                          (alist-ref (alist-ref 'vaultUuid item) vaults equal?)
                                          (alist-ref (alist-ref 'templateUuid item) templates equal?)))
                                  items)
                             (lambda (a b)
                               (string<? (car a) (car b))))))
                   (gpg --batch --yes -eaq --default-recipient-self -o ,cache-file)))
      (unless success
        (format (current-error-port) "Cache error")
        (exit 1)))))

(define selection
  (run/strings (pipe
                (gpg --batch --yes -qd ,cache-file)
                (begin
                  (let* ((lines (read))
                         ;; Add two to account for zero width space and column space
                         (item-pad (+ 2 (apply max (map (lambda (x) (string-length (car x))) lines))))
                         (vault-pad (+ 2 (apply max (map (lambda (x) (string-length (cadr x))) lines)))))
                    (for-each (lambda (x)
                                (display
                                 (string-append
                                  (string-pad-right (string-append (car x) "\u200b") item-pad)
                                  "<span color=\"#7C6F64\">\u200b"
                                  (string-pad-right (string-append (cadr x) "\u200b") vault-pad)
                                  "</span><span color=\"#83A598\">\u200b"
                                  (caddr x)
                                  "\u200b</span>\n")))
                              lines)))
                (rofi -dmenu "-i" -p 1password -markup-rows -theme-str "* {font: \"Iosevka 8\";}"))))

(when (null? selection) (exit))

(define vault/item (string-split (car selection) "\u200b"))

(define vault (caddr vault/item))
(define item (car vault/item))

(define password ($ (op get item ,item --vault ,vault --fields password)))

(run (pipe (echo ,password) (xclip -sel c -r)))

;; Clear if still on clipboard
(process-fork
 (lambda ()
   (sleep 90)
   (let-values (((xclip-out xclip-in xclip-pid) (process "xclip" '("-sel" "c" "-o"))))
     (let ((clipboard (read-line xclip-out)))
       (when (and (not (eof-object? clipboard))
                  (string=? password clipboard))
         (system "printf '' | xclip -sel c"))))))
