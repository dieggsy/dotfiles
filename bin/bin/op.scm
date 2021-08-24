#!/usr/bin/chicken-scheme
(import (chicken process)
        (chicken process-context)
        (only (chicken io) read-line)
        (only (chicken file posix) file-modification-time)
        (only (chicken file) file-exists?)
        (only (chicken time) current-seconds)
        (only (chicken pretty-print) pp)
        (only (chicken string) string-split ->string)
        (prefix medea medea:))

(define args (command-line-arguments))

(define env-file "/tmp/op-env")
(define cache-file "/tmp/op-cache")

(medea:json-parsers (cons
                     (cons 'array (lambda (x) x))
                     (medea:json-parsers)))

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
    (call-with-output-file env-file
      (lambda (port) (display (read-line auth-out) port)))))

(define op-env (call-with-input-file env-file read-line))

(define (op args)
  (apply string-append op-env " && op " (intersperse (map ->string args) " ")))

(when (or (not (file-exists? cache-file))
          (< (file-modification-time cache-file)
             (- (current-seconds)
                (* 24 60 60)))
          (member  "-recache" args))
  (let*-values (((vaults-out vaults-in vaults-pid)
                 (process (op '(list vaults))))
                ((items-out items-in items-pid)
                 (process (op '(list items))))
                ((vaults) (map
                           (lambda (vault)
                             (cons
                              (cdar vault)
                              (cdadr vault)))
                           (medea:read-json vaults-out))))
    (let*-values (((pid normal vaults-status) (process-wait vaults-pid))
                  ((pid normal items-status) (process-wait items-pid)))
      (unless (and (zero? vaults-status) (zero? items-status))
        (display "Error getting vaults or items")
        (exit 1)))
    (call-with-output-file cache-file
      (lambda (port)
        (map
         (lambda (item)
           (display (alist-ref (alist-ref 'vaultUuid item) vaults equal?) port)
           (display "/" port)
           (display (alist-ref 'title (alist-ref 'overview item)) port)
           (newline port))
         (medea:read-json items-out))))))

(define-values (rofi-out rofi-in rofi-pid)
  (process "rofi -dmenu -i -p '1password'"))

(call-with-input-file cache-file
  (lambda (port)
    (let loop ((line (read-line port)))
      (unless (eof-object? line)
        (display line rofi-in)
        (newline rofi-in)
        (loop (read-line port))))
    (close-output-port rofi-in)))

(define pwd)

(define selection (read-line rofi-out))

(when (eof-object? selection)
  (exit))

(define vault/item (string-split selection "/"))

(define vault (car vault/item))
(define item (cadr vault/item))

(define-values (xclip-out xclip-in xclip-pid) (process "xclip" '("-sel" "c")))

(define-values (op-out op-in op-pid)
  (process (op `(get item ,(string-append "'" item "'") --vault ,vault --fields password))))

(define password (read-line op-out))

(display password xclip-in)
(close-output-port xclip-in)

;; Delete if still on clipboard
(process-fork
 (lambda ()
   (sleep 90)
   (let-values (((xclip-out xclip-in xclip-pid) (process "xclip" '("-sel" "c" "-o"))))
     (when (string=? password (read-line xclip-out))
       (system "printf '' | xclip -sel c")))))
