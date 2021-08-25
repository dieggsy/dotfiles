#!/usr/bin/chicken-scheme
;; AUTOCOMPILE: -O5
(import (chicken process)
        (chicken process-context)
        (chicken port)
        icu
        utf8)

(define args (command-line-arguments))

(define-values (rofi-out rofi-in rofi-pid)
  (if (member "-rofi" args)
      (process "rofi -dmenu -i -p unicode | cut -d$'\t' -f1 | xclip -r -selection clipboard")
      (values #f #f #f)))

(with-output-to-port (or rofi-in (current-output-port))
  (lambda ()
    (do ((i 32 (add1 i)))
        ((= i 918000))
      (let* ((char (integer->char i))
             (name (char-string-name char)))
        (when name
          (display char)
          (display #\tab)
          (display "U+")
          (display (number->string i 16))
          (display #\tab)
          (display name)
          (newline))))))
