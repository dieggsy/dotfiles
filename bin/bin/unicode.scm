#!/usr/bin/chicken-scheme
;; AUTOCOMPILE: -O5
(import chicken.format
        icu
        utf8)
(do ((i 32 (add1 i)))
    ((= i 918000))
  (let* ((char (integer->char i))
         (name (char-string-name char)))
    (when name
      (display "U+")
      (display (number->string i 16))
      (display #\tab)
      (display char)
      (display #\tab)
      (display name)
      (newline)
      ;; (printf "U+~a\t~a\t~a\n" (number->string i 16) (string char) name)
      )))
