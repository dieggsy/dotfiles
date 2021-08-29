#!/usr/bin/chicken-scheme
;; AUTOCOMPILE: -O5

(import (chicken format)
        (chicken string)
        (chicken file)
        (chicken pathname)
        (chicken process-context)
        (chicken process)
        (chicken port)
        utf8)

(define power-dir "/sys/class/power_supply")
(define charging (not (zero? (call-with-input-file (make-pathname (list power-dir "AC") "online") read))))
(define args (command-line-arguments))

(define-values (rofi-out rofi-in rofi-pid)
  (if (member "-rofi" args)
      (process "rofi" '("-dmenu" "-theme" "menu"
                        "-markup-rows" "-theme-str"
                        "#inputbar {enabled:false;}"
                        "-theme-str" "* {font: \"Iosevka Term 8\";}") )
      (values #f #f #f)))

(define (print-battery-level name energy-now energy-full)
  (let* ((ratio (exact->inexact (/ energy-now energy-full)))
         (percent (inexact->exact (round (* 100 ratio))))
         (battery-color (cond (charging "color=\"#B8BB26\"")
                              ((< percent 20) "color=\"#FB4933\"" )
                              (else "")))
         (batt-length 12)
         (boxes-color (inexact->exact (round (* batt-length ratio))))
         (boxes-gray (- batt-length boxes-color)))
    (display name)
    (display #\space)
    (display (string-append
              (if (member "-rofi" args)
                  (format "<span ~a>" battery-color)
                  "")
              (make-string boxes-color #\━)
              (if (member "-rofi" args)
                  "</span>"
                  "")))
    (display (string-append
              (if (member "-rofi" args)
                  "<span color=\"#7C6F64\">"
                  "")
              (make-string boxes-gray #\┉)
              (if (member "-rofi" args)
                  "</span>"
                  "")))
    (display #\space)
    (print percent)))

(with-output-to-port (or rofi-in (current-output-port))
  (lambda ()
    (let loop ((files (directory power-dir))
               (num 0)
               (denom 0))
      (cond ((null? files)
             ;; (newline)
             (print-battery-level " ALL" num denom))
            ((and-let* ((filename (car files))
                        ((> (string-length filename) 3))
                        ((substring-ci=? "BAT" filename 0 0 3)))
               (let* ((energy-now (call-with-input-file (make-pathname (list power-dir filename) "energy_now") read))
                      (energy-full (call-with-input-file (make-pathname (list power-dir filename) "energy_full") read)))
                 (print-battery-level filename energy-now energy-full)
                 (loop (cdr files) (+ energy-now num) (+ energy-full denom)))))
            (else
             (loop (cdr files) num denom))))))


;; (process-wait rofi)
