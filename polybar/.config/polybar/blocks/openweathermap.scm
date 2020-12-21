#!/usr/bin/chicken-csi -script
(import (only chicken.format format)
        (only medea read-json)
        (only http-client with-input-from-request)
        (only uri-common make-uri form-urlencoded-separator))

;; This makes the query parameters separated by the '&' char, http-client
;; defaults to ';', which I believe is standards-conforming but isn't very
;; widely used.
(form-urlencoded-separator "&")

(define api-key
  ;; Create an account on openweathermap.org and aftert signing in get an API
  ;; key from: https://home.openweathermap.org/api_keys
  "00000000000000000000000000000000")

(define city-id
  ;; City id can be found here: http://openweathermap.org/find
  ;; Should be an integer
  6254926)

(define units
  ;; "imperial" or "metric"
  "imperial")

(define weather-url
  (make-uri scheme: 'https
            host: "api.openweathermap.org"
            path: '(/ "data" "2.5" "weather")

            query: `((id . ,city-id)
                     (appid . ,api-key)
                     (units . ,units))))

;; Format using polybar's 'lemonbar tags'
(define (polyform text . properties)
  (let loop ((props properties)
             (text text))
    (if (null? props)
        text
        (let* ((prop (car props))
               (tag (substring prop 0 1)))
          (loop (cdr props)
                (format "%{~a}~a%{~a-}" prop text tag)))))
  ;; (format "%{~a~a}~a%{~a-}" tag info text tag)
  )

;; (define (roundint int) (inexact->exact (round int)))
(define roundint (o inexact->exact round))

(define (main)
  (let* ((info (with-input-from-request weather-url #f read-json))
         (temp (roundint (alist-ref 'temp (alist-ref 'main info))))
         (desc (alist-ref 'description (vector-ref (alist-ref 'weather info) 0)))
         ;; (feels-like (roundint (alist-ref 'feels_like (alist-ref 'main info))))
         (hi/lo (format " ~a/~a"
                        (roundint (alist-ref 'temp_max (alist-ref 'main info)))
                        (roundint (alist-ref 'temp_min (alist-ref 'main info))))))
    (display "Weather: ")
    (display temp)
    (display "°")
    (display (polyform hi/lo "T2"))
    (display " ")
    ;; (display (polyform desc "F#DD6F48"))
    (newline)
    ;; (printf "~a ~a~%"
    ;;         temp
    ;;         ;; (get-icon icon)
    ;;         icon)
    ))

(main)
