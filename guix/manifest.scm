(use-modules (srfi srfi-88))

(specifications->manifest
 (append
  (map
   (lambda (spec)
     (keyword->string (car spec)))
   (read (open-input-file
          (format #f
                  "~a/dotfiles/guix/custom-hashes.scm"
                  (getenv "HOME")))))
  '("font-dejavu"
    "font-sarasa-gothic"
    "font-symbola"
    "font-weather-icons"
    "glibc-locales"
    "nss-certs")))
