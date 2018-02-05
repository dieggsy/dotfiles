(use-modules (srfi srfi-88))

(specifications->manifest
 (append
  (map
   (lambda (spec) (keyword->string (car spec)))
   (read (open-input-file
          (format #f
                  "~a/dotfiles/guix/custom-hashes.scm"
                  (getenv "HOME")))))
  '("font-dejavu"
    "glibc-locales"
    "neovim"
    "nss-certs"
    "termite"
    "python-hy"
    "font-weather-icons")))
