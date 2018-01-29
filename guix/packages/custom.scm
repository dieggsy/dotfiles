;; -*- geiser-scheme-implementation: guile -*-
(define-module (custom)
  #:use-module (deps)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  ;; emacs
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages web)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages base)
  ;; iosevka
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages node)
  #:use-module (gnu packages fontutils)
  #:use-module (guix build-system gnu)

  #:use-module (gnu packages wm)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages xdisorg)
  #:use-module (srfi srfi-1))

(define custom-hashes
  (read (open-input-file
         (format #f
                 "~a/dotfiles/guix/custom-hashes.scm"
                 (getenv "HOME")))))

(define (get-custom key name)
  (assoc-ref
   (assoc-ref custom-hashes name)
   key))

(define (get-custom-uri name)
  (let ((info (assoc-ref custom-hashes name)))
    (cond ((assoc-ref info 'uri-format)
           (format #f
                   (assoc-ref info 'uri-format)
                   (assoc-ref info 'gitsha)))
          ((assoc-ref info 'github)
           (format #f "https://github.com/~a/~a/archive/~a.zip"
                   (car (assoc-ref info 'github))
                   (cadr (assoc-ref info 'github))
                   (assoc-ref info 'gitsha))))))

(define-public emacs-git
  (package
   (inherit emacs)
   (name "emacs-git")
   (version (git-version
             (get-custom 'version name)
             (get-custom 'rev name)
             (get-custom 'gitsha name)))
   (source (origin
            (method url-fetch)
            (file-name (git-file-name name version))
            (uri (get-custom-uri name))
            (sha256 (base32 (get-custom 'sha256 name)))))
   (inputs
    `(("jansson" ,jansson)
      ("webkitgtk" ,webkitgtk)
      ("lcms" ,lcms)
      ("gpm" ,gpm)
      ("libxcomposite" ,libxcomposite)
      ,@(package-inputs emacs)))
   (native-inputs
    `(("autoconf" ,autoconf)
      ,@(package-native-inputs emacs)))
   (arguments
    (substitute-keyword-arguments
     `(#:tests?
       #f
       #:configure-flags
       '("--with-jansson"
         "--with-modules"
         "--with-xwidgets")
       ,@(package-arguments emacs))
     ((#:phases phases)
      `(modify-phases
        ,phases
        (add-before
         'configure 'autogen
         (lambda _
           (zero? (system* "./autogen.sh"))))))))))

(define-public sxhkd-git
  (package
   (inherit sxhkd)
   (name "sxhkd-git")
   (version (git-version
             (get-custom 'version name)
             (get-custom 'rev name)
             (get-custom 'gitsha name)))
   (source
    (origin
     (file-name (string-append (git-file-name name version) ".zip"))
     (method url-fetch)
     (uri (get-custom-uri name))
     (sha256 (base32 (get-custom 'sha256 name)))))
   (native-inputs
    `(("unzip" ,unzip)
      ,@(package-native-inputs bspwm)))))

(define-public bspwm-git
  (package
   (inherit bspwm)
   (name "bspwm-git")
   (version (git-version
             (get-custom 'version name)
             (get-custom 'rev name)
             (get-custom 'gitsha name)))
   (source
    (origin
     (file-name (string-append (git-file-name name version) ".zip"))
     (method url-fetch)
     (uri (get-custom-uri "bspwm-git"))
     (sha256 (base32 (get-custom 'sha256 "bspwm-git")))))
   (inputs
    `(("sxhkd" ,sxhkd-git)
      ,@(alist-delete "sxhkd" (package-inputs bspwm))))
   (native-inputs
    `(("unzip" ,unzip)
      ,@(package-native-inputs bspwm)))))

;; (define-public font-iosevka-custom
;;   (package
;;    (inherit font-iosevka)
;;    (name "font-iosevka-custom")
;;    (version "1.13.4")
;;    (source (origin
;;             (method url-fetch)
;;             (uri
;;              (string-append "https://github.com/be5invis/Iosevka/archive/v"
;;                             version
;;                             ".tar.gz")
;;              ;; (string-append
;;              ;;  "https://github.com/be5invis/Iosevka/archive/"
;;              ;;  "9df394a4ab6f3c75f0cb0b95ae7ec0c64d531000"
;;              ;;  ".zip")
;;              )
;;             (sha256
;;              (base32 "0harh619djcqx61xvigvgxw2rqb21fmsnx1yz396p1m0bvygk6c2")
;;              ;; (base32 "170508r22w38f1zpsxb4zr7chjgb67ghjq4q2lmlkx7mq21v5730")
;;              )))
;;    (build-system gnu-build-system)
;;    (inputs
;;     `(("node" ,node)
;;       ("ttfautohint" ,ttfautohint)
;;       ("otfcc" ,otfcc)))
;;    (arguments
;;     `(#:tests?
;;       #f
;;       #:make-flags '("custom set=term")
;;       #:phases
;;       (modify-phases
;;        %standard-phases
;;        (delete 'configure)
;;        (add-after
;;         'patch-source-shebangs 'npm
;;         (lambda _
;;           (let ((npm (string-append (assoc-ref %build-inputs "node")
;;                                     "/bin/npm")))
;;             (and
;;              (setenv "HOME" ".")
;;              (zero? (system* npm "set" "strict-ssl" "false"))
;;              (zero? (system* npm "install" "-g"))))))
;;        (add-before
;;         'build 'customize
;;         (lambda _
;;           (zero?
;;            (system*
;;             "make"
;;             "custom-config"
;;             "set=term"
;;             "design='term v-asterisk-low v-l-zshaped v-i-zshaped v-a-singlestorey v-g-singlestorey v-m-shortleg'"
;;             "italic='v-l-zshaped v-i-zshaped v-a-singlestorey v-g-singlestorey v-m-shortleg'"))))
;;        (replace
;;         'install
;;         (lambda* (#:key outputs #:allow-other-keys)
;;           (let* ((font-dir (string-append %output "/share/fonts/truetype")))
;;             (for-each (lambda (file)
;;                         (install-file file font-dir))
;;                       (find-files "dist/iosevka-term/ttf" ""))))))))))
