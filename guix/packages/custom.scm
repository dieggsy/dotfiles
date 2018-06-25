;; -*- geiser-scheme-implementation: guile -*-
(read-enable 'r7rs-symbols)
(define-module (custom)
  #:use-module (deps)
  #:use-module ((guix licenses) #:prefix |license:|)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-88)
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
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system font)
  #:use-module (gnu packages python))

(define custom-hashes
  (read (open-input-file
         (format #f
                 "~a/dotfiles/guix/custom-hashes.scm"
                 (getenv "HOME")))))

(define (get-custom key name)
  (assoc-ref
   (assoc-ref custom-hashes (string->keyword name))
   key))

(define (get-custom-uri name)
  (let ((info (assoc-ref custom-hashes (string->keyword name))))
    (cond ((assoc-ref info 'uri-format)
           (format #f
                   (assoc-ref info 'uri-format)
                   (assoc-ref info 'gitsha)))
          ((assoc-ref info 'github)
           (format #f "https://github.com/~a/~a/archive/~a.zip"
                   (car (assoc-ref info 'github))
                   (cadr (assoc-ref info 'github))
                   (assoc-ref info 'gitsha))))))

(define (get-custom-repo name)
  (let ((info (assoc-ref custom-hashes (string->keyword name))))
    (cond ((assoc-ref info 'repo)
           (assoc-ref info 'repo))
          ((assoc-ref info 'github)
           (format #f "https://github.com/~a/~a.git"
                   (car (assoc-ref info 'github))
                   (cadr (assoc-ref info 'github)))))))

(define (custom-git-version name)
  (git-version
   (get-custom 'version name)
   (get-custom 'rev name)
   (get-custom 'gitsha name)))

(define-public emacs-git
  (package
    (inherit emacs)
    (name "emacs-git")
    (version (custom-git-version name))
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
           ;; #:make-flags
           ;; '("bootstrap")
           #:configure-flags
           '("--with-jansson"
             "--with-modules"
             "--with-xwidgets")
           ,@(package-arguments emacs))
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'autogen
             (lambda _
               (zero? (system* "./autogen.sh"))))))))))

(define-public font-weather-icons
  (package
    (name "font-weather-icons")
    (version "2.0.10")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://github.com/erikflowers/weather-icons/archive/"
                    version
                    ".zip"))
              (sha256
               (base32
                "0hgqiry1xgfmbr84aj3941bnljr7vv0igyk496dyxpg3vmsrq0b6"))))
    (build-system font-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'chdir
           (lambda _
             (chdir (string-append "weather-icons-" ,version "/font")))))))
    (home-page "https://erikflowers.github.io/weather-icons/")
    (synopsis "215 Weather Themed Icons and CSS")
    (description
     "Weather Icons is the only icon font and CSS with 222 weather themed icons,
ready to be dropped right into Bootstrap, or any project that needs high
quality weather, maritime, and meteorological based icons!")
    (license #f)))

(define-public font-symbola
  (package
    (name "font-symbola")
    (version "11.00")
    (source (origin
              (method url-fetch/zipbomb)
              (uri "http://users.teilar.gr/~g1951d/Symbola.zip")
              (sha256
               (base32 "181qsvfc20q2pypfj4qn53jilr62248nzbanfq4q3mzxcn0rg24k"))))
    (build-system font-build-system)
    (home-page "http://users.teilar.gr/~g1951d/")
    (synopsis "Font for unicode symbols")
    (description #f)
    (license #f)))

(define-public font-sarasa-gothic
  (package
    (name "font-sarasa-gothic")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/be5invis/Sarasa-Gothic/releases/download/v"
                           version
                           "/sarasa-gothic-ttf-"
                           version
                           ".7z"))
       (sha256
        (base32 "18ycw57k7yhrbb8njzhzk6x32xnjal61wr48qxkqy4lh9zfy0p22"))))
    (build-system font-build-system)
    (native-inputs `(("p7zip" ,p7zip)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (and (zero? (system* "7za" "e" source))))))))
    (home-page "https://github.com/be5invis/Sarasa-Gothic/")
    (synopsis "This is SARASA GOTHIC, a Chinese & Japanese programming font
based on Iosevka and Source Han Sans.")
    (description #f)
    (license license:bsd-3)))
