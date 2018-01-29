(define-module (deps)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages build-tools)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public premake5
  (package
   (inherit premake4)
   (name "premake")
   (version "5.0")
   (source (origin
            (method url-fetch)
            (uri
             "https://github.com/premake/premake-core/releases/download/v5.0.0-alpha12/premake-5.0.0-alpha12-src.zip")
            (sha256
             (base32
              "0lb7ad9qsf0i69y05ak118yar9im97x7y4drw8ilw0h0n7ssk92z"))))
   (arguments
    (substitute-keyword-arguments
     (package-arguments premake4)
     ((#:phases phases)
      `(modify-phases
        ,phases
        (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (install-file "../../bin/release/premake5"
                                 (string-append (assoc-ref outputs "out") "/bin"))
                   #t))))))))

(define-public otfcc
  (package
   (name "otfcc")
   (version "0.9.6")
   (source (origin
            (method url-fetch)
            (uri
             (string-append
              "https://github.com/caryll/otfcc/archive/v"
              version
              ".tar.gz"))
            (sha256
             (base32
              "0hlm0xvqzmrzxi5navnynv3c8hm37zflzv6z2y2lh8g2h8nvpq81"))))
   (build-system gnu-build-system)
   (inputs `(("premake5" ,premake5)))
   (arguments
    `(#:tests?
      #f
      #:make-flags '("config=release_x64")
      #:phases
      (modify-phases
       %standard-phases
       (delete 'configure)
       (add-before
        'build 'premake
        (lambda _
          (let ((premake5 (string-append (assoc-ref %build-inputs "premake5")
                                         "/bin/premake5")))
            (and
             (zero? (system* premake5 "gmake"))
             (chdir "build/gmake")
             (setenv "CC" "gcc")
             #t))))
       (replace
        'install
        (lambda* (#:key outputs #:allow-other-keys)
          (and
           (chdir "../../")
           (install-file "bin/release-x64/otfccdump"
                         (string-append %output "/bin"))
           (install-file "bin/release-x64/otfccbuild"
                         (string-append %output "/bin"))
           #t))))))
   (home-page "https://github.com/caryll/otfcc")
   (synopsis "Optimized OpenType builder and inspector.")
   (description "The otfcc is a C library and utility used for parsing and
writing OpenType font files.")
   (license license:asl2.0)))
