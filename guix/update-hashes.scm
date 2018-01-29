#!/usr/bin/csi -s
(use posix
     scsh-process
     srfi-13)

(define (assoc-ref lst key)
  (alist-ref key lst equal?))

(define custom-hashes
  (read (open-input-file
         (format
          "~a/dotfiles/guix/custom-hashes.scm"
          (get-environment-variable "HOME")))))



(define (get-custom-uri name #!optional gitsha)
  (let ((info (assoc-ref custom-hashes name)))
    (cond ((assoc-ref info 'uri-format)
           (format
            (assoc-ref info 'uri-format)
            (or gitsha
                (assoc-ref info 'gitsha))))
          ((assoc-ref info 'github)
           (format #f "https://github.com/~a/~a/archive/~a.zip"
                   (car (assoc-ref info 'github))
                   (cadr (assoc-ref info 'github))
                   (or gitsha
                       (assoc-ref info 'gitsha)))))))

(define (get-custom-repo name)
  (let ((info (assoc-ref custom-hashes name)))
    (cond ((assoc-ref info 'repo)
           (assoc-ref info 'repo))
          ((assoc-ref info 'github)
           (format #f "https://github.com/~a/~a.git"
                   (car (assoc-ref info 'github))
                   (cadr (assoc-ref info 'github)))))))

(define (get-custom key name)
  (case key
    ((gitsha-remote)
     (car
      (string-split
       (run/string (git ls-remote ,(get-custom-repo name) master)) "\t")))
    ((sha256-remote)
     (last
      (run/strings
       (guix download ,(get-custom-uri
                        name
                        (get-custom 'gitsha-remote name))))))
    (else
     (assoc-ref
      (assoc-ref custom-hashes name)
      key))))

(define (update-hashes #!optional (port #t))
  (let loop ((pkgs custom-hashes))
    (if (null? pkgs)
        '()
        (let* ((pkg (car pkgs))
               (name (car pkg))
               (info (cdr pkg)))
          ;; (print name)
          (if (not (string= (get-custom 'gitsha name)
                            (get-custom 'gitsha-remote name)))
              (begin
                (format port "Updating: ~a~%" name)
                (cons
                 (cons
                  name
                  (begin
                    (alist-update
                     'sha256
                     (get-custom 'sha256-remote name)
                     (alist-update
                      'gitsha
                      (get-custom 'gitsha-remote name)
                      (alist-update
                       'rev
                       (number->string
                        (+ 1
                           (string->number
                            (get-custom 'rev name))))
                       info)))))
                 (loop (cdr pkgs))))
              (cons pkg
                    (loop (cdr pkgs))))))))
(define (main)
  (let ((port (current-output-port)))
    (with-output-to-file "custom-hashes.scm"
      (lambda () (pp (update-hashes port))))))

(main)
