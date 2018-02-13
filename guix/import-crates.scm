#!/usr/bin/csi -s
(use scsh-process
     irregex
     srfi-13)

(define (get-deps name)
  (let* ((short-name (if (string-prefix? "rust-" name)
                         (string-drop name 5)
                         name))
         (package (run/string (pipe (guix import crate ,short-name) (tail -n+2)))))
    (values
     (call-with-input-string package read)
     (remove
      (cut string= <> name)
      (delete-duplicates
       (map
        (cut string-trim-both <> #\")
        (irregex-extract
         "\"(rust-.*?)\""
         package)))))))

(define args (command-line-arguments))

(define (fetch-crates name)
  (let loop ((deps (list name))
             (seen '()))
    (cond ((null? deps)
           #f)
          ((member (car deps) seen)
           (loop (cdr deps)
                 seen))
          (else
           (let-values (((package new-deps) (get-deps (car deps))))
             (when (or (null? args)
                       (= (length new-deps) (string->number (car args))))
               (pp  (list 'define-public
                          (string->symbol (last (cadr package)))
                          package)))
             (let ((new-unseen (remove (cut member <> seen) new-deps)))
               (loop (append (cdr deps) new-unseen)
                     (cons (car deps) seen))))))))

(fetch-crates "rust-ripgrep")
