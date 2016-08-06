;;; Find and load the correct package.el

;; When switching between Emacs 23 and 24, we always use the bundled package.el in Emacs 24
(let ((package-el-site-lisp-dir
	   (expand-file-name "site-lisp/package" user-emacs-directory)))
  (when (and (file-directory-p package-el-site-lisp-dir)
			 (> emacs-major-version 23))
	(message "Removing local package.el from load-path to avoid shadowing bundled version")
	(setq load-path (remove package-el-site-lisp-dir load-path))))

(require 'package)


;;; Standard package repositories
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpy" . "https://jorgenschaefer.github.io/packages/"))

;;; Pin some packages to specific repositories.
(setq package-pinned-packages '((gtags . "marmalade")))

;; If gpg cannot be found, signature checking will fail, so we
;; conditionally enable it according to whether gpg is available. We
;; re-run this check once $PATH has been configured
(defun sanityinc/package-maybe-enable-signatures ()
  (setq package-check-signature (when (executable-find "gpg") 'allow-unsigned)))

(sanityinc/package-maybe-enable-signatures)
(after-load 'init-exec-path
			(sanityinc/package-maybe-enable-signatures))


;;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
    "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
	(if (package-installed-p package min-version)
		t
	  (if (or (assoc package package-archive-contents) no-refresh)
		  (package-install package)
		(progn
		  (package-refresh-contents)
		  (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
    "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
	(condition-case err
		(require-package package min-version no-refresh)
	  (error
	   (message "Couldn't install package `%s': %S" package err)
	   nil)))


;;; Fire up package.el
(setq package-enable-at-startup nil)
(package-initialize)

(provide 'init-elpa)
