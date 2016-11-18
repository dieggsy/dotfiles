(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'load-path
			 (expand-file-name "lisp" user-emacs-directory))

(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/")

(package-initialize)

(require 'org) 
(require 'ox)
(require 'cl)  
(setq org-export-async-debug t)
