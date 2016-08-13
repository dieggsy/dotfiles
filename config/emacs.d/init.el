;; init.el -- My Emacs configuration
;-*-Emacs-Lisp-*-

;;; Commentary:
;;
;; Idk, man.
;;
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path
			 (expand-file-name "lisp" user-emacs-directory))

;; Add directories in "lisp" folder
(let ((files (directory-files-and-attributes "~/.emacs.d/lisp" t)))
  (dolist (file files)
	(let ((filename (car file))
		  (dir (nth 1 file)))
	  (when (and dir
				 (not (string-suffix-p "." filename)))
		(add-to-list 'load-path (car file))))))

(add-to-list 'exec-path "/usr/local/bin")
(require 'init-utils)
(require 'init-elpa)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
    (require 'use-package))

;; Good stuff.
(setq inhibit-splash-screen t
	  inhibit-startup-message t
	  inhibit-startup-echo-area-message t)
(menu-bar-mode 0)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(show-paren-mode 1)
(electric-pair-mode)
(add-hook 'minibuffer-setup-hook (lambda ()
								   (electric-pair-mode)))
(setq display-time-load-average nil)
(setq cursor-type 'bar)
(fringe-mode 0)
(add-to-list 'default-frame-alist '(font . "Menlo-10"))
(setq scroll-conservatively 10000)
(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))
(setq completion-cycle-threshold t)

; Store backups in .emacsbackups, autosaves in temp dir
(defvar backup-dir "~/.emacsbackups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))

;; Thanks, @aaronbieber
(require 'init-global-functions)

(require 'init-evil)

;; Org prerequisites
(use-package visual-fill-column
	 :ensure t)
(require 'init-org)

;; Packages
(use-package company
  ;; Supposedly better than autocomplete, but idk...
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (use-package company-jedi
	;; Not sure this is actually working for me
	:ensure t
	:config
	(defun my/python-mode-hook ()
	  (add-to-list 'company-backends 'company-jedi))
	(add-hook 'python-mode-hook 'my/python-mode-hook)))


(use-package dictionary
  ;; Dictionary search!
  :ensure t)

(use-package magit
  ;; Git (but cooler) for emacs
  :ensure t)


(use-package sphinx-doc
  ;; Sorta neat, though not entirely complete IMO
  :ensure t
  :config
  (add-hook 'python-mode-hook
			(lambda ()
			  (require 'sphinx-doc)
			  (sphinx-doc-mode t))))


(use-package vimish-fold
  :ensure t)


(use-package ag
  ;; Silver searcher
  :ensure t
  :defer t
  :init
  (use-package wgrep-ag  
	;; Guess I need this first
	:ensure t
	:commands (wgrep-ag-setup))
  :config
  (add-hook 'ag-mode-hook
			(lambda ()
			  (wgrep-ag-setup)
			  (define-key ag-mode-map (kbd "n") 'evil-search-next)
			  (define-key ag-mode-map (kbd "N") 'evil-search-previous)))
  (setq ag-executable "/usr/local/bin/ag")
  (setq ag-highlight-search t)
  (setq ag-reuse-buffers t)
    (setq ag-reuse-window t))


(use-package helm
  ;; Kind of an Alfred for emacs
  :ensure t
  :init
  (require 'helm-config)
  :config
  (use-package helm-descbinds
	;; To describe keys in a nicer way
	:ensure t)
  (use-package helm-projectile
	;; To use with projectile
	:ensure t
	:config
	(projectile-global-mode))
  ;; (use-package helm-ag
  ;; 	:ensure t)
  (helm-mode 1)
  (helm-autoresize-mode t)
  (global-set-key (kbd "M-x") 'helm-M-x)  
  (global-set-key (kbd "<f1>") 'helm-find-files)  
  (global-set-key (kbd "<f2>") 'helm-mini)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-buffer-max-length 40)
  (setq helm-locate-command "mdfind -name %s %s") ; Use spotlight for search
  (global-set-key (kbd "M-y") 'helm-show-kill-ring))


;; (use-package jedi
;;   ;; Hasn't been working smoothly recently
;;   :ensure t
;;   :init
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t))

;; (use-package elpy
;;   ;; Eh, I don't know...
;;   :ensure t
;;   :config
;;   (elpy-enable)
;;   (setq elpy-rpc-backend "jedi"))

(use-package markdown-mode
  :ensure t)

(use-package markdown-preview-mode
  :ensure t)

;; (use-package yafolding
;;   ;; Man, good code folding is hard to come by in emacs
;;   ;; This one's ok, but there are a couple know issues that
;;   ;; don't quite make it worth it, I think.
;;   :ensure t
;;   :config
;;   (defun air--yafolding-kbd ()
;; 	(local-set-key (kbd "C-c <up>") 'yafolding-hide-all)
;; 	(local-set-key (kbd "C-c <down>") 'yafolding-show-all)
;; 	(local-set-key (kbd "C-c <left>") 'yafolding-hide-element)
;; 	(local-set-key (kbd "C-c <right>") 'yafolding-show-element)
;; 	(local-set-key [C-tab] 'yafolding-toggle-element))
;;   (add-hook 'python-mode-hook 'yafolding-mode)
;;   (add-hook 'python-mode-hook 'air--yafolding-kbd))

(use-package yasnippet
  ;; SNIPPETS!!!
  :ensure t
  :config
  (yas-global-mode 1))

;; (use-package auto-complete
;;   ;; Supposedly not as good as company mode
;;   :ensure t
;;   :config
;;   (global-auto-complete-mode t))


(use-package csv-mode
  ;; I'll give this a shot
  :ensure t)

(use-package flycheck
  ;; Pep8 check, basically
  :ensure t
  :config
  (global-set-key (kbd "<f11>") 'flycheck-mode))

(use-package multi-term
  ;; Supposed to be nicer than ansi-term
  :ensure t)

;; (use-package autopair
;;   ;; Recommended to use electric-pair now?
;;   :ensure t
;;   :config
;;   (autopair-global-mode))

(use-package fill-column-indicator
  ;; I like a line length limit indicator in Python
  :ensure t
  :init
  (setq-default fci-rule-column 79)
  (global-set-key (kbd "<f10>") 'fci-mode)
  (add-hook 'python-mode-hook 'fci-mode))

(use-package rainbow-delimiters
  ;; Better parentheses coloring
  :ensure t
  :init
  (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; (use-package smart-mode-powerline-theme
;;   :ensure t)

;; (use-package smart-mode-line
;;   :ensure t
;;   :config
;;   (setq sml/no-confirm-load-theme t)
;;   (setq sml/theme 'dark)
;;   (setq rm-whitelist '(""))
;;   (setq system-uses-terminfo nil)
;;   (sml/setup)
;;   (display-time-mode)
;;   (display-time-update)
;;   (fancy-battery-mode)
;;   (setq fancy-battery-show-percentage t))

;; (use-package powerline
;;   :ensure t
;;   :init
;;   (setq powerline-default-separator nil)
;;   :config
;;   (powerline-evil-vim-color-theme))

;; (use-package powerline-evil
;;   :ensure t)

(use-package fancy-battery
  ;; Something something battery
  :ensure t
  :config
  (fancy-battery-mode)
  (setq fancy-battery-show-percentage t)
  (fancy-battery-update))

(use-package spaceline
  ;; Similar to vim's powerline, this one looks clean
  ;; and 'just works', to an extent
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode)
  (spaceline-toggle-minor-modes-off)
  (spaceline-toggle-battery-on)
  (spaceline-toggle-buffer-size-off)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (set-face-background 'spaceline-evil-normal "#afd700")
  (set-face-foreground 'spaceline-evil-normal "#005f00")
  (set-face-background 'spaceline-evil-insert "#0087af")
  (set-face-foreground 'spaceline-evil-insert "white")
  (set-face-background 'spaceline-evil-visual "#ff8700")
  (set-face-foreground 'spaceline-evil-visual "#870000"))

(use-package highlight-numbers
  ;; Neat-o
  :ensure t
  :init
  (add-hook 'python-mode-hook 'highlight-numbers-mode))

(use-package imenu-anywhere
  ;; Imenu on steroids
  :ensure t
  :config
  (global-set-key (kbd "<f5>") 'imenu-anywhere))

(use-package highlight-parentheses
  ;; Make parenthesis I'm currently in stand out
  :ensure t)



;;; Custom Key Bindings ;;;
;; (global-set-key (kbd "<f8> <f1>")
;; 				'(lambda () (interactive) (ansi-term "/usr/local/bin/ipython")))
;; (global-set-key (kbd "<f8> <f2>")
;; 				'(lambda () (interactive) (ansi-term "/Users/diego/.virtualenvs/py2/bin/ipython")))
;; (global-set-key (kbd "<f9>") 'linum-mode)
;; (global-set-key (kbd "M-RET") 'python-shell-send-buffer)

;; Global stuff
(global-hl-line-mode 1)
;; (global-linum-mode 1)
(setq linum-delay t)
(global-auto-revert-mode t)
(setq whitespace-style '(face trailing))
(setq column-number-mode t)

;; For matlab, I guess
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;; Python stuff
(defun ipython ()
  (interactive)
  (ansi-term "/usr/local/bin/ipython"))
(defun ipython2()
  (interactive)
  (ansi-term "/Users/diego/.virtualenvs/py2/bin/ipython"))
(add-hook 'python-mode-hook 'highlight-parentheses-mode)
(add-hook 'python-mode-hook 'hs-minor-mode)
;;(add-hook 'python-mode-hook 'yafolding-mode)
(add-hook 'python-mode-hook 'yas-minor-mode)
(add-hook 'python-mode-hook
		  (lambda () (set (make-local-variable 'comment-inline-offset) 2)))
(add-hook 'python-mode-hook (lambda () (setq tab-width 4)))
(add-hook 'python-mode-hook (lambda () (linum-mode 1)))
(setenv "PYTHONPATH" "/usr/local/bin/python3")

;; Emacs-lisp stuff
(defun my-lisp-mode-config ()
  (setq ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers))
  (local-set-key (kbd "C-c <up>") 'hs-hide-all)
  (local-set-key (kbd "C-c <down>") 'hs-show-all)
  (local-set-key (kbd "C-c <left>") 'hs-hide-block)
  (local-set-key (kbd "C-c <right>") 'hs-show-block))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-config)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook (lambda () (linum-mode 1)))

;; Disable fci mode when autocomplete popup menu happens
(defun sanityinc/fci-enabled-p ()
    (and (boundp 'fci-mode) fci-mode))
(defvar sanityinc/fci-mode-suppressed nil)
(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
	(when fci-enabled
	  (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-enabled)
	  (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
			 (null popup-instances))
	(setq sanityinc/fci-mode-suppressed nil)
	(turn-on-fci-mode)))

;; Highlight curent line number 
(defface my-linum-hl
  `((t :inherit linum :background ,(face-background 'hl-line nil t)))
  "Face for the current line number."
  :group 'linum)

(defvar my-linum-format-string "%3d")

(add-hook 'linum-before-numbering-hook 'my-linum-get-format-string)

(defun my-linum-get-format-string ()
  (let* ((width (1+ (length (number-to-string
							 (count-lines (point-min) (point-max))))))
		 (format (concat "%" (number-to-string width) "d ")))
	(setq my-linum-format-string format)))

(defvar my-linum-current-line-number 0)

(setq linum-format 'my-linum-format)

(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face
			  (if (eq line-number my-linum-current-line-number)
				  'my-linum-hl
				'linum)))

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
	ad-do-it))
(ad-activate 'linum-update)
(add-hook 'term-mode-hook (lambda ()
							(setq-local global-hl-line-mode
										nil)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#303030" :foreground "#d0d0d0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(column-marker-1 ((t (:background "#7f7f7f"))))
 '(comint-highlight-prompt ((t nil)))
 '(company-preview ((t (:inherit default :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "white" :foreground "black"))))
 '(company-tooltip-selection ((t (:background "color-75"))))
 '(eww-form-textarea ((t (:foreground "#000000" :box 1))))
 '(font-lock-builtin-face ((t (:foreground "#56C0C2"))))
 '(font-lock-comment-face ((t (:foreground "#6c6c6c" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#56C0C2"))))
 '(font-lock-function-name-face ((t (:foreground "#5fafff"))))
 '(font-lock-keyword-face ((t (:foreground "#C678DD" :weight normal))))
 '(font-lock-string-face ((t (:foreground "#87d787"))))
 '(font-lock-variable-name-face ((t (:foreground "#D19A66"))))
 '(helm-ff-file ((t (:foreground "#d0d0d0"))))
 '(helm-match ((t (:foreground "brightred"))))
 '(helm-selection ((t (:background "#3a3a3a" :distant-foreground "black"))))
 '(hl-line ((t (:background "#3a3a3a"))))
 '(linum ((t (:foreground "#4e4e4e"))))
 '(my-linum-hl ((t (:background "#3a3a3a" :foreground "#ff0000"))))
 '(powerline-active1 ((t (:inherit mode-line :background "#262626" :foreground "color-247"))))
 '(powerline-active2 ((t (:inherit mode-line :background "#262626"))))
 '(region ((t (:background "#4e4e4e"))))
 '(sh-quoted-exec ((t (:foreground "#af5fff"))))
 '(sml/battery ((t nil)) t)
 '(sml/col-number ((t (:inherit sml/global))))
 '(sml/time ((t (:foreground "#af5f00")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-disable-faces (quote (font-lock-comment-face font-lock-doc-face)))
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-format "%a %d %b  %H:%M ")
 '(elpy-modules
   (quote
	(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(fill-column 79)
 '(helm-boring-buffer-regexp-list
   (quote
	("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*epc")))
 '(org-babel-load-languages (quote ((python . t) (emacs-lisp . t))))
 '(org-blank-before-new-entry (quote ((heading) (plain-list-item))))
 '(org-confirm-babel-evaluate nil)
 '(org-src-fontify-natively t)
 '(powerline-evil-tag-style (quote verbose))
 '(projectile-globally-ignored-files (quote ("TAGS" ".DS_Store")))
 '(sml/name-width 40)
 '(sml/replacer-regexp-list
   (quote
	(("^~/org/" ":Org:")
	 ("^~/\\.emacs\\.d/elpa/" ":ELPA:")
	 ("^~/\\.emacs\\.d/" ":ED:")
	 ("^/sudo:.*:" ":SU:")
	 ("^~/Documents/" ":Doc:")
	 ("^~/Dropbox/" ":DB:")
	 ("^:\\([^:]*\\):Documento?s/" ":\\1/Doc:")
	 ("^~/[Gg]it/" ":Git:")
	 ("^~/[Gg]it[Hh]ub/" ":Git:")
	 ("^~/[Gg]it\\([Hh]ub\\|\\)-?[Pp]rojects/" ":Git:")
	 ("\"^~/Dropbox \\(MIT\\)/\"" "\":DB:\""))))
 '(vc-follow-symlinks t))
