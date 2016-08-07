;;; init.el -- My Emacs configuration
;-*-Emacs-Lisp-*-

;;; Commentary:
;;
;; Idk, man.
;;
;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

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

;; Essential settings.
(setq inhibit-splash-screen t
	  inhibit-startup-message t
	  inhibit-startup-echo-area-message t)
(menu-bar-mode 0)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(show-paren-mode 1)
(setq display-time-load-average nil)
(setq cursor-type 'bar)
(fringe-mode 0)
(add-to-list 'default-frame-alist '(font . "Menlo-10"))
(setq scroll-conservatively 10000)

; Store backups/autosaves in temp dir
(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
(setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))

(require 'init-global-functions)

(require 'init-evil)

(use-package dictionary :ensure t)

(use-package magit
  :ensure t)

;; (use-package company
;;   :ensure t
;;   :config
;;   (add-hook 'after-init-hook 'global-company-mode))

(use-package wgrep-ag
  :ensure t
  :commands (wgrep-ag-setup))

(use-package ag
  :ensure t
  :defer t
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

;; (use-package smooth-scrolling
;;   :ensure t
;;   :config
;;   (smooth-scrolling-mode 1)
;;   (setq smooth-scroll-margin 1))

(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-mode-fuzzy-match t)
  (setq helm-buffer-max-length 40)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring))

;; (use-package openwith
;; 	:ensure t
;;   :config
;;   (setq openwith-associations '(("\\.pdf\\'" "preview" (file)))

(use-package jedi
  :ensure t
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (setq jedi:complete-on-dot t))

;; (use-package elpy
;;   :ensure t
;;   :config
;;   (elpy-enable)
;;   (setq elpy-rpc-backend "jedi"))
(use-package markdown-mode
  :ensure t)
(use-package markdown-preview-mode
  :ensure t)
(use-package yafolding
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Set Yasnippet's key binding to shift+tab
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
  (yas-global-mode 1))

(use-package flycheck
  :ensure t
  :config
  (global-set-key (kbd "<f11>") 'flycheck-mode))

(use-package multi-term
  :ensure t)

(use-package autopair
  :ensure t
  :config
  (autopair-global-mode))

(use-package fill-column-indicator
  :ensure t
  :init
  (setq-default fci-rule-column 79)
  (global-set-key (kbd "<f10>") 'fci-mode)
  (add-hook 'python-mode-hook 'fci-mode))

(use-package rainbow-delimiters
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
  :ensure t
  :config
  (fancy-battery-update))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
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
  :ensure t
  :init
  (add-hook 'python-mode-hook 'highlight-numbers-mode))

(use-package imenu-anywhere
  :ensure t
  :config
  (global-set-key (kbd "<f5>") 'imenu-anywhere))

(use-package highlight-parentheses
  :ensure t)


(setq-default tab-width 4)
(setq tab-stop-list (number-sequence 4 200 4))
(setq completion-cycle-threshold t)

;;; Custom Key Bindings ;;;
(global-set-key (kbd "<f1>") 'helm-find-files)
(global-set-key (kbd "<f2>") 'switch-to-buffer)
(global-set-key (kbd "<f8> <f1>")
				'(lambda () (interactive) (ansi-term "/usr/local/bin/ipython")))
(global-set-key (kbd "<f8> <f2>")
				'(lambda () (interactive) (ansi-term "/Users/diego/.virtualenvs/py2/bin/ipython")))
(global-set-key (kbd "<f9>") 'linum-mode)
(global-set-key (kbd "M-RET") 'python-shell-send-buffer)

;; Global stuff
(global-hl-line-mode 1)
(global-linum-mode 1)
(setq linum-delay t)
(global-auto-revert-mode t)
(setq whitespace-style '(face trailing))
(setq column-number-mode t)

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
(setenv "PYTHONPATH" "/usr/local/bin/python3")

(setq org-log-done 'time)
(setq org-log-done 'note)

;; Emacs-lisp stuff
(defun my-lisp-mode-config ()
  (setq ac-sources '(ac-source-symbols ac-source-words-in-same-mode-buffers))
  (local-set-key (kbd "C-c <up>") 'hs-hide-all)
  (local-set-key (kbd "C-c <down>") 'hs-show-all)
  (local-set-key (kbd "C-c <left>") 'hs-hide-block)
  (local-set-key (kbd "C-c <right>") 'hs-show-block))

(global-auto-complete-mode t)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-config)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)

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


;;; HIGHLIGHT CURRENT LINE ;;;
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
 '(default ((t (:inherit nil :stipple nil :background "#262626" :foreground "#d0d0d0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(column-marker-1 ((t (:background "#7f7f7f"))))
 '(comint-highlight-prompt ((t nil)))
 '(eww-form-textarea ((t (:foreground "#000000" :box 1))))
 '(font-lock-builtin-face ((t (:foreground "#56C0C2"))))
 '(font-lock-comment-face ((t (:foreground "#6c6c6c" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#56C0C2"))))
 '(font-lock-function-name-face ((t (:foreground "#61AFEF"))))
 '(font-lock-keyword-face ((t (:foreground "#C678DD" :weight normal))))
 '(font-lock-string-face ((t (:foreground "#87d787"))))
 '(font-lock-variable-name-face ((t (:foreground "#D19A66"))))
 '(helm-match ((t (:foreground "brightred"))))
 '(helm-selection ((t (:background "#303030" :distant-foreground "black"))))
 '(hl-line ((t (:background "#303030"))))
 '(linum ((t (:foreground "#4e4e4e"))))
 '(my-linum-hl ((t (:inherit linum :background "#303030" :foreground "#ff0000"))))
 '(region ((t (:background "#3a3a3a"))))
 '(sh-quoted-exec ((t (:foreground "#af5fff"))))
 '(sml/battery ((t nil)) t)
 '(sml/col-number ((t (:inherit sml/global))))
 '(sml/time ((t (:foreground "#af5f00")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-format "%a %d %b  %H:%M ")
 '(elpy-modules
   (quote
	(elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(fill-column 79)
 '(powerline-evil-tag-style (quote verbose))
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
