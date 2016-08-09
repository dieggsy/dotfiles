;;; init-evil.el -- My evil mode configuration.
;;; Commentary:
;;; Code:
(defun air--config-evil-leader ()
  "Configure evil leader mode."
  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states 1)
  (evil-leader/set-key
	"."  'switch-to-previous-buffer
	":"  'eval-expression
	"<right>" 'other-window
	"B"  'magit-blame-toggle
	"D"  'kill-this-buffer
	"b"  'helm-mini	;; Switch to another buffer
	"c"  'comment-dwim
	"d"  'delete-trailing-whitespace
	"f" 'helm-find
	"i"  'helm-imenu-anywhere ;; Jump to function in buffer
	"la" (lambda () (interactive) (linum-mode) (fci-mode))
	"lc" 'fci-mode
	"lf" 'flycheck-mode      
	"ll" 'linum-mode
	"lw" 'whitespace-mode ;; Show invisible characters 
	"nn" 'air-narrow-dwim	   ;; Narrow to region and enter normal mode
	"o"  'delete-other-windows ;; C-w o
	"s"  'helm-projectile-switch-project
	"t"  'helm-locate		   
	"w"  'save-buffer
	"x"  'helm-M-x
	"y"  'helm-show-kill-ring
	;; "T"  'gtags-find-tag
	;; "t"  'gtags-reindex
 	"g"  'magit-status
	)

  (defun magit-blame-toggle ()
	"Toggle magit-blame-mode on and off interactively."
	(interactive)
	(if (and (boundp 'magit-blame-mode) magit-blame-mode)
		(magit-blame-quit)
	        (call-interactively 'magit-blame))))

(defun air--config-evil ()
  "Configure evil mode."

  ;; Use Emacs state in these additional modes.
  (dolist (mode '(ag-mode
				  flycheck-error-list-mode
				  git-rebase-mode
				  octopress-mode
				  octopress-server-mode
				  octopress-process-mode
				  sunshine-mode
				  term-mode))
	(add-to-list 'evil-emacs-state-modes mode))

  (delete 'term-mode evil-insert-state-modes)

  ;; Use insert state in these additional modes.
  (dolist (mode '(magit-log-edit-mode))
	(add-to-list 'evil-insert-state-modes mode))

  (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))

  (evil-add-hjkl-bindings occur-mode-map 'emacs
	(kbd "/")       'evil-search-forward
	(kbd "n")       'evil-search-next
	(kbd "N")       'evil-search-previous
	(kbd "C-d")     'evil-scroll-down
	(kbd "C-u")     'evil-scroll-up
	(kbd "C-w C-w") 'other-window)
  ;; Global bindings.
  (define-key evil-normal-state-map (kbd "<down>") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<up>")   'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "-")     'helm-find-files)
  (define-key evil-normal-state-map (kbd "g/")    'occur-last-search)
  (define-key evil-normal-state-map (kbd "[i")    'show-first-occurrence)
  (define-key evil-insert-state-map (kbd "C-e")   'end-of-line) ;; I know...

  (defun minibuffer-keyboard-quit ()
	    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
		(interactive)
		(if (and delete-selection-mode transient-mark-mode mark-active)
			(setq deactivate-mark  t)
		  (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
		  (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (add-hook 'evil-mode-hook 'air--config-evil)
  (evil-mode 1)

  (use-package evil-leader
	:ensure t
	:config
	(global-evil-leader-mode)
	(air--config-evil-leader))

  (use-package evil-indent-textobject
	:ensure t))

(provide 'init-evil)
