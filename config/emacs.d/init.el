(org-babel-load-file "~/.emacs.d/config.org")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#303030" :foreground "#d0d0d0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
 '(buffer-menu-buffer ((t (:foreground "black"))))
 '(column-marker-1 ((t (:background "#7f7f7f"))))
 '(comint-highlight-prompt ((t nil)))
 '(company-preview ((t (:inherit default :underline t))))
 '(company-preview-common ((t (:inherit company-preview))))
 '(company-tooltip ((t (:background "white" :foreground "black"))))
 '(company-tooltip-selection ((t (:background "#5fafff"))))
 '(eww-form-textarea ((t (:foreground "#000000" :box 1))))
 '(font-lock-builtin-face ((t (:foreground "#56C0C2"))))
 '(font-lock-comment-face ((t (:foreground "#6c6c6c" :slant italic))))
 '(font-lock-constant-face ((t (:foreground "#56C0C2"))))
 '(font-lock-function-name-face ((t (:foreground "#5fafff"))))
 '(font-lock-keyword-face ((t (:foreground "#C678DD" :weight normal))))
 '(font-lock-string-face ((t (:foreground "#87d787"))))
 '(font-lock-type-face ((t (:inherit font-lock-function-name-face))))
 '(font-lock-variable-name-face ((t (:foreground "#D19A66"))))
 '(header-line ((t (:background "#444444" :foreground "#d0d0d0"))))
 '(helm-ff-file ((t (:foreground "#d0d0d0"))))
 '(helm-header ((t nil)))
 '(helm-match ((t (:foreground "#Ff0000"))))
 '(helm-selection ((t (:background "#3a3a3a" :distant-foreground "#000000"))))
 '(helm-source-header ((t (:inherit font-lock-function-name-face :slant italic))))
 '(helm-visible-mark ((t (:background "#5fafff" :foreground "black"))))
 '(hl-line ((t (:background "#3a3a3a"))))
 '(ivy-current-match ((t (:inherit default))))
 '(linum ((t (:foreground "#4e4e4e"))))
 '(minibuffer-prompt ((t (:inherit font-lock-variable-name-face))))
 '(mode-line ((t (:background "grey75" :foreground "black" :box nil))))
 '(my-linum-hl ((t (:background "#3a3a3a" :foreground "#ff0000"))))
 '(powerline-active1 ((t (:inherit mode-line :background "#262626" :foreground "#9e9e9e"))))
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
 '(custom-safe-themes
   (quote
	("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(display-time-24hr-format t)
 '(display-time-default-load-average nil)
 '(display-time-format "%a %d %b  %H:%M ")
 '(elpy-modules
   (quote
	(elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-yasnippet elpy-module-sane-defaults)))
 '(fill-column 79)
 '(helm-boring-buffer-regexp-list
   (quote
	("\\` " "\\*helm" "\\*helm-mode" "\\*Echo Area" "\\*Minibuf" "\\*epc")))
 '(helm-scroll-amount 1)
 '(org-babel-load-languages (quote ((python . t) (emacs-lisp . t))))
 '(org-babel-python-command "python3")
 '(org-blank-before-new-entry (quote ((heading . t) (plain-list-item . t))))
 '(org-confirm-babel-evaluate nil)
 '(org-highlight-latex-and-related nil)
 '(org-latex-classes
   (quote
	(("article" "\\documentclass[11pt]{article}"
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
	  ("\\paragraph{%s}" . "\\paragraph*{%s}")
	  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	 ("report" "\\documentclass[11pt]{report}"
	  ("\\part{%s}" . "\\part*{%s}")
	  ("\\chapter{%s}" . "\\chapter*{%s}")
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	 ("book" "\\documentclass[11pt]{book}"
	  ("\\part{%s}" . "\\part*{%s}")
	  ("\\chapter{%s}" . "\\chapter*{%s}")
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")
	  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
	 ("IEEE" "\\documentclass[conference]{IEEEtran}"
	  ("\\section{%s}" . "\\section*{%s}")
	  ("\\subsection{%s}" . "\\subsection*{%s}")))))
 '(org-modules
   (quote
	(org-bbdb org-bibtex org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m)))
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

