;;; -*- lexical-binding: t; -*-
;;(package-initialize)
(defconst emacs-start-time (current-time))
(require 'package)
(setq package-enable-at-startup nil)

(load-file "~/.emacs.d/config.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#32302F" "#FB4934" "#B8BB26" "#FABD2F" "#83A598" "#D3869B" "#17CCD5" "#EBDBB2"])
 '(compilation-message-face (quote default))
 '(counsel-find-file-ignore-regexp "\\`\\.")
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("67b10c9473d9f1cfe8d1f5943ccfeea823250d1ea547641600e65ff5a8e0a5b3" "8b26f75ff4c58aa2ab84eefcaee8ea34b35498f87aa998ba5bc3a705700eb945" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" "1d77d43f4f40a5a986c4420a3eabe43d828e20cc068b37e7cbf05079cc94a7b3" "61c4345c064bc18cfaf24e1417e11eed20ded0d3aff85856b34fd99abe851b97" "16241cf61a64014901a8dfb661cff9e5d9702f743f4e888c26de8c92267242d6" "63fd72d437963b632631e698379662c2e43f0238e675a7821f25ff1c6a3e200a" "a95356e6a494dc27d71fa7b8ecafdf5c60901aad175357a7b0da8c9878af258f" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "e3d28baa3817819c97fa8e80e388b7e72d959c3e2b11c3cc7870848329875623" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(doc-view-resolution 300)
 '(fill-column 79)
 '(gnus-large-newsgroup 4000)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#20240E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#20240E" . 100))) t)
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(notmuch-hello-sections
   (quote
    (notmuch-hello-insert-saved-searches notmuch-hello-insert-search notmuch-hello-insert-recent-searches notmuch-hello-insert-alltags notmuch-hello-insert-footer)))
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "folder:Inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a"))))
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(org-babel-load-languages
   (quote
    ((python . t)
     (emacs-lisp . t)
     (calc . t)
     (ipython . t)
     (shell . t))))
 '(org-babel-python-command "python3")
 '(org-confirm-babel-evaluate nil)
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
 '(org-pandoc-options (quote ((standalone . t))))
 '(org-src-lang-modes
   (quote
    (("ipython" . python)
     ("ocaml" . tuareg)
     ("elisp" . emacs-lisp)
     ("ditaa" . artist)
     ("asymptote" . asy)
     ("dot" . fundamental)
     ("sqlite" . sql)
     ("calc" . fundamental)
     ("C" . c)
     ("cpp" . c++)
     ("C++" . c++)
     ("screen" . shell-script)
     ("html" . html))))
 '(package-selected-packages
   (quote
    (ox-gfm web-mode ivy-hydra outshine haskell-snippets evil-matchit evil-anzu js2-mode company-ghci help+ help-fns+ auto-yasnippet hungry-delete flycheck-haskell ghc haskell-mode applescript-mode highlight-quoted evil-mc eyebrowse evil-indent-plus esup wiki-search pcre2el pandoc-mode async nyan-mode toc-org hacker-typer package-lint diminish bind-key bind-map weechat-alert weechat eimp visual-fill-column json-mode org-gcal lib-requires elmacro darktooth-theme dash quelpa-use-package quelpa package-build no-littering autothemer bookmark+ headlong nlinum-relative zone-nyan zone-matrix yasnippet yapfify yaml-mode xkcd which-key vimrc-mode use-package twittering-mode sx sunshine spray sphinx-doc speed-type spaceline smex selectric-mode sane-term restart-emacs ranger rainbow-mode rainbow-delimiters pyenv-mode-auto persistent-scratch paradox ox-pandoc osx-trash org-plus-contrib ob-ipython nm melpa-upstream-visit matlab-mode markdown-preview-mode magithub lua-mode info+ imenu-anywhere highlight-tail highlight-parentheses highlight-numbers highlight-defined google-translate google-this git-gutter-fringe focus flyspell-correct-ivy flycheck flx fireplace fill-column-indicator fancy-battery exec-path-from-shell evil-smartparens evil-numbers evil-nerd-commenter evil-multiedit evil-magit evil-leader evil-indent-textobject eshell-z eshell-prompt-extras emojify elfeed-org elfeed-goodies ein drawille disable-mouse devdocs define-word csv-mode crux counsel-projectile counsel-osx-app counsel-bbdb company-anaconda coffee-mode clojure-mode centered-cursor-mode bbdb-vcard bbdb-ext atomic-chrome aggressive-indent ace-window)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(python-shell-completion-native-enable nil)
 '(python-shell-interpreter "python")
 '(recentf-exclude
   (quote
    ("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|BRANCH_DESCRIPTION\\)\\'" "/elpa/" "/xkcd/")) t)
 '(safe-local-eval-forms
   (quote
    ((add-hook
      (quote write-file-hooks)
      (quote time-stamp))
     (add-hook
      (quote write-file-functions)
      (quote time-stamp))
     (add-hook
      (quote before-save-hook)
      (quote time-stamp)
      nil t)
     (add-hook
      (quote before-save-hook)
      (quote delete-trailing-whitespace)
      nil t)
     (when
         (fboundp
          (quote rainbow-mode))
       (rainbow-mode 1)))))
 '(safe-local-variable-values
   (quote
    ((aggressive-indent-mode)
     (after-save-hook lambda nil
                      (byte-compile-file
                       (locate-user-emacs-file "init.el")))
     (after-save-hook lambda nil
                      (org-beamer-export-to-pdf t))
     (after-save-hook git-gutter diego/async-tangle-init)
     (after-save-hook git-gutter diego/async-babel-tangle)
     (org-pretty-entities))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25 t)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#20240E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-prompt-face ((t (:foreground "lightBlue2" :weight bold))))
 '(notmuch-search-unread-face ((t (:foreground "#83a598"))))
 '(table-cell ((t nil)))
 '(term ((t (:inherit default))))
 '(variable-pitch ((t (:family "Open Sans")))))

(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'timer-list 'disabled nil)

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

;; Local Variables:
;; after-save-hook: (lambda () (byte-compile-file (locate-user-emacs-file "init.el")))
;; End:
