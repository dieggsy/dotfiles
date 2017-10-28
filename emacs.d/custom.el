(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
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
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-script eshell-term eshell-tramp eshell-unix)))
 '(eshell-visual-commands
   (quote
    ("htop" "cmus" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm" "alsamixer")))
 '(evil-emacs-state-modes
   (quote
    (archive-mode bbdb-mode biblio-selection-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode calc-trail-mode cfw:calendar-mode completion-list-mode custom-theme-choose-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ess-help-mode etags-select-mode fj-mode flycheck-error-list-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode jde-javadoc-checker-report-mode magit-popup-mode magit-popup-sequence-mode magit-branch-manager-mode magit-commit-mode magit-key-mode magit-rebase-mode magit-wazzup-mode mh-folder-mode monky-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode pdf-outline-buffer-mode pdf-view-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode sunshine-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode)))
 '(evil-insert-state-modes
   (quote
    (comint-mode erc-mode eshell-mode geiser-repl-mode gud-mode inferior-apl-mode inferior-caml-mode inferior-emacs-lisp-mode inferior-j-mode inferior-python-mode inferior-scheme-mode inferior-sml-mode internal-ange-ftp-mode magit-log-edit-mode org-capture-mode prolog-inferior-mode reb-mode shell-mode slime-repl-mode term-mode wdired-mode)))
 '(evil-motion-state-modes
   (quote
    (apropos-mode Buffer-menu-mode calendar-mode color-theme-mode command-history-mode compilation-mode dictionary-mode ert-results-mode help-mode Man-mode speedbar-mode undo-tree-visualizer-mode woman-mode)))
 '(evil-overriding-maps
   (quote
    ((Buffer-menu-mode-map)
     (color-theme-mode-map)
     (comint-mode-map)
     (compilation-mode-map)
     (grep-mode-map)
     (dictionary-mode-map)
     (ert-results-mode-map . motion)
     (Info-mode-map . motion)
     (speedbar-key-map)
     (speedbar-file-key-map)
     (speedbar-buffers-key-map)
     (proced-mode-map)
     (profiler-report-mode-map)
     (process-menu-mode-map)
     (image-mode-map))))
 '(fci-rule-color "#504945")
 '(fill-column 79)
 '(focus-mode-to-thing
   (quote
    ((prog-mode . defun)
     (text-mode . sentence)
     (org-mode . paragraph))))
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
 '(ibuffer-saved-filter-groups
   (quote
    (("Default"
      ("Dired"
       (mode . dired-mode))
      ("ERC"
       (mode . erc-mode))))))
 '(ivy-ignore-buffers
   (quote
    ("\\` " "\\`\\*LV\\*" "\\`\\*magit" "\\`\\*epc" "\\`\\*Calc" "\\`\\*Colors" "\\`\\*helm" "\\`\\*Help" "\\`\\*Packages" "\\`\\*Customize" "\\`\\*info" "\\`\\*Compile" "\\`\\*anaconda-mode" "\\`\\*scratch" "\\`\\*Messages" "\\`todo\\.org" "\\`gcal\\.org" "\\`notes\\.org" "\\`archive\\.org" "\\`\\.org-gcal-token" "\\`elfeed\\.org" "\\`\\*elfeed-log\\*" "\\`\\*Man" "\\`\\*Quail" "\\`\\*Paradox Report\\*" "\\`\\*Backtrace\\*" "\\`\\*slime-events\\*" "\\`\\*slime-compilation\\*" "\\`\\*inferior-lisp\\*" "\\`\\*Completions\\*" "\\`\\*embrace-help\\*" "\\`\\*geiser messages*" "\\`\\*Geiser dbg\\*" "\\`\\*tramp/.*" "\\`freenode" "\\`#" "\\`irc.freenode" "\\`nil\\'" "\\`\\*Process List\\*")))
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
 '(org-babel-python-command "python")
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
    (org-bbdb org-bibtex org-crypt org-docview org-gnus org-info org-irc org-mhe org-rmail org-w3m)))
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
    (erc-hl-nicks emms zoom-window suggest hungry-delete-mode gruvbox-theme geiser chicken-scheme evil-embrace evil-surround adoc-mode sokoban tldr doom-themes gist unfill prettify-utils ox-twbs elfeed pdf-tools notmuch fontawesome turing-machine s dired+ rainbow-identifiers color-identifiers-mode slime-company slime cmake-mode flycheck-package elpy w3 xpm dumb-jump password-store langtool ox-gfm web-mode ivy-hydra outshine haskell-snippets evil-matchit evil-anzu js2-mode company-ghci help+ help-fns+ auto-yasnippet hungry-delete flycheck-haskell ghc haskell-mode applescript-mode highlight-quoted evil-mc eyebrowse evil-indent-plus esup wiki-search pcre2el pandoc-mode async nyan-mode toc-org hacker-typer package-lint diminish bind-key bind-map eimp visual-fill-column json-mode org-gcal lib-requires elmacro dash quelpa-use-package no-littering autothemer bookmark+ headlong zone-nyan zone-matrix yasnippet yapfify yaml-mode xkcd which-key vimrc-mode use-package twittering-mode sx sunshine spray sphinx-doc speed-type spaceline smex selectric-mode sane-term restart-emacs ranger rainbow-mode rainbow-delimiters pyenv-mode-auto persistent-scratch paradox ox-pandoc osx-trash org-plus-contrib ob-ipython nm melpa-upstream-visit matlab-mode markdown-preview-mode magithub lua-mode info+ imenu-anywhere highlight-tail highlight-parentheses highlight-numbers highlight-defined google-translate google-this git-gutter-fringe focus flyspell-correct-ivy flycheck flx fireplace fill-column-indicator fancy-battery exec-path-from-shell evil-smartparens evil-numbers evil-nerd-commenter evil-multiedit evil-magit evil-indent-textobject eshell-z eshell-prompt-extras emojify elfeed-org elfeed-goodies ein drawille disable-mouse devdocs define-word csv-mode crux counsel-projectile counsel-osx-app counsel-bbdb company-anaconda coffee-mode clojure-mode centered-cursor-mode bbdb-vcard bbdb-ext atomic-chrome aggressive-indent ace-window)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(python-shell-completion-native-enable nil)
 '(python-shell-interpreter "python")
 '(recentf-exclude
   (quote
    ("/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\\|BRANCH_DESCRIPTION\\)\\'" "/elpa/" "/xkcd/")))
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
    ((column-enforce-mode)
     (d/async-babel-tangle-decrypt . t)
     (auto-save-mode)
     (org-tags-match-list-sublevels)
     (org-src-fontify-natively)
     (org-log-done)
     (rainbow-mode . t)
     (aggressive-indent-mode)
     (after-save-hook lambda nil
                      (byte-compile-file
                       (buffer-file-name)))
     (after-save-hook lambda nil
                      (org-beamer-export-to-pdf t))
     (after-save-hook git-gutter d/async-babel-tangle)
     (org-pretty-entities))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25 t)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tramp-syntax (quote default) nil (tramp))
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
 '(notmuch-search-unread-face ((t (:foreground "#83a598"))))
 '(table-cell ((t nil)))
 '(term ((t (:inherit default))))
 '(variable-pitch ((t (:family "Open Sans")))))
