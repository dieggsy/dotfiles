(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("67b10c9473d9f1cfe8d1f5943ccfeea823250d1ea547641600e65ff5a8e0a5b3" "8b26f75ff4c58aa2ab84eefcaee8ea34b35498f87aa998ba5bc3a705700eb945" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "06dbcfac3705aaaa79e1a3264c6fd44ef0cf86ef5ed67930e4007e63a8c1e8ee" "1d77d43f4f40a5a986c4420a3eabe43d828e20cc068b37e7cbf05079cc94a7b3" "61c4345c064bc18cfaf24e1417e11eed20ded0d3aff85856b34fd99abe851b97" "16241cf61a64014901a8dfb661cff9e5d9702f743f4e888c26de8c92267242d6" "63fd72d437963b632631e698379662c2e43f0238e675a7821f25ff1c6a3e200a" "a95356e6a494dc27d71fa7b8ecafdf5c60901aad175357a7b0da8c9878af258f" "0e219d63550634bc5b0c214aced55eb9528640377daf486e13fb18a32bf39856" "e3d28baa3817819c97fa8e80e388b7e72d959c3e2b11c3cc7870848329875623" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-term eshell-tramp eshell-unix)))
 '(eshell-output-filter-functions
   (quote
    (eshell-truncate-buffer eshell-postoutput-scroll-to-bottom eshell-handle-control-codes eshell-handle-ansi-color eshell-watch-for-password-prompt)))
 '(eshell-visual-commands
   (quote
    ("htop" "cmus" "vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm" "alsamixer" "nmtui" "nmtui-connect" "nmtui-edit" "nmtui-hostname")))
 '(evil-emacs-state-modes
   (quote
    (archive-mode bbdb-mode biblio-selection-mode bookmark-bmenu-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode calc-trail-mode cfw:calendar-mode completion-list-mode custom-theme-choose-mode debugger-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode doc-view-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ess-help-mode etags-select-mode fj-mode flycheck-error-list-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode jde-javadoc-checker-report-mode magit-popup-mode magit-popup-sequence-mode magit-branch-manager-mode magit-commit-mode magit-key-mode magit-rebase-mode magit-wazzup-mode mh-folder-mode monky-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode occur-mode org-agenda-mode package-menu-mode pdf-outline-buffer-mode pdf-view-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode sunshine-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode sx-question-mode sx-question-list-mode)))
 '(evil-insert-state-modes
   (quote
    (exwm-mode comint-mode erc-mode eshell-mode geiser-repl-mode gud-mode inferior-apl-mode inferior-caml-mode inferior-emacs-lisp-mode inferior-j-mode inferior-python-mode inferior-scheme-mode inferior-sml-mode internal-ange-ftp-mode magit-log-edit-mode org-capture-mode prolog-inferior-mode reb-mode shell-mode slime-repl-mode term-mode wdired-mode exwm-mode)))
 '(evil-motion-state-modes
   (quote
    (apropos-mode Buffer-menu-mode calendar-mode color-theme-mode command-history-mode compilation-mode dictionary-mode ert-results-mode help-mode Man-mode speedbar-mode undo-tree-visualizer-mode woman-mode helpful-mode elisp-refs-mode)))
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
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(ivy-ignore-buffers
   (quote
    ("\\` " "\\`\\*LV\\*" "\\`\\*magit" "\\`\\*epc" "\\`\\*Calc" "\\`\\*Colors" "\\`\\*helm" "\\`\\*Help" "\\`\\*Packages" "\\`\\*Customize" "\\`\\*info" "\\`\\*Compile" "\\`\\*anaconda-mode" "\\`\\*scratch" "\\`\\*Messages" "\\`todo\\.org" "\\`gcal\\.org" "\\`notes\\.org" "\\`archive\\.org" "\\`\\.org-gcal-token" "\\`elfeed\\.org" "\\`\\*elfeed-log\\*" "\\`\\*Man" "\\`\\*Quail" "\\`\\*Paradox Report\\*" "\\`\\*Backtrace\\*" "\\`\\*slime-events\\*" "\\`\\*slime-compilation\\*" "\\`\\*inferior-lisp\\*" "\\`\\*Completions\\*" "\\`\\*embrace-help\\*" "\\`\\*geiser messages*" "\\`\\*Geiser dbg\\*" "\\`\\*tramp/.*" "\\`freenode" "\\`#" "\\`irc.freenode" "\\`nil\\'" "\\`\\*Process List\\*")))
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
     (org-pretty-entities)
     (after-save-hook org-hugo-export-subtree-to-md-after-save))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25 t)
 '(tramp-syntax (quote default) nil (tramp)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(table-cell ((t nil))))
