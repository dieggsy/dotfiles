(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
    (archive-mode bbdb-mode biblio-selection-mode bookmark-edit-annotation-mode browse-kill-ring-mode bzr-annotate-mode calc-mode calc-trail-mode cfw:calendar-mode completion-list-mode delicious-search-mode desktop-menu-blist-mode desktop-menu-mode dvc-bookmarks-mode dvc-diff-mode dvc-info-buffer-mode dvc-log-buffer-mode dvc-revlist-mode dvc-revlog-mode dvc-status-mode dvc-tips-mode ediff-meta-mode efs-mode Electric-buffer-menu-mode emms-browser-mode emms-mark-mode emms-metaplaylist-mode emms-playlist-mode ess-help-mode etags-select-mode fj-mode flycheck-error-list-mode gc-issues-mode gdb-breakpoints-mode gdb-disassembly-mode gdb-frames-mode gdb-locals-mode gdb-memory-mode gdb-registers-mode gdb-threads-mode gist-list-mode gnus-article-mode gnus-browse-mode gnus-group-mode gnus-server-mode gnus-summary-mode google-maps-static-mode jde-javadoc-checker-report-mode magit-popup-mode magit-popup-sequence-mode magit-branch-manager-mode magit-commit-mode magit-key-mode magit-rebase-mode magit-wazzup-mode mh-folder-mode monky-mode mu4e-main-mode mu4e-headers-mode mu4e-view-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode org-agenda-mode pdf-outline-buffer-mode pdf-view-mode rcirc-mode rebase-mode recentf-dialog-mode reftex-select-bib-mode reftex-select-label-mode reftex-toc-mode sldb-mode slime-inspector-mode slime-thread-control-mode slime-xref-mode sr-buttons-mode sr-mode sr-tree-mode sr-virtual-mode sunshine-mode tar-mode tetris-mode tla-annotate-mode tla-archive-list-mode tla-bconfig-mode tla-bookmarks-mode tla-branch-list-mode tla-browse-mode tla-category-list-mode tla-changelog-mode tla-follow-symlinks-mode tla-inventory-file-mode tla-inventory-mode tla-lint-mode tla-logs-mode tla-revision-list-mode tla-revlog-mode tla-tree-lint-mode tla-version-list-mode twittering-mode urlview-mode vc-annotate-mode vc-dir-mode vc-git-log-view-mode vc-hg-log-view-mode vc-svn-log-view-mode vm-mode vm-summary-mode w3m-mode wab-compilation-mode xgit-annotate-mode xgit-changelog-mode xgit-diff-mode xgit-revlog-mode xhg-annotate-mode xhg-log-mode xhg-mode xhg-mq-mode xhg-mq-sub-mode xhg-status-extra-mode sx-question-mode sx-question-list-mode)))
 '(evil-insert-state-modes
   (quote
    (exwm-mode comint-mode erc-mode eshell-mode geiser-repl-mode gud-mode inferior-apl-mode inferior-caml-mode inferior-emacs-lisp-mode inferior-j-mode inferior-python-mode inferior-scheme-mode inferior-sml-mode internal-ange-ftp-mode magit-log-edit-mode org-capture-mode prolog-inferior-mode reb-mode shell-mode slime-repl-mode wdired-mode exwm-mode)))
 '(evil-motion-state-modes
   (quote
    (Info-mode apropos-mode Buffer-menu-mode calendar-mode color-theme-mode command-history-mode dictionary-mode ert-results-mode Man-mode speedbar-mode undo-tree-visualizer-mode woman-mode helpful-mode elisp-refs-mode)))
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
    ("\\` " "\\`\\*LV\\*" "\\`\\*magit" "\\`\\*epc" "\\`\\*Calc" "\\`\\*Colors" "\\`\\*helm" "\\`\\*Help" "\\`\\*Packages" "\\`\\*Customize" "\\`\\*info" "\\`\\*Compile" "\\`\\*anaconda-mode" "\\`\\*scratch" "\\`\\*Messages" "\\`todo\\.org" "\\`gcal\\.org" "\\`notes\\.org" "\\`archive\\.org" "\\`\\.org-gcal-token" "\\`elfeed\\.org" "\\`\\*elfeed-log\\*" "\\`\\*Man" "\\`\\*Quail" "\\`\\*Paradox Report\\*" "\\`\\*Backtrace\\*" "\\`\\*slime-events\\*" "\\`\\*slime-compilation\\*" "\\`\\*inferior-lisp\\*" "\\`\\*Completions\\*" "\\`\\*embrace-help\\*" "\\`\\*geiser messages*" "\\`\\*Geiser dbg\\*" "\\`\\*tramp/.*" "\\`freenode" "\\`#" "\\`irc.freenode" "\\`nil\\'" "\\`\\*Process List\\*" "\\`\\*eshell:")))
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
