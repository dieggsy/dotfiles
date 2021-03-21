(setq safe-local-variable-values '((aggressive-indent-mode) (eval when (fboundp 'rainbow-mode) (rainbow-mode 1)) (eval conf-quote-normal nil) (org-log-done) (eval add-hook 'after-save-hook 'd/async-babel-tangle 'append 'local) (aggressive-indent-mode)))



