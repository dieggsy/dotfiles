;; Author: Diego A. Mundo <diegoamundo@gmail.com>
;; URL: https://github.com/therockmandolinist/something


(deftheme diego
  "My preferred colorscheme, heavily inspired by (or almost exactly like) atom-one-dark")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'diego
   ;; Defaults
   ;; `(default ((t (:background "#303030" :foreground "#d0d0d0"))))
   `(default ((t (:background "#3a3a3a" :foreground "#d0d0d0"))))
   ;; `(font-lock-builtin-face ((t (:foreground "#5FAFAF"))))
   ;; `(font-lock-builtin-face ((t (:foreground "#AFD7D7"))))
   `(font-lock-builtin-face ((t (:foreground "#87AFAF"))))
   `(font-lock-comment-face ((t (:foreground "#6c6c6c" :slant italic))))
   `(font-lock-constant-face ((t (:foreground "#5FAFAF"))))
   ;; `(font-lock-constant-face ((t (:foreground "#AFD7D7"))))
   ;; `(font-lock-constant-face ((t (:foreground "#87AFAF"))))
   `(font-lock-function-name-face ((t (:foreground "#5fafff"))))
   `(font-lock-keyword-face ((t (:foreground "#C678DD" :weight normal))))
   `(font-lock-string-face ((t (:foreground "#87d787"))))
   `(font-lock-type-face ((t (:inherit font-lock-function-name-face))))
   ;; `(font-lock-variable-name-face ((t (:foreground "#D19A66"))))
   `(font-lock-variable-name-face ((t (:foreground "#D7AF87"))))
   `(region ((t (:background "#4e4e4e"))))
   `(minibuffer-prompt ((t (:inherit font-lock-variable-name-face))))

   ;; Fonts
   `(fixed-pitch ((t (:family "Consolas"))))
   `(variable-pitch ((t (:family "Helvetica Neue Light"))))
   
   
   ;; Company
   `(company-preview ((t (:inherit default :underline t))))
   `(company-preview-common ((t (:inherit company-preview))))
   `(company-tooltip ((t (:background "#d0d0d0" :foreground "black"))))
   `(company-tooltip-selection ((t (:background "#5fafff"))))
   `(eww-form-textarea ((t (:foreground "#000000" :box 1))))
   `(header-line ((t (:background "#444444" :foreground "#d0d0d0"))))

   ;; Helm
   `(helm-ff-file ((t (:foreground "#d0d0d0"))))
   `(helm-header ((t nil)))
   `(helm-match ((t (:foreground "#Ff0000"))))
   `(helm-selection ((t (:background "#3a3a3a" :distant-foreground "#000000"))))
   `(helm-source-header ((t (:inherit font-lock-function-name-face :slant italic))))
   `(helm-visible-mark ((t (:background "#5fafff" :foreground "black"))))

   ;; Ivy
   `(ivy-current-match ((t nil)))
   `(ivy-modified-buffer ((t (:inherit helm-buffer-not-saved))))
   `(swiper-line-face ((t (:inherit hl-line))))

   ;;Email 
   `(message-header-subject ((t (:foreground "OliveDrab1" :height 1.2))))
   `(message-mml ((t (:inherit font-lock-comment-face :slant normal))))
   `(notmuch-search-unread-face ((t (:inherit font-lock-function-name-face))))

   ;; Smart-mode-line
   `(sml/battery ((t nil)) t)
   `(sml/col-number ((t (:inherit sml/global))))
   `(sml/time ((t (:foreground "#af5f00"))))
   
   ;; Other
   `(column-marker-1 ((t (:background "#7f7f7f"))))
   `(comint-highlight-prompt ((t nil)))
   `(hl-line ((t (:background "#444444"))))
   `(linum ((t (:foreground "#4e4e4e"))))
   `(mode-line ((t (:background "grey75" :foreground "black" :box nil))))
   `(powerline-active1 ((t (:inherit mode-line :background "#262626" :foreground "#9e9e9e"))))
   `(powerline-active2 ((t (:inherit mode-line :background "#262626"))))
   `(sh-quoted-exec ((t (:foreground "#af5fff"))))
   `(minesweeper-neighbor ((t (:inherit hl-line))))
   `(custom-button ((t (:background "lightgrey" :foreground "black"))))
   `(custom-button-mouse ((t (:background "grey90" :foreground "black"))))
   `(custom-button-pressed ((t (:background "lightgrey" :foreground "black"))))
   `(font-lock-variable-name-face ((t (:foreground "#DFAF8F"))))
   `(minesweeper-neighbor ((t (:inherit hl-line))))
   `(twittering-uri-face ((t (:inherit font-lock-builtin-face :underline t))))
   `(my-linum-hl ((t (:background "#444444" :foreground "#fb3460"))))
   `(markdown-inline-code-face ((t (:inherit (fixed-pitch font-lock-constant-face) :background "#444444"))))
   `(markdown-pre-face ((t (:inherit (fixed-pitch font-lock-constant-face) :background "#444444"))))
   `(whitespace-trailing ((t (:background "#fb3460" :foreground "yellow" :weight bold))))
   `(org-verbatim ((t (:inherit font-lock-constant-face))))
   `(org-code ((t (:inherit org-verbatim))))
   ))

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))

(provide-theme 'diego)
