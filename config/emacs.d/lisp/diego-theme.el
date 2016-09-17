;; Author: Diego A. Mundo <diegoamundo@gmail.com>
;; URL: https://github.com/therockmandolinist/something


(deftheme diego
  "My preferred colorscheme")

(let ((class '((class color) (min-colors 89))))
  (custom-theme-set-faces
   'diego
   `(default ((t (:inherit nil :stipple nil :background "#303030" :foreground "#d0d0d0" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
   `(buffer-menu-buffer ((t (:inherit ivy-virtual))))
   `(column-marker-1 ((t (:background "#7f7f7f"))))
   `(comint-highlight-prompt ((t nil)))
   `(company-preview ((t (:inherit default :underline t))))
   `(company-preview-common ((t (:inherit company-preview))))
   `(company-tooltip ((t (:background "white" :foreground "black"))))
   `(company-tooltip-selection ((t (:background "#5fafff"))))
   `(eww-form-textarea ((t (:foreground "#000000" :box 1))))
   `(font-lock-builtin-face ((t (:foreground "#56C0C2"))))
   `(font-lock-comment-face ((t (:foreground "#6c6c6c" :slant italic))))
   `(font-lock-constant-face ((t (:foreground "#56C0C2"))))
   `(font-lock-function-name-face ((t (:foreground "#5fafff"))))
   `(font-lock-keyword-face ((t (:foreground "#C678DD" :weight normal))))
   `(font-lock-string-face ((t (:foreground "#87d787"))))
   `(font-lock-type-face ((t (:inherit font-lock-function-name-face))))
   `(font-lock-variable-name-face ((t (:foreground "#D19A66"))))
   `(header-line ((t (:background "#444444" :foreground "#d0d0d0"))))
   `(helm-ff-file ((t (:foreground "#d0d0d0"))))
   `(helm-header ((t nil)))
   `(helm-match ((t (:foreground "#Ff0000"))))
   `(helm-selection ((t (:background "#3a3a3a" :distant-foreground "#000000"))))
   `(helm-source-header ((t (:inherit font-lock-function-name-face :slant italic))))
   `(helm-visible-mark ((t (:background "#5fafff" :foreground "black"))))
   `(hl-line ((t (:background "#3a3a3a"))))
   `(ivy-current-match ((t nil)))
   `(ivy-modified-buffer ((t (:inherit helm-buffer-not-saved))))
   `(linum ((t (:foreground "#4e4e4e"))))
   `(minibuffer-prompt ((t (:inherit font-lock-variable-name-face))))
   `(mode-line ((t (:background "grey75" :foreground "black" :box nil))))
   `(my-linum-hl ((t (:background "#3a3a3a" :foreground "#ff0000"))))
   `(powerline-active1 ((t (:inherit mode-line :background "#262626" :foreground "#9e9e9e"))))
   `(powerline-active2 ((t (:inherit mode-line :background "#262626"))))
   `(region ((t (:background "#4e4e4e"))))
   `(sh-quoted-exec ((t (:foreground "#af5fff"))))
   `(sml/battery ((t nil)) t)
   `(sml/col-number ((t (:inherit sml/global))))
   `(sml/time ((t (:foreground "#af5f00"))))
   `(swiper-line-face ((t (:inherit hl-line))))))

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))

(provide-theme 'diego)
