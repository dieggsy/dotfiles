;;; init-org.el --- Set up Org Mode
;;; Commentary:

;; Basic Org Mode configuration, assuming presence of Evil & Evil Leader.

;; Helper functions
(defun air--org-global-custom-ids ()
  "Find custom ID fields in all org agenda files."
  (let ((files (org-agenda-files))
        file
        air-all-org-custom-ids)
    (while (setq file (pop files))
      (with-current-buffer (org-get-agenda-file-buffer file)
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (re-search-forward "^[ \t]*:CUSTOM_ID:[ \t]+\\(\\S-+\\)[ \t]*$"
                                      nil t)
              (add-to-list 'air-all-org-custom-ids
                           `(,(match-string-no-properties 1)
                             ,(concat file ":" (number-to-string (line-number-at-pos))))))))))
    air-all-org-custom-ids))

(defun air-org-goto-custom-id ()
  "Go to the location of CUSTOM-ID, or prompt interactively."
  (interactive)
  (let* ((all-custom-ids (air--org-global-custom-ids))
         (custom-id (completing-read
                     "Custom ID: "
                     all-custom-ids)))
    (when custom-id
      (let* ((val (cadr (assoc custom-id all-custom-ids)))
             (id-parts (split-string val ":"))
             (file (car id-parts))
             (line (string-to-int (cadr id-parts))))
        (pop-to-buffer (org-get-agenda-file-buffer file))
        (goto-char (point-min))
        (forward-line line)
        (org-reveal)
        (org-up-element)))))

(defun air-org-insert-custom-id-link ()
  "Insert an Org link to a custom ID selected interactively."
  (interactive)
  (let* ((all-custom-ids (air--org-global-custom-ids))
         (custom-id (completing-read
                     "Custom ID: "
                     all-custom-ids)))
    (when custom-id
      (let* ((val (cadr (assoc custom-id all-custom-ids)))
             (id-parts (split-string val ":"))
             (file (car id-parts))
             (line (string-to-int (cadr id-parts))))
        (org-insert-link nil (concat file "::#" custom-id) custom-id)))))

(defun air-org-set-category-property (value)
  "Set the category property of the current item to VALUE."
  (interactive (list (org-read-property-value "CATEGORY")))
  (org-set-property "CATEGORY" value))

(defun air-org-insert-heading (&optional subheading)
  "Insert a heading or a subheading.

If the optional SUBHEADING is t, insert a subheading.  Inserting
headings always respects content."
  (interactive "P")
  (if subheading
      (org-insert-subheading t)
    (org-insert-heading t)))

(defun air-org-insert-scheduled-heading (&optional subheading)
  "Insert a new org heading scheduled for today.

Insert the new heading at the end of the current subtree if
FORCE-HEADING is non-nil."
  (interactive "P")
  (if subheading
      (org-insert-subheading t)
    (org-insert-todo-heading t t))
  (org-schedule nil (format-time-string "%Y-%m-%d")))

(defun air-org-task-capture ()
  "Capture a task with my default template."
  (interactive)
  (org-capture nil "a"))

(defun air-org-agenda-capture ()
  "Capture a task in agenda mode, using the date at point."
  (interactive)
  (let ((org-overriding-default-time (org-get-cursor-date)))
    (org-capture nil "a")))

(defun air-org-agenda-toggle-date (current-line)
  "Toggle `SCHEDULED' and `DEADLINE' tag in the capture buffer."
  (interactive "P")
  (save-excursion
    (let ((search-limit (if current-line
                            (line-end-position)
                          (point-max))))

      (if current-line (beginning-of-line)
        (beginning-of-buffer))
      (if (search-forward "DEADLINE:" search-limit t)
          (replace-match "SCHEDULED:")
        (and (search-forward "SCHEDULED:" search-limit t)
             (replace-match "DEADLINE:"))))))

(defun air-pop-to-org-todo (split)
  "Visit my main TODO list, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/Dropbox (MIT)/org/todo.org" split))

(defun air-pop-to-org-notes (split)
  "Visit my main notes file, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/Dropbox (MIT)/org/notes.org" split))

(defun air-pop-to-org-vault (split)
  "Visit my encrypted vault file, in the current window or a SPLIT."
  (interactive "P")
  (air--pop-to-file "~/Dropbox (MIT)/org/vault.gpg" split))

(defun air-pop-to-org-agenda (split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  (org-agenda-list nil "today" 'day)
  (when (not split)
    (delete-other-windows)))

(defun air--org-insert-list-leader-or-self (char)
  "If on column 0, insert space-padded CHAR; otherwise insert CHAR.

This has the effect of automatically creating a properly indented list
leader; like hyphen, asterisk, or plus sign; without having to use
list-specific key maps."
  (if (= (current-column) 0)
      (insert (concat " " char " "))
    (insert char)))

(defun air--org-swap-tags (tags)
  "Replace any tags on the current headline with TAGS.

The assumption is that TAGS will be a string conforming to Org Mode's
tag format specifications, or nil to remove all tags."
  (let ((old-tags (org-get-tags-string))
        (tags (if tags
                  (concat " " tags)
                "")))
    (save-excursion
      (beginning-of-line)
      (re-search-forward
       (concat "[ \t]*" (regexp-quote old-tags) "[ \t]*$")
       (line-end-position) t)
      (replace-match tags)
      (org-set-tags t))))

(defun air-org-set-tags (tag)
  "Add TAG if it is not in the list of tags, remove it otherwise.

TAG is chosen interactively from the global tags completion table."
  (interactive
   (list (let ((org-last-tags-completion-table
                (if (derived-mode-p 'org-mode)
                    (org-uniquify
                     (delq nil (append (org-get-buffer-tags)
                                       (org-global-tags-completion-table))))
                  (org-global-tags-completion-table))))
           (completing-read
            "Tag: " 'org-tags-completion-function nil nil nil
            'org-tags-history))))
  (let* ((cur-list (org-get-tags))
         (new-tags (mapconcat 'identity
                              (if (member tag cur-list)
                                  (delete tag cur-list)
                                (append cur-list (list tag)))
                              ":"))
         (new (if (> (length new-tags) 1) (concat " :" new-tags ":")
                nil)))
    (air--org-swap-tags new)))


;;; Code:
(use-package org
  :ensure t
  :defer t
  :commands (org-capture)
  :bind (("C-c c" .   air-org-task-capture)
         ("C-c l" .   org-store-link)
         ("C-c t n" . air-pop-to-org-notes)
         ("C-c t t" . air-pop-to-org-todo)
         ("C-c t v" . air-pop-to-org-vault)
         ("C-c t a" . air-pop-to-org-agenda)
         ("C-c t A" . org-agenda)
         ("C-c f k" . org-search-view)
         ("C-c f t" . org-tags-view)
         ("C-c f i" . air-org-goto-custom-id))
  :config
  (eval-after-load "org"
	  '(require 'ox-md nil t))
  (setq org-agenda-text-search-extra-files '(agenda-archives))
  (setq org-agenda-files '("~/Dropbox (MIT)/org/"))
  (setq org-todo-keywords
        '((sequence "☛ TODO" "○ IN-PROGRESS" "⚑ WAITING" "|" "✓ DONE" "✗ CANCELED")))
  (setq org-blank-before-new-entry '((heading . t)
                                     (plain-list-item . t)))
  (setq org-capture-templates
        '(("a" "My TODO task format." entry
           (file "todo.org")
           "* ☛ TODO %?\nSCHEDULED: %t")
          ("n" "A (work-related) note." entry
           (file+headline "notes.org" "Work")
           "* %?\n%u\n\n"
           :jump-to-captured t)))
  (setq org-default-notes-file "~/Dropbox (MIT)/org/todo.org")
  (setq org-directory "~/Dropbox (MIT)/org")
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done (quote time))
  (setq org-log-redeadline (quote time))
  (setq org-log-reschedule (quote time))
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-insert-heading-respect-content t)
  (setq org-ellipsis " …")
  (setq org-startup-with-inline-images t)
  (set-face-attribute 'org-upcoming-deadline nil :foreground "gold1")

  (evil-leader/set-key-for-mode 'org-mode
    "$"  'org-archive-subtree
    "a"  'org-agenda
    "c"  'air-org-set-category-property
    "d"  'org-deadline
    "ns" 'org-narrow-to-subtree
    "p"  'org-set-property
    "s"  'org-schedule
    "t"  'air-org-set-tags)

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (setq org-habit-graph-column 50)
              (define-key org-agenda-mode-map "j"          'org-agenda-next-line)
              (define-key org-agenda-mode-map "k"          'org-agenda-previous-line)
              (define-key org-agenda-mode-map "n"          'org-agenda-next-date-line)
              (define-key org-agenda-mode-map "p"          'org-agenda-previous-date-line)
              (define-key org-agenda-mode-map "c"          'air-org-agenda-capture)
              (define-key org-agenda-mode-map "R"          'org-revert-all-org-buffers)
              (define-key org-agenda-mode-map (kbd "RET")  'org-agenda-switch-to)

              (define-prefix-command 'air-org-run-shortcuts)
              (define-key air-org-run-shortcuts "f" (tiny-menu-run-item "org-files"))
              (define-key air-org-run-shortcuts "t" (tiny-menu-run-item "org-things"))
              (define-key air-org-run-shortcuts "c" (tiny-menu-run-item "org-captures"))
              (define-key air-org-run-shortcuts "l" (tiny-menu-run-item "org-links"))
              (define-key org-agenda-mode-map (kbd "\\") air-org-run-shortcuts)))

  (add-hook 'org-capture-mode-hook
            (lambda ()
              (evil-define-key 'insert org-capture-mode-map (kbd "C-d") 'air-org-agenda-toggle-date)
              (evil-define-key 'normal org-capture-mode-map (kbd "C-d") 'air-org-agenda-toggle-date)
              (evil-insert-state)))

  (add-hook 'org-mode-hook
            (lambda ()
              ;; Special plain list leader inserts
              (dolist (char '("+" "-"))
                (define-key org-mode-map (kbd char)
                  `(lambda ()
                    (interactive)
                    (air--org-insert-list-leader-or-self ,char))))

              ;; Normal maps
              (define-key org-mode-map (kbd "C-c d")   (lambda ()
                                                         (interactive) (air-org-agenda-toggle-date t)))
              (define-key org-mode-map (kbd "C-c ,")   'org-time-stamp-inactive)
              (define-key org-mode-map (kbd "C-|")     'air-org-insert-scheduled-heading)
              (define-key org-mode-map (kbd "C-\\")    'air-org-insert-heading)
              (define-key org-mode-map (kbd "C-<")     'org-metaleft)
              (define-key org-mode-map (kbd "C->")     'org-metaright)
              (define-key org-mode-map (kbd "S-r")     'org-revert-all-org-buffers)
              (define-key org-mode-map (kbd "C-c C-l") (tiny-menu-run-item "org-links"))

              (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
              (evil-define-key 'normal org-mode-map ">>"        'org-metaright)
              (evil-define-key 'normal org-mode-map "<<"        'org-metaleft)
              (evil-define-key 'normal org-mode-map (kbd "C-S-l") 'org-shiftright)
              (evil-define-key 'normal org-mode-map (kbd "C-S-h") 'org-shiftleft)
              (evil-define-key 'insert org-mode-map (kbd "C-S-l") 'org-shiftright)
              (evil-define-key 'insert org-mode-map (kbd "C-S-h") 'org-shiftleft)

              ;; Navigation
              (evil-define-key 'normal org-mode-map (kbd "]n") 'org-forward-heading-same-level)
              (evil-define-key 'normal org-mode-map (kbd "[n") 'org-backward-heading-same-level)
              (define-key org-mode-map (kbd "C-S-j") (lambda ()
                                                       (interactive)
                                                       (org-up-element)
                                                       (org-forward-heading-same-level 1)))
              (define-key org-mode-map (kbd "C-S-k") 'org-up-element)

              ;; Use fill column, but not in agenda
              (setq fill-column 100)
              (when (not (eq major-mode 'org-agenda-mode))
                (visual-line-mode)
                (visual-fill-column-mode))
              (flyspell-mode)
              (org-indent-mode))))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  ;; (setq org-bullets-bullet-list '("•"))
  (use-package ox-twbs
	:ensure t) 
  )

(provide 'init-org)
;;; init-org.el ends here
