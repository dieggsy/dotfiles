(require 'cl-lib)

(defun dired-git--inside-git-repository-p ()
  (with-temp-buffer
    (when (zerop (process-file "git" nil t nil "rev-parse" "--is-inside-work-tree"))
      (goto-char (point-min))
      (string= "true" (buffer-substring-no-properties
                       (point) (line-end-position))))))

(defun dired-git--overlay-git-status (status-hash dired-buffer)
  (with-current-buffer dired-buffer
    (save-excursion
      (beginning-of-buffer)
      (while (not (eobp))
        (let* ((filename (dired-get-filename nil t))
               (beg (line-beginning-position))
               (ov (make-overlay beg (+ 2 beg))))
          (if (and filename
                   (not (member filename (list "." "..")))
                   (not (eolp)))
              (let* ((stat (gethash filename status-hash))
                     (untracked-p (string= stat "??")))
                (if stat
                    (progn
                      ;; (setq stat (concat stat " "))
                      (unless untracked-p
                        (put-text-property 0 1 'face '(:foreground "#B8BB26") stat))
                      (put-text-property (or (and untracked-p
                                                  0)
                                             1) 2 'face '(:foreground "#FB4933") stat)
                      (overlay-put ov 'display stat))
                  ;; (overlay-put ov 'display "   ")
                  ))
            ;; (overlay-put ov 'display "   ")
            ))
        (forward-line 1)))))

(defun dired-git--parse-git-status (root proc-buffer dired-buffer)
  (let* ((file-statuses (make-hash-table :test 'equal))
         (git-status-string (with-current-buffer proc-buffer (buffer-string)))
         (git-status-list (split-string git-status-string "\n" t)))
    (dolist (line git-status-list)
      (let ((stat (substring-no-properties line 0 2))
            (paths (reverse
                    (split-string
                     (substring-no-properties line 3)
                     "/"))))
        ;; (message line)
        (cl-loop for i = 0 then (+ 1 i)
                 while (< i (length paths))
                 do
                 (let* ((file (string-join (reverse (nthcdr i paths)) "/"))
                        (full-path (expand-file-name (concat root file))))
                   (unless (gethash full-path file-statuses)
                     (puthash full-path stat file-statuses))))))
    (dired-git--overlay-git-status file-statuses dired-buffer)))

(defun dired-git-status ()
  (when (dired-git--inside-git-repository-p)
    (let* ((curbuf (current-buffer))
           (root (locate-dominating-file default-directory ".git/"))
           (proc-buf (get-buffer-create (format " *dired-git-%s*" dired-directory)))
           (old-proc (get-buffer-process proc-buf)))
      (when (and old-proc (process-live-p old-proc))
        (kill-process old-proc))
      (with-current-buffer proc-buf
        (erase-buffer))
      (unless (not root)
        (let ((proc
               (start-file-process "dired-git-status" proc-buf "git" "status" "--porcelain")))
          (set-process-query-on-exit-flag proc nil)
          (set-process-sentinel
           proc
           `(lambda (proc _event)
              (when (eq (process-status proc) 'exit)
                (if (/= (process-exit-status proc) 0)
                    (message "Git process failed.")
                  (dired-git--parse-git-status ,root ,proc-buf ,curbuf))))))))))

(provide 'dired-git)
