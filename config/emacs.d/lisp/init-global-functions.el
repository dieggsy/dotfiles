(defun occur-last-search ()
   "Run `occur` with the last evil search term."
   (interactive)
   ;; Use the appropriate search term based on regexp setting.
   (let ((term (if evil-regexp-search
				   (car-safe regexp-search-ring)
				 (car-safe search-ring))))
	 ;; If a search term exists, execute `occur` on it.
	 (if (> (length term) 0)
		 (occur term)
	   (message "No term to search for."))))

(defun show-first-occurrence ()
  "Display the location of the word at point's first occurrence in the buffer."
  (interactive)
  (save-excursion
	(let ((search-word (thing-at-point 'symbol t)))
	  (goto-char 1)
	  (re-search-forward search-word)
	  (message (concat
				"L" (number-to-string (line-number-at-pos)) ": "
				(replace-regexp-in-string
				 "[ \t\n]*\\'"
				 ""
				 (thing-at-point 'line t)
				 ))))))

(defun switch-to-previous-buffer ()
    "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
	(interactive)
	  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;; Helpers for narrowing.
(defun narrow-and-set-normal ()
  "Narrow to the region and, if in a visual mode, set normal mode."
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (if (string= evil-state "visual")
	  (progn (evil-normal-state nil)
			 (evil-goto-first-line))))

(defun narrow-to-region-or-subtree ()
  "Narrow to a region, if set, otherwise to an Org subtree, if present."
  (interactive)
  (if (and mark-active
		   (not (= (region-beginning) (region-end))))
	  (narrow-and-set-normal)
	(if (derived-mode-p 'org-mode)
		(org-narrow-to-subtree))))

(defun air-narrow-dwim ()
    "Narrow to a thing or widen based on context.
Attempts to follow the Do What I Mean philosophy."
	(interactive)
	(if (buffer-narrowed-p)
		(widen)
	  (narrow-to-region-or-subtree)))


(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
	  (let* ((this-win-buffer (window-buffer))
			 (next-win-buffer (window-buffer (next-window)))
			 (this-win-edges (window-edges (selected-window)))
			 (next-win-edges (window-edges (next-window)))
			 (this-win-2nd (not (and (<= (car this-win-edges)
										 (car next-win-edges))
									 (<= (cadr this-win-edges)
										 (cadr next-win-edges)))))
			 (splitter
			  (if (= (car this-win-edges)
					 (car (window-edges (next-window))))
				  'split-window-horizontally
				'split-window-vertically)))
		(delete-other-windows)
		(let ((first-win (selected-window)))
		  (funcall splitter)
		  (if this-win-2nd (other-window 1))
		  (set-window-buffer (selected-window) this-win-buffer)
		  (set-window-buffer (next-window) next-win-buffer)
		  (select-window first-win)
		  (if this-win-2nd (other-window 1))))))


(provide 'init-global-functions)
