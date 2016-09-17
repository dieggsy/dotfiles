(require 'cl)
(require 'ivy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General-purpose formatting functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ivy--cands-get-details (format_spec cand_info_fns entries)
  "Extract symbols from FORMAT_SPEC, and evaluate the
corresponding function found in CAND_INFO_FNS for all ENTRIES.
For example:
(setq my_format_spec '(make-upper-case
		       \" string doesn't count as key \"
		       (add-gmbh . \"%s\")))
(setq my_info_fns (list
		   '(make-upper-case . upcase)
		    '(add-gmbh . (lambda (x) (concat x \" GmbH\")))))
(ivy--cands-get-details my_format_spec my_info_fns '(\"audi\"))

-->  ((make-upper-case . \"AUDI\") (add-gmbh . \"audi GmbH\"))."
  (cl-flet* ((calc-symbol-entry
	     (entry info_sym)
	     (let ((info_fn (cdr (assq info_sym cand_info_fns))))
	       (cons info_sym (funcall info_fn entry))))

	    (calc-all-symbols-entry
	     (info_symbols entry)
	     (mapcar (apply-partially #'calc-symbol-entry entry) info_symbols))
	    )
    
    (let ((info_symbols
	   (mapcar
	    (lambda (x) (if (consp x) (car x) x))
	    (remove-if #'stringp format_spec))))
      
      (mapcar (apply-partially #'calc-all-symbols-entry info_symbols)
	      entries)
      )))

(defun ivy--format-maxlen-col (cands_info col)
  "Compute the max length of COL for entries in CANDS_INFO.
Return this value as string."
  (cl-flet* ((get-symstr-len
	      (cand symstr)
	      (length
	       (cond ((stringp symstr) symstr)
		     ((consp symstr)
		      (format (cdr symstr) (cdr (assq (car symstr cand)))))
		     (t (format "%s" (cdr (assq symstr cand)))))))
	     (get-col-len
	      (col cand)
	      (apply '+
		     (mapcar (apply-partially #'get-symstr-len cand) col)))
	     )
    (number-to-string (apply #'max (mapcar (apply-partially #'get-col-len col) cands_info)))
    ))

(defun ivy--format-template-columns (format_spec default_colfrmt cands_info)
  (let ((columns nil)
	(col nil)
	(colfrmt default_colfrmt))

    ;; Split the elements of format_spec into columns
    ;; based on the presence of the element "<col>".
    ;; Each new "<col>" creates a new column.
    ;; If an element is a string of the form "%.*?s"
    ;; use that string in place of default_colfrmt. 
    (dolist (entry format_spec)
      (if (stringp entry)
	  (cond ((string-equal "<col>" entry)
		 (if col
		     (push (cons colfrmt (nreverse col)) columns))
		 (setq col nil)
		 (setq colfrmt default_colfrmt))
		((string-match "%.*?s" entry)
		 (setq colfrmt entry))
		(t
		 (push entry col)))
	(push entry col)))
    (if col (push (cons colfrmt (nreverse col)) columns))
    (setq columns (nreverse columns))
    ;; Each col in columns now starts with a frmt string
    ;; e.g., (car col) --> "%10s" or "%-<MAXLEN>.<MAXLEN>s"
    ;; the rest of each col contains the symbols and strings
    ;; to be displayed.
    
    ;; Iterate over columns and check if the column's format string
    ;; contains "<MAXLEN>".
    ;; If it does calculate the maximum length of that column
    ;; and replace "<MAXLEN>" with that value.
    (dolist (col columns)
      (if (string-match "<MAXLEN>" (car col))
	  (setcar col
		  (replace-regexp-in-string
		   "<MAXLEN>"
		   (ivy--format-maxlen-col cands_info (cdr col))
		   (car col)))))
    columns
    ))
      
(defun ivy--format-make-rows (cands_info template_columns)
  (cl-flet* ((listcat (x) (apply #'concat x))
	     (format-symbol (symbol row)
			    (or (cdr (assq symbol row)) ""))
	     (format-element
	      (row elem)
	      (cond ((stringp elem) elem)
		    ((consp elem)
		     (format (cdr elem) (format-symbol (car elem) row)))
		    (t (format-symbol elem row))))
	     (format-column
	      (template_col row)
	      (format (car template_col) (listcat (mapcar (apply-partially #'format-element row) (cdr template_col)))))
	     (format-row
	      (template_cols row)
	      (listcat (mapcar (lambda (x) (format-column x row)) template_cols)))
	     )
    (mapcar (apply-partially #'format-row template_columns) cands_info)))

(defun ivy--format-details (cands format_spec info_fns &optional default_colfrmt)
  "This is the main ivy--format-* function.
You shouldn't need to touch the other ones in general"
  (if (not default_colfrmt)
      (setq default_colfrmt "%-<MAXLEN>.<MAXLEN>s"))
  
  (let* ((cands_info (ivy--cands-get-details format_spec info_fns cands))
	 (template_columns (ivy--format-template-columns format_spec default_colfrmt cands_info)))
    (ivy--format-make-rows cands_info template_columns)
    ))

(defun ivy-detailed-candidates (candidates format_spec info_fns &optional default_colfrmt)
  "Given a list of CANDIDATES calculate the detailed candidates
based on FORMAT_SPEC, INFO_FNS, and optionally DEFAULT_COLFRMT.

It returns an association list with each element of the form
(detailed candidates . candidates)

This association list can be given directly to `ivy-read'.
If passed to `ivy-read' the 'detailed candidates' will
be displayed, while the ivy-read actions will be passed
the corresponding candidate.
"
  (let ((cands_detail
	 (ivy--format-details candidates format_spec info_fns default_colfrmt)))
    (mapcar* #'cons cands_detail candidates)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for calculating information about a buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'.
Plus a bit."
  (let* ((offset (max (- (length dir) max-length) 0))
	(chopped_string (substring dir offset)))
    (replace-regexp-in-string "^[^~][^/].*?/" "…/" chopped_string)))

(defun shorten-buffer (bname max-length)
  "Show up to `max-length' characters of a buffer name `bname'"
  (let ((mlen (min (length bname) max-length)))
    (substring bname 0 mlen)))

(defmacro ivy--buffer-make-info-fn(info_symbol real_buf_expr virt_buf_expr)
  (let ((sym info_symbol)
	(symfaces))
    `(cons ,sym
	   (lambda (buffer)
	     (setq symfaces (cdr (assq ,sym ivy-buffer-format-faces)))
	     ;; (message "%s faces --> %s" ,sym symfaces)
	     (if (get-buffer buffer)
		 (let ((buffer_object (get-buffer buffer)))
		   (if (car symfaces)
		       (propertize ,real_buf_expr 'face (car symfaces))
		     ,real_buf_expr))
	       (if (cdr symfaces)
		   (propertize ,virt_buf_expr 'face (cdr symfaces))
		 ,virt_buf_expr))))
    )
  )

(defvar ivy--buffer-info-fns
  (list

   (ivy--buffer-make-info-fn
    'buffer-name
    (shorten-buffer buffer ivy-buffer-max-buffer-display-length)
    (shorten-buffer buffer ivy-buffer-max-buffer-display-length))

   (ivy--buffer-make-info-fn
    'mode
    (with-current-buffer buffer_object (format-mode-line mode-name))
    '"Virtual")
   
   (ivy--buffer-make-info-fn
    'dir
    (shorten-directory (abbreviate-file-name
			(or (with-current-buffer
				buffer_object default-directory) ""))
		       ivy-buffer-max-dir-display-length)
    (shorten-directory (abbreviate-file-name
			(file-name-directory
			 (cdr (assq buffer ivy--virtual-buffers))))
		       ivy-buffer-max-dir-display-length))

   (ivy--buffer-make-info-fn
    'process
    (if (get-buffer-process buffer_object) "º" "")
    '"")

   (ivy--buffer-make-info-fn
    'file-name
    (file-name-nondirectory (or (buffer-file-name buffer_object) ""))
    '"")
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User customizable display settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar ivy-buffer-format
  '(
    buffer-name   "<col>"  "|"
    mode process  "<col>"  "|"
    dir file-name "<col>"  "|"
    )
  "Describe the display format for `ivy-switch-buffer'.
It is a list containing elements that are symbols, cons cells,
or strings.

- symbol: can be any symbol defined in `ivy--buffer-info-fns'
          e.g., BUFFER-NAME or MODE
- cons cell: (symbol . formatting_string)
          e.g., (BUFFER-NAME . \"[%s]\")
          will format BUFFER-NAME inside of square brackets
- string: Either \"<col>\" which left-aligns all of the following
          text before the next \"<col>\", or another string which
          is inserted literally in the current column.
"
  )

(defvar ivy-buffer-format-faces
  (list
   '(buffer-name . (font-lock-builtin-face . font-lock-comment-face))
   '(dir . (font-lock-warning-face . nil))
   '(process . (font-lock-variable-name-face . nil))
   '(mode . (font-lock-type-face . nil))
   )
  "Describe the text faces (coloring, font, etc) of the symbols.
It is a list containing cons cells.

Eachs cons cell has the format:
    '(SYMBOL . (Real buffer face . Virtual buffer face))
For example, the cons cell:
    '(mode . (font-lock-type-face . nil))
    Will apply `font-lock-type-face' to the output of mode
    function for real buffers, and apply nothing to the
    output for a virtual buffer.
"
  )

(defvar ivy-buffer-max-dir-display-length 25
  "Truncate the display length of a directory to this value.")

(defvar ivy-buffer-max-buffer-display-length 50
  "Truncate the display length of buffers to this value.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code for formatting entries in minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ivy--buffer-display-list ()
  (let ((total_bufs (ivy--buffer-list "" ivy-use-virtual-buffers))
	(initial_choice (buffer-name
			 (other-buffer (current-buffer))))
	(bufs_info)
	(bufs_frmted_strs)
	(template_columns))

    (push initial_choice total_bufs)
    (delete-duplicates total_bufs :from-end t)
    (ivy-detailed-candidates total_bufs ivy-buffer-format  ivy--buffer-info-fns)
    ))

;; Add this action due to a change in how ivy-call handles alists
;; https://github.com/abo-abo/swiper/commit/c009b28337f408fe571b24be7bdb304bbc596a76
(defun ivy--switch-buffer-action-alist (x)
  (ivy--switch-buffer-action
   (if (stringp x) x (cdr x))))

(defun ivy-switch-buffer ()
  "Switch to another buffer."
  (interactive)
  (progn
    (if (not ivy-mode)
	(call-interactively 'switch-to-buffer)
      (let ((this-command 'ivy-switch-buffer))
	(ivy-read "Switch to buffer: "
		  (ivy--buffer-display-list)
		  :action #'ivy--switch-buffer-action-alist
		  :keymap ivy-switch-buffer-map
		  :sort nil))))
  )

(provide 'ivy_buffer_extend)
