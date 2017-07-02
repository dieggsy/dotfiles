(defun pop-all (list-of-lists)
  "Pop each list in the list, return list of pop results
and an indicator if some list has been exhausted."
  (loop for tail on list-of-lists collect (pop (car tail))))

(defun transpose (list-of-lists)
  "Transpose the matrix."
  (loop with tails = (copy-list list-of-lists)
        while (some #'consp tails) ; or any?
        collect (pop-all tails)))

(defun format-spreadsheet-paste ()
  (interactive)
  (let* ((reg (buffer-substring-no-properties (region-beginning) (region-end)))
         (headers (split-string (car (split-string reg "\n")) "\t"))
         (trials (cdr (split-string reg "T[0-9]+")))
         (final-string "{\n")
         (counter 0))
    (dolist (trial ;; (length)
             trials)
      (incf counter)
      (let* ((trial-lines (cl-remove-if
                           (lambda (x) (string-prefix-p "Rate:" x))
                           (mapcar #'string-trim (split-string trial "\n"))))
             (trial-tabsplit (transpose
                              (append
                               (list headers)
                               (cl-remove-if
                                (lambda (x) (string-empty-p (car x)))
                                (mapcar (lambda (x) (split-string x "\t"))
                                        trial-lines))))))
        (setq final-string (concat final-string "\"T" (number-to-string counter) "\": {"))
        (dotimes (x  (length headers))
          (let ((heading (car (nth x trial-tabsplit)))
                (other (cdr (nth x trial-tabsplit))))
            (if (= x 0)
                (setq final-string (concat final-string
                                           (format "\n\"%s\": " heading)
                                           (format "[\"%s\"]" (string-join other "\", \""))))
              (setq final-string (concat final-string
                                         (format "\n\"%s\": " heading)
                                         (format "[%s]" (string-join other ", "))))))
          (unless (= x (1- (length headers)))
            (setq final-string (concat final-string ","))))
        (if  (= counter (length trials))
            (setq final-string (concat final-string "\n}\n"))
          (setq final-string (concat final-string "\n},\n")))))
    (setq final-string (concat final-string "}\n"))
    (setq final-string (replace-regexp-in-string
                        " \\[.*?\\]\"" "\""
                        (replace-regexp-in-string ", \\(, \\)+" ", " final-string)))

    (kill-region (region-beginning) (region-end))
    (insert final-string)))
