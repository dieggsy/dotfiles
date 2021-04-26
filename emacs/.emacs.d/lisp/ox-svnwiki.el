;;; ox-snvwiki.el --- Org Export Backend to svnwiki

(eval-when-compile (require 'cl-lib))
(require 'ox-ascii)
(require 'ox-publish)
(require 'rx)

;;; Define Back-End
(org-export-define-backend 'svnwiki
  '((bold . org-svnwiki-bold)
    (paragraph . org-svnwiki-paragraph)
    (section . org-svnwiki-section)
    (code . org-svnwiki-verbatim)
    (example-block . org-svnwiki-example-block)
    (export-block . org-svnwiki-export-block)
    (superscript . org-svnwiki-superscript)
    (subscript . org-svnwiki-subscript)
    (fixed-width . org-svnwiki-example-block)
    (headline . org-svnwiki-headline)
    (table . org-html-table)
    (table-cell . org-html-table-cell)
    (table-row . org-org-html-table-row)
    (plain-text . org-svnwiki-plain-text)
    (horizontal-rule . org-svnwiki-horizontal-rule)
    (inline-src-block . org-svnwiki-verbatim)
    (italic . org-svnwiki-italic)
    (item . org-svnwiki-item)
    (link . org-svnwiki-link)
    (plain-list . org-svnwiki-plain-list)
    (quote-block . org-svnwiki-example-block)
    (src-block . org-svnwiki-src-block)
    (verbatim . org-svnwiki-verbatim))
  ;; :filters-alist '((:filter-parse-tree . org-svnwiki-separate-elements))
  :menu-entry
  '(?w "Export to svnwiki Formatting"
       ((?W "To temporary buffer"
            (lambda (a s v b) (org-svnwiki-export-as-svnwiki a s v)))
        (?w "To file" (lambda (a s v b) (org-svnwiki-export-to-svnwiki a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-svnwiki-export-to-svnwiki t s v)
                (org-open-file (org-svnwiki-export-to-svnwiki nil s v))))))))

(defun org-svnwiki-subscript (subscript contents _info)
  "Transcode a SUBSCRIPT object from Org to ASCII.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (if (org-element-property :use-brackets-p subscript)
      (format "_(%s)" contents)
    (format "_%s" contents)))

(defun org-svnwiki-superscript (superscript contents _info)
  "Transcode a SUPERSCRIPT object from Org to ASCII.
CONTENTS is the contents of the object.  INFO is a plist holding
contextual information."
  (if (org-element-property :use-brackets-p superscript)
      (format "^(%s)" contents)
    (format "^%s" contents)))

(defun org-svnwiki-paragraph (paragraph contents info)
  contents)

;;;; Bold
(defun org-svnwiki-bold (bold contents info)
  "Transcode BOLD object into svnwiki WikiFormat format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format "'''%s'''" contents))

;;;; Example Block
(defun org-svnwiki-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK element into svnwiki WikiFormat format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  ;; (concat (replace-regexp-in-string
  ;;          "^" "{{{\n"
  ;;          (org-remove-indentation
  ;;           (org-export-format-code-default example-block info))) "\n}}}")
  ;; (concat  "{{{\n" (org-remove-indentation
  ;;                   (org-export-format-code-default example-block info)) "}}}")
  (concat " " (replace-regexp-in-string "\n" "\n " (org-export-format-code-default example-block info)))
  )

;;; Export Block
(defun org-svnwiki-export-block (export-block contents info)
  "Transcode a EXPORT-BLOCK element from Org to Svnwiki WikiFormat.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (if (member (org-element-property :type export-block) '("svnwiki" "html"))
      (org-remove-indentation (org-element-property :value export-block))))


(defun org-svnwiki-headline (headline contents info)
  "Transcode HEADLINE element into svnwiki WikiFormat format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (let* ((level (org-export-get-relative-level headline info))
         (title (org-export-data (org-element-property :title headline) info))
         )
    (concat (make-string (1+ level) ?=) " " title "\n" contents)
    ))

(defun org-svnwiki-section (section contents info)
  "Transcode SECTION element into Trac WikiFormat format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)

;;;; Horizontal Rule
(defun org-svnwiki-horizontal-rule (horizontal-rule contents info)
  "Transcode HORIZONTAL-RULE element into svnwiki WikiFormat format.
CONTENTS is the horizontal rule contents.  INFO is a plist used
as a communication channel."
  "----")

;;;; Italic
(defun org-svnwiki-italic (italic contents info)
  "Transcode ITALIC object into Svnwiki WikiFormat format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "''%s''" contents))

(defun org-svnwiki-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to ASCII.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information."
  contents)

;;;; Item
(defun org-svnwiki-item (item contents info)
  "Transcode ITEM element into Svnwiki WikiFormat format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
         ;; (struct (org-element-property :structure item))
         ;; (bullet (if (not (eq type 'ordered)) "*"
         ;;           (concat (number-to-string
         ;;                    (car (last (org-list-get-item-number
         ;;                                (org-element-property :begin item)
         ;;                                struct
         ;;                                (org-list-prevs-alist struct)
         ;;                                (org-list-parents-alist struct)))))
         ;;                   ".")))
         ;; (bullet-length (length bullet))
         )
    (cond ((eq type 'descriptive)
           (let* ((tag (car (org-element-property :tag item)))
                  (info (org-export-data tag info)))
             (message "%s %s" tag contents)
             (if (member tag '("macro" "procedure" "read" "parameter" "record"
                                     "string" "class" "method" "constant" "setter"
                                     "syntax" "type"))
                 (format "<%s>%s</%s>" tag (string-trim-right contents) tag)
               (format "; %s : %s" (or info "(no_tag)") contents))))
          ;; (t
          ;;  (concat bullet
          ;;          " "
          ;;          (cl-case (org-element-property :checkbox item)
          ;;            (on "[X] ")
          ;;            (trans "[-] ")
          ;;            (off "[ ] "))
          ;;          (let ((tag (org-element-property :tag item)))
          ;;            (and tag (format "**%s:** "(org-export-data tag info))))
          ;;          (and contents
          ;;               (org-trim (replace-regexp-in-string "^" (make-string (1+ bullet-length) ? ) contents)))))
          )))

;;;; Link
(defun org-svnwiki-link (link contents info)
  "Transcode LINK object into Svnwiki WikiFormat format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((path (org-element-property :raw-link link)))
    (if (not contents) (format "[[%s]]" path)
      (format "[[%s|%s]]" path contents))))

(defun org-svnwiki-plain-text (text info)
  "Transcode a TEXT string into Trac WikiFormat format.
TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  ;; (setq text (replace-regexp-in-string "\\^" "!^" text))
  ;; (let ((case-fold-search nil))
  ;;   (unless (eq 'link (org-element-type (org-element-property :parent text)))
  ;;     (setq text (replace-regexp-in-string   (rx bow (group (and  upper (one-or-more lower) (+  (and upper (one-or-more lower))))) eow)
  ;;                                          "!\\1" text))))
  text)

;;;; Plain List
;; (defun org-trac-plain-list (plain-list contents info)
;;   "Transcode PLAIN-LIST element into Trac WikiFormat format.
;; CONTENTS is the plain-list contents.  INFO is a plist used as
;; a communication channel."
;;   contents)

;;; Source Block
(defun org-svnwiki-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Flavored Svnwiki WikiFormat format.
CONTENTS is nil.  INFO is a plist used as a communication channel."
  (let* ((lang (org-element-property :language src-block))
         (lang (or (assoc-default lang '(("emacs-lisp" . "elisp"))) lang))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "<enscript highlight=" lang ">\n"))
         (suffix "\n</enscript>\n"))
    (concat prefix code suffix)))

(defun org-svnwiki-verbatim (verbatim contents info)
  "Transcode VERBATIM object into Trac WikiFormat format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
(let ((value (org-element-property :value verbatim)))
  (format "{{%s}}" value)))

;; (defun org-svnwiki-separate-elements (tree backend info)
;;   "Fix blank lines between elements.

;; TREE is the parse tree being exported.  BACKEND is the export
;; back-end used.  INFO is a plist used as a communication channel.

;; Enforce a blank line between elements.  There are two exceptions
;; to this rule:

;;   1. Preserve blank lines between sibling items in a plain list,

;;   2. In an item, remove any blank line before the very first
;;      paragraph and the next sub-list when the latter ends the
;;      current item.

;; Assume BACKEND is `trac'."
;;   (org-element-map tree (remq 'item org-element-all-elements)
;;     (lambda (e)
;;       (org-element-put-property
;;        e :post-blank
;;        (if (or (and (eq (org-element-type e) 'paragraph)
;;                     (eq (org-element-type (org-element-property :parent e)) 'item)
;;                     (org-export-first-sibling-p e info)
;;                     (let ((next (org-export-get-next-element e info)))
;;                       (and (eq (org-element-type next) 'plain-list)
;;                            (not (org-export-get-next-element next info)))))
;;                (eq (org-element-type e) 'table-row)
;;                )
;; 	   0
;; 	 1))))
;;   ;; Return updated tree.
;;   tree)
;;;###autoload
(defun org-svnwiki-export-as-svnwiki (&optional async subtreep visible-only)
  "Export current buffer to a SVN WikiFormat buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org Trac Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'svnwiki "*Org svnwiki Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-svnwiki-convert-region-to-svnwiki ()
  "Assume the current region has 'org-mode' syntax, and convert it to Trac.
This can be used in any buffer.  For example, you can write an
itemized list in 'org-mode' syntax in a Trac WikiFormat buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'svnwiki))


;;;###autoload
(defun org-svnwiki-export-to-svnwiki (&optional async subtreep visible-only)
  "Export current buffer to a Trac WikiFormat file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".svnwiki" subtreep)))
    (org-export-to-file 'svnwiki outfile async subtreep visible-only)))

(provide 'ox-trac)
;;; ox-trac.el ends here

