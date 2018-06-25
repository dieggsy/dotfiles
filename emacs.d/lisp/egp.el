;;; egp.el --- Display git info in eshell -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Diego A. Mundo
;; Author: Diego A. Mundo <diegoamundo@gmail.com>
;; Created: 2017-05-13
;; Version: 0.1.0
;; Keywords: eshell git prompt
;; Package-Requires: ((emacs "24") (cl-lib "0.6.1"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Basically a spin-off of eshell-prompt-extras

;;; Code:
(require 'tramp)
(require 'cl-lib)
(require 'eshell)
(require 'em-ls)
(eval-when-compile
  (require 'subr-x))

(defgroup egp nil
  "Eshell git prompt"
  :group 'eshell-prompt)

(defcustom egp-prompt-symbol "λ"
  "Prompt symbol."
  :group 'egp)

(defface egp-remote-face
  '((t (:foreground "#D3869B")))
  "Face of remote info in prompt."
  :group 'egp)

(defface egp-dir-face
  '((t (:inherit eshell-ls-directory)))
  "Face of directory in prompt."
  :group 'egp)

(defface egp-root-face
  '((t (:inherit eshell-ls-unreadable-face)))
  "Face of sudo symbol in prompt."
  :group 'egp)

(defface egp-symbol-face
  '((t (:foreground "#DD6F48")))
  "Face of prompt symbol."
  :group 'egp)

(defun egp-fish-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((path (replace-regexp-in-string "^/.*?:.*?:" "" path))
         (components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))


(defun egp-stylize (color symbol &optional num)
  (if (or (not num) (and num (not (zerop num))))
      (propertize (concat symbol (when num (format "%s" num)))
                  'face
                  `(:foreground ,color))
    ""))

(defun egp-get-git-status ()
  (unless (or (file-remote-p default-directory)
              (not (locate-dominating-file default-directory ".git")))
    (let ((status (shell-command-to-string "git status --porcelain -b 2> /dev/null")))
      (let* ((split (split-string status "\n" t))
             (first-line (car split))
             (branch
              (cond ((string-match-p "(no branch)" first-line)
                     (concat
                      ":"
                      (string-trim-right
                       (shell-command-to-string "git rev-parse --short HEAD"))))
                    ((string-match-p "No commits yet" first-line)
                     "master")
                    (t (car (split-string
                             (cadr (split-string first-line " " t))
                             "\\."
                             t)))))
             (ahead-behind-pos (cl-position (string-to-char "[") first-line))
             (ahead-pos (and ahead-behind-pos
                             (string-match-p "ahead"
                                             first-line
                                             ahead-behind-pos)))
             (behind-pos (and ahead-behind-pos
                              (string-match-p "behind"
                                              first-line
                                              ahead-behind-pos)))
             (ahead (if ahead-pos
                        (string-to-number (substring first-line
                                                     (+ ahead-pos 6)
                                                     (+ ahead-pos 7)))
                      0))
             (behind (if behind-pos
                         (string-to-number (substring first-line
                                                      (+ behind-pos 7)
                                                      (+ behind-pos 8)))
                       0))
             (files (cdr split))
             (status-list (mapcar (lambda (str)
                                    (substring str 0 2))
                                  files))
             (staged (cl-count-if
                      (lambda (str)
                        (string-match-p
                         "^\\(?:A[ DM]\\|C[ DM]\\|D[ M]\\|M[ DM]\\|R[ DM]\\)$"
                         str))
                      status-list))
             (conflicts (cl-count-if
                         (lambda (str)
                           (string-match-p
                            "^\\(?:A[AU]\\|D[DU]\\|U[ADU]\\)$"
                            str))
                         status-list))
             (modified (cl-count-if
                        (lambda (str)
                          (string-match-p
                           "^\\(?: [DM]\\|A[DM]\\|C[DM]\\|M[DM]\\|R[DM]\\)$"
                           str))
                        status-list))
             (dirty (member "??" status-list)))
        (format "(%s%s%s|%s%s%s%s%s)"
                (egp-stylize "#B8BB26" branch)
                (egp-stylize "#D3869B" "↑" ahead)
                (egp-stylize "#D3869B" "↓" behind)
                (egp-stylize "#83A598" "●" staged)
                (egp-stylize "#FB4933" "×" conflicts)
                (egp-stylize "#FB4933" "+" modified)
                (if dirty "…" "")
                (if (and (zerop staged)
                         (zerop conflicts)
                         (zerop modified)
                         (not dirty))
                    (egp-stylize "#B8BB26" "✓")
                  ""))))))

;;;###autoload
(defun egp-theme ()
  "A eshell-prompt lambda theme with directory shrinking."
  (setq eshell-prompt-regexp "^[^#\nλ]* λ[#]* ")
  (propertize
   (concat
    (let* ((host (file-remote-p default-directory 'host)))
      (when host
        (propertize
         (cond ((and default-directory (string= host (system-name)))
                (concat "@" (file-remote-p default-directory 'user)))
               (default-directory (concat (file-remote-p default-directory
                                                         'user)
                                          "@" host)))
         'face 'egp-remote-face)))
    (egp-get-git-status)
    (propertize (egp-fish-path (eshell/pwd) 0) 'face 'egp-dir-face)
    " "
    (propertize egp-prompt-symbol 'face 'egp-symbol-face)
    (propertize (if (= (user-uid) 0) "#" "") 'face 'egp-root-face)
    " ")
   'read-only t
   'front-sticky '(:font-lock-face read-only)
   'rear-nonsticky '(:font-lock-face read-only)))

(provide 'egp)
;;; egp.el ends here
