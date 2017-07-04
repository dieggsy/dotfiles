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

(defgroup epe nil
  "Eshell git prompt"
  :group 'eshell-prompt)

(defface egp-remote-face
  '((t (:inherit font-lock-comment-face)))
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

(defun egp-git-status (&rest args)
  (shell-command-to-string
   (concat "/usr/bin/git status " (string-join args " "))))

(defun egp-search-all (string &rest substrings)
  (let ((count 0))
    (dolist (substr substrings)
      (cl-incf count (cl-loop
                      for i = 0 then (1+ j)
                      for counter = 0 then (1+ counter)
                      as j = (string-match substr string i)
                      when (not j)
                      return counter)))
    count))

(defun egp-stylize (color symbol str)
  (propertize (concat symbol (when str (format "%s" str)))
              'face
              `(:foreground ,color)))

(defun egp-get-branch (git-status)
  (cond ((string= git-status "") nil)
        ((string-match "no branch" git-status)
         (let ((commit (shell-command-to-string
                        "/usr/bin/git rev-parse --short HEAD")))
           (egp-stylize "#B8BB26" nil (concat ":"
                                              (substring commit 0 (1- (length commit)))))))
        (t
         (egp-stylize "#B8BB26"  nil (substring git-status
                                                (+ (string-match "## " git-status) 3)
                                                (string-match "\\.\\.\\." git-status))))))


(defun egp-get-ahead-behind (git-status)
  (let* ((first-line (substring git-status 0 (string-match "\n" git-status)))
         (ahead (string-match "ahead" first-line))
         (behind (string-match "behind" first-line)))
    (cond ((and ahead behind)
           (concat
            (egp-stylize "#d3869b" "↑" (substring first-line
                                                  (+ 6 ahead)
                                                  (string-match "," first-line)))
            (egp-stylize "#d3869b" "↓" (substring first-line
                                                  (+ 7 behnd)
                                                  (string-match "]" first-line)))))
          (ahead
           (egp-stylize "#d3869b" "↑" (substring first-line
                                                 (+ 6 ahead)
                                                 (cl-search "]" first-line))))
          (behind
           (egp-stylize "#d3869b" "↓" (substring first-line
                                                 (+ 7 behind)
                                                 (cl-search "]" first-line)))))))

(defun egp-get-staged (git-status)
  (let ((num (+ (egp-search-all git-status
                                "\nA  "
                                "\nM  "))))
    (when (/= num 0)
      (egp-stylize "#83A598"  "●" num))))

(defun egp-get-modified (git-status)
  (let ((num (+ (egp-search-all git-status
                                "\n M "
                                "\nAM "
                                "\n T "))))
    (when (/= num 0)
      (egp-stylize "#FB4933"  "✚" num))))

(defun egp-get-conflicts (git-status)
  (let ((num (+ (egp-search-all git-status
                                "\nDD "
                                "\nAU "
                                "\nUD "
                                "\nUA "
                                "\nDU "
                                "\nAA "
                                "\nUU "))))
    (when (/= num 0)
      (egp-stylize "#FB4933"   "✖" num))))

(defun egp-get-dirty (git-status)
  (when (string-match  "\n\\?\\? " git-status)
    "…"))

(defun egp-get-git-status ()
  (let ((status (egp-git-status "--porcelain" "-b" "2>" "/dev/null")))
    (when (not (string= status ""))
      (let ((branch (egp-get-branch status))
            (ahead-behind (egp-get-ahead-behind status))
            (staged (egp-get-staged status))
            (conflicts (egp-get-conflicts status))
            (modified (egp-get-modified status))
            (dirty (egp-get-dirty status)))
        (concat "("
                branch
                ahead-behind
                "|"
                (if (or staged conflicts modified dirty)
                    (concat staged
                            conflicts
                            modified
                            dirty)
                  (egp-stylize "#B8BB26"  "✓" nil))
                ")")))))

;;;###autoload
(defun egp-theme ()
  "A eshell-prompt lambda theme with directory shrinking."
  ;; (setq eshell-prompt-regexp "^[^#\nλ]* λ[#]* ")
  (setq eshell-prompt-regexp "^.* [#]*")
  (concat
   (when (tramp-tramp-file-p default-directory)
     (propertize
      (concat (tramp-file-name-user
               (tramp-dissect-file-name default-directory))
              "@"
              (tramp-file-name-real-host
               (tramp-dissect-file-name default-directory))
              " ")
      'face
      'egp-remote-face))

   (egp-get-git-status)

   (propertize (egp-fish-path (eshell/pwd) 0) 'face 'egp-dir-face)
   (propertize (if (= (user-uid) 0) "#" "") 'face 'egp-root-face)
   " "))

(provide 'egp)
;;; egp.el ends here
