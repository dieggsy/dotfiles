;;; ob-iwan.el --- Hello there -*- lexical-binding: t; -*-

;;; Commentary:

;; Lol

;; Requires mpv and youtube-dl

;; Recommended you do something ike
;;   (add-to-list 'org-src-lang-modes '("obi-wan" . iwan))
;; before loading, if you want "obi-wan" src blocks to trigger execution too.

;;; Code:
(require 'ob)

(defun org-babel-execute:iwan (&rest _)
  (start-process-shell-command
   "mpv"
   nil
   "mpv https://youtu.be/rEq1Z0bjdwc?t=8s --no-video")
  "Hello there!")

(provide 'ob-iwan)
