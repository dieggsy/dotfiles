#!/usr/bin/csi -ss
(import (only (chicken io) read-string)
        (only (chicken process) call-with-input-pipe)
        (only (chicken string) substring-index string-split))

(define (main args)
  (define wid (car args))
  (define class (cadr args))
  (define instance (caddr args))
  (define title (cadr (string-split
                       (call-with-input-pipe
                        (string-append "xwininfo -id " wid)
                        (cut read-string #f <>))
                       "\"")))

  (cond ((substring-index "emacs" instance)
         (display "state=tiled desktop=^2"))
        ((substring-index "_erc__floating_" instance)
         (with-output-to-file "/tmp/erc-float"
           (lambda () (display wid) (newline)))
         (display "layer=above state=floating hidden=on sticky=on rectangle=1085x560+15+855"))
        ((string=? instance "gl")
         (with-output-to-file "/tmp/mpv-float"
           (lambda () (display wid) (newline))))
        ((substring-index "_eshell__floating_" instance)
         (with-output-to-file "/tmp/esh-float"
           (lambda () (display wid) (newline)))
         (display "state=floating hidden=on sticky=on rectangle=1084x560+720+870"))
        ((string=? instance "urxvt")
         (when (string=? title "htop")
           (display "state=floating")))
        ((string=? instance "urxvt-float")
         (with-output-to-file "/tmp/urxvt-float"
           (lambda () (display wid) (newline)))
         (display "layer=above state=floating sticky=on rectangle=1085x560+1450+855"))
        ((and (string=? instance "guvcview")
              (substring-index "Video" title))
         (display "sticky=on rectangle=520x390+2015+206"))
        ((and (string=? instance "vlc")
              (string=? title "vlc"))
         (display "layer=above border=off"))))
