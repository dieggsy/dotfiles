; SCCS I.D.  %W%  delta: %G% %U%  date: %H% %T%

;;;; tablature-mode.el -- modes for editing guitar tablature in GNU emacs

; Contains chord-mode and lead-mode, both minor modes of tab-mode.

; Author:  Mark R. Rubin <mark@phineas.jpl.nasa.gov>
; Version: 1.00
; Date:    9/20/93

; This code is released into the public domain without any express or implied
; warranty.  The author accepts no responsibility for any consequences
; arising from its use.

; This code may be distributed and modified in any way; however, please
; retain notice of original authorship in this and any derivative work.

; revision history:
; 1.00  9/20/93		Added 'xfretboard and related functions and
;			variables.  Removed optional args from 'chord-mode
;			and 'lead-mode.  Removed "tab-" prefix from
;			'tab-lead-mode and 'tab-chord-mode variables.
;			Added 'tab-delete-note, and fixed 'tab-delete-
;			chord-backward (wasn't handling non-tab delete,
;			and wipe out tuning).  Added 'tab-forward-barline
;			(and backward), and 'tab-up-staff (and down).
;			Added 'tab-set-tuning and 'tab-delete-current-note.
; 0.09	9/ 4/93		Added chord-spelling, and changed logic of 'tab-
;			analyze-chord and 'tab-analyze-chord-internal.  Added
;			'tab-12-tone-chords flag and function.  Fixed in/out
;			of tab handling of `+' key.
; 0.08	8/ 8/93		Broke 'tab-analyze-chord into two parts for
;			detection of X/Y chords.  Changed complicated defmacro
;			to defun due to speed/garbage-collection concerns.
;			Added 'tab-current-tuning, and changed
;			'tab-learn-tuning to set it.  Changed 'tab-copy-retune
;			and 'tab-analyze-chord to use 'tab-current-tuning.
;			Added 'tab-higher-string and 'tab-lower-string.  Added
;			'tab-move-string and 'tab-goto-string utilities.
;			Changed 'tab-label-chord to handle "X,noY/Z" chords.
;			Changed 'tab-pending-embellishement, 'tab-analyze-note,
;			and 'tab-analyze-fret to use 'nil rather than normal
;			data type value for flag.  Removed redundant "(progn
;			(let", etc. constructs.  Changed 'tab-label-chord
;			alignment of name over tab chord.
; 0.07	 8/ 7/93	Finished 'tab-label-chord.  Added more chords to
;			'tab-analyze-chord.
; 0.06   8/ 6/93	Added generic 'tab-begin-end-region, with safety
;			checks.  Changed 'tab-kill-internal to use it.
;			Changed 'tab-transpose to work on region.  Improved
;			tab-mode documentation.  Added 'tab-analyze-chord
;			and 'tab-label-chord.  Allowed 'tab-change-position
;			to use prefix arg.  Added 'tab-note-name.
;			Change 'tab-transpose-chord to use 'tab-analyze-fret.
; 0.05   8/ 2/93	Added 'tab-copy-retune.  Changed 'tab-transpose to use
;			'tab-transpose-chord.
; 0.04   8/ 1/93	Fixed 'tab-transpose in lead mode.  Added alternate
;			tunings.  Improved mode documentation.
; 0.03	 7/31/93	Removed 'tab-delete-note-backward ("\C-h") and
;			replaced with mode-dependent 'tab-delete-note.
; 0.02	 7/29/93	Added hard-coded VT-100 arrow-key bindings.  Added
;			"pending embellishment", indicated on mode line.
;			Added "X" embellishment.
; 0.01	 7/28/93	Original posting to rec.music.makers.guitar.tablature,
;			alt.guitar.tab,rec.music.makers.guitar,alt.guitar,
;			gnu.emacs.sources



; CUSTOMIZABLE DEFAULTS

(defvar tab-note-names
	["E" "F" "F#" "G" "Ab" "A" "Bb" "B" "C" "C#" "D" "Eb"]
"Names of notes (like A# vs. Bb) for 'tab-analyze-chord.  Change via
\\[tab-note-name] (tab-note-name)"
)

(defvar tab-current-tuning ; must match tab-X-string-prefix, below
	[0 7 3 10 5 0]
"Numeric values of the six strings, high-to-low, in current tuning."
)

; must match tab-current-tuning, above
(defvar tab-0-string-prefix "e-|" "Unique beginning of string 0 line")
(defvar tab-1-string-prefix "B-|" "Unique beginning of string 1 line")
(defvar tab-2-string-prefix "G-|" "Unique beginning of string 2 line")
(defvar tab-3-string-prefix "D-|" "Unique beginning of string 3 line")
(defvar tab-4-string-prefix "A-|" "Unique beginning of string 4 line")
(defvar tab-5-string-prefix "E-|" "Unique beginning of string 5 line")

(defvar tab-12-tone-chords
	t
"*Spell chords in understandable, rational, 12-tone system in addition
to normal 1st, 3rd, 5th, b7th, etc.  Can take value `t' for true, or
`nil' for false."
)

(defvar fretboard-program-name
	(concat (getenv "HOME") "/bin/xfretboard")
"*Default path name of auxiliary X-Windows fretboard program."
)

; end of customizable defaults



(defvar lead-mode
	nil
"On/off state of lead minor mode."
)

(defvar chord-mode
	nil
"On/off state of lead chord mode."
)

(defvar tab-mode-map
	 nil
"Mode map for tab mode.
Commands:
\\{tab-mode-map}"
)

(defvar tab-current-string
	0
"What string cursor is on."
)

(defvar tab-position
	0
"What fret index finger is on."
)

(defvar tab-position-as-string
	"0"
"String variant of tab-position, for mode line"
)

(defvar tab-pending-embellishment
	nil
"Embellishment to be added to next entered note, or blank if none"
)

(defvar tab-killed-width
	""
"Width of last killed region"
)

(defvar tab-last-chord
	""
"Chord analyzed by `\\[tab-analyze-chord]' (tab-analyze-chord), available
for automatic insertion into tab by `\\[tab-label-chord]' (tab-label-chord)"
)

(defvar tab-process
	nil
"External process which feeds commands to tab-mode."
)



(defun tab-mode () ; ----------------------------------------------------------
"Major mode for entering tablature.  Always use minor modes lead-mode
or chord-mode instead.

In tab-mode, single keys represent notes on the guitar fretboard, and
pressing them creates tablature.  This only happens if the cursor is
in a tablature staff; otherwise the keys have their normal, text, meaning.
The keys are:

                   strings

             E   A    D   G   B   e

                1   2   3   4   5   6         N
                 q   w   e   r   t   y        N+1    frets
                  a   s   d   f   g   h       N+2
                   z   x   c   v   b   n      N+3

In chord-mode, the cursor remains in place (for entry of multiple-note
chords) until manually moved forward with SPACE or `\\[forward-char]'.  In
lead-mode, the cursor automatically advances after each note.

For more information on a key or action, do:
	`\\[describe-key]' and then enter the key(s) for the action
or
	`\\[describe-function]' and then enter the name of the action


	KEY	ACTION

	{	enter chord mode
	}	enter lead mode

	=	make a new tablature staff

	<	decrement base fret position by one (is printed on mode line)
	>	increment base fret position by one (is printed on mode line)
	?	prompt for numeric entry of base fret position

	SPACE 	move one tab position forward 
	\\[tab-forward-char]	move one tab position forward 
	\\[tab-backward-char]	move one tab position backward 
	\\[tab-forward-barline]	move forward one bar line
	\\[tab-backward-barline]	move back one bar line
	\\[tab-up-staff]	move up one staff
	\\[tab-down-staff]	move down one staff


	C-h	delete previous (lead-mode) or current (chord-mode) note
	C-?	delete previous note/chord
	\\[tab-delete-chord-forward]	delete current note/chord

	C-i	insert blank space

	|	enter bar line

	[	mark current note as hammer-on
	]	mark current note as pull-off
	;	mark current note as bend
	'	mark current note as release
	/	mark current note as slide-up
	\	mark current note as slide-down
	~	mark current note as vibrato
	(	mark current note as ghost note
	-	mark current note as normal note

	+	transpose notes in region by N frets (tab-transpose)

	\\[xfretboard]	start xfretboard (optional graphical interface)

	\\[tab-copy-region-as-kill]	memorize tab between dot and mark (incl).
	\\[tab-kill-region]	as above, but also delete
	\\[tab-yank]	insert previously killed tablature

	\\[tab-copy-retune]	copy tab staff, transposing to current tuning
	\\[tab-learn-tuning]	memorize new tuning (cursor first string)
	\\[tab-analyze-chord]	analyze chord (cursor on root note)
	\\[tab-label-chord]	insert previously analyzed chord name
	\\[tab-note-name]	change whether chords are A# vs. Bb, etc.

	\\[tab-higher-string]	move note to next higher string
	\\[tab-lower-string]	move note to next higher string

	\\[tab-up-12]	move note up   12 frets
	\\[tab-down-12]	move note down 12 frets

Tablature mode recognizes when the cursor is on a tab staff (and draws
new tab staffs) with six, three-character long, strings.  Each of the six
must be unique.  To change these strings (e.g. for alternate tunings),
enter them (while *not* in tab-mode) at the beginnings of six consecutive
lines, and use `\\[execute-extended-command] tab-learn-tuning'.


Full list of commands:
\\{tab-mode-map}"


(interactive)
(setq major-mode 'tab-mode)
(setq mode-name "Tab")

	(if (not tab-mode-map) (tab-make-mode-map))
(use-local-map tab-mode-map)

(make-local-variable 'tab-current-string)
(make-local-variable 'tab-position)
(make-local-variable 'tab-position-as-string)
(make-local-variable 'tab-pending-embellishment)
(make-local-variable 'tab-killed-width)
(make-local-variable 'tab-note-names)
(make-local-variable 'tab-last-chord)
(make-local-variable 'tab-current-tuning)
(make-local-variable 'tab-12-tone-chords)
(make-local-variable 'tab-process)
(make-local-variable 'tab-0-string-prefix)
(make-local-variable 'tab-1-string-prefix)
(make-local-variable 'tab-2-string-prefix)
(make-local-variable 'tab-3-string-prefix)
(make-local-variable 'tab-4-string-prefix)
(make-local-variable 'tab-5-string-prefix)
(make-local-variable 'fretboard-program-name)

(make-local-variable 'lead-mode)
	(if (not (assq 'lead-mode minor-mode-alist))
	(setq minor-mode-alist (cons '(lead-mode " Lead")
				     minor-mode-alist)))
(setq lead-mode nil)

(make-local-variable 'chord-mode)
	(if (not (assq 'chord-mode minor-mode-alist))
	(setq minor-mode-alist (cons '(chord-mode " Chord")
				     minor-mode-alist)))
(setq chord-mode nil)

(setq mode-line-format (list ""
			     'mode-line-modified
			     'mode-line-buffer-identification
			     "   "
			     'global-mode-string
			     "   %[("
			     'mode-name
			     'minor-mode-alist
			     "--"
			     'tab-position-as-string
			     'tab-pending-embellishment
			     "%n"
			     'mode-line-process
			     ")%]----"
			     '(line-number-mode "L%l--")
			     '(-3 . "%p")
			     "-%-"))

) ; tab-mode



(defun lead-mode ()
"Turn on lead-mode, a minor mode of tab-mode.
Use `\\[describe-function] tab-mode' to see documentation for tab-mode."
(interactive)

	(if (not (equal major-mode 'tab-mode))
	(tab-mode))

(setq lead-mode t)
(setq chord-mode nil)

(set-buffer-modified-p (buffer-modified-p))   ; No-op, but updates mode line.
)   ; lead-mode



(defun chord-mode ()
"Turn on chord-mode, a minor mode of tab-mode.
Use `\\[describe-function] tab-mode' to see documentation for tab-mode."
(interactive)

	(if (not (equal major-mode 'tab-mode))
	(tab-mode))

(setq chord-mode t)
(setq lead-mode nil)

(set-buffer-modified-p (buffer-modified-p))   ; No-op, but updates mode line.
)   ; chord-mode



(defun tab-12-tone-chords (arg) ; ---------------------------------------------
"Toggle 'tab-12-tone-chords flag, or set/clear according to optional argument.
Flag controls whether chord spelling also includes rational 12-tone version."
(interactive "P")
(setq tab-12-tone-chords (if (null arg) (not tab-12-tone-chords)
					(> (prefix-numeric-value arg) 0)))
) ; tab-12-tone-chords



(defun rebind-keys (stock custom) ; -------------------------------------------
"Rebind to second arg all keys currently bound to first arg."
(let ((binding-list (where-is-internal stock)))
	(while binding-list
	(define-key tab-mode-map (car binding-list) custom)
	(setq binding-list (cdr binding-list))
	)
)) ; rebind-keys



(defun tab-make-mode-map () ; -------------------------------------------------
"Create tab mode map"

; DEBUG ... hard-coded arrow-key bindings
(global-unset-key	 "\M-[")
; (global-unset-key	 "\M-O")

(setq tab-mode-map (copy-keymap (current-global-map)))

; DEBUG ... hard-coded arrow-key bindings
(define-key tab-mode-map "\M-[A"	'previous-line)
(define-key tab-mode-map "\M-[B"	'next-line)
(define-key tab-mode-map "\M-[C"	'tab-forward-char)
(define-key tab-mode-map "\M-[D"	'tab-backward-char)
; (define-key tab-mode-map "\M-OA"	'previous-line)
; (define-key tab-mode-map "\M-OB"	'next-line)
; (define-key tab-mode-map "\M-OC"	'tab-forward-char)
; (define-key tab-mode-map "\M-OD"	'tab-backward-char)

; DEBUG ... doesn't work in 19.X in non-X mode
; (define-key tab-mode-map [up]		'previous-line)
; (define-key tab-mode-map [down]		'next-line)
; (define-key tab-mode-map [right]	'tab-forward-char)
; (define-key tab-mode-map [left]		'tab-backward-char)

(let ((key-ndx 32))
	(while (< key-ndx 128) (progn
	(define-key tab-mode-map (char-to-string key-ndx) 'tab-unused-key)
	(setq key-ndx (1+ key-ndx))
	))
)

(define-key tab-mode-map "{"	'chord-mode)
(define-key tab-mode-map "}"	'lead-mode)

(define-key tab-mode-map "="	'tab-make-staff)

(define-key tab-mode-map "<"	'tab-decrement-position)
(define-key tab-mode-map ">"	'tab-increment-position)
(define-key tab-mode-map "?"	'tab-set-position)

(define-key tab-mode-map " "	'tab-forward)
(define-key tab-mode-map "|"	'tab-barline)

(define-key tab-mode-map "\C-h"	'tab-delete-note)
(define-key tab-mode-map "\C-?"	'tab-delete-chord-backward)
(rebind-keys 'delete-char 'tab-delete-chord-forward)

(define-key tab-mode-map "\C-i"	'tab-insert)

(rebind-keys 'backward-char 'tab-backward-char)
(rebind-keys 'forward-char  'tab-forward-char)
(rebind-keys 'scroll-down   'tab-up-staff)
(rebind-keys 'scroll-up     'tab-down-staff)

(rebind-keys 'kill-region 'tab-kill-region)
(rebind-keys 'copy-region-as-kill 'tab-copy-region-as-kill)
(rebind-keys 'yank 'tab-yank)

(define-key tab-mode-map "+"	'tab-transpose)
(define-key tab-mode-map "8"	'tab-analyze-chord)
(define-key tab-mode-map "*"	'tab-label-chord)

(define-key tab-mode-map "9"	'tab-higher-string)
(define-key tab-mode-map "o"	'tab-lower-string)
(define-key tab-mode-map "0"	'tab-up-12)
(define-key tab-mode-map "p"	'tab-down-12)

(define-key tab-mode-map "["	'tab-hammer)
(define-key tab-mode-map "]"	'tab-pull)
(define-key tab-mode-map ";"	'tab-bend)
(define-key tab-mode-map "'"	'tab-release)
(define-key tab-mode-map "/"	'tab-slide-up)
(define-key tab-mode-map "\\"	'tab-slide-down)
(define-key tab-mode-map "~"	'tab-vibrato)
(define-key tab-mode-map "("	'tab-ghost)
(define-key tab-mode-map "."	'tab-muffled)
(define-key tab-mode-map "-"	'tab-normal)

; Chord diagram style keybindings
(define-key tab-mode-map "\M-1"	'tab-E-open)
(define-key tab-mode-map "\M-2"	'tab-A-open)
(define-key tab-mode-map "\M-3"	'tab-D-open)
(define-key tab-mode-map "\M-4"	'tab-G-open)
(define-key tab-mode-map "\M-5"	'tab-B-open)
(define-key tab-mode-map "\M-6"	'tab-e-open)
(define-key tab-mode-map "!"	'tab-E-1)
(define-key tab-mode-map "@"	'tab-A-1)
(define-key tab-mode-map "#"	'tab-D-1)
(define-key tab-mode-map "$"	'tab-G-1)
(define-key tab-mode-map "%"	'tab-B-1)
(define-key tab-mode-map "^"	'tab-e-1)
(define-key tab-mode-map "1"	'tab-E0)
(define-key tab-mode-map "2"	'tab-A0)
(define-key tab-mode-map "3"	'tab-D0)
(define-key tab-mode-map "4"	'tab-G0)
(define-key tab-mode-map "5"	'tab-B0)
(define-key tab-mode-map "6"	'tab-e0)
(define-key tab-mode-map "q"	'tab-E1)
(define-key tab-mode-map "w"	'tab-A1)
(define-key tab-mode-map "e"	'tab-D1)
(define-key tab-mode-map "r"	'tab-G1)
(define-key tab-mode-map "t"	'tab-B1)
(define-key tab-mode-map "y"	'tab-e1)
(define-key tab-mode-map "a"	'tab-E2)
(define-key tab-mode-map "s"	'tab-A2)
(define-key tab-mode-map "d"	'tab-D2)
(define-key tab-mode-map "f"	'tab-G2)
(define-key tab-mode-map "g"	'tab-B2)
(define-key tab-mode-map "h"	'tab-e2)
(define-key tab-mode-map "z"	'tab-E3)
(define-key tab-mode-map "x"	'tab-A3)
(define-key tab-mode-map "c"	'tab-D3)
(define-key tab-mode-map "v"	'tab-G3)
(define-key tab-mode-map "b"	'tab-B3)
(define-key tab-mode-map "n"	'tab-e3)
(define-key tab-mode-map "Z"	'tab-E4)
(define-key tab-mode-map "X"	'tab-A4)
(define-key tab-mode-map "C"	'tab-D4)
(define-key tab-mode-map "V"	'tab-G4)
(define-key tab-mode-map "B"	'tab-B4)
(define-key tab-mode-map "N"	'tab-e4)

; Jeff Healey style keybindings -- change tab-mode documentation if used.
; (define-key tab-mode-map "z"	'tab-E0)
; (define-key tab-mode-map "a"	'tab-A0)
; (define-key tab-mode-map "q"	'tab-D0)
; (define-key tab-mode-map "1"	'tab-G0)
; (define-key tab-mode-map "Z"	'tab-D0)
; (define-key tab-mode-map "A"	'tab-G0)
; (define-key tab-mode-map "Q"	'tab-B0)
; (define-key tab-mode-map "!"	'tab-e0)
; (define-key tab-mode-map "x"	'tab-E1)
; (define-key tab-mode-map "s"	'tab-A1)
; (define-key tab-mode-map "w"	'tab-D1)
; (define-key tab-mode-map "2"	'tab-G1)
; (define-key tab-mode-map "X"	'tab-D1)
; (define-key tab-mode-map "S"	'tab-G1)
; (define-key tab-mode-map "W"	'tab-B1)
; (define-key tab-mode-map "@"	'tab-e1)
; (define-key tab-mode-map "c"	'tab-E2)
; (define-key tab-mode-map "d"	'tab-A2)
; (define-key tab-mode-map "e"	'tab-D2)
; (define-key tab-mode-map "3"	'tab-G2)
; (define-key tab-mode-map "C"	'tab-D2)
; (define-key tab-mode-map "D"	'tab-G2)
; (define-key tab-mode-map "E"	'tab-B2)
; (define-key tab-mode-map "#"	'tab-e2)
; (define-key tab-mode-map "v"	'tab-E3)
; (define-key tab-mode-map "f"	'tab-A3)
; (define-key tab-mode-map "r"	'tab-D3)
; (define-key tab-mode-map "4"	'tab-G3)
; (define-key tab-mode-map "V"	'tab-D3)
; (define-key tab-mode-map "F"	'tab-G3)
; (define-key tab-mode-map "R"	'tab-B3)
; (define-key tab-mode-map "$"	'tab-e3)
; (define-key tab-mode-map "b"	'tab-E4)
; (define-key tab-mode-map "g"	'tab-A4)
; (define-key tab-mode-map "t"	'tab-D4)
; (define-key tab-mode-map "5"	'tab-G4)
; (define-key tab-mode-map "B"	'tab-D4)
; (define-key tab-mode-map "G"	'tab-G4)
; (define-key tab-mode-map "T"	'tab-B4)
; (define-key tab-mode-map "%"	'tab-e4)
; (define-key tab-mode-map "n"	'tab-E5)
; (define-key tab-mode-map "h"	'tab-A5)
; (define-key tab-mode-map "y"	'tab-D5)
; (define-key tab-mode-map "6"	'tab-G5)
; (define-key tab-mode-map "N"	'tab-D5)
; (define-key tab-mode-map "H"	'tab-G5)
; (define-key tab-mode-map "Y"	'tab-B5)
; (define-key tab-mode-map "^"	'tab-e5)
; (define-key tab-mode-map "m"	'tab-E6)
; (define-key tab-mode-map "j"	'tab-A6)
; (define-key tab-mode-map "u"	'tab-D6)
; (define-key tab-mode-map "7"	'tab-G6)
; (define-key tab-mode-map "M"	'tab-D6)
; (define-key tab-mode-map "J"	'tab-G6)
; (define-key tab-mode-map "U"	'tab-B6)
; (define-key tab-mode-map "&"	'tab-e6)

) ; tab-make-mode-map



(defun tab-check-in-tab () ; --------------------------------------------------
"Return t/nil whether cursor is in a tab staff line.  Also, force cursor
to nearest modulo 3 note position.  Set global variable tab-current-string."
(let ((placemark (point-marker))
      (in-tab t)
      (alignment (% (1+ (current-column)) 3))
      (real-case-fold-search case-fold-search))

(beginning-of-line)
(setq case-fold-search nil)

	(cond
	((looking-at tab-0-string-prefix) (setq tab-current-string 0))
	((looking-at tab-1-string-prefix) (setq tab-current-string 1))
	((looking-at tab-2-string-prefix) (setq tab-current-string 2))
	((looking-at tab-3-string-prefix) (setq tab-current-string 3))
	((looking-at tab-4-string-prefix) (setq tab-current-string 4))
	((looking-at tab-5-string-prefix) (setq tab-current-string 5))
	(t (setq in-tab nil))
	)

(goto-char placemark)
(setq case-fold-search real-case-fold-search)

	; put cursor on note position
	(if in-tab
		(cond
		((< (current-column) 5) (forward-char (- 5 (current-column))))
		((/= alignment 0) (backward-char alignment))
		)
	)

(setq in-tab in-tab)

)) ; tab-check-in-tab



(defun tab-decrement-position() ; ---------------------------------------------
(interactive)
	(if (tab-check-in-tab)
		(if (> tab-position 0) (setq tab-position (1- tab-position)))
	(insert (this-command-keys)))
(setq tab-position-as-string (int-to-string tab-position))
(set-buffer-modified-p (buffer-modified-p))   ; No-op, but updates mode line.
)   ; tab-decrement-position



(defun tab-increment-position() ; ---------------------------------------------
(interactive)
	(if (tab-check-in-tab)
	(setq tab-position (1+ tab-position))
	(insert (this-command-keys)))
(setq tab-position-as-string (int-to-string tab-position))
(set-buffer-modified-p (buffer-modified-p))   ; No-op, but updates mode line.
)   ; tab-increment-position



(defun tab-set-position (fret) ; ----------------------------------------------
"Prompt for numeric entry of current fret position"
(interactive "P")
	(if (tab-check-in-tab) (progn
		(if fret 
		(setq tab-position fret)
		(progn ; else
		(setq fret (read-string "Fret: "))
		(setq tab-position (string-to-int fret))
		))

		(if (< tab-position 0) (setq tab-position 0))
	(setq tab-position-as-string (int-to-string tab-position))
	(set-buffer-modified-p (buffer-modified-p))
	)
	; else
	(insert (this-command-keys)))
)   ; tab-set-position



(defun tab-forward-char (count) ; ---------------------------------------------
(interactive "p")
(let ((original-column (current-column)))

	(if (tab-check-in-tab) (progn
		(if (< original-column 5) (backward-char 3))
	(forward-char (* count 3))
	)
	; else
	(forward-char count)
	)
)) ; tab-forward-char



(defun tab-backward-char (count) ; --------------------------------------------
(interactive "p")
	(if (tab-check-in-tab) (setq count (* count 3)))
(backward-char count)
) ; tab-backward-char



(defun tab-forward-barline () ; -----------------------------------------------
(interactive)
	(if (tab-check-in-tab) (progn
		(if (looking-at "|") (forward-char 1))
	(re-search-forward "|\\|$")
	(tab-check-in-tab)
	))
) ; tab-forward-barline



(defun tab-backward-barline () ; ----------------------------------------------
(interactive)
	(if (tab-check-in-tab) (progn
	(re-search-backward "|\\|^")
	(tab-check-in-tab)
	))
) ; tab-backward-barline



(defun tab-up-staff (count) ; -------------------------------------------------
(interactive "p")
(let ((column (current-column))
      (search-top (concat "^" tab-0-string-prefix))
      (real-case-fold-search case-fold-search))

	(if (tab-check-in-tab) (previous-line (1+ tab-current-string)))

(setq case-fold-search nil)

	(while (> count 0) (progn
	(re-search-backward search-top nil t)
	(setq count (1- count))
	))

(beginning-of-line)
(forward-char column)
(tab-check-in-tab)

(setq case-fold-search real-case-fold-search)

)) ; tab-up-staff



(defun tab-down-staff (count) ; -----------------------------------------------
(interactive "p")
(let ((column (current-column))
      (search-top (concat "^" tab-0-string-prefix))
      (real-case-fold-search case-fold-search))

	(if (tab-check-in-tab) (next-line 1))

(setq case-fold-search nil)

	(while (> count 0) (progn
	(re-search-forward search-top nil t)
	(setq count (1- count))
	))

(beginning-of-line)
(forward-char column)
(tab-check-in-tab)

(setq case-fold-search real-case-fold-search)

)) ; tab-down-staff



(defun tab-make-staff () ; ------------------------------------------------- */
"Make a tab staff.  Do below current staff if in staff, or at cursor if
not already in staff."
(interactive)

	(if (tab-check-in-tab) (progn
	(forward-line (- 6 tab-current-string))
	(beginning-of-line)
	(newline 2)
	)
	; else
	(beginning-of-line)
	)

(insert tab-0-string-prefix) (insert-char ?- (- (frame-width) 5)) (newline)
(insert tab-1-string-prefix) (insert-char ?- (- (frame-width) 5)) (newline)
(insert tab-2-string-prefix) (insert-char ?- (- (frame-width) 5)) (newline)
(insert tab-3-string-prefix) (insert-char ?- (- (frame-width) 5)) (newline)
(insert tab-4-string-prefix) (insert-char ?- (- (frame-width) 5)) (newline)
(insert tab-5-string-prefix) (insert-char ?- (- (frame-width) 5)) (newline)

(forward-line -6)
(setq tab-current-string 0)
(forward-char 5)

) ; tab-make-staff



(defun tab-column (character-string) ; ----------------------------------------
"Draw vertical line of ARG down staff.  ARG must be 3-char string"
(let ((linecount 6))

(backward-char 2)
(setq temporary-goal-column (current-column))
(previous-line tab-current-string)

	(while (> linecount 0)
	(insert character-string)
	(delete-char 3)
	(backward-char 3)
		(if (> linecount 1) (next-line 1))
	(setq linecount (1- linecount))
	)

(next-line -5)
(setq tab-current-string 0)

	(if (< (current-column) (- (frame-width) 5))
	(forward-char 5)
	(forward-char 2)
	)
)) ; tab-column



(defun tab-barline () ; -------------------------------------------------------
"Draw a barline down staff"
(interactive)
	(if (tab-check-in-tab)
	(tab-column "--|")
	(insert (this-command-keys)))
)


(defun tab-forward () ; -------------------------------------------------------
"Move cursor forward one tablature space"
(interactive)
(let ((original-column (current-column)))

	(if (tab-check-in-tab) (progn
		(if (< original-column 5) (backward-char 3))
	(forward-char 3)
	)
	; else
	(insert (this-command-keys)))
)) ; tab-forward



(defun tab-delete () ; --------------------------------------------------------
"Delete vertical `chord' of notes"
(let ((index 0) (placemark))
(setq temporary-goal-column (current-column))
(previous-line tab-current-string)
(backward-char 2)
	(while (< index 6)
	(delete-char 3)
	(setq placemark (point-marker))
	(end-of-line)
	(insert "---")
	(goto-char placemark)
	(setq temporary-goal-column (current-column))
		(if (< index 5) (next-line 1))
	(setq index (1+ index))
	)

	(if (/= tab-current-string 5) (next-line (- tab-current-string 5)))
(forward-char 2)
)) ; tab-delete



(defun tab-delete-chord-forward (count) ; -------------------------------------
"Delete vertical `chord' of notes at cursor position"
(interactive "p")
	(if (<= count 0) (setq count 1))

	(if (tab-check-in-tab)
		(while (> count 0) (progn
		(tab-delete)
		(setq count (1- count))
		))
	; else
	(delete-char count)
	)
) ; tab-delete-chord-forward



(defun tab-delete-chord-backward (count) ; ------------------------------------
"Delete vertical `chord' of notes to left of cursor position"
(interactive "p")
	(if (<= count 0) (setq count 1))

	(if (tab-check-in-tab)
		(while (and (> count 0) (> (current-column) 5)) (progn
		(backward-char 3)
		(tab-delete)
		(setq count (1- count))
		))
	; else
	(delete-backward-char count)
	)
) ; tab-delete-chord-backward



(defun tab-delete-note (count) ; ----------------------------------------------
"Delete previous note on current string (lead-mode) 
or current note (chord-mode)."
(interactive "p")

	(if (tab-check-in-tab) (progn
		(if (and lead-mode (> (current-column) 5)) (progn
		(backward-char 2)
		(delete-backward-char 3)
		(insert "---")
		(backward-char 1)
		))

		(if chord-mode (progn
		(forward-char 1)
		(delete-backward-char 3)
		(insert "---")
		(backward-char 1) 
		))
	)
	; else
	(delete-backward-char count)
	)
) ; tab-delete-note



(defun tab-delete-current-note () ; -------------------------------------------
"Delete note (if any) that cursor is on, regardless of minor mode"
(interactive)

	(if (tab-check-in-tab) (progn
	(forward-char 1)
	(delete-backward-char 3)
	(insert "---")
	(backward-char 1) 
	))
) ; tab-delete-current-note



(defun tab-insert (count) ; ---------------------------------------------------
"Insert blank tablature space at cursor position"
(interactive "p")

	(if (tab-check-in-tab)
	(let ((index 0) (placemark))
	(setq temporary-goal-column (current-column))
	(previous-line tab-current-string)
	(backward-char 2)
		(while (< index 6)
		(setq placemark (point-marker))
		(insert-char ?- (* count 3))
		(end-of-line)
		(delete-backward-char (* count 3))
		(goto-char placemark)
		(setq temporary-goal-column (current-column))
			(if (< index 5) (next-line 1))
		(setq index (1+ index))
		)
	(next-line (- tab-current-string 5))
	(forward-char 2)
	)
	; else
	(insert-char ?\t 1)
	)
) ; tab-insert



(defun tab-begin-end-region (caller-begin caller-end) ; -----------------------
"Set CALLER-BEGIN/CALLER-END to leftmost/rightmost of top tab line above
dot or mark.  Return t if dot was left of mark, nil otherwise.
Check that both dot and mark are inside same staff of tab."

(let ((placemark (point-marker))
      (local-begin)
      (local-end)
      (begin-col)
      (end-col)
      (dot-before-mark))

; figure beginning and end
(setq begin-col (current-column))
(goto-char (mark-marker))
(setq end-col (current-column))

	(if (< begin-col end-col) (progn
	(setq local-begin placemark)
	(setq local-end (mark-marker))
	(setq dot-before-mark t)
	)
	(progn ; else
	(setq local-begin (mark-marker))
	(setq local-end placemark)
	(setq dot-before-mark nil)
	))	

; set beginning to top staff line
(goto-char local-begin)
	(if (not (tab-check-in-tab)) (progn
	(goto-char placemark)
	(error "Mark not in tablature"))
	)
(setq temporary-goal-column (current-column))
(previous-line tab-current-string)
(setq local-begin (point-marker))

; set end to top staff line
(goto-char local-end)
	(if (not (tab-check-in-tab)) (progn
	(goto-char placemark)
	(error "Mark not in tablature"))
	)
(setq temporary-goal-column (current-column))
(previous-line tab-current-string)
(setq local-end (point-marker))

; check begin and end in same tab staff
(goto-char local-begin)
	(if (< local-end local-begin) (progn
	(goto-char placemark)
	(error "Dot and mark must be in same tab staff"))
	)
(re-search-forward "$" local-end 2 1)
	(if (/= local-end (point-marker)) (progn
	(goto-char placemark)
	(error "Dot and mark must be in same tab staff"))
	)

; return values
(set caller-begin local-begin)
(set caller-end local-end)
(setq dot-before-mark dot-before-mark)

)) ; tab-begin-end-region



(defun tab-kill-internal (delete) ; -------------------------------------------
"Delete region of tab, putting in rectangle-kill ring if ARG is t" 
(let ((placemark (point-marker))
      (begin) (end)
      (begin-col) (end-col)
      (index 0)
      (dot-before-mark)
      (original-string tab-current-string)
     )

; figure rectangle beginning and end (inclusive these notes)
(setq dot-before-mark (tab-begin-end-region 'begin 'end))
(goto-char begin)
(backward-char 2)
(setq begin (point-marker))
(setq begin-col (current-column))
(goto-char end)
(setq temporary-goal-column (current-column))
(next-line 5)
(forward-char 1)
(setq end (point-marker))
(setq end-col (current-column))

; do it
(setq tab-killed-width (- end-col begin-col))
(kill-rectangle begin end)
(goto-char begin)

	(if delete (progn
	; extend staff
		(while (< index 6)
		(end-of-line)
		(insert-char ?- tab-killed-width)
		(forward-line)
		(setq index (1+ index))
		)
	(goto-char begin)
	(forward-char 2)
	(setq tab-current-string 0)
	)
	(progn ; else
	(yank-rectangle)
	(setq tab-current-string original-string)
		(if dot-before-mark (progn
		(goto-char begin)
		(forward-char 2)
		(setq temporary-goal-column (current-column))
			(if (/= tab-current-string 0)
			(next-line tab-current-string))
		)
		(progn ; else
		(backward-char 1)
		(setq temporary-goal-column (current-column))
		(previous-line (- 5 tab-current-string))
		))
	))

)) ; tab-kill-internal



(defun tab-kill-region () ; ---------------------------------------------------
"Kill region of tab to rectangle kill ring"
(interactive)
	(if (tab-check-in-tab)
	(tab-kill-internal t)
	(kill-region (point-marker) (mark-marker))
	)
) ; tab-kill-region



(defun tab-copy-region-as-kill () ; -------------------------------------------
"Copy region of tab to rectangle kill ring"
(interactive)
	(if (tab-check-in-tab)
	(tab-kill-internal nil)
	(copy-region-as-kill (point-marker) (mark-marker))
	)
) ; tab-copy-region-as-kill



(defun tab-yank () ; ----------------------------------------------------------
"Insert region of tab from rectangle kill ring"
(interactive)
	(if (tab-check-in-tab)
	(let ((placemark (point-marker)) (top-line) (index 0))
	(setq temporary-goal-column (current-column))
	(previous-line tab-current-string)
	(backward-char 2)
	(setq top-line (point-marker))
		(while (< index 6)
		(end-of-line)
		(delete-backward-char tab-killed-width)
		(forward-line)
		(setq index (1+ index))
		)
	(goto-char top-line)
	(yank-rectangle)
	(goto-char placemark)
	)
	; else
	(yank)
	)
) ; tab-yank



(defun tab-transpose (frets) ; ------------------------------------------------
"Transpose notes in region up or down by numeric prefix or prompted-for frets"
(interactive "P")

	(if (tab-check-in-tab)
	(let ((input-string)
	      (fret-array [0 0 0 0 0 0])
	      (begin)
	      (end))

		(if (not frets) (progn
		(setq input-string
		      (read-string "Transpose region by N frets: "))
		(setq frets (string-to-int input-string))
		))

	(fillarray fret-array frets)
	(message "Transposing region by %d frets ..." frets)

	(tab-begin-end-region 'begin 'end)
	(goto-char begin)

		(while (<= (point-marker) end) (progn
		(tab-transpose-chord fret-array)
			(if (< (current-column) (- (frame-width) 3))
			(forward-char 3)
			(end-of-line)
			)
		))

	(setq tab-current-string 0)
	(message "Finished transposing region by %d frets." frets)
	)
	; else
	(insert (this-command-keys))
	)
) ; tab-transpose



(defun tab-copy-retune () ; ---------------------------------------------------
"If cursor is on top line of tab staff, will copy staff and change into
current tuning."
(interactive)
(let ((old-cursor)
      (new-cursor)
      (old-tuning	[0 0 0 0 0 0])
      (diff)
      (tuning-diff	[0 0 0 0 0 0])
      (ndx)
      (placemark)
     )

(message "Copying this staff and converting to current tuning ...")

; make new staff
(setq old-cursor (point-marker))
(forward-line 6)
	; find blank line, or end of file
	(while (not (looking-at "^$")) (forward-line 1))
(newline 1)
(tab-make-staff)
(beginning-of-line)
(setq new-cursor (point-marker))

; learn tunings
(goto-char old-cursor)
(tab-analyze-tuning old-tuning)
(goto-char new-cursor)
(setq ndx 0)
	(while (< ndx 6) (progn
	(setq diff (- (aref old-tuning ndx) (aref tab-current-tuning ndx)))
		(if (> diff  6) (setq diff (- diff 12)))
		(if (< diff -6) (setq diff (+ diff 12)))
	(aset tuning-diff ndx diff)
	(setq ndx (1+ ndx))
	))

; copy old staff to new
; delete new staff past tuning signature
(goto-char new-cursor)
(forward-char 3)
(setq new-cursor (point-marker))
(forward-line 5)
(end-of-line)
(kill-rectangle new-cursor (point-marker))
(goto-char new-cursor)

; memorize old staff past tuning signature
(goto-char old-cursor)
(forward-char 3)
(setq old-cursor (point-marker))
(forward-line 5)
(end-of-line)
(kill-rectangle old-cursor (point-marker))
(goto-char old-cursor)
(yank-rectangle)

; copy
(goto-char new-cursor)
(yank-rectangle)
(goto-char new-cursor)
(forward-char 2)

; change tuning
	(while (< (current-column) (- (frame-width) 2)) (progn
	(tab-transpose-chord tuning-diff)
		(if (< (current-column) (- (frame-width) 3))
		(forward-char 3)
		(end-of-line)
		)
	))

(message "Finished copying into current tuning.")

)) ; tab-copy-retune ()



(defun tab-analyze-tuning (tuning) ; ------------------------------------------
"Fill six-element array TUNING with numeric values representing letter
notes at beginning of current plus next 5 screen lines."
(let ((placemark (point-marker)) (ndx 0) (numeric))

	(while (< ndx 6) (progn
	(beginning-of-line)
		(cond
		((looking-at "[Ee]") (setq numeric  0))
		((looking-at "[Ff]") (setq numeric  1))
		((looking-at "[Gg]") (setq numeric  3))
		((looking-at "[Aa]") (setq numeric  5))
		((looking-at "[Bb]") (setq numeric  7))
		((looking-at "[Cc]") (setq numeric  8))
		((looking-at "[Dd]") (setq numeric 10))
		(t		   (setq numeric  0))
		)
	(forward-char 1)
		(if (looking-at "#") (setq numeric (1+ numeric)))
		(if (looking-at "b") (setq numeric (1- numeric)))

		(if (< numeric 0) (setq numeric (+ 12 numeric)))
	(aset tuning ndx numeric)

	(forward-line 1)
	(setq ndx (1+ ndx))
	))

(goto-char placemark)

)) ; tab-analyze-tuning



(defun tab-transpose-chord (transpositions) ; ---------------------------------
"Transpose chord at cursor by 6-element array of fret offsets."
(let ((note))

(setq tab-current-string 0)
	(while (< tab-current-string 6) (progn
	(setq note (tab-analyze-fret))

		(if note (progn
		(setq note (+ note (aref transpositions tab-current-string)))
			(if (< note 0) (setq note (+ 12 note)))
		(tab-string (int-to-string note) tab-current-string)
			(if lead-mode (backward-char 3))
		))

	(setq temporary-goal-column (current-column))
		(if (< tab-current-string 5)
		(next-line 1)
		(previous-line 5))
	(setq tab-current-string (1+ tab-current-string))
	))
)) ; tab-transpose-chord



(defun tab-learn-tuning () ; --------------------------------------------------
"Memorize 3-character beginning of current plus next 5 screen lines as
new tuning.  Each line must be unique."
(interactive)

(tab-analyze-tuning tab-current-tuning)

(tab-learn-string 'tab-0-string-prefix)
(tab-learn-string 'tab-1-string-prefix)
(tab-learn-string 'tab-2-string-prefix)
(tab-learn-string 'tab-3-string-prefix)
(tab-learn-string 'tab-4-string-prefix)
(tab-learn-string 'tab-5-string-prefix)
(forward-line -6)
) ; tab-learn-tuning



(defun tab-learn-string (string) ; --------------------------------------------
"Copy first three characters of line into STRING."
(let ((begin))
(beginning-of-line)
(setq begin (point-marker))
(forward-char 3)
(set string (buffer-substring begin (point-marker)))
(forward-line 1)
)) ; tab-learn-string



(defun tab-set-tuning (string) ; ----------------------------------------------
"Given 18-character string (six 3-char tunings) set tuning to it."
(let ((ndx 0) (begin 0) (end 3) (one-string))

	(if (not (tab-check-in-tab))  (tab-make-staff))
(next-line (- 5 tab-current-string))

	(while (< ndx 6) (progn
	(beginning-of-line)
	(delete-char 3)
	(insert (substring string begin end))
	(setq begin end)
	(setq end (+ end 3))
		(if (< ndx 5) (previous-line 1))
	(setq ndx (1+ ndx))
	))

(tab-learn-tuning)
(tab-check-in-tab)

)) ; tab-set-tuning



(defun tab-note-name () ; -----------------------------------------------------
"Change names for printing chords (e.g. A# vs. Bb). First enter current name
of note, then new name."
(interactive)

(let ((old) (new) (ndx 0) (searching t))

(setq old (read-string (format "Old note (one of %s): " tab-note-names)))
	(while (and searching (< ndx 12)) (progn
		(if (string= old (aref tab-note-names ndx))
		(setq searching nil))
	(setq ndx (1+ ndx))
	))

	(if searching
	(error "Must enter one of %s" tab-note-names)
	(setq ndx (1- ndx)))
	
(setq new (read-string (format "New note name for %s: "
				(aref tab-note-names ndx))))
(aset tab-note-names ndx new)
)) ; tab-note-name



(defun tab-label-chord () ; ---------------------------------------------------
"Insert previously analyzed chord above current tab staff.  Can only be
used immediately after `\\[tab-analyze-chord]' (tab-analyze-chord)"
(interactive)

(let ((name-width (length tab-last-chord))
      (chord-column)
      (name-begin)
      (delete-begin)
      (placemark (point-marker)))

	(if (not (equal last-command 'tab-analyze-chord))
	(error "Use only immediately after `%s' (tab-analyze-chord)"
	       (car (where-is-internal 'tab-analyze-chord tab-mode-map))))

; go to appropriate column, and to line above tab
	(cond
	((= name-width 2) (backward-char 1))
	((> name-width 2) (backward-char 2))
	)
(setq chord-column (current-column))
(setq temporary-goal-column chord-column)
(previous-line (1+ tab-current-string))

; insert spaces if necessary
	(if (< (current-column) chord-column) (progn
	; insert spaces
	(indent-to-column chord-column)
	(setq name-begin (point-marker))
	(beginning-of-line)
	(untabify (point-marker) name-begin)
	(move-to-column chord-column)
	))

; insert chord name
(insert tab-last-chord)

; remove spaces equal to inserted name
	(while (and (> name-width 0) (looking-at " " )) (progn
	(delete-char 1)
	(setq name-width (1- name-width))
	))

(goto-char placemark)

)) ; tab-label-chord



(defun tab-analyze-chord () ; -------------------------------------------------
"Analyze chord.  Note cursor is on is assumed to be root.  Repeat usage
moves root to next chord note.  Use `\\[tab-label-chord]' (tab-label-chord)
immediately afterwards to insert chord into tab."

(interactive)

	(if (tab-check-in-tab)
	(let ((root-note-marker)
	      (root-string)
	      (root)
	      (note)
	      (bass-note)
	      (bass-note-name)
	      (bass-note-pos)
	      (chord [12 12 12 12 12 12]) ; "no note"
	      (chord-notes [0 0 0 0 0 0 0 0 0 0 0 0])
	      (root-name)
	      (chord-name)
	      (chord-disclaimer)
	      (chord-spelling)
	      (number-of-notes 0))

	(fillarray chord 12) ; "no note"
	(fillarray chord-notes 0)

	; get root
		(if (or (equal last-command this-command)
			(not (looking-at "[0-9]")))
		(tab-next-chord-note))
	(setq root-note-marker (point-marker))
	(setq root-string tab-current-string)
	(setq root (tab-analyze-note))
	(setq root-name (aref tab-note-names root))

	; get chord notes
	(setq temporary-goal-column (current-column))
	(previous-line tab-current-string)
	(setq tab-current-string 0)
		(while (< tab-current-string 6) (progn
		(setq note (tab-analyze-note))
			(if note (progn
			(setq bass-note	note)

			(setq note (- note root))
				(if (< note 0)  (setq note (+ note 12)))
				(if (> note 11) (setq note (- note 12)))

				(if (= (aref chord-notes note) 0)
				(setq number-of-notes (1+ number-of-notes)))

			(aset chord tab-current-string note)
			(aset chord-notes note (1+ (aref chord-notes note)))

			(setq bass-note-name (aref tab-note-names bass-note))
			(setq bass-note-pos note)
			))
		(setq temporary-goal-column (current-column))
		(next-line 1)
		(setq tab-current-string (1+ tab-current-string))
		))
	(goto-char root-note-marker)
	(setq tab-current-string root-string)

	; analyze chord
	(tab-analyze-chord-internal chord
				    chord-notes
				    'chord-name
				    'chord-disclaimer
				    'chord-spelling)

		; if unknown, and root != bass, and bass unique, try without
		(if (and (string= chord-name "??")
		         (/= root bass-note)
			 (= 1 (aref chord-notes bass-note-pos)))
		(progn
		; remove bass note from chord and try again
		(aset chord-notes bass-note-pos 0)
		(setq number-of-notes (1- number-of-notes))
		(tab-analyze-chord-internal chord
					    chord-notes
					    'chord-name
					    'chord-disclaimer
					    'chord-spelling)
			(if (not (string= chord-name "??"))
			(setq chord-name
			      (concat chord-name "/" bass-note-name)))
		))

	(setq tab-last-chord (concat root-name chord-name))
	(message "chord: %s%s ... %s"
		 tab-last-chord
		 chord-disclaimer
		 chord-spelling)
	)
	; else
	(insert (this-command-keys))
	)

) ; tab-analyze-chord



(defun tab-analyze-chord-internal (chord
				   chord-notes
				   chord-name-arg
				   chord-disclaimer-arg
				   chord-spelling-arg)
"Given a 6-element CHORD array, with one note per string, low-to-high,
with 0=root and -1==no_note; a 12-element CHORD-NOTES array containing
occurrances of notes 0-11, 0=root.  Will fill in CHORD-NAME with name
of chord (`m7b5', etc.) or `??' if unknown, CHORD-DISCLAIMER with `,no5'
info, and CHORD-SPELLING with strings (`root', `5th', `X', etc.) describing
each note in chord."

(let ((number-of-notes)
      (ndx 1)
      (chord-description [])
      (local-chord-name)
      (local-chord-disclaimer)
      (local-chord-spelling)
     )

	(while (< ndx 12) (progn
		(if (> (aref chord-notes ndx) 0)
		(setq chord-description
		      (vconcat chord-description (list ndx))))
	(setq ndx (1+ ndx))
	))
(setq number-of-notes (1+ (length chord-description)))

(defmacro tc (notes specials name disclaimer)
(list 'tab-chordtest notes
		     specials
		     name
		     disclaimer
		     'chord-description
		     'chord
		     ''local-chord-name
		     ''local-chord-disclaimer
		     ''local-chord-spelling)
)


    (cond
    ((= number-of-notes 1)
    (tc [] []	 "" ",no3,no5")
    )
    ((= number-of-notes 2)
	(cond
	((tc [ 3] []		"m"	",no5"	))
	((tc [ 4] []		""	",no5"	))
	((tc [ 7] []		"5"	""	))
	((tc [10] []		"7"	",no3,5"))
	((tc [11] []		"maj7"	",no3,5"))

	((tc []   []		"??"	""	))
	)
    )
    ((= number-of-notes 3)
	(cond
	((tc [2  7] []		"sus2"	""	))

	((tc [3  5] [5 "11"]	"m11"	",no5,7"))
	((tc [3  6] []		"mb5"	""	))
	((tc [3  7] []		"m"	""	))
	((tc [3  8] [8 "+5"]	"m+"	""	))
	((tc [3  9] []		"m6"	",no5"	))
	((tc [3 10] []		"m7"	",no5"	))

	((tc [4  5] [5 "11"]	"11"	",no5,7"))
	((tc [4  6] []		"-5"	""	))
	((tc [4  7] []		""	""	))
	((tc [4  8] [8 "+5"]	"+"	""	))
	((tc [4  9] []		"6"	",no5"	))
	((tc [4 10] []		"7"	",no5"	))
	((tc [4 11] []		"maj7"	",no5"	))

	((tc [5  7] [5 "sus4"]	"sus4"	""	))
	((tc [5 10] [5 "sus4"]	"7sus4"	",no5"	))

	((tc [7 10] []		"7"	",no3"	))
	((tc [7 11] []		"maj7"	",no3"	))

	((tc []     []			"??"	""	))
	)
    )
    ((= number-of-notes 4)
	(cond
	((tc [2  3  7] [2 "9"]		"madd9"		""	))
	((tc [2  3 10] [2 "9"]		"m9"		",no5"	))
	((tc [2  4  7] [2 "9"]		"add9"		""	))
	((tc [2  4 10] [2 "9"]		"9"		",no5"	))
	((tc [2  7 10] [2 "sus2"]	"7sus2"		""	))
	((tc [2  7 11] [2 "sus2"]	"maj7sus2"	""	))

	((tc [3  4 10] [3 "#9"]		"7#9"		",no5"	))
	((tc [3  5  7] [5 "11"]		"madd11"	""	))
	((tc [3  6  9] [9 "bb7"]	"dim"		""	))
	((tc [3  6 10] []		"m7b5"		""	))
	((tc [3  7  9] []		"m6"		""	))
	((tc [3  7 10] []		"m7"		""	))
	((tc [3  8 10] [8 "+5"]		"m7+5"		""	))

	((tc [4  5  7] [5 "11"]		"add11"		""	))
	((tc [4  6  7] [6 "+11"]	"add+11"	""	))
	((tc [4  6  9] []		"add6b5"	""	))
	((tc [4  6 10] []		"7b5"		""	))
	((tc [4  6 11] []		"maj7b5"	""	))
	((tc [4  7  9] []		"6"		""	))
	((tc [4  7 10] []		"7"		""	))
	((tc [4  7 11] []		"maj7"		""	))
	((tc [4  8 10] [8 "+5"]		"7+5"		""	))
	((tc [4  8 11] [8 "+5"]		"maj7+5"	""	))

	((tc [5  7 10] [5 "sus4"]	"7sus4"		""	))
	((tc [5  7 11] [5 "sus4"]	"maj7sus4"	""	))

	((tc []        []		"??"		""	))
	)
    )
    ((= number-of-notes 5)
	(cond
	((tc [1  3  7 10] [1 "b9"]		"m7b9"		""	))
	((tc [1  4  7 10] [1 "b9"]		"7b9"		""	))

	((tc [2  3  5  7] [5 "11"]		"m11"		",no7"	))
	((tc [2  3  5 10] [5 "11"]		"m11"		",no5"	))
	((tc [2  3  6  9] [2 "9" 9 "bb7"]	"dim9"		""	))
	((tc [2  3  6 10] [2 "9"]		"m9b5"		""	))
	((tc [2  3  7  9] [2 "9"]		"m6add9"	""	))
	((tc [2  3  7 10] [2 "9"]		"m9"		""	))
	((tc [2  4  5  7] [2 "9" 5 "11"]	"11"		",no7"	))
	((tc [2  4  5 10] [2 "9" 5 "11"]	"11"		",no5"	))
	((tc [2  4  6 10] [2 "9"]		"9b5"		""	))
	((tc [2  4  7  9] [2 "9"]		"6add9"		""	))
	((tc [2  4  7 10] [2 "9"]		"9"		""	))
	((tc [2  4  7 11] [2 "9"]		"maj7add9"	""	))
	((tc [2  5  7 10] [2 "9" 5 "sus4"]	"9sus4"		""	))

	((tc [3  4  7 10] [3 "#9"]		"7#9"		""	))
	((tc [3  5  7 10] [5 "11"]		"m11"		",no9"	))
	; m11,no9 == m7add11

	((tc [4  5  7 10] [5 "11"]		"11"		",no9"	))
	; 11,no9 == 7add11
	((tc [4  6  7 10] [6 "+11"]		"7add+11"	""	))

	((tc []		  []			"??"		""	))
	)
    )
    ((= number-of-notes 6)
	(cond
	((tc [1  3  5  7 10] [1 "b9" 5 "11"]		"m11b9"	""	))
	((tc [1  4  5  7 10] [1 "b9" 5 "11"]		"11b9"	""	))

	((tc [2  3  5  6  9] [2 "9" 5 "11" 9 "bb7"]	"dim11" ""	))
	((tc [2  3  5  6 10] [2 "9" 5 "11"]		"m11b5"	""	))
	((tc [2  3  5  7  9] [2 "9" 5 "11" 9 "13"]	"m13"	",no7"	))
	((tc [2  3  5  7 10] [2 "9" 5 "11"]		"m11"	""	))
	((tc [2  3  5  9 10] [2 "9" 5 "11" 9 "13"]	"m13"	",no5"	))
	((tc [2  4  5  6 10] [2 "9" 5 "11"]		"11b5"	""	))
	((tc [2  4  5  7  9] [2 "9" 5 "11" 9 "13"]	"13"	",no7"	))
	((tc [2  4  5  7 10] [2 "9" 5 "11"]		"11"	""	))
	((tc [2  4  5  9 10] [2 "9" 5 "11" 9 "13"]	"13"	",no5"	))
	((tc [2  5  7  9 10] [2 "9" 5 "11"]		"13"	"no3"	))

	((tc [3  5  7  9 10] [5 "11" 9 "13"]		"m13"	",no9"	))
	((tc [4  5  7  9 10] [5 "11" 9 "13"]		"13"	",no9"	))

	((tc []		     []				"??"	""	))
	)
    )
    )

(set chord-name-arg       local-chord-name)
(set chord-disclaimer-arg local-chord-disclaimer)
(set chord-spelling-arg   local-chord-spelling)


; 0     1    2    3     4    5        6       7     8    9     10      11 
; root  b2   2nd  min3  3rd  4th      b5      5th   b6   6th   7th     maj7th
; 8th   b9   9th  b3         11th     +11           +    13th  dom7th  7th
;            sus2            sus4                   aug

)) ; tab-analyze-chord-internal



(defun tab-chordtest (notes ; -------------------------------------------------
		      degree-names
		      name
		      disclaimer
		      chord-description
		      chord
		      chord-name
		      chord-disclaimer
		      chord-spelling)
"Give an n-element NOTES array, modified DEGREE_NAMES for them, NAME and
DISCLAIMER strings, and an n-element CHORD_DESCRIPTION.  If notes and
chord-description match, will use 6-element CHORD array and fill in
CHORD-NAME with name, CHORD-DISCLAIMER with disclaimer, and CHORD-SPELLING
and return t.  Otherwise, leaves all alone and returns nil."
(let ((normal-names ["rt" "b2" "2" "b3" "3" "4" "b5"
		     "5" "b6" "6" "7" "maj7" "x"])
      (names)
      (ndx 0))

	(if (or (equal notes chord-description) (= (length notes) 0)) (progn
	(set chord-name name)
	(set chord-disclaimer disclaimer)

	(setq names (copy-sequence normal-names))
		(while (< ndx (length degree-names)) (progn
		(aset names
		      (aref degree-names ndx)
		      (aref degree-names (1+ ndx)))

		(setq ndx (+ ndx 2))
		))

	(set chord-spelling
	     (format "%s %s %s %s %s %s  (%s to %s)"
		     (aref names (aref chord 5))
		     (aref names (aref chord 4))
		     (aref names (aref chord 3))
		     (aref names (aref chord 2))
		     (aref names (aref chord 1))
		     (aref names (aref chord 0))
		     tab-5-string-prefix
		     tab-0-string-prefix))

		(if tab-12-tone-chords
		(let ((adjusted-chord [0 0 0 0 0 0]) (ndx 0))

			(while (< ndx 6)
			(let ((note (aref chord (- 5 ndx))))
				(if (= note 12)
				(aset adjusted-chord ndx 'x)
				(aset adjusted-chord ndx note))
			(setq ndx (1+ ndx))
			))

		(set chord-spelling
		     (format "%s  %s" (eval chord-spelling) adjusted-chord))
		))

	(eval t)
	))
)) ; tab-chordtest



(defun tab-analyze-fret () ; --------------------------------------------------
"Return numeric fret value of note cursor is on, or nil if no note"
(let ((digits 1) (fret nil) (end))

	(if (looking-at "[0-9]") (progn
	(forward-char 1)
	(setq end (point-marker))
	(backward-char 2)
		(if (not (looking-at "[12]")) (progn
		(forward-char 1)
		(setq digits 0)
		))

	(setq fret (string-to-int (buffer-substring (point-marker) end)))
	(forward-char digits)
	))

(setq fret fret)

)) ; tab-analyze-fret



(defun tab-analyze-note () ; --------------------------------------------------
"Return numeric note value of note cursor is on, or nil if no note"
(let ((fret) (note nil))
(setq fret (tab-analyze-fret))
	(if fret (progn
	(setq note (+ fret (aref tab-current-tuning tab-current-string)))
		(if (>= note 12) (setq note (% note 12)))
	))
(eval note)
)) ; tab-analyze-note




(defun tab-next-chord-note () ; -----------------------------------------------
(let ((strings-checked 0) (searching t))

	(while (and searching (< strings-checked 6)) (progn
	(setq temporary-goal-column (current-column))
		(if (= tab-current-string 5) (progn
		(previous-line 5)
		(setq tab-current-string 0)
		)
		(progn ; else
		(next-line 1)
		(setq tab-current-string (1+ tab-current-string))
		))

		(if (looking-at "[0-9]") (setq searching nil))
	(setq strings-checked (1+ strings-checked))
	))

	(if searching (error "No notes in chord"))

)) ; tab-next-chord-note



(defun tab-higher-string () ; -------------------------------------------------
"Move note to next-higher string, recursively with wrap-around until blank
string found or all six strings done."
(interactive)
	(if (tab-check-in-tab)
	(tab-higher-lower-string t)
	(insert (this-command-keys)))
) ; tab-higher-string



(defun tab-lower-string () ; -------------------------------------------------
"Move note to next-lower string, recursively with wrap-around until blank
string found or all six strings done."
(interactive)
	(if (tab-check-in-tab)
	(tab-higher-lower-string nil)
	(insert (this-command-keys)))
) ; tab-lower-string



(defun tab-higher-lower-string (higher) ; -------------------------------------
"Internal routine to do work of 'tab-higher-string if ARG is t, else
'tab-lower-string'"

(let ((notes-to-move t)
      (moving-note nil)
      (moving-fret nil)
      (in-way-note (tab-analyze-note))
      (in-way-fret (tab-analyze-fret))
      (moves -1))

	(if (null in-way-note)
	(error "Must be on note to move to higher/lower string"))

(setq tab-pending-embellishment nil)

	(while notes-to-move (progn
	; erase note in way
	(delete-char 1)
	(delete-backward-char 2)
	(insert "---")
	(backward-char 1)

	; transpose moving note (if any) to this new string
		(if moving-note (let ((new-fret) (old-fret))

		(setq new-fret
		      (- moving-note (aref tab-current-tuning
					   tab-current-string)))
		(setq old-fret moving-fret)

			(if (< new-fret 0) (setq new-fret (+ new-fret 12)))

			(cond
			((and (> new-fret old-fret) (> new-fret 12))
				(if (> (- new-fret old-fret) 6)
				(setq new-fret (- new-fret 12)))
			)
			((and (> old-fret new-fret) (<= new-fret 12))
				(if (> (- old-fret new-fret) 6)
				(setq new-fret (+ new-fret 12)))
			)
			)

		; put transposed note on new line
		(tab-string (int-to-string new-fret) tab-current-string)
		))

	; note in the way will now move
	(setq moving-note in-way-note)
	(setq moving-fret in-way-fret)

	; set flag to exit
	(setq notes-to-move moving-note)

	; goto next string and get note in the way (if any)
		(if higher
		(tab-move-string -1)
		(tab-move-string  1))
	(setq in-way-note (tab-analyze-note))
	(setq in-way-fret (tab-analyze-fret))

	; count how many notes moved
	(setq moves (1+ moves))
	))

; get back to note cursor on at beginning
	(if (< moves 6)
		(if higher
		(tab-move-string moves)
		(tab-move-string (- 0 moves)))
	)

)) ; tab-higher-lower-string



(defun tab-move-string (strings) ;---------------------------------------------
"Move absolute value of STRINGS, down if positive, up if negative."

(setq temporary-goal-column (current-column))

	(cond
	((> strings 0)
		(if (<= (+ strings tab-current-string) 5) (progn
		(next-line strings)
		(setq tab-current-string (+ tab-current-string strings))
		)
		(progn ; else
		(setq strings (- 6 strings))
		(previous-line strings)
		(setq tab-current-string (- tab-current-string strings))
		))
	)

	((< strings 0)
		(if (>= (+ strings tab-current-string) 0) (progn
		(next-line strings)
		(setq tab-current-string (+ tab-current-string strings))
		)
		(progn ; else
		(setq strings (+ 6 strings))
		(next-line strings)
		(setq tab-current-string (+ tab-current-string strings))
		))
	)
	)
) ; tab-move-string


(defun tab-goto-string (string) ; ---------------------------------------------
"Go to STRING string, where 0<=string<=5.  Reset tab-current-string"
(setq temporary-goal-column (current-column))
(next-line (- string tab-current-string))
(setq tab-current-string string)
) ; tab-goto-string



(defun tab-up-12 () ; ---------------------------------------------------------
"Move current note up 12 frets"
(interactive)
	(if (tab-check-in-tab)
	(let ((fret (tab-analyze-fret)))
		(if (and (/= fret -1) (<= fret 12)) (progn
		(setq fret (+ fret 12))
		(tab-string (int-to-string fret) tab-current-string)
		))
	)
	; else
	(insert (this-command-keys)))
) ; tab-up-12



(defun tab-down-12 () ; -------------------------------------------------------
"Move current note up 12 frets"
(interactive)
	(if (tab-check-in-tab)
	(let ((fret (tab-analyze-fret)))
		(if (and (/= fret -1) (>= fret 12)) (progn
		(setq fret (- fret 12))
		(tab-string (int-to-string fret) tab-current-string)
		))
	)
	; else
	(insert (this-command-keys)))
) ; tab-down-12



(defun xfretboard ()
"Start auxiliary X-Windows fretboard process.  Press <return> for default
name, or enter pathname to xfretboard program."
(interactive)
(let ((program-name))

	(if tab-process (let ((status (process-status tab-process)))
		(if (not (or (string= status "exit")
			     (string= status "signal")))
		(error "Only one external process at a time.  Currently: %s"
		       (process-command tab-process))
		)
	))

(setq program-name
      (read-file-name (format "Fretboard program (default %s): "
			      fretboard-program-name)
		      nil
		      fretboard-program-name
		      t))

(message "Starting program %s ...")
(setq tab-process (start-process program-name (current-buffer) program-name))
(set-process-filter tab-process 'tab-read-process)
(message "Graphical interface window should appear momentarily")

)) ; xfretboard


(defun tab-read-process (process string)
"Read input from external program, and process as tab-mode commands."
(let ((old-buffer (current-buffer))
      (string-pos 0)
      (string-length (1-(length string)))
      (read-result))

; Having this forces each buffer to have its own separate X-Windows prog. 
;	(if (not (equal process tab-process))
;	(error "Input from %s, not tab-process (%s)" process tab-process))

	(if (not (equal major-mode 'tab-mode)) (progn
	(display-buffer (process-buffer process))
	(set-buffer (process-buffer process))
	))

	(while (< string-pos string-length) (progn
	(setq read-result (read-from-string string string-pos))
	(eval (car read-result))
	(setq string-pos (cdr read-result))
	))

(set-buffer old-buffer)
))



(defun tab-unused-key () ; ----------------------------------------------------
"Ignore keypress if on tab staff; insert normally otherwise"
(interactive)
	(if (not (tab-check-in-tab)) (insert (this-command-keys)))
)



(defun tab-embellishment (special-character) ; --------------------------------
"Mark current note with ARG character"
	(if (tab-check-in-tab)
		(if (looking-at "-") (progn
		(setq tab-pending-embellishment special-character)
		(set-buffer-modified-p (buffer-modified-p))
		)
		(progn ; else
		(backward-char 1)
			(if (looking-at "[12]") (backward-char 1))
		(delete-char 1)
		(insert special-character)
		(forward-char 1)
			(if (not (looking-at "[0-9]")) (backward-char 1))
		))
	; else
	(insert (this-command-keys))
	)
) ; tab-embellishment



(defun tab-hammer () ; --------------------------------------------------------
(interactive)
(tab-embellishment "h")
) ; tab-hammer

(defun tab-pull () ; ----------------------------------------------------------
(interactive)
(tab-embellishment "p")
) ; tab-pull

(defun tab-bend () ; ----------------------------------------------------------
(interactive)
(tab-embellishment "b")
) ; tab-bend

(defun tab-release () ; -------------------------------------------------------
(interactive)
(tab-embellishment "r")
) ; tab-release

(defun tab-slide-up () ; ------------------------------------------------------
(interactive)
(tab-embellishment "/")
) ; tab-slide-up

(defun tab-slide-down () ; ----------------------------------------------------
(interactive)
(tab-embellishment "\\")
) ; tab-slide-down

(defun tab-vibrato () ; -------------------------------------------------------
(interactive)
(tab-embellishment "~")
) ; tab-vibrato

(defun tab-ghost () ; ---------------------------------------------------------
(interactive)
(tab-embellishment "(")
) ; tab-ghost

(defun tab-normal () ; -------------------------------------------------------
(interactive)
(tab-embellishment "-")
) ; tab-normal

(defun tab-muffled () ; -------------------------------------------------------
(interactive)
(tab-embellishment "X")
) ; tab-muffled



(defun tab-string (symbol string) ; -------------------------------------------
"Place first arg note on second arg string."
(setq temporary-goal-column (current-column))
(previous-line (- tab-current-string string))
(delete-char 1)
(backward-char 1)
	(if (looking-at "[-12]")
	(delete-char 1)
	; else
	(delete-backward-char 1)
	(forward-char 1)
	)

	(if (< (length symbol) 2) (progn
	(backward-char 1)
	(insert "-")
	(forward-char 1)
	))

(insert symbol)

	(if tab-pending-embellishment (progn
	(backward-char (length symbol))
	(delete-backward-char 1)
	(insert tab-pending-embellishment)
	(forward-char (length symbol))
	(setq tab-pending-embellishment nil)
	(set-buffer-modified-p (buffer-modified-p))
	))

	(if chord-mode (backward-char 1))
	(if lead-mode (forward-char 2))

(setq tab-current-string string)
) ; tab-string



(defun tab-E (symbol) ; -------------------------------------------------------
(tab-string symbol 5)
)

(defun tab-A (symbol) ; -------------------------------------------------------
(tab-string symbol 4)
)

(defun tab-D (symbol) ; -------------------------------------------------------
(tab-string symbol 3)
)

(defun tab-G (symbol) ; -------------------------------------------------------
(tab-string symbol 2)
)

(defun tab-B (symbol) ; -------------------------------------------------------
(tab-string symbol 1)
)

(defun tab-e (symbol) ; -------------------------------------------------------
(tab-string symbol 0)
)



(defun tab-E-fret (fret) ; ---------------------------------------------------
	(if (tab-check-in-tab)
	(tab-E (int-to-string fret))
	(insert (this-command-keys)))
)

(defun tab-A-fret (fret) ; ---------------------------------------------------
	(if (tab-check-in-tab)
	(tab-A (int-to-string fret))
	(insert (this-command-keys)))
)

(defun tab-D-fret (fret) ; ---------------------------------------------------
	(if (tab-check-in-tab)
	(tab-D (int-to-string fret))
	(insert (this-command-keys)))
)

(defun tab-G-fret (fret) ; ---------------------------------------------------
	(if (tab-check-in-tab)
	(tab-G (int-to-string fret))
	(insert (this-command-keys)))
)

(defun tab-B-fret (fret) ; ---------------------------------------------------
	(if (tab-check-in-tab)
	(tab-B (int-to-string fret))
	(insert (this-command-keys)))
)

(defun tab-e-fret (fret) ; ---------------------------------------------------
	(if (tab-check-in-tab)
	(tab-e (int-to-string fret))
	(insert (this-command-keys)))
)



(defun tab-E-open () ; --------------------------------------------------------
(interactive)
(tab-E-fret 0)
)

(defun tab-A-open () ; --------------------------------------------------------
(interactive)
(tab-A-fret 0)
)

(defun tab-D-open () ; --------------------------------------------------------
(interactive)
(tab-D-fret 0)
)

(defun tab-G-open () ; --------------------------------------------------------
(interactive)
(tab-G-fret 0)
)

(defun tab-B-open () ; --------------------------------------------------------
(interactive)
(tab-B-fret 0)
)

(defun tab-e-open () ; --------------------------------------------------------
(interactive)
(tab-e-fret 0)
)



(defun tab-E-1 () ; -----------------------------------------------------------
(interactive)
	(if (> tab-position 0)
	(tab-E-fret (- tab-position 1))
	(tab-E-fret tab-position))
)

(defun tab-A-1 () ; -----------------------------------------------------------
(interactive)
	(if (> tab-position 0)
	(tab-A-fret (- tab-position 1))
	(tab-A-fret tab-position))
)

(defun tab-D-1 () ; -----------------------------------------------------------
(interactive)
	(if (> tab-position 0)
	(tab-D-fret (- tab-position 1))
	(tab-D-fret tab-position))
)

(defun tab-G-1 () ; -----------------------------------------------------------
(interactive)
	(if (> tab-position 0)
	(tab-G-fret (- tab-position 1))
	(tab-G-fret tab-position))
)

(defun tab-B-1 () ; -----------------------------------------------------------
(interactive)
	(if (> tab-position 0)
	(tab-B-fret (- tab-position 1))
	(tab-B-fret tab-position))
)

(defun tab-e-1 () ; -----------------------------------------------------------
(interactive)
	(if (> tab-position 0)
	(tab-e-fret (- tab-position 1))
	(tab-e-fret tab-position))
)



(defun tab-E0 () ; ------------------------------------------------------------
(interactive)
(tab-E-fret tab-position)
)

(defun tab-A0 () ; ------------------------------------------------------------
(interactive)
(tab-A-fret tab-position)
)

(defun tab-D0 () ; ------------------------------------------------------------
(interactive)
(tab-D-fret tab-position)
)

(defun tab-G0 () ; ------------------------------------------------------------
(interactive)
(tab-G-fret tab-position)
)

(defun tab-B0 () ; ------------------------------------------------------------
(interactive)
(tab-B-fret tab-position)
)

(defun tab-e0 () ; ------------------------------------------------------------
(interactive)
(tab-e-fret tab-position)
)



(defun tab-E1 () ; ------------------------------------------------------------
(interactive)
(tab-E-fret (+ tab-position 1))
)

(defun tab-A1 () ; ------------------------------------------------------------
(interactive)
(tab-A-fret (+ tab-position 1))
)

(defun tab-D1 () ; ------------------------------------------------------------
(interactive)
(tab-D-fret (+ tab-position 1))
)

(defun tab-G1 () ; ------------------------------------------------------------
(interactive)
(tab-G-fret (+ tab-position 1))
)

(defun tab-B1 () ; ------------------------------------------------------------
(interactive)
(tab-B-fret (+ tab-position 1))
)

(defun tab-e1 () ; ------------------------------------------------------------
(interactive)
(tab-e-fret (+ tab-position 1))
)



(defun tab-E2 () ; ------------------------------------------------------------
(interactive)
(tab-E-fret (+ tab-position 2))
)

(defun tab-A2 () ; ------------------------------------------------------------
(interactive)
(tab-A-fret (+ tab-position 2))
)

(defun tab-D2 () ; ------------------------------------------------------------
(interactive)
(tab-D-fret (+ tab-position 2))
)

(defun tab-G2 () ; ------------------------------------------------------------
(interactive)
(tab-G-fret (+ tab-position 2))
)

(defun tab-B2 () ; ------------------------------------------------------------
(interactive)
(tab-B-fret (+ tab-position 2))
)

(defun tab-e2 () ; ------------------------------------------------------------
(interactive)
(tab-e-fret (+ tab-position 2))
)



(defun tab-E3 () ; ------------------------------------------------------------
(interactive)
(tab-E-fret (+ tab-position 3))
)

(defun tab-A3 () ; ------------------------------------------------------------
(interactive)
(tab-A-fret (+ tab-position 3))
)

(defun tab-D3 () ; ------------------------------------------------------------
(interactive)
(tab-D-fret (+ tab-position 3))
)

(defun tab-G3 () ; ------------------------------------------------------------
(interactive)
(tab-G-fret (+ tab-position 3))
)

(defun tab-B3 () ; ------------------------------------------------------------
(interactive)
(tab-B-fret (+ tab-position 3))
)

(defun tab-e3 () ; ------------------------------------------------------------
(interactive)
(tab-e-fret (+ tab-position 3))
)



(defun tab-E4 () ; ------------------------------------------------------------
(interactive)
(tab-E-fret (+ tab-position 4))
)

(defun tab-A4 () ; ------------------------------------------------------------
(interactive)
(tab-A-fret (+ tab-position 4))
)

(defun tab-D4 () ; ------------------------------------------------------------
(interactive)
(tab-D-fret (+ tab-position 4))
)

(defun tab-G4 () ; ------------------------------------------------------------
(interactive)
(tab-G-fret (+ tab-position 4))
)

(defun tab-B4 () ; ------------------------------------------------------------
(interactive)
(tab-B-fret (+ tab-position 4))
)

(defun tab-e4 () ; ------------------------------------------------------------
(interactive)
(tab-e-fret (+ tab-position 4))
)



(defun tab-E5 () ; ------------------------------------------------------------
(interactive)
(tab-E-fret (+ tab-position 5))
)

(defun tab-A5 () ; ------------------------------------------------------------
(interactive)
(tab-A-fret (+ tab-position 5))
)

(defun tab-D5 () ; ------------------------------------------------------------
(interactive)
(tab-D-fret (+ tab-position 5))
)

(defun tab-G5 () ; ------------------------------------------------------------
(interactive)
(tab-G-fret (+ tab-position 5))
)

(defun tab-B5 () ; ------------------------------------------------------------
(interactive)
(tab-B-fret (+ tab-position 5))
)

(defun tab-e5 () ; ------------------------------------------------------------
(interactive)
(tab-e-fret (+ tab-position 5))
)



(defun tab-E6 () ; ------------------------------------------------------------
(interactive)
(tab-E-fret (+ tab-position 6))
)

(defun tab-A6 () ; ------------------------------------------------------------
(interactive)
(tab-A-fret (+ tab-position 6))
)

(defun tab-D6 () ; ------------------------------------------------------------
(interactive)
(tab-D-fret (+ tab-position 6))
)

(defun tab-G6 () ; ------------------------------------------------------------
(interactive)
(tab-G-fret (+ tab-position 6))
)

(defun tab-B6 () ; ------------------------------------------------------------
(interactive)
(tab-B-fret (+ tab-position 6))
)

(defun tab-e6 () ; ------------------------------------------------------------
(interactive)
(tab-e-fret (+ tab-position 6))
)
