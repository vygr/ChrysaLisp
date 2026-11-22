;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a tracker's module of whatever kind read into a song, which
; lib/media/tracker.inc plays: those with a tag of their own first, a MOD
; last, as one of the Soundtracker's has none
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/media/mod.inc")
(import "lib/media/s3m.inc")
(import "lib/media/xm.inc")
(import "lib/media/it.inc")
(import "lib/media/med.inc")
(import "lib/media/ahx.inc")

;the endings of the files it reads, as a file picker is given them
(defq +module_exts ".mod .s3m .xm .it .med .mmd0 .mmd1 .mmd2 .mmd3 .mmdc .ahx .hvl")

;the notes of an octave, as a tracker names them
(defq +module_notes ''("C-" "C#" "D-" "D#" "E-" "F-" "F#" "G-" "G#" "A-" "A#" "B-"))

(defun module-hex-digit (n)
	; (module-hex-digit n) -> str
	;the low 4 bits of a number, as a tracker shows them
	(elem-get "0123456789ABCDEF" (logand n 15)))

(defun module-note-text (note)
	; (module-note-text note) -> str
	;a cell's note as a tracker shows it, three letters
	(cond
		((= note 0) "---")
		((= note +note_cut) "^^^")
		((= note +note_off) "===")
		((= note +note_fade) "~~~")
		((cat (elem-get +module_notes (% (dec note) 12)) (str (/ (dec note) 12))))))

(defun module-cell-text (song cell)
	; (module-cell-text song cell) -> str
	;a cell of a song's pattern, (note sample volume effect number), as its
	;tracker shows it: an S3M's volume and its effects by letter, A 1; an
	;XM's volume column, a volume or an effect of a sign and a digit, and
	;its effects by number then letter, G 16; an IT's volume column, a
	;volume or an effect of a letter and a digit, and its effects by
	;letter; a MOD's and an AHX's effects by number, and a MED's by two
	(bind '(note ins vol effect param) cell)
	(defq mode (. song :find :mode))
	(cat (module-note-text note) " " (if (> ins 0) (byte-to-hex-str ins) "..")
		(case mode
			(:st3 (if (= vol +no_volume) " .." (cat " " (byte-to-hex-str vol))))
			(:ft2 (cond
				((< vol 0x10) " ..")
				((<= vol 0x50) (cat " " (byte-to-hex-str (- vol 0x10))))
				((cat " " (elem-get "-------+DUSVPLRM" (>> vol 4)) (module-hex-digit vol)))))
			(:it (cond
				((<= vol 64) (cat " " (byte-to-hex-str vol)))
				((< vol 125) (defq at (/ (- vol 65) 10)) (cat " " (elem-get "ABCDEF" at) (str (- vol 65 (* at 10)))))
				((<= 128 vol 192) (cat " P" (module-hex-digit (min 15 (/ (- vol 128) 4)))))
				((<= 193 vol 212) (defq at (/ (- vol 193) 10)) (cat " " (elem-get "GH" at) (str (- vol 193 (* at 10)))))
				(" ..")))
			(:t ""))
		" " (cond
			((and (= effect 0) (= param 0)) (if (eql mode :med) "...." "..."))
			((find mode '(:st3 :it)) (cat (char (+ 64 effect)) (byte-to-hex-str param)))
			((eql mode :med) (cat (byte-to-hex-str effect) (byte-to-hex-str param)))
			((< effect 16) (cat (module-hex-digit effect) (byte-to-hex-str param)))
			((cat (char (+ 55 effect)) (byte-to-hex-str param))))))

(defun module-cell-width (song)
	; (module-cell-width song) -> num
	;how many letters a cell of a song is shown in, (module-cell-text), and
	;the space after it
	(inc (length (module-cell-text song '(0 0 0 0 0)))))

(defun module-tagged? (data)
	; (module-tagged? data) -> :t | :nil
	;bytes that begin as a module of a tracker that tags its own: an S3M,
	;an XM, an IT, a MED, an AHX or a HivelyTracker's
	(or (starts-with "IMPM" data) (starts-with "Extended Module: " data)
		(and (>= (length data) 48) (eql (slice data 44 48) "SCRM"))
		(and (> (length data) 3)
			(or (and (starts-with "MMD" data) (find (elem-get data 3) "0123C") :t)
				(and (or (starts-with "THX" data) (starts-with "HVL" data)) (<= (code data 1 3) 1))))))

(defun module-read (data &optional bare)
	; (module-read data [bare]) -> :nil | song
	;bare, its samples' bytes not had, for what only shows it
	(or (s3m-read data bare) (xm-read data bare) (it-read data bare) (med-read data bare) (ahx-read data bare)
		;a Soundtracker's MOD has no tag, so any bytes may be taken for one:
		;not those of another tracker's that its own reader would not take
		(if (or (mod-tagged data) (not (module-tagged? data))) (mod-read data bare))))
