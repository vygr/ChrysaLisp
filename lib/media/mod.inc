;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a module of the Amiga's trackers, ProTracker's MOD and those like it,
; read into a song, which lib/media/tracker.inc plays: its samples, its
; patterns and the order they are played in. Of 31 samples and a tag at
; 1080 that says how many channels, M.K. and the like 4, 6CHN 6, 8CHN or
; FLT8 8, 10CH to 32CH so many; with no tag the Soundtracker's 15 and 4.
;
; A song, of whatever tracker, is the same: its :kind, :title, :channels,
; :samples, :orders, :restart, :patterns and the :rows of each, its first
; :speed and :tempo, its :global volume, the :pans of its channels, 0 left
; to 256 right, and the :mode its effects are of, :mod or :st3, and its
; :pitch, :amiga or :st3. A cell of a pattern is 5 numbers: its note, 0
; none, 1 C-0 and on, or +note_cut or +note_off; its sample, 0 none; its
; volume, +no_volume none; its effect and that effect's number. A sample's
; :data is 16 bit signed, low byte first, (media-widen)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi "lib/media/media_widen" media-widen)

(defq +mod_rows 64 +cell_size 5 +no_volume 255 +note_cut 254 +note_off 255
	+widen_s8 0 +widen_u8 1 +widen_s16 2 +widen_u16 3 +widen_d8 4 +widen_d16 5
	;the periods of the notes of finetune 0, C-1 to B-3, as ProTracker has
	;them, the others are worked out from these
	+mod_periods ''(856 808 762 720 678 640 604 570 538 508 480 453
		428 404 381 360 339 320 302 285 269 254 240 226
		214 202 190 180 170 160 151 143 135 127 120 113))

(defun mod-text (data at n)
	; (mod-text data at n) -> str
	;a name, to its first byte of 0, what is not to be shown a space
	(defq text (slice data at (+ at n)))
	(reduce! (# (cat %0 (if (<= 32 (code %1) 126) %1 " ")))
		(list text) "" 0 (ifn (find "\x00" text) n)))

(defun mod-channels (tag)
	; (mod-channels tag) -> :nil | num
	;how many channels the tag at 1080 says, :nil if it is not one
	(defq digits "0123456789")
	(cond
		((find tag '("M.K." "M!K!" "M&K!" "FLT4" "4CHN" "N.T.")) 4)
		((find tag '("8CHN" "OCTA" "CD81" "FLT8")) 8)
		((and (find (first tag) digits) (eql (slice tag 1 4) "CHN"))
			(str-as-num (slice tag 0 1)))
		((and (find (first tag) digits) (find (second tag) digits)
				(find (slice tag 2 4) '("CH" "CN")))
			(str-as-num (slice tag 0 2)))))

(defun mod-note (period)
	; (mod-note period) -> num
	;the note of a period, 13 C-1 to 48 B-3, the nearest, 0 none
	(if (= period 0) 0
		(defq best 0 gap +max_int)
		(each (lambda (p)
				(when (< (defq d (abs (- p period))) gap) (setq gap d best (!))))
			+mod_periods)
		(+ 13 best)))

(defun mod-sample (data at)
	; (mod-sample data at) -> sample
	;the 30 bytes of a sample's header: its name, how long, its finetune,
	;-8 to 7, its volume, 0 to 64, and its loop, in samples, a loop of 2 or
	;less none
	(defq tune (logand (code data 1 (+ at 24)) 15))
	(scatter (Emap) :name (mod-text data at 22) :length (* 2 (code-be data 2 (+ at 22)))
		:finetune (if (> tune 7) (- tune 16) tune) :volume (min 64 (code data 1 (+ at 25)))
		:loop_start (* 2 (code-be data 2 (+ at 26))) :loop_length (* 2 (code-be data 2 (+ at 28)))))

(defun mod-pattern (data at channels)
	; (mod-pattern data at channels) -> nums
	;the cells of a pattern, a row of channels after another
	(defq cells (nums))
	(each! (lambda ()
			(defq b (+ at (* (!) 4)) b0 (code data 1 b) b2 (code data 1 (+ b 2)))
			(push cells (mod-note (logior (<< (logand b0 15) 8) (code data 1 (inc b))))
				(logior (logand b0 0xf0) (>> b2 4)) +no_volume (logand b2 15) (code data 1 (+ b 3))))
		'() 0 (* +mod_rows channels))
	cells)

(defun mod-read (data &optional bare)
	; (mod-read data [bare]) -> :nil | song
	;the song of a module's bytes, :nil if they are not one; bare, its
	;samples' bytes not had, for what only shows it
	(when (>= (length data) 600)
		(defq channels (if (>= (length data) 1084) (mod-channels (slice data 1080 1084)))
			count (if channels 31 15) base (+ 20 (* 30 count))
			head (if channels 1084 (+ base 130)) channels (ifn channels 4)
			songlen (code data 1 base)
			orders (map! (# (code data 1 (+ base 2 (!)))) '() 0 128)
			;all 128 of the order table, a pattern after the song may be there
			patterns (inc (reduce (const max) orders 0))
			size (* +mod_rows channels 4))
		(when (and (<= 1 songlen 128) (<= (+ head (* patterns size)) (length data)))
			(defq at (+ head (* patterns size)) samples (list))
			;the samples' bytes after the patterns, one after another, the last
			;may be cut short by the file's end
			(each! (lambda ()
					(defq sample (mod-sample data (+ 20 (* 30 (!))))
						n (max 0 (min (. sample :find :length) (- (length data) at)))
						start (. sample :find :loop_start) len (. sample :find :loop_length))
					(. sample :insert :data (if bare "" (media-widen (str-alloc (* 2 n)) data at +widen_s8)))
					(. sample :insert :length n)
					;a loop past the end cut back to it, one of 2 or less none
					(setq len (min len (- n start)))
					(if (<= len 2) (scatter sample :loop_start -1 :loop_end n)
						(scatter sample :loop_start start :loop_end (+ start len)))
					(push samples (scatter sample :pingpong :nil))
					(setq at (+ at n)))
				'() 0 count)
			(scatter (Emap) :kind "MOD" :mode :mod :pitch :amiga :title (mod-text data 0 20)
				:channels channels :samples samples :orders (slice orders 0 songlen)
				:restart (if (< (code data 1 (inc base)) songlen) (code data 1 (inc base)) 0)
				:patterns (map! (# (mod-pattern data (+ head (* (!) size)) channels)) '() 0 patterns)
				:rows (map! (# +mod_rows) '() 0 patterns)
				:speed 6 :tempo 125 :global 64
				:pans (map! (# (if (find (% (!) 4) '(0 3)) 0 256)) '() 0 channels)))))
