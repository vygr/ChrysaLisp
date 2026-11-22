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
; to 256 right, and the :mode its effects are of, which player of
; lib/media/tracker.inc plays it, :mod :st3 :ft2 :it :med or :ahx, and its
; :pitch, :linear if its periods are, an XM's or an IT's. A cell of a
; pattern is 5 numbers: its note, 0 none, 1 C-0 and on, or +note_cut,
; +note_off or +note_fade; its sample, 0 none; its volume, +no_volume
; none; its effect and that effect's number. A sample's :data is 16 bit
; signed, low byte first, (media-widen)
;
; What every reader does is here once: a byte or a number of a file that
; may be cut short, a text to its 0, a pattern of nothing, the Amiga's
; pans, the first speed and tempo, and a sample's bytes made the mixer's.
; A signed byte is the core's, (get-byte). A pan of 255 is as far right as
; libxmp has one, and an IT's and a MED's are its: they are left so
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi "lib/media/media_widen" media-widen)

(defq +mod_rows 64 +cell_size 5 +no_volume 255 +note_fade 253 +note_cut 254 +note_off 255
	+widen_s8 0 +widen_u8 1 +widen_s16 2 +widen_u16 3 +widen_d8 4 +widen_d16 5
	+widen_s16be 6 +widen_s8x2 7 +widen_s16bex2 8
	;the periods of the notes of finetune 0, C-1 to B-3, as ProTracker has
	;them, the others are worked out from these
	+mod_periods ''(856 808 762 720 678 640 604 570 538 508 480 453
		428 404 381 360 339 320 302 285 269 254 240 226
		214 202 190 180 170 160 151 143 135 127 120 113))

(defun mod-u8 (data at)
	; (mod-u8 data at) -> num
	;a byte of a file, 0 past its end: a file cut short, or one that points
	;where it has nothing, reads as nothing there, and is no error
	(if (< -1 at (length data)) (code data 1 at) 0))

(defun mod-s8 (data at)
	; (mod-s8 data at) -> num
	;a byte of -128 to 127, 0 past the file's end; of bytes known to be
	;there it is the core's (get-byte)
	(sign-extend (mod-u8 data at) 8))

(defun mod-u16 (data at)
	; (mod-u16 data at) -> num
	;two bytes, low first, 0 past the file's end
	(if (and (>= at 0) (<= (+ at 2) (length data))) (code data 2 at) 0))

(defun mod-u32 (data at)
	; (mod-u32 data at) -> num
	;four bytes, low first, 0 past the file's end
	(if (and (>= at 0) (<= (+ at 4) (length data))) (code data 4 at) 0))

(defun mod-bytes (data at n)
	; (mod-bytes data at n) -> str
	;so many bytes of a file, fewer or none past its end
	(defq end (length data))
	(slice data (max 0 (min at end)) (max 0 (min (+ at n) end))))

(defun mod-cstr (data at n)
	; (mod-cstr data at n) -> str
	;the bytes of a text of n at most, to its first byte of 0, or the
	;file's end
	(defq text (mod-bytes data at n))
	(slice text 0 (ifn (find "\x00" text) (length text))))

(defun mod-text (data at n)
	; (mod-text data at n) -> str
	;a name, to its first byte of 0, what is not to be shown a space
	(reduce (# (cat %0 (if (<= 32 (code %1) 126) %1 " "))) (mod-cstr data at n) ""))

(defun mod-cells (rows channels &optional volume)
	; (mod-cells rows channels [volume]) -> nums
	;the cells of a pattern of nothing: no note, sample or effect, and a
	;volume of none, +no_volume, or the one a tracker has for none
	(defq cells (fill (nums) (* rows channels +cell_size) 0) volume (ifn volume +no_volume))
	(if (/= volume 0) (each! (# (elem-set cells (+ (* (!) +cell_size) 2) volume)) '() 0 (* rows channels)))
	cells)

(defun mod-pans (channels left right)
	; (mod-pans channels left right) -> (pan ...)
	;the Amiga's places of its channels: left right right left, and round
	(map! (# (if (find (% (!) 4) '(0 3)) left right)) '() 0 channels))

(defun mod-speed (n)
	; (mod-speed n) -> speed
	;a song's first speed as its header has it, 6 if it has none
	(if (> n 0) n 6))

(defun mod-tempo (n)
	; (mod-tempo n) -> tempo
	;a song's first tempo as its header has it, 125 if it has none that is one
	(if (>= n 32) n 125))

(defun mod-frames (data at n unit)
	; (mod-frames data at n unit) -> num
	;how many of a sample's n frames, of unit bytes each, the file has from
	;at: one cut short by the file's end is as long as what is there
	(max 0 (min n (/ (- (length data) at) unit))))

(defun mod-widen-kind (wide signed)
	; (mod-widen-kind wide signed) -> kind
	;the kind of a sample's bytes, (media-widen): 8 or 16 bits, signed or not
	(if wide (if signed +widen_s16 +widen_u16) (if signed +widen_s8 +widen_u8)))

(defun mod-widen (data at n kind bare)
	; (mod-widen data at n kind bare) -> str
	;so many frames of a sample's bytes at at made the mixer's, of that kind,
	;(media-widen), which reads as many as it is told: none of them if bare
	(if bare "" (media-widen (str-alloc (* 2 n)) data at kind)))

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

(defun mod-tagged (data)
	; (mod-tagged data) -> :nil | channels
	;how many channels the tag of a module's bytes says, :nil if they have
	;none, as a Soundtracker's of 15 samples has not
	(if (>= (length data) 1084) (mod-channels (slice data 1080 1084))))

(defun mod-nearest (period)
	; (mod-nearest period) -> num
	;the note nearest a period, 0 to 35, of finetune 0
	(defq best 0 gap +max_int)
	(each (lambda (p)
			(when (< (defq d (abs (- p period))) gap) (setq gap d best (!))))
		+mod_periods)
	best)

(defun mod-note (period)
	; (mod-note period) -> num
	;the note of a period, 13 C-1 to 48 B-3, the nearest, 0 none
	(if (= period 0) 0 (+ 13 (mod-nearest period))))

(defun mod-sample (data at)
	; (mod-sample data at) -> sample
	;the 30 bytes of a sample's header: its name, how long, its finetune,
	;-8 to 7, its volume, 0 to 64, and its loop, in samples, a loop of 2 or
	;less none
	(scatter (Emap) :name (mod-text data at 22) :length (* 2 (code-be data 2 (+ at 22)))
		:finetune (sign-extend (logand (code data 1 (+ at 24)) 15) 4) :volume (min 64 (code data 1 (+ at 25)))
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
		(defq channels (mod-tagged data)
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
						n (mod-frames data at (. sample :find :length) 1)
						start (. sample :find :loop_start) len (. sample :find :loop_length))
					(. sample :erase :loop_length)
					(scatter sample :data (mod-widen data at n +widen_s8 bare) :length n)
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
				:pans (mod-pans channels 0 256)))))
