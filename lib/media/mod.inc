;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a module of the Amiga's trackers, ProTracker's MOD and those like it,
; read into a song, which lib/media/tracker.inc plays: its samples, its
; patterns and the order they are played in. Of 31 samples and a tag at
; 1080 that says how many channels, M.K. and the like 4, 6CHN 6, 8CHN or
; FLT8 8, 10CH to 32CH so many; with no tag the Soundtracker's 15 and 4.
; A cell of a pattern is 4 numbers, the period of its note, 0 none, its
; sample, 0 none, its effect and that effect's number
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +mod_rows 64 +mod_cell 4)

(defun mod-text (data at n)
	; (mod-text data at n) -> str
	;a name, to its first byte of 0, what is not to be shown a space
	(defq text (slice data at (+ at n)))
	(reduce (# (cat %0 (if (<= 32 (code %1) 126) %1 " ")))
		(slice text 0 (ifn (find "\x00" text) n)) ""))

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
			(defq b (+ at (* (!) +mod_cell)) b0 (code data 1 b) b2 (code data 1 (+ b 2)))
			(push cells (logior (<< (logand b0 15) 8) (code data 1 (inc b)))
				(logior (logand b0 0xf0) (>> b2 4)) (logand b2 15) (code data 1 (+ b 3))))
		'() 0 (* +mod_rows channels))
	cells)

(defun mod-read (data)
	; (mod-read data) -> :nil | song
	;the song of a module's bytes, :nil if they are not one
	(when (>= (length data) 600)
		(defq channels (if (>= (length data) 1084) (mod-channels (slice data 1080 1084)))
			count (if channels 31 15) base (+ 20 (* 30 count))
			head (if channels 1084 (+ base 130)) channels (ifn channels 4)
			songlen (code data 1 base)
			orders (map! (# (code data 1 (+ base 2 (!)))) '() 0 128)
			;all 128 of the order table, a pattern after the song may be there
			patterns (inc (reduce (const max) orders 0))
			size (* +mod_rows channels +mod_cell))
		(when (and (<= 1 songlen 128) (<= (+ head (* patterns size)) (length data)))
			(defq at (+ head (* patterns size)) samples (list))
			;the samples' bytes after the patterns, one after another, the last
			;may be cut short by the file's end
			(each! (lambda ()
					(defq sample (mod-sample data (+ 20 (* 30 (!))))
						n (max 0 (min (. sample :find :length) (- (length data) at)))
						start (. sample :find :loop_start) len (. sample :find :loop_length))
					(. sample :insert :data (slice data at (+ at n)))
					(. sample :insert :length n)
					;a loop past the end cut back to it, one of 2 or less none
					(setq len (min len (- n start)))
					(if (<= len 2) (scatter sample :loop_start -1 :loop_length 0)
						(scatter sample :loop_start start :loop_length len))
					(push samples sample)
					(setq at (+ at n)))
				'() 0 count)
			(scatter (Emap) :kind "MOD" :title (mod-text data 0 20) :channels channels
				:samples samples :orders (slice orders 0 songlen)
				:restart (if (< (code data 1 (inc base)) songlen) (code data 1 (inc base)) 0)
				:rows +mod_rows
				:patterns (map! (# (mod-pattern data (+ head (* (!) size)) channels)) '() 0 patterns)))))
