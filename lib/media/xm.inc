;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a module of FastTracker 2, an XM, read into a song as lib/media/mod.inc
; has them, which lib/media/tracker.inc plays. Up to 32 channels,
; patterns of 1 to 256 rows packed, and instruments: each a map of which
; of its samples plays each note, envelopes of volume and of pan, a fade
; once let go, and a vibrato of its own. A cell's volume is the byte of
; its volume column as it is, 0 none, and its effects are ProTracker's
; and FastTracker's, G 16 to X 33
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/media/mod.inc")

(defun xm-signed (data at)
	; (xm-signed data at) -> num
	;a byte of -128 to 127
	(if (> (defq b (code data 1 at)) 127) (- b 256) b))

(defun xm-pattern (data at channels)
	; (xm-pattern data at channels) -> (rows cells next)
	;a pattern's cells unpacked and where the next pattern is: a byte
	;with its top bit set says which of the five follow, else it is the
	;note and the other four do
	(defq head (code data 4 at) rows (code data 2 (+ at 5)) size (code data 2 (+ at 7))
		rows (if (= rows 0) 256 rows) cells (nums) at (+ at head) end (+ at size))
	(times (* rows channels) (push cells 0 0 0 0 0))
	(when (> size 0)
		(defq c 0)
		(while (and (< at end) (< c (* rows channels)))
			(defq b (code data 1 at) cell (* c +cell_size) fields (list 0 0 0 0 0))
			(cond
				((bits? b 128)
					(setq at (inc at))
					(each! (lambda ()
							(when (bits? b (<< 1 (!)))
								(elem-set fields (!) (code data 1 at))
								(setq at (inc at))))
						'() 0 5))
				(:t (setq fields (map! (# (code data 1 (+ at (!)))) '() 0 5) at (+ at 5))))
			(bind '(note ins vol effect param) fields)
			;key off 97, the effects of no meaning to it left out
			(if (find effect '(18 19 22 23 24 26 28 30 31 32)) (setq effect 0 param 0))
			(if (> effect 33) (setq effect 0 param 0))
			(elem-set cells cell (cond ((= note 97) +note_off) ((> note 97) 0) (note)))
			(elem-set cells (+ cell 1) ins)
			(elem-set cells (+ cell 2) vol)
			(elem-set cells (+ cell 3) effect)
			(elem-set cells (+ cell 4) param)
			(setq c (inc c))))
	(list rows cells end))

(defun xm-envelope (data at points flags sus ls le)
	; (xm-envelope data at points flags sus ls le) -> envelope
	;an envelope's points, tick and value of each, on if it says so and has
	;some, its sustain point and its loop
	(defq n (min points 12))
	(scatter (Emap) :on (and (bits? flags 1) (> n 0))
		:sustain (if (bits? flags 2) sus -1) :loop (if (bits? flags 4) (list ls le) :nil)
		:points (map! (# (list (code data 2 (+ at (* 4 (!)))) (code data 2 (+ at (* 4 (!)) 2))))
			'() 0 n)))

(defun xm-read (data &optional bare)
	; (xm-read data [bare]) -> :nil | song
	;the song of an XM's bytes, :nil if they are not one; bare, its
	;samples' bytes not had
	(when (and (>= (length data) 80) (eql (slice data 0 17) "Extended Module: ")
			(>= (code data 2 58) 0x104))
		(defq headsz (code data 4 60) songlen (code data 2 64) restart (code data 2 66)
			channels (code data 2 68) count (code data 2 70) insnum (code data 2 72)
			flags (code data 2 74) at (+ 60 headsz) patterns (list) rows (list)
			samples (list) instruments (list))
		(when (and (<= 1 channels 32) (<= songlen 256) (<= count 256))
			;the patterns, and one empty one for the orders past them
			(times count
				(bind '(n cells next) (xm-pattern data at channels))
				(push rows n)
				(push patterns cells)
				(setq at next))
			(push rows 64)
			(push patterns (apply (const nums) (apply (const cat) (map! (# '(0 0 0 0 0)) '() 0 (* 64 channels)))))
			;the instruments, each its samples' headers then their bytes
			(times insnum
				(defq size (code data 4 at) nsamples (code data 2 (+ at 27))
					ins (scatter (Emap) :name (mod-text data (+ at 4) 22) :map (list) :fade 0
						:volume (scatter (Emap) :on :nil) :pan (scatter (Emap) :on :nil)
						:vib_wave 0 :vib_sweep 0 :vib_depth 0 :vib_rate 0 :first -1))
				(cond
					((and (> nsamples 0) (>= size 243))
						(defq first_sample (length samples))
						(scatter ins :first first_sample
							:map (map! (# (if (< (defq s (code data 1 (+ at 33 (!)))) nsamples) (+ first_sample s) -1))
								'() 0 96)
							:volume (xm-envelope data (+ at 129) (code data 1 (+ at 225)) (code data 1 (+ at 233))
								(code data 1 (+ at 227)) (code data 1 (+ at 228)) (code data 1 (+ at 229)))
							:pan (xm-envelope data (+ at 177) (code data 1 (+ at 226)) (code data 1 (+ at 234))
								(code data 1 (+ at 230)) (code data 1 (+ at 231)) (code data 1 (+ at 232)))
							:vib_wave (code data 1 (+ at 235)) :vib_sweep (code data 1 (+ at 236))
							:vib_depth (code data 1 (+ at 237)) :vib_rate (code data 1 (+ at 238))
							:fade (* 2 (code data 2 (+ at 239)))
							:samples nsamples)
						(defq shsize (code data 4 (+ at 29)) heads (+ at size) from (+ heads (* shsize nsamples)))
						(times nsamples
							(defq h (+ heads (* shsize (- (length samples) first_sample)))
								bytes (code data 4 h) kind (code data 1 (+ h 14)) wide (bits? kind 16)
								unit (if wide 2 1) n (/ bytes unit)
								ls (/ (code data 4 (+ h 4)) unit) ll (/ (code data 4 (+ h 8)) unit)
								have (max 0 (min n (/ (- (length data) from) unit)))
								sample (scatter (Emap) :name (mod-text data (+ h 18) 22) :length have
									:volume (min 64 (code data 1 (+ h 12)))
									:finetune (xm-signed data (+ h 13)) :pan (code data 1 (+ h 15))
									:relative (xm-signed data (+ h 16))
									:loop_start -1 :loop_end have :pingpong :nil
									:data (if bare "" (media-widen (str-alloc (* 2 have)) data from
										(if wide +widen_d16 +widen_d8)))))
							;a stereo sample's left half only
							(if (bits? kind 32) (scatter sample :length (setq have (/ have 2))))
							(and (> (logand kind 3) 0) (> ll 0) (< ls have)
								(scatter sample :loop_start ls :loop_end (min have (+ ls ll))
									:pingpong (bits? kind 2)))
							(push samples sample)
							(setq from (+ from bytes)))
						(setq at from))
					(:t (setq at (+ at size))))
				(push instruments ins))
			(scatter (Emap) :kind "XM" :mode :ft2 :pitch (if (bits? flags 1) :linear :amiga)
				:title (mod-text data 17 20) :channels channels :restart restart
				:orders (map! (# (min count (code data 1 (+ 80 (!))))) '() 0 songlen)
				:patterns patterns :rows rows :samples samples :instruments instruments
				:speed (if (> (code data 2 76) 0) (code data 2 76) 6)
				:tempo (if (>= (code data 2 78) 32) (code data 2 78) 125)
				:global 64 :pans (map! (# 128) '() 0 channels)
				;FastTracker's own bugs, as those that made it with it
				:ft2bugs (or (eql (slice data 38 55) "FastTracker v2.00") (eql (slice data 38 46) "OpenMPT "))))))
