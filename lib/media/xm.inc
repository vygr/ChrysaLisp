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

(defun xm-pattern (data at channels)
	; (xm-pattern data at channels) -> (rows cells next)
	;a pattern's cells unpacked and where the next pattern is: a byte
	;with its top bit set says which of the five follow, else it is the
	;note and the other four do
	(defq head (mod-u32 data at) rows (mod-u16 data (+ at 5)) size (mod-u16 data (+ at 7))
		rows (if (= rows 0) 256 rows) cells (mod-cells rows channels 0) at (+ at head) end (+ at size))
	(when (> size 0)
		(defq c 0)
		(while (and (< at end) (< c (* rows channels)))
			(defq b (mod-u8 data at) cell (* c +cell_size) fields (list 0 0 0 0 0))
			(cond
				((bits? b 128)
					(setq at (inc at))
					(each! (lambda ()
							(when (bits? b (<< 1 (!)))
								(elem-set fields (!) (mod-u8 data at))
								(setq at (inc at))))
						'() 0 5))
				(:t (setq fields (map! (# (mod-u8 data (+ at (!)))) '() 0 5) at (+ at 5))))
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
		:points (map! (# (list (mod-u16 data (+ at (* 4 (!)))) (mod-u16 data (+ at (* 4 (!)) 2))))
			'() 0 n)))

(defun xm-read (data &optional bare)
	; (xm-read data [bare]) -> :nil | song
	;the song of an XM's bytes, :nil if they are not one; bare, its
	;samples' bytes not had
	(when (and (>= (length data) 80) (eql (slice data 0 17) "Extended Module: ")
			(>= (mod-u16 data 58) 0x104))
		(defq headsz (mod-u32 data 60) songlen (mod-u16 data 64) restart (mod-u16 data 66)
			channels (mod-u16 data 68) count (mod-u16 data 70) insnum (mod-u16 data 72)
			flags (mod-u16 data 74) at (+ 60 headsz) patterns (list) rows (list)
			samples (list) instruments (list))
		;what is past the file's end reads as nothing, (mod-u8): a file cut
		;short is the song as far as it goes
		(when (and (<= 1 channels 32) (<= songlen 256) (<= count 256) (<= insnum 256))
			;the patterns, and one empty one for the orders past them
			(times count
				(bind '(n cells next) (xm-pattern data at channels))
				(push rows n)
				(push patterns cells)
				(setq at next))
			(push rows 64)
			(push patterns (mod-cells 64 channels 0))
			;the instruments, each its samples' headers then their bytes
			(times insnum
				;one of no samples has a map of no sample for each of its 96
				;keys too, a note on it plays nothing
				(defq size (mod-u32 data at) nsamples (mod-u16 data (+ at 27)) nsamples (if (> nsamples 255) 0 nsamples)
					ins (scatter (Emap) :name (mod-text data (+ at 4) 22) :map (map! (lambda () -1) '() 0 96) :fade 0
						:volume (scatter (Emap) :on :nil) :pan (scatter (Emap) :on :nil)
						:vib_wave 0 :vib_sweep 0 :vib_depth 0 :vib_rate 0 :first -1))
				(cond
					((> nsamples 0)
						(defq first_sample (length samples))
						;its map, envelopes, vibrato and fade, if its header is long
						;enough to have them: one a packer has cut them from plays its
						;first sample for each key, as libxmp has it
						(if (>= size 241)
							(scatter ins :first first_sample
								:map (map! (# (if (< (defq s (mod-u8 data (+ at 33 (!)))) nsamples) (+ first_sample s) -1))
									'() 0 96)
								:volume (xm-envelope data (+ at 129) (mod-u8 data (+ at 225)) (mod-u8 data (+ at 233))
									(mod-u8 data (+ at 227)) (mod-u8 data (+ at 228)) (mod-u8 data (+ at 229)))
								:pan (xm-envelope data (+ at 177) (mod-u8 data (+ at 226)) (mod-u8 data (+ at 234))
									(mod-u8 data (+ at 230)) (mod-u8 data (+ at 231)) (mod-u8 data (+ at 232)))
								:vib_wave (mod-u8 data (+ at 235)) :vib_sweep (mod-u8 data (+ at 236))
								:vib_depth (mod-u8 data (+ at 237)) :vib_rate (mod-u8 data (+ at 238))
								:fade (* 2 (mod-u16 data (+ at 239))))
							(scatter ins :first first_sample :map (map! (lambda () first_sample) '() 0 96)))
						(defq shsize (mod-u32 data (+ at 29)) heads (+ at size) from (+ heads (* shsize nsamples)))
						(times nsamples
							(defq h (+ heads (* shsize (- (length samples) first_sample)))
								bytes (mod-u32 data h) kind (mod-u8 data (+ h 14)) wide (bits? kind 16)
								unit (if wide 2 1) n (/ bytes unit)
								ls (/ (mod-u32 data (+ h 4)) unit) ll (/ (mod-u32 data (+ h 8)) unit)
								have (mod-frames data from n unit)
								sample (scatter (Emap) :name (mod-text data (+ h 18) 22) :length have
									:volume (min 64 (mod-u8 data (+ h 12)))
									:finetune (mod-s8 data (+ h 13)) :pan (mod-u8 data (+ h 15))
									:relative (mod-s8 data (+ h 16))
									:loop_start -1 :loop_end have :pingpong :nil
									:data (mod-widen data from have (if wide +widen_d16 +widen_d8) bare)))
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
				:orders (map! (# (min count (mod-u8 data (+ 80 (!))))) '() 0 songlen)
				:patterns patterns :rows rows :samples samples :instruments instruments
				:speed (mod-speed (mod-u16 data 76)) :tempo (mod-tempo (mod-u16 data 78))
				:global 64 :pans (map! (# 128) '() 0 channels)
				;FastTracker's own bugs, as those that made it with it
				:ft2bugs (or (eql (slice data 38 55) "FastTracker v2.00") (eql (slice data 38 46) "OpenMPT "))))))
