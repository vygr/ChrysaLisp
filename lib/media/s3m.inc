;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a module of Scream Tracker 3, an S3M, read into a song as
; lib/media/mod.inc has them, which lib/media/tracker.inc plays. Up to 32
; channels, samples of 8 or 16 bits each with its speed at C-4, patterns
; of 64 rows packed, a cell only of what a channel has, and the effects
; by letter, A 1 to Z 26
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/media/mod.inc")

(defun s3m-sample (data at signed bare)
	; (s3m-sample data at signed bare) -> sample
	;an instrument's header and its sample's bytes, signed or not: one not
	;of a sample, an Adlib's, is of none, and one past the file's end has
	;no name either
	(defq there (<= (+ at 80) (length data))
		sample (scatter (Emap) :name (if there (mod-text data (+ at 48) 28) "") :data "" :length 0
			:loop_start -1 :loop_end 0 :pingpong :nil :volume 0 :c2spd 8363))
	(when (and there (= (code data 1 at) 1))
		(defq flags (code data 1 (+ at 31)) wide (bits? flags 4)
			from (* 16 (logior (<< (code data 1 (+ at 13)) 16) (code data 2 (+ at 14))))
			n (mod-frames data from (code data 4 (+ at 16)) (if wide 2 1))
			ls (code data 4 (+ at 20)) le (min n (code data 4 (+ at 24))))
		(scatter sample :length n :volume (min 64 (code data 1 (+ at 28))) :c2spd (code data 2 (+ at 32))
			:data (mod-widen data from n (mod-widen-kind wide signed) bare))
		(if (and (bits? flags 1) (> le (inc ls)))
			(scatter sample :loop_start ls :loop_end le)
			(. sample :insert :loop_end n)))
	sample)

(defun s3m-pattern (data at channels)
	; (s3m-pattern data at channels) -> nums
	;the cells of a pattern unpacked, a row of channels after another: a
	;byte of which channel and what follows of it, 0 the row's end
	(defq cells (mod-cells 64 channels) row 0)
	;what is past the file's end reads as nothing, (mod-u8)
	(when (< 0 at (length data))
		(defq end (min (length data) (+ at (mod-u16 data at))) at (+ at 2))
		(while (and (< row 64) (< at end))
			(defq b (mod-u8 data at) c (logand b 31) cell (* (+ (* row channels) c) +cell_size)
				keep (< c channels))
			(setq at (inc at))
			(cond
				((= b 0) (setq row (inc row)))
				(:t (when (bits? b 32)
						(defq n (mod-u8 data at))
						(when keep
							(elem-set cells cell (cond ((= n 255) 0) ((= n 254) +note_cut)
								((+ 1 (* 12 (>> n 4)) (logand n 15)))))
							(elem-set cells (inc cell) (mod-u8 data (inc at))))
						(setq at (+ at 2)))
					(when (bits? b 64)
						(if keep (elem-set cells (+ cell 2) (mod-u8 data at)))
						(setq at (inc at)))
					(when (bits? b 128)
						(when keep
							(elem-set cells (+ cell 3) (mod-u8 data at))
							(elem-set cells (+ cell 4) (mod-u8 data (inc at))))
						(setq at (+ at 2)))))))
	cells)

(defun s3m-read (data &optional bare)
	; (s3m-read data [bare]) -> :nil | song
	;the song of an S3M's bytes, :nil if they are not one; bare, its
	;samples' bytes not had
	(when (and (>= (length data) 96) (eql (slice data 44 48) "SCRM") (= (code data 1 29) 16)
			(<= 1 (defq sign_kind (code data 2 42)) 2))
		(defq ordnum (code data 2 32) insnum (code data 2 34) patnum (code data 2 36)
			flags (code data 2 38) version (code data 2 40) gv (code data 1 48) mv (code data 1 51)
			;as many channels as the last of the 32 that is one
			channels (inc (reduce! (# (if (/= (code data 1 (+ 64 (!))) 255) (!) %0)) '() -1 0 32))
			orders (map! (# (mod-u8 data (+ 96 (!)))) '() 0 ordnum)
			patterns (min patnum (inc (reduce (lambda (top o) (if (< o 254) (max top o) top)) orders -1)))
			at (+ 96 ordnum) pans_at (+ at (* 2 insnum) (* 2 patnum)))
		;the mix's volume, and stereo or not, as each version of it says
		(defq stereo (bits? mv 128) mix (logand mv 127))
		(cond
			((= sign_kind 1) (setq mix (max 16 (min 127 (* 16 (inc (logand mv 15))))) stereo (bits? mv 16)))
			((find mv '(2 18)) (setq mix 32 stereo (bits? mv 16)))
			((= mix 0) (setq mix 48))
			((< mix 16) (setq mix 16)))
		(unless stereo (setq mix (/ (* mix 8) 11)))
		(and (> channels 0) (> patterns 0) (<= pans_at (length data))
			(scatter (Emap) :kind "S3M" :mode :st3 :pitch :st3 :title (mod-text data 0 28)
				:channels channels :orders orders :restart 0
				:samples (map! (# (s3m-sample data (* 16 (mod-u16 data (+ at (* 2 (!))))) (= sign_kind 1) bare)) '() 0 insnum)
				:patterns (map! (# (s3m-pattern data (* 16 (mod-u16 data (+ at (* 2 insnum) (* 2 (!))))) channels))
					'() 0 patterns)
				:rows (map! (# 64) '() 0 patterns)
				:speed (mod-speed (code data 1 49)) :tempo (mod-tempo (code data 1 50))
				;the volume the song starts at, as Scream Tracker has it: none
				;said by one before 3.20 is all of it
				:global (if (and (= gv 0) (< version 0x1320)) 64 (min 64 gv)) :mix mix :markers :t :fast (or (bits? flags 64) (= version 0x1300))
				:pans (map! (lambda ()
						(defq x (logand (code data 1 (+ 64 (!))) 31)
							pan (if (and stereo (< x 16)) (if (< x 8) 0x30 0xc0) 128))
						;its own of the table after the pointers, if there is one
						(and (= (code data 1 53) 252) (< (+ pans_at (!)) (length data))
							(bits? (defq p (code data 1 (+ pans_at (!)))) 32)
							(setq pan (* 16 (logand p 15))))
						pan)
					'() 0 channels)))))
