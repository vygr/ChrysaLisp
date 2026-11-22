;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a module of Impulse Tracker, an IT, read into a song as lib/media/mod.inc
; has them, which lib/media/tracker.inc plays. Up to 64 channels,
; patterns of up to 200 rows packed, samples of 8 or 16 bits, packed or
; not, each with its speed at C-5 and a loop and a sustain loop, and, if it
; has them, instruments: each a map of which sample plays each note, and
; what, envelopes of volume, of pan and of pitch, a fade, and what a new
; note does to the one before. A cell's volume is the byte of its volume
; column as it is, +no_volume none, and its effects are by letter, A 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/media/mod.inc")

(ffi "lib/media/media_itunpack" media-itunpack)

(defq +itunpack_16 1 +itunpack_215 2 +itunpack_unsigned 4
	;the mix volume of a ModPlug Tracker 1.16 file is turned down by 96 over
	;this, by its channels, as libxmp and OpenMPT have it
	+mpt_preamp ''(0x60 0x60 0x60 0x70 0x80 0x88 0x90 0x98 0xA0 0xA4 0xA8 0xB0 0xB4 0xB8 0xBC 0xC0))

(defun it-envelope (data at)
	; (it-envelope data at) -> envelope
	;an envelope of 25 points, a tick of two bytes and a value of one each,
	;on if it says so and has some, its loop and its sustain loop
	(defq flags (code data 1 at) n (min 25 (code data 1 (inc at))))
	(scatter (Emap) :on (and (bits? flags 1) (> n 0)) :carry (bits? flags 8) :filter (bits? flags 128)
		:loop (if (bits? flags 2) (list (code data 1 (+ at 2)) (code data 1 (+ at 3))))
		:sustain (if (bits? flags 4) (list (code data 1 (+ at 4)) (code data 1 (+ at 5))))
		:points (map! (# (list (code data 2 (+ at 7 (* 3 (!)))) (get-byte data (+ at 6 (* 3 (!))))))
			'() 0 n)))

(defun it-envelope-off ()
	; (it-envelope-off) -> envelope
	(scatter (Emap) :on :nil :carry :nil :filter :nil :loop :nil :sustain :nil :points (list)))

(defun it-keys (data at)
	; (it-keys data at) -> ((sample transpose) ...)
	;each of the 120 notes' sample, -1 none, and the note it plays as
	(map! (lambda ()
			(defq s (dec (code data 1 (+ at (* 2 (!)) 1))))
			(if (>= s 0) (list s (- (code data 1 (+ at (* 2 (!)))) (!))) (list -1 0)))
		'() 0 120))

(defun it-instrument-none (name)
	; (it-instrument-none name) -> instrument
	;an instrument of nothing: no map, no envelopes, what a new note does
	;to the one before the least
	(scatter (Emap) :name name :nna 0 :dct 0 :dca 0 :fade 0 :vol 64
		:pan -1 :ifc 0 :ifr 0 :rv 0 :rp 0 :map (list) :volume (it-envelope-off)
		:panning (it-envelope-off) :pitch (it-envelope-off)))

(defun it-instrument (data at new)
	; (it-instrument data at new) -> instrument
	;an instrument of the format of IT 2, new, or of IT 1, each 554 bytes:
	;one the file has not all of is of nothing
	(defq there (<= (+ at 554) (length data))
		ins (it-instrument-none (if there (mod-text data (+ at 32) 26) "")))
	(and there (eql (slice data at (+ at 4)) "IMPI")
		(cond
			(new
				(defq dfp (code data 1 (+ at 25)) volume (it-envelope data (+ at 304))
					panning (it-envelope data (+ at 386)) pitch (it-envelope data (+ at 468)))
				;a pan envelope about 32, a filter's about 32 and of four
				;times as much, a pitch's of 50 a half semitone
				(. panning :insert :points (map (# (list (first %0) (+ (second %0) 32))) (. panning :find :points)))
				(. pitch :insert :points (map (# (list (first %0)
						(if (. pitch :find :filter) (* 4 (+ (second %0) 32)) (* 50 (second %0)))))
					(. pitch :find :points)))
				(scatter ins :nna (code data 1 (+ at 17)) :dct (code data 1 (+ at 18))
					:dca (elem-get '(0 2 3 3) (min 3 (code data 1 (+ at 19))))
					:fade (<< (code data 2 (+ at 20)) 6) :vol (>> (min 128 (code data 1 (+ at 24))) 1)
					:pan (if (bits? dfp 128) -1 (* 4 dfp)) :rv (code data 1 (+ at 26)) :rp (code data 1 (+ at 27))
					:ifc (code data 1 (+ at 58)) :ifr (code data 1 (+ at 59)) :map (it-keys data (+ at 64))
					:volume volume :panning panning :pitch pitch))
			(:t ;IT 1's: a volume envelope only, of nodes of a byte each
				(defq flags (code data 1 (+ at 17)) nodes (+ at 504) n 0)
				(while (and (< n 25) (/= (code data 1 (+ nodes (* 2 n))) 255)) (setq n (inc n)))
				(scatter ins :nna (code data 1 (+ at 26)) :dct (if (> (code data 1 (+ at 27)) 0) 1 0)
					:fade (<< (code data 2 (+ at 24)) 7) :map (it-keys data (+ at 64))
					:volume (scatter (Emap) :on (and (bits? flags 1) (> n 0)) :carry :nil :filter :nil
						:loop (if (bits? flags 2) (list (code data 1 (+ at 18)) (code data 1 (+ at 19))))
						:sustain (if (bits? flags 4) (list (code data 1 (+ at 20)) (code data 1 (+ at 21))))
						:points (map! (# (list (code data 1 (+ nodes (* 2 (!)))) (code data 1 (+ nodes (* 2 (!)) 1))))
							'() 0 n))))))
	;the sample it plays when its note has none, the first of its map
	(. ins :insert :first (reduce (lambda (s (k &ignore)) (if (and (< s 0) (>= k 0)) k s)) (. ins :find :map) -1)))

(defun it-sample (data at bare)
	; (it-sample data at bare) -> sample
	;a sample's header and its bytes: 8 or 16 bits, signed or not, packed
	;or not; bare, not its bytes
	(defq sample (scatter (Emap) :name "" :data "" :length 0 :loop_start -1 :loop_end 0 :pingpong :nil
		:sus_start -1 :sus_end 0 :sus_pingpong :nil :volume 0 :gvl 64 :pan -1 :xpo 0 :fin 0
		:vib_rate 0 :vib_depth 0 :vib_wave 0 :vib_sweep 0))
	(when (and (<= (+ at 80) (length data)) (eql (slice data at (+ at 4)) "IMPS"))
		(defq flags (code data 1 (+ at 18)) cvt (code data 1 (+ at 46)) dfp (code data 1 (+ at 47))
			n (code data 4 (+ at 48)) ls (code data 4 (+ at 52)) le (code data 4 (+ at 56))
			c5spd (code data 4 (+ at 60)) ss (code data 4 (+ at 64)) se (code data 4 (+ at 68))
			from (code data 4 (+ at 72)) wide (bits? flags 2))
		;its speed at C-5 a note and a 128th of one off 8363, as libxmp has it
		(defq c (if (> c5spd 0) (n2i (* (const (n2d 1536)) (/ (log (/ (n2d c5spd) (const (n2d 8363)))) (const (log (n2d 2)))))) 0))
		(scatter sample :name (mod-text data (+ at 20) 26) :volume (min 64 (code data 1 (+ at 19)))
			:gvl (min 64 (code data 1 (+ at 17))) :pan (if (bits? dfp 128) (* 4 (logand dfp 127)) -1)
			:xpo (/ c 128) :fin (% c 128) :vib_rate (code data 1 (+ at 76)) :vib_depth (<< (code data 1 (+ at 77)) 1)
			:vib_wave (code data 1 (+ at 79)) :vib_sweep (>>> (- 255 (code data 1 (+ at 78))) 1))
		(when (and (bits? flags 1) (> n 1) (< from (length data)))
			(cond
				((bits? flags 8)
					;packed, a byte or more for each eight samples
					(setq n (min n (* 8 (- (length data) from))))
					(unless bare (. sample :insert :data (media-itunpack (str-alloc (* 2 n)) data from
						(logior (if wide +itunpack_16 0) (if (bits? cvt 4) +itunpack_215 0)
							(if (bits? cvt 1) 0 +itunpack_unsigned))))))
				(:t (setq n (mod-frames data from n (if wide 2 1)))
					(. sample :insert :data (mod-widen data from n (mod-widen-kind wide (bits? cvt 1)) bare))))
			(. sample :insert :length n)
			(and (bits? flags 16) (< ls (setq le (min le n)))
				(scatter sample :loop_start ls :loop_end le :pingpong (bits? flags 64)))
			(and (bits? flags 32) (< ss (setq se (min se n)))
				(scatter sample :sus_start ss :sus_end se :sus_pingpong (bits? flags 128)))))
	sample)

(defun it-pattern (data at events)
	; (it-pattern data at events) -> rows
	;a pattern's cells unpacked into events, (row channel cell) each: a
	;byte of which channel and if a mask follows, the mask of what follows
	;and of what is as the channel's last
	(defq rows 64)
	(when (> at 0)
		(defq size (code data 2 at) rows (code data 2 (+ at 2)) at (+ at 8)
			end (min (length data) (+ at size)) row 0 masks (nums) lasts (list) last_s (nums))
		(times 64 (push masks 0) (push lasts (list 0 0 +no_volume 0 0)) (push last_s 0))
		(when (<= 1 rows 1024)
			(while (and (< row rows) (< at end))
				(defq b (code data 1 at) at (inc at))
				(cond
					((= b 0) (setq row (inc row)))
					(:t (defq c (logand (dec b) 63) cell (list 0 0 +no_volume 0 0) prev (elem-get lasts c))
						(when (bits? b 128)
							(elem-set masks c (code data 1 at))
							(setq at (inc at)))
						(defq mask (elem-get masks c))
						(when (bits? mask 1)
							(defq n (code data 1 at) at (inc at))
							(elem-set cell 0 (cond ((= n 255) +note_off) ((= n 254) +note_cut) ((> n 119) +note_fade) ((inc n))))
							(elem-set prev 0 (first cell)))
						(when (bits? mask 2)
							(elem-set cell 1 (code data 1 at))
							(elem-set prev 1 (second cell))
							(setq at (inc at)))
						(when (bits? mask 4)
							(elem-set cell 2 (code data 1 at))
							(elem-set prev 2 (third cell))
							(setq at (inc at)))
						(when (bits? mask 8)
							(defq fx (code data 1 at) param (code data 1 (inc at)) at (+ at 2))
							(cond
								;OpenMPT's smooth Zxx, \\, kept as 31, past the volume
								;column's of the player
								((= fx 28) (setq fx 31))
								((> fx 26) (setq fx 0 param 0))
								((= fx 19)
									;S00 the channel's last S of the pattern
									(if (= param 0) (setq param (elem-get last_s c)) (elem-set last_s c param))))
							(elem-set cell 3 fx)
							(elem-set cell 4 param)
							(elem-set prev 3 fx)
							(elem-set prev 4 param))
						(if (bits? mask 16) (elem-set cell 0 (elem-get prev 0)))
						(if (bits? mask 32) (elem-set cell 1 (elem-get prev 1)))
						(if (bits? mask 64) (elem-set cell 2 (elem-get prev 2)))
						(when (bits? mask 128)
							(elem-set cell 3 (elem-get prev 3))
							(elem-set cell 4 (elem-get prev 4)))
						(push events (list row c cell))))))
		(if (> rows 1024) (setq rows 64)))
	rows)

(defun it-macros (data flags special at)
	; (it-macros data flags special at) -> :nil | (params fixed)
	;the MIDI macros of the song's own, if it has them, after its offsets
	;and its edit history: those of SFx, 16, and of Z80 to ZFF, 128, a
	;text each to its first nul, of 31 at most
	(when (or (bits? flags 128) (bits? special 8))
		(if (bits? special 2) (setq at (+ at 2 (* 8 (code data 2 at)))))
		(setq at (+ at (* 9 32)))
		(when (<= (+ at (* 144 32)) (length data))
			(defq texts (map! (lambda ()
					(mod-cstr data (+ at (* 32 (!))) 31))
				'() 0 144))
			(list (slice texts 0 16) (slice texts 16 144)))))

(defun it-modplug? (data cwt cmwt at insnum smpnum patnum)
	; (it-modplug? data cwt cmwt at insnum smpnum patnum) -> :t | :nil
	;made by ModPlug Tracker, 1.16 or before, or OpenMPT 1.17, as libxmp
	;tells them: a 1.16 says it is IT 2.17 of format 2.00, one before it
	;IT 2.02 of 2.00 with its patterns before its samples
	(or (= cwt 0x888) (and (= cmwt 0x200) (= cwt 0x217))
		(and (= cmwt 0x200) (= cwt 0x202) (> smpnum 0) (> patnum 0)
			(< 0 (code data 4 (+ at (* 4 (+ insnum smpnum)))) (code data 4 (+ at (* 4 insnum)))))))

(defun it-read (data &optional bare)
	; (it-read data [bare]) -> :nil | song
	;the song of an IT's bytes, :nil if they are not one; bare, its
	;samples' bytes not had
	(when (and (>= (length data) 192) (eql (slice data 0 4) "IMPM"))
		(defq ordnum (code data 2 32) insnum (code data 2 34) smpnum (code data 2 36)
			patnum (code data 2 38) cwt (code data 2 40) cmwt (code data 2 42) flags (code data 2 44) gv (code data 1 48)
			special (code data 2 46)
			at (+ 192 ordnum) sample_mode (not (bits? flags 4)))
		(when (and (<= gv 128) (<= insnum 255) (<= smpnum 255) (<= patnum 255)
				(<= (+ at (* 4 (+ insnum smpnum patnum))) (length data)))
			(defq samples (map! (# (it-sample data (code data 4 (+ at (* 4 (+ insnum (!))))) bare)) '() 0 smpnum)
				instruments (if sample_mode
					;a song of samples, each its own instrument
					(map (lambda (sample)
							(defq s (!))
							(scatter (it-instrument-none (. sample :find :name))
								:first (if (> (. sample :find :length) 0) s -1)
								:map (map! (# (list s 0)) '() 0 120)))
						samples)
					(map! (# (it-instrument data (code data 4 (+ at (* 4 (!)))) (>= cmwt 0x200))) '() 0 insnum))
				events (list) channels 1 rows (list) patterns (list))
			;each pattern's events, then as many channels as the song uses
			(each! (lambda ()
					(push rows (it-pattern data (code data 4 (+ at (* 4 (+ insnum smpnum (!))))) (defq e (list))))
					(each (# (setq channels (max channels (inc (second %0))))) e)
					(push events e))
				'() 0 patnum)
			(each (lambda (e n)
					(defq cells (mod-cells n channels))
					(each (lambda ((row c cell))
							(defq i (* (+ (* row channels) c) +cell_size))
							(each (# (elem-set cells (+ i (!)) %0)) cell))
						e)
					(push patterns cells))
				events rows)
			(scatter (Emap) :kind "IT" :mode :it :pitch (if (bits? flags 8) :linear :amiga)
				:title (mod-text data 4 26) :channels channels :restart 0 :markers :t
				:orders (map! (# (code data 1 (+ 192 (!)))) '() 0 ordnum)
				:patterns patterns :rows rows :samples samples :instruments instruments
				:sample_mode sample_mode :old_fx (bits? flags 16) :link_gxx (bits? flags 32)
				:speed (mod-speed (code data 1 50)) :tempo (mod-tempo (code data 1 51))
				:macros (it-macros data flags special (+ at (* 4 (+ insnum smpnum patnum))))
				:global gv :mix (if (it-modplug? data cwt cmwt at insnum smpnum patnum)
					(/ (* (code data 1 49) 96) (elem-get +mpt_preamp (>> (max 1 (min 31 channels)) 1)))
					(code data 1 49))
				:pans (map! (lambda ()
						(defq p (logand (code data 1 (+ 64 (!))) 127))
						(cond ((not (bits? flags 1)) 128) ((= p 100) 128) ((min 255 (* 4 (logand p 127))))))
					'() 0 channels)
				:chvols (map! (# (min 64 (code data 1 (+ 128 (!))))) '() 0 channels)
				:mutes (map! (# (bits? (code data 1 (+ 64 (!))) 128)) '() 0 channels)))))
