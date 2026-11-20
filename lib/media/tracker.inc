;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a tracker's song played, a song of lib/media/mod.inc: its rows a tick
; at a time, the effects of the Amiga's ProTracker on each channel, and
; the channels mixed by media_mix, lib/media/lisp_tracker.vp, into frames
; of 16 bit stereo. The Amiga's clock sets a note's pitch, its period the
; clocks between one sample and the next. A tick is 2.5 / bpm of a second,
; a row speed ticks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi "lib/media/media_mix" media-mix)

(defq +mix_size 7 +tracker_clock 3546895 +period_lo 113 +period_hi 856
	;the periods of the notes of finetune 0, C-1 to B-3, as ProTracker has
	;them, the others are worked out from these
	+tracker_periods ''(856 808 762 720 678 640 604 570 538 508 480 453
		428 404 381 360 339 320 302 285 269 254 240 226
		214 202 190 180 170 160 151 143 135 127 120 113)
	;the half of a sine of vibrato and tremolo
	+tracker_sine ''(0 24 49 74 97 120 141 161 180 197 212 224 235 244 250 253
		255 253 250 244 235 224 212 197 180 161 141 120 97 74 49 24))

(defun tracker-tables ()
	; (tracker-tables) -> ((period ...) ...)
	;the periods of the 36 notes for each finetune, -8 to 7, by it and 8:
	;each a 96th of a semitone less
	(map! (lambda ()
			(defq tune (- (!) 8))
			(map (# (n2i (+ (* (n2d %0) (pow (const (n2d 2)) (/ (n2d (neg tune)) (const (n2d 96))))) (const (n2d 0.5)))))
				+tracker_periods))
		'() 0 16))

(defun tracker-note (period)
	; (tracker-note period) -> num
	;the note nearest a period, of finetune 0
	(defq best 0 gap +max_int)
	(each (lambda (p)
			(when (< (defq d (abs (- p period))) gap) (setq gap d best (!))))
		+tracker_periods)
	best)

(defun tracker-channel (pan)
	; (tracker-channel pan) -> channel
	(scatter (Emap) :sample -1 :next -1 :volume 0 :period 0 :finetune 0 :note 0
		:target 0 :porta 0 :vib_speed 0 :vib_depth 0 :vib_pos 0 :vib_wave 0
		:trem_speed 0 :trem_depth 0 :trem_pos 0 :trem_wave 0 :offset 0
		:effect 0 :param 0 :loop_row 0 :loop_count 0 :pan pan :delay -1
		:period_out 0 :volume_out 0 :trigger :nil :start 0))

(defun tracker-player (song rate &optional separation)
	; (tracker-player song rate [separation]) -> player
	;a player of a song at a rate of frames a second. The channels are as
	;the Amiga's, left right right left, and separation, 0 to 256, is how far
	;apart, 256 if not said
	(defq channels (. song :find :channels) sep (ifn separation 256))
	(scatter (Emap) :song song :rate rate :channels channels
		:chans (map! (# (tracker-channel (if (find (% (!) 4) '(0 3)) (- 128 (/ sep 2)) (+ 128 (/ sep 2)))))
			'() 0 channels)
		:voices (apply (const nums) (apply (const cat) (map! (lambda () '(-1 0 0 0 0 -1 0)) '() 0 channels)))
		:samples (map (# (. %0 :find :data)) (. song :find :samples))
		:tables (tracker-tables) :speed 6 :bpm 125 :tick 0 :row 0 :order 0
		:jump :nil :jump_order 0 :jump_row 0 :delay 0 :repeat :nil :carry 0
		:visited (Fset 101) :ended :nil :acc (nums) :gain (/ 2048 (max 4 channels))))

(defun tracker-period (player chan note)
	; (tracker-period player chan note) -> period
	;the period of a note, 0 to 35, at the channel's finetune
	(elem-get (elem-get (. player :find :tables) (+ 8 (. chan :find :finetune)))
		(max 0 (min 35 note))))

(defun tracker-wave (wave pos)
	; (tracker-wave wave pos) -> num
	;a vibrato's or tremolo's wave at pos, 0 to 63, 0 to 255, its sign by
	;which half: sine, a ramp down, or square
	(defq at (logand pos 31)
		v (case (logand wave 3)
			(1 (if (< pos 32) (* at 8) (- 255 (* at 8))))
			(2 255)
			(:t (elem-get +tracker_sine at))))
	(if (< pos 32) v (neg v)))

(defun tracker-slide (chan param)
	; (tracker-slide chan param)
	;the volume up by its high half, or down by its low
	(defq up (>> param 4) down (logand param 15) v (. chan :find :volume))
	(. chan :insert :volume (if (> up 0) (min 64 (+ v up)) (max 0 (- v down)))))

(defun tracker-trigger (chan)
	; (tracker-trigger chan)
	;the note of a channel played from its start, or from an offset
	(scatter chan :sample (. chan :find :next) :trigger :t)
	(if (< (logand (. chan :find :vib_wave) 4) 4) (. chan :insert :vib_pos 0))
	(if (< (logand (. chan :find :trem_wave) 4) 4) (. chan :insert :trem_pos 0)))

(defun tracker-cell (player chan cell)
	; (tracker-cell player chan cell)
	;the first tick of a row for a channel: its note, its sample, and what
	;its effect does at once
	(bind '(period sample effect param) cell)
	(defq x (>> param 4) y (logand param 15) song (. player :find :song))
	(scatter chan :effect effect :param param :trigger :nil :start 0 :delay -1)
	;a sample alone sets its volume, with a note it is played
	(when (and (> sample 0) (<= sample (length (. song :find :samples))))
		(defq s (elem-get (. song :find :samples) (dec sample)))
		(scatter chan :next (dec sample) :volume (. s :find :volume) :finetune (. s :find :finetune)))
	(if (and (= effect 14) (= x 5)) (. chan :insert :finetune (if (> y 7) (- y 16) y)))
	(when (> period 0)
		(defq note (tracker-note period) at (tracker-period player chan note))
		(cond
			((find effect '(3 5)) (. chan :insert :target at))
			(:t (scatter chan :note note :period at)
				(if (and (= effect 14) (= x 13) (> y 0)) (. chan :insert :delay y)
					(tracker-trigger chan)))))
	(case effect
		(3 (if (> param 0) (. chan :insert :porta param)))
		(4 (if (> x 0) (. chan :insert :vib_speed x))
			(if (> y 0) (. chan :insert :vib_depth y)))
		(7 (if (> x 0) (. chan :insert :trem_speed x))
			(if (> y 0) (. chan :insert :trem_depth y)))
		(8 (. chan :insert :pan param))
		(9 (if (> param 0) (. chan :insert :offset param))
			(. chan :insert :start (* 256 (. chan :find :offset))))
		(11 (scatter player :jump :t :jump_order param :jump_row 0))
		(12 (. chan :insert :volume (min 64 param)))
		(13 (unless (. player :find :jump) (. player :insert :jump_order (inc (. player :find :order))))
			(scatter player :jump :t :jump_row (if (> (defq r (+ (* x 10) y)) 63) 0 r)))
		(14 (case x
			(1 (. chan :insert :period (max +period_lo (- (. chan :find :period) y))))
			(2 (. chan :insert :period (min +period_hi (+ (. chan :find :period) y))))
			(4 (. chan :insert :vib_wave y))
			(6 (cond
				((= y 0) (. chan :insert :loop_row (. player :find :row)))
				((= (. chan :find :loop_count) 0)
					(. chan :insert :loop_count y)
					(scatter player :jump :t :jump_order (. player :find :order) :jump_row (. chan :find :loop_row) :looping :t))
				(:t (. chan :insert :loop_count (defq left (dec (. chan :find :loop_count))))
					(if (> left 0)
						(scatter player :jump :t :jump_order (. player :find :order) :jump_row (. chan :find :loop_row) :looping :t)))))
			(7 (. chan :insert :trem_wave y))
			(8 (. chan :insert :pan (* y 17)))
			(10 (. chan :insert :volume (min 64 (+ (. chan :find :volume) y))))
			(11 (. chan :insert :volume (max 0 (- (. chan :find :volume) y))))
			(12 (if (= y 0) (. chan :insert :volume 0)))
			(14 (if (= (. player :find :delay) 0) (. player :insert :delay y)))))
		(15 (cond
			((= param 0) (. player :insert :ended :t))
			((< param 32) (. player :insert :speed param))
			((. player :insert :bpm param)))))
	(scatter chan :period_out (. chan :find :period) :volume_out (. chan :find :volume)))

(defun tracker-effect (player chan tick)
	; (tracker-effect player chan tick)
	;a tick after a row's first: what the channel's effect does over time
	(defq effect (. chan :find :effect) param (. chan :find :param)
		x (>> param 4) y (logand param 15) period (. chan :find :period))
	(. chan :insert :trigger :nil)
	(case effect
		(0 (when (> param 0)
			(setq period (tracker-period player chan (+ (tracker-note (. chan :find :period))
				(elem-get (list 0 x y) (% tick 3)))))))
		(1 (. chan :insert :period (setq period (max +period_lo (- period param)))))
		(2 (. chan :insert :period (setq period (min +period_hi (+ period param)))))
		((3 5) (defq to (. chan :find :target) by (. chan :find :porta))
			(when (> to 0)
				(. chan :insert :period (setq period (if (< period to) (min to (+ period by)) (max to (- period by))))))
			(if (= effect 5) (tracker-slide chan param)))
		((4 6) (defq pos (. chan :find :vib_pos))
			(setq period (+ period (/ (* (tracker-wave (. chan :find :vib_wave) pos) (. chan :find :vib_depth)) 128)))
			(. chan :insert :vib_pos (logand (+ pos (. chan :find :vib_speed)) 63))
			(if (= effect 6) (tracker-slide chan param)))
		(10 (tracker-slide chan param))
		(14 (case x
			(9 (if (and (> y 0) (= (% tick y) 0)) (tracker-trigger chan)))
			(12 (if (= tick y) (. chan :insert :volume 0)))
			(13 (if (= tick y) (tracker-trigger chan))))))
	(defq volume (. chan :find :volume))
	(when (= effect 7)
		(defq pos (. chan :find :trem_pos))
		(setq volume (max 0 (min 64 (+ volume (/ (* (tracker-wave (. chan :find :trem_wave) pos) (. chan :find :trem_depth)) 64)))))
		(. chan :insert :trem_pos (logand (+ pos (. chan :find :trem_speed)) 63)))
	(scatter chan :period_out period :volume_out volume))

(defun tracker-voices (player)
	; (tracker-voices player)
	;each channel's voice for the mixer, as the channel is now
	(defq voices (. player :find :voices) song (. player :find :song) rate (. player :find :rate)
		gain (. player :find :gain))
	(each (lambda (chan)
			(defq at (* (!) +mix_size) sample (. chan :find :sample))
			(when (and (. chan :find :trigger) (>= sample 0))
				;a note played: its sample from the start, or the offset, and
				;its loop
				(defq s (elem-get (. song :find :samples) sample) len (. s :find :length)
					ls (. s :find :loop_start) start (. chan :find :start))
				(cond
					((< start len))
					((>= ls 0) (setq start ls))
					((setq sample -1)))
				(elem-set voices at sample)
				(elem-set voices (+ at 1) (<< start 16))
				(elem-set voices (+ at 5) ls)
				(elem-set voices (+ at 6) (if (>= ls 0) (+ ls (. s :find :loop_length)) len)))
			(defq period (. chan :find :period_out) pan (. chan :find :pan)
				weight (* (. chan :find :volume_out) gain))
			(elem-set voices (+ at 2) (if (> period 0) (/ (<< +tracker_clock 16) (* period rate)) 0))
			(elem-set voices (+ at 3) (/ (* weight (- 256 pan)) 256))
			(elem-set voices (+ at 4) (/ (* weight pan) 256)))
		(. player :find :chans)))

(defun tracker-row (player)
	; (tracker-row player)
	;the first tick of a row: each channel's cell
	(defq song (. player :find :song) order (. player :find :order) row (. player :find :row)
		channels (. player :find :channels)
		cells (elem-get (. song :find :patterns) (elem-get (. song :find :orders) order))
		at (* row channels 4))
	(each (lambda (chan)
			(defq c (+ at (* (!) 4)))
			(tracker-cell player chan (slice cells c (+ c 4))))
		(. player :find :chans)))

(defun tracker-advance (player)
	; (tracker-advance player)
	;on to the next row, or where a jump or break says. At the song's end it
	;goes round from where it says, and a row played before, not by a loop,
	;means it has gone round: it has ended
	(defq song (. player :find :song) order (. player :find :order) row (. player :find :row)
		orders (length (. song :find :orders)) visited (. player :find :visited))
	(cond
		((. player :find :jump)
			(setq order (. player :find :jump_order) row (. player :find :jump_row))
			(when (. player :find :looping)
				;a pattern's loop plays its rows again, and they are not a sign
				;of the song going round
				(each! (# (. visited :erase (+ (* order 64) (!)))) '() row 64)))
		((>= (setq row (inc row)) 64) (setq row 0 order (inc order))))
	(when (>= order orders) (setq order (. song :find :restart)) (. player :insert :ended :t))
	(if (. visited :find (defq key (+ (* order 64) row))) (. player :insert :ended :t) (. visited :insert key))
	(scatter player :order order :row row :jump :nil :looping :nil))

(defun tracker-tick (player)
	; (tracker-tick player) -> frames
	;one tick of the song, and how many frames it lasts
	(defq tick (. player :find :tick) chans (. player :find :chans))
	(cond
		((and (= tick 0) (not (. player :find :repeat))) (tracker-row player))
		((each (# (tracker-effect player %0 tick)) chans)))
	(tracker-voices player)
	(. player :insert :tick (setq tick (inc tick)))
	(when (>= tick (. player :find :speed))
		(. player :insert :tick 0)
		(cond
			;a pattern delay plays the row again, its notes not played again
			((> (defq delay (. player :find :delay)) 0)
				(scatter player :delay (dec delay) :repeat :t))
			(:t (scatter player :repeat :nil)
				(tracker-advance player))))
	;2.5 / bpm seconds, what is over a whole frame carried to the next
	(defq rate (* 5 (. player :find :rate)) bpm (* 2 (. player :find :bpm))
		n (+ rate (. player :find :carry)))
	(. player :insert :carry (% n bpm))
	(/ n bpm))

(defun tracker-mix (player frames)
	; (tracker-mix player frames) -> str
	;so many frames of the voices as they are now
	(defq acc (. player :find :acc))
	(if (< (length acc) (* 2 frames)) (. player :insert :acc (setq acc (fill (nums) (* 2 frames) 0))))
	(media-mix (str-alloc (* 4 frames)) acc (. player :find :samples) (. player :find :voices)))

(defun tracker-next (player)
	; (tracker-next player) -> str
	;the frames of the song's next tick
	(tracker-mix player (tracker-tick player)))

(defun tracker-length (song rate)
	; (tracker-length song rate) -> frames
	;how long a song plays for, to its end, its ticks played and not mixed
	(defq player (tracker-player song rate) frames 0)
	(until (. player :find :ended) (setq frames (+ frames (tracker-tick player))))
	frames)

(defun tracker-skip (player frames)
	; (tracker-skip player frames)
	;each voice moved on so many frames, as the mixer would, nothing mixed:
	;round its loop, or ended
	(defq voices (. player :find :voices))
	(each! (lambda ()
			(defq at (* (!) +mix_size))
			(when (>= (elem-get voices at) 0)
				(defq pos (+ (elem-get voices (+ at 1)) (* (elem-get voices (+ at 2)) frames))
					ls (elem-get voices (+ at 5)) endq (<< (elem-get voices (+ at 6)) 16))
				(cond
					((< pos endq))
					((>= ls 0) (setq pos (+ (<< ls 16) (% (- pos endq) (- endq (<< ls 16))))))
					((elem-set voices at -1)))
				(elem-set voices (+ at 1) pos)))
		'() 0 (. player :find :channels)))

(defun tracker-seek (player frames)
	; (tracker-seek player frames) -> frames
	;a player from its start played on to the first tick at or past so many
	;frames, its rows as they are played and its voices moved on, nothing
	;mixed, and where that is
	(defq at 0)
	(while (and (< at frames) (not (. player :find :ended)))
		(defq n (tracker-tick player))
		(tracker-skip player n)
		(setq at (+ at n)))
	at)
