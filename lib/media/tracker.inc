;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a tracker's song played, a song of lib/media/module.inc: its
; rows a tick at a time, the effects of its tracker on each channel, and
; the channels mixed by media_mix, lib/media/lisp_tracker.vp, into frames
; of 16 bit stereo. A tick is 2.5 / bpm of a second, a row speed ticks.
;
; The effects are of the tracker that made the song, its :mode: :mod
; those of the Amiga's ProTracker, the pt- functions, :st3 those of Scream
; Tracker 3, the st3- functions, :ft2 those of FastTracker 2, the ft2-
; functions. A note's pitch is a period, the clocks between one sample and
; the next: of the Amiga's clock for :mod, of four times as fine for :st3
; and for an XM's Amiga periods, or an XM's linear period, 64 a semitone
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/media/mod.inc")

(ffi "lib/media/media_mix" media-mix)

(defq +mix_size 8 +mix_pingpong 1 +mix_back 2
	+amiga_clock 3546895 +st3_clock 14317056 +period_lo 113 +period_hi 856 +st3_period_lo 64
	;the half of a sine of vibrato and tremolo
	+tracker_sine ''(0 24 49 74 97 120 141 161 180 197 212 224 235 244 250 253
		255 253 250 244 235 224 212 197 180 161 141 120 97 74 49 24)
	;the period of so many semitones up, 16.16, for an arpeggio
	+semitones ''(65536 61858 58386 55109 52016 49097 46341 43740
		41285 38968 36781 34716 32768 30929 29193 27554)
	;Scream Tracker's periods of the notes of octave 0
	+st3_periods ''(1712 1616 1524 1440 1356 1280 1208 1140 1076 1016 960 907)
	;a retrig's change of volume, by its type: added, times, divided
	+st3_retrig ''((0 1 1) (-1 1 1) (-2 1 1) (-4 1 1) (-8 1 1) (-16 1 1) (0 2 3) (0 1 2)
		(0 1 1) (1 1 1) (2 1 1) (4 1 1) (8 1 1) (16 1 1) (0 3 2) (0 2 1) (0 0 1))
	;what a channel of Scream Tracker does this row, :fx bits
	+fx_vslide 1 +fx_fvslide 2 +fx_bend 4 +fx_fbend 8 +fx_porta 16 +fx_vib 32
	+fx_trem 64 +fx_tremor 128 +fx_retrig 256 +fx_vslide2 512 +fx_pslide 1024 +fx_gslide 2048
	+ft2_clock 14317456)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the voices of the mixer
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ft2-step (player period)
	; (ft2-step player period) -> step
	;a frame's step of a linear period, 64 a semitone: 8363 a second at
	;4608, C-4, twice that 768 less
	(defq e (- 4608 period) q (if (>= e 0) (/ e 768) (dec (/ (inc e) 768)))
		f (* 8363 (elem-get (. player :find :exp2) (- e (* q 768)))))
	(/ (if (>= q 0) (<< f q) (>> f (neg q))) (. player :find :rate)))

(defun tracker-voices (player)
	; (tracker-voices player)
	;each channel's voice for the mixer, as the channel is now
	(defq voices (. player :find :voices) song (. player :find :song) rate (. player :find :rate)
		gain (. player :find :gain) scale (. player :find :scale) clock (<< (. player :find :clock) 16)
		linear (eql (. player :find :pitch) :linear))
	(each (lambda (chan)
			(defq at (* (!) +mix_size) sample (. chan :find :sample))
			(when (. chan :find :stop)
				(scatter chan :stop :nil :trigger :nil)
				(elem-set voices at -1))
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
				(elem-set voices (+ at 6) (if (>= ls 0) (. s :find :loop_end) len))
				(elem-set voices (+ at 7) (if (and (>= ls 0) (. s :find :pingpong)) +mix_pingpong 0)))
			(. chan :insert :trigger :nil)
			(defq period (. chan :find :period_out) pan (ifn (. chan :find :pan_out) (. chan :find :pan))
				weight (/ (* (. chan :find :volume_out) gain) scale))
			(elem-set voices (+ at 2) (cond
				((<= period 0) 0)
				(linear (ft2-step player period))
				((/ clock (* period rate)))))
			(elem-set voices (+ at 3) (/ (* weight (- 256 pan)) 256))
			(elem-set voices (+ at 4) (/ (* weight pan) 256)))
		(. player :find :chans)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ProTracker, the effects of a MOD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pt-tables ()
	; (pt-tables) -> ((period ...) ...)
	;the periods of the 36 notes for each finetune, -8 to 7, by it and 8:
	;each a 96th of a semitone less
	(map! (lambda ()
			(defq tune (- (!) 8))
			(map (# (n2i (+ (* (n2d %0) (pow (const (n2d 2)) (/ (n2d (neg tune)) (const (n2d 96))))) (const (n2d 0.5)))))
				+mod_periods))
		'() 0 16))

(defun pt-note (period)
	; (pt-note period) -> num
	;the note nearest a period, 0 to 35, of finetune 0
	(defq best 0 gap +max_int)
	(each (lambda (p)
			(when (< (defq d (abs (- p period))) gap) (setq gap d best (!))))
		+mod_periods)
	best)

(defun pt-channel (pan)
	; (pt-channel pan) -> channel
	(scatter (Emap) :sample -1 :next -1 :volume 0 :period 0 :finetune 0 :note 0
		:target 0 :porta 0 :vib_speed 0 :vib_depth 0 :vib_pos 0 :vib_wave 0
		:trem_speed 0 :trem_depth 0 :trem_pos 0 :trem_wave 0 :offset 0
		:effect 0 :param 0 :loop_row 0 :loop_count 0 :pan pan :delay -1
		:period_out 0 :volume_out 0 :trigger :nil :start 0 :stop :nil))

(defun pt-period (player chan note)
	; (pt-period player chan note) -> period
	;the period of a note, 0 to 35, at the channel's finetune
	(elem-get (elem-get (. player :find :tables) (+ 8 (. chan :find :finetune)))
		(max 0 (min 35 note))))

(defun pt-wave (wave pos)
	; (pt-wave wave pos) -> num
	;a vibrato's or tremolo's wave at pos, 0 to 63, 0 to 255, its sign by
	;which half: sine, a ramp down, or square
	(defq at (logand pos 31)
		v (case (logand wave 3)
			(1 (if (< pos 32) (* at 8) (- 255 (* at 8))))
			(2 255)
			(:t (elem-get +tracker_sine at))))
	(if (< pos 32) v (neg v)))

(defun pt-slide (chan param)
	; (pt-slide chan param)
	;the volume up by its high half, or down by its low
	(defq up (>> param 4) down (logand param 15) v (. chan :find :volume))
	(. chan :insert :volume (if (> up 0) (min 64 (+ v up)) (max 0 (- v down)))))

(defun pt-trigger (chan)
	; (pt-trigger chan)
	;the note of a channel played from its start, or from an offset
	(scatter chan :sample (. chan :find :next) :trigger :t)
	(if (< (logand (. chan :find :vib_wave) 4) 4) (. chan :insert :vib_pos 0))
	(if (< (logand (. chan :find :trem_wave) 4) 4) (. chan :insert :trem_pos 0)))

(defun pt-cell (player chan cell)
	; (pt-cell player chan cell)
	;the first tick of a row for a channel: its note, its sample, and what
	;its effect does at once
	(bind '(note sample & effect param) cell)
	(defq x (>> param 4) y (logand param 15) song (. player :find :song))
	(scatter chan :effect effect :param param :trigger :nil :start 0 :delay -1)
	;a sample alone sets its volume, with a note it is played
	(when (and (> sample 0) (<= sample (length (. song :find :samples))))
		(defq s (elem-get (. song :find :samples) (dec sample)))
		(scatter chan :next (dec sample) :volume (. s :find :volume) :finetune (. s :find :finetune)))
	(and (= effect 14) (= x 5) (. chan :insert :finetune (if (> y 7) (- y 16) y)))
	(when (> note 0)
		(defq n (- note 13) at (pt-period player chan n))
		(cond
			((find effect '(3 5)) (. chan :insert :target at))
			(:t (scatter chan :note n :period at)
				(if (and (= effect 14) (= x 13) (> y 0)) (. chan :insert :delay y)
					(pt-trigger chan)))))
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

(defun pt-effect (player chan tick)
	; (pt-effect player chan tick)
	;a tick after a row's first: what the channel's effect does over time
	(defq effect (. chan :find :effect) param (. chan :find :param)
		x (>> param 4) y (logand param 15) period (. chan :find :period))
	(. chan :insert :trigger :nil)
	(case effect
		(0 (when (> param 0)
			(setq period (pt-period player chan (+ (pt-note (. chan :find :period))
				(elem-get (list 0 x y) (% tick 3)))))))
		(1 (. chan :insert :period (setq period (max +period_lo (- period param)))))
		(2 (. chan :insert :period (setq period (min +period_hi (+ period param)))))
		((3 5) (defq to (. chan :find :target) by (. chan :find :porta))
			(when (> to 0)
				(. chan :insert :period (setq period (if (< period to) (min to (+ period by)) (max to (- period by))))))
			(if (= effect 5) (pt-slide chan param)))
		((4 6) (defq pos (. chan :find :vib_pos))
			(setq period (+ period (/ (* (pt-wave (. chan :find :vib_wave) pos) (. chan :find :vib_depth)) 128)))
			(. chan :insert :vib_pos (logand (+ pos (. chan :find :vib_speed)) 63))
			(if (= effect 6) (pt-slide chan param)))
		(10 (pt-slide chan param))
		(14 (case x
			(9 (and (> y 0) (= (% tick y) 0) (pt-trigger chan)))
			(12 (if (= tick y) (. chan :insert :volume 0)))
			(13 (if (= tick y) (pt-trigger chan))))))
	(defq volume (. chan :find :volume))
	(when (= effect 7)
		(defq pos (. chan :find :trem_pos))
		(setq volume (max 0 (min 64 (+ volume (/ (* (pt-wave (. chan :find :trem_wave) pos) (. chan :find :trem_depth)) 64)))))
		(. chan :insert :trem_pos (logand (+ pos (. chan :find :trem_speed)) 63)))
	(scatter chan :period_out period :volume_out volume))

(defun pt-row (player)
	; (pt-row player)
	;the first tick of a row: each channel's cell
	(defq song (. player :find :song) channels (. player :find :channels)
		cells (elem-get (. song :find :patterns) (elem-get (. song :find :orders) (. player :find :order)))
		at (* (. player :find :row) channels +cell_size))
	(each (lambda (chan)
			(defq c (+ at (* (!) +cell_size)))
			(pt-cell player chan (slice cells c (+ c +cell_size))))
		(. player :find :chans)))

(defun pt-advance (player)
	; (pt-advance player)
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
				(each! (# (. visited :erase (+ (* order 256) (!)))) '() row 64)))
		((>= (setq row (inc row)) 64) (setq row 0 order (inc order))))
	(when (>= order orders) (setq order (. song :find :restart)) (. player :insert :ended :t))
	(if (. visited :find (defq key (+ (* order 256) row))) (. player :insert :ended :t) (. visited :insert key))
	(scatter player :order order :row row :jump :nil :looping :nil))

(defun pt-tick (player)
	; (pt-tick player)
	;one tick of a MOD
	(defq tick (. player :find :tick) chans (. player :find :chans))
	(cond
		((and (= tick 0) (not (. player :find :repeat))) (pt-row player))
		((each (# (pt-effect player %0 tick)) chans)))
	(tracker-voices player)
	(. player :insert :tick (setq tick (inc tick)))
	(when (>= tick (. player :find :speed))
		(. player :insert :tick 0)
		(cond
			;a pattern delay plays the row again, its notes not played again
			((> (defq delay (. player :find :delay)) 0)
				(scatter player :delay (dec delay) :repeat :t))
			(:t (scatter player :repeat :nil)
				(pt-advance player)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Scream Tracker 3, the effects of an S3M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun st3-channel (pan)
	; (st3-channel pan) -> channel
	(scatter (Emap) :ins -1 :sample -1 :note 0 :volume 64 :pan pan :period 0 :c2spd 0
		:fx 0 :vslide 0 :fvslide 0 :bend 0 :fbend 0 :target 0 :porta 0 :pdir 0
		:mem 0 :porta_mem 0 :vib_mem 0 :vib_pos 0 :vib_rate 0 :vib_depth 0 :vib_wave 0
		:trem_pos 0 :trem_rate 0 :trem_depth 0 :trem_wave 0 :arp (list 0) :arp_at 0
		:tremor_up 0 :tremor_down 0 :tremor_count 0 :retrig_val 0 :retrig_count 0
		:retrig_type 0 :offset 0 :delay 0 :delayed :nil :loop_row 0 :loop_count 0 :fade 1
		:period_out 0 :volume_out 0 :trigger :nil :start 0 :stop :nil))

(defun st3-period (note c2spd)
	; (st3-period note c2spd) -> period
	;the period of a note, 1 C-0 and on, of a sample of so many frames a
	;second at C-4, as Scream Tracker has it
	(defq n (dec note))
	(if (> c2spd 0) (/ (* 8363 16 (>> (elem-get +st3_periods (% n 12)) (/ n 12))) c2spd) 0))

(defun st3-wave (wave pos)
	; (st3-wave wave pos) -> num
	;a vibrato's or tremolo's wave at pos, 0 to 63, -255 to 255: sine, a
	;ramp down, a square of 255 and 0, or random
	(case (logand wave 3)
		(1 (- 255 (* pos 8)))
		(2 (if (< pos 32) 255 0))
		(3 (- (random 512) 256))
		(:t (if (< pos 32) (elem-get +tracker_sine pos) (neg (elem-get +tracker_sine (- pos 32)))))))

(defun st3-mem (chan param)
	; (st3-mem chan param) -> param
	;Scream Tracker's one memory of an effect's number, for most of them:
	;0 is the last that was not
	(if (= param 0) (. chan :find :mem) (. chan :insert :mem param) param))

(defun chan-fx (chan bit)
	; (chan-fx chan bit)
	(. chan :insert :fx (logior (. chan :find :fx) bit)))

(defun tracker-vslide (chan param mem down)
	; (tracker-vslide chan param mem down)
	;a volume slide: x0 up, 0y down, each tick but the first, down first if
	;both and down; xF up and Fy down at once; 00 the last, of mem
	(if (= param 0) (setq param (. chan :find mem)))
	(defq x (>> param 4) y (logand param 15))
	(cond
		((and (= y 15) (/= x 0))
			(scatter chan mem param :fvslide x) (chan-fx chan +fx_fvslide))
		((and (= x 15) (/= y 0))
			(scatter chan mem param :fvslide (neg y)) (chan-fx chan +fx_fvslide))
		(:t (chan-fx chan +fx_vslide)
			(when (/= param 0)
				(scatter chan mem param :vslide (cond
					((and down (/= y 0)) (neg y)) ((/= x 0) x) ((neg y)))))
			;D0F and DF0, as each tick and the first
			(defq last_param (. chan :find mem))
			(when (or (= (>> last_param 4) 15) (= (logand last_param 15) 15))
				(. chan :insert :fvslide (. chan :find :vslide))
				(chan-fx chan +fx_fvslide)))))

(defun st3-toneporta (player chan note)
	; (st3-toneporta player chan note)
	;a note, if there is one, to slide to, and which way
	(when (and (> note 0) (<= note 120))
		(defq s (elem-get (. (. player :find :song) :find :samples) (. chan :find :ins)))
		(. chan :insert :target (st3-period note (. s :find :c2spd))))
	(. chan :insert :pdir (if (< (. chan :find :period) (. chan :find :target)) 1 -1))
	(chan-fx chan +fx_porta))

(defun st3-effect (player chan effect param note row)
	; (st3-effect player chan effect param note row)
	;what an effect of a row does at once, and sets going for its ticks
	(case effect
		(1 (if (> (setq param (st3-mem chan param)) 0) (. player :insert :speed param)))
		(2 (scatter player :pbreak :t :jump_to param :jumpline 0))
		(3 (scatter player :pbreak :t :jumpline (+ (* 10 (>> param 4)) (logand param 15))))
		(4 (tracker-vslide chan param :mem :t))
		((5 6)
			;E down, F up, xx each tick, Fx fine and Ex extra fine at once
			(defq param (st3-mem chan param) x (>> param 4) y (logand param 15)
				way (if (= effect 5) 1 -1))
			(cond
				((= x 15) (when (> y 0) (. chan :insert :fbend (* way 4 y)) (chan-fx chan +fx_fbend)))
				((= x 14) (when (> y 0) (. chan :insert :fbend (* way y)) (chan-fx chan +fx_fbend)))
				(:t (. chan :insert :bend (* way 4 param)) (chan-fx chan +fx_bend))))
		(7 (if (= param 0) (setq param (. chan :find :porta_mem))
				(. chan :insert :porta_mem param))
			(when (> param 0) (scatter chan :mem param :porta (* 4 param)))
			(if (>= (. chan :find :ins) 0) (st3-toneporta player chan note)))
		((8 21)
			;H vibrato, U fine
			(if (= param 0) (setq param (. chan :find :vib_mem))
				(. chan :insert :vib_mem param))
			(if (> param 0) (. chan :insert :mem param))
			(defq x (>> param 4) y (logand param 15))
			(if (> y 0) (. chan :insert :vib_depth (if (= effect 8) (* y 4) y)))
			(if (> x 0) (. chan :insert :vib_rate x))
			(chan-fx chan +fx_vib))
		(9 (setq param (st3-mem chan param))
			(scatter chan :tremor_up (max 1 (>> param 4)) :tremor_down (max 1 (logand param 15)))
			(chan-fx chan +fx_tremor))
		(10 (if (> (setq param (st3-mem chan param)) 0)
				(. chan :insert :arp (list 0 (>> param 4) (logand param 15)))))
		(11 (chan-fx chan +fx_vib) (tracker-vslide chan param :mem :t))
		(12 (if (>= (. chan :find :ins) 0) (st3-toneporta player chan note))
			(tracker-vslide chan param :mem :t))
		(15 (setq param (st3-mem chan param))
			(if (> note 0) (. chan :insert :offset (<< param 8))))
		(17 (when (> (setq param (st3-mem chan param)) 0)
				(scatter chan :retrig_val param :retrig_type (>> param 4)))
			(if (> note 0) (. chan :insert :retrig_count (inc (logand (. chan :find :retrig_val) 15))))
			(chan-fx chan +fx_retrig))
		(18 (setq param (st3-mem chan param))
			(if (> (logand param 15) 0) (. chan :insert :trem_depth (logand param 15)))
			(if (> (>> param 4) 0) (. chan :insert :trem_rate (>> param 4)))
			(chan-fx chan +fx_trem))
		(19 (setq param (st3-mem chan param))
			(defq x (>> param 4) y (logand param 15))
			(case x
				(3 (. chan :insert :vib_wave (logand y 3)))
				(4 (. chan :insert :trem_wave (logand y 3)))
				(8 (. chan :insert :pan (* y 16)))
				(11 (cond
					((= y 0) (. chan :insert :loop_row row))
					((> (defq count (. chan :find :loop_count)) 0)
						(. chan :insert :loop_count (setq count (dec count)))
						(if (> count 0)
							(scatter player :loop_to (. chan :find :loop_row) :looping :t)
							;after its last time round a loop starts after it
							(. chan :insert :loop_row (inc row))))
					(:t (. chan :insert :loop_count y)
						(scatter player :loop_to (. chan :find :loop_row) :looping :t))))
				(12 (when (> y 0)
						(scatter chan :retrig_val (inc y) :retrig_count (inc y) :retrig_type 16)
						(chan-fx chan +fx_retrig)))
				(14 (if (= (. player :find :pdelay) 0) (. player :insert :pdelay y)))))
		(20 (if (>= param 32) (. player :insert :bpm param)))
		(22 (. player :insert :gvol (min 64 param)))
		(24 (. chan :insert :pan (if (= param 0xa4) 128 (min 256 (* param 2)))))))

(defun st3-event (player chan cell row)
	; (st3-event player chan cell row)
	;a channel's cell read: its note, its sample, its volume and effect
	(bind '(note ins vol effect param) cell)
	(defq song (. player :find :song) samples (. song :find :samples)
		porta (find effect '(7 12)) played :nil use_volume :nil other :nil)
	(scatter chan :fx 0 :arp (list 0) :arp_at 0 :tremor_up 0 :tremor_down 0)
	;a sample, its volume used unless the cell gives one, but with a slide
	;to a note only the volume of a new one
	(when (> ins 0)
		(setq use_volume :t)
		(scatter chan :fade 1 :offset 0)
		(cond
			((> ins (length samples)) (setq use_volume :nil))
			((/= (dec ins) (. chan :find :ins))
				(setq other :t)
				(cond
					(porta (. chan :insert :volume (. (elem-get samples (dec ins)) :find :volume))
						(setq use_volume :nil))
					((. chan :insert :ins (dec ins)))))))
	;a note played, cut, or slid to
	(when (> note 0)
		(cond
			((= note +note_cut) (. chan :insert :fade 0) (setq use_volume :nil))
			(porta (if other (. chan :insert :offset 0)))
			((and (<= note 120) (>= (defq at (. chan :find :ins)) 0))
				(defq s (elem-get samples at))
				(when (> (. s :find :length) 0)
					(scatter chan :sample at :c2spd (. s :find :c2spd))
					(setq played note)))))
	(when played (scatter chan :vib_pos 0 :trem_pos 0))
	(defq new_volume (/= vol +no_volume))
	(if new_volume (. chan :insert :volume (min 64 vol)))
	(st3-effect player chan effect param note row)
	(when played
		(defq period (st3-period played (. chan :find :c2spd)))
		(. chan :insert :target period)
		(if (or (< (. chan :find :period) 1) (not porta)) (. chan :insert :period period))
		(scatter chan :note played :trigger :t :start (. chan :find :offset)))
	(and use_volume (not new_volume) (>= (. chan :find :ins) 0)
		(. chan :insert :volume (. (elem-get samples (. chan :find :ins)) :find :volume)))
	;Scream Tracker sets a volume of the cell at so much of the global
	(if new_volume
		(. chan :insert :volume (/ (* (. chan :find :volume) (. player :find :gvol)) 64))))

(defun st3-row (player)
	; (st3-row player)
	;the first tick of a row: each channel's cell, or held back by a note
	;delay to a later tick of it
	(defq song (. player :find :song) channels (. player :find :channels) row (. player :find :row)
		cells (elem-get (. song :find :patterns) (elem-get (. song :find :orders) (. player :find :order)))
		at (* row channels +cell_size))
	(each (lambda (chan)
			(defq c (+ at (* (!) +cell_size)) cell (slice cells c (+ c +cell_size)))
			(bind '(& & & effect param) cell)
			;a speed is had first, a delay is of it
			(and (= effect 1) (> param 0) (. player :insert :speed param))
			(if (and (= effect 19) (= (>> param 4) 13) (> (logand param 15) 0))
				(scatter chan :delay (inc (logand param 15)) :delayed cell)
				(st3-event player chan cell row)))
		(. player :find :chans)))

(defun st3-play (player chan tick0)
	; (st3-play player chan tick0)
	;a tick of a channel: its volume and pitch as its effects move them
	(defq fx (. chan :find :fx))
	(when (> (. chan :find :delay) 0)
		(. chan :insert :delay (dec (. chan :find :delay)))
		(when (= (. chan :find :delay) 0)
			(st3-event player chan (. chan :find :delayed) (. player :find :row))
			(setq fx (. chan :find :fx))))
	(when (>= (. chan :find :ins) 0)
		(defq volume (. chan :find :volume) period (. chan :find :period))
		;a retrig, or a cut, every so many ticks
		(when (bits? fx +fx_retrig)
			(. chan :insert :retrig_count (defq count (dec (. chan :find :retrig_count))))
			(when (<= count 0)
				(if (< (. chan :find :retrig_type) 16) (scatter chan :trigger :t :start 0))
				(bind '(add mult by) (elem-get +st3_retrig (. chan :find :retrig_type)))
				(setq volume (/ (* (+ volume add) mult) by))
				(. chan :insert :retrig_count (logand (. chan :find :retrig_val) 15))))
		;slides of the volume, each tick but the first, or fine at the first
		(and (bits? fx +fx_vslide) (or (not tick0) (. (. player :find :song) :find :fast))
			(setq volume (+ volume (. chan :find :vslide))))
		(and tick0 (bits? fx +fx_fvslide) (setq volume (+ volume (. chan :find :fvslide))))
		(. chan :insert :volume (setq volume (max 0 (min 64 volume))))
		;slides of the pitch
		(cond
			(tick0 (if (bits? fx +fx_fbend) (setq period (+ period (. chan :find :fbend)))))
			(:t (if (bits? fx +fx_bend) (setq period (+ period (. chan :find :bend))))
				(when (and (bits? fx +fx_porta) (> (defq to (. chan :find :target)) 0))
					(defq by (. chan :find :porta))
					(when (if (> (. chan :find :pdir) 0) (>= (setq period (+ period by)) to)
							(<= (setq period (- period by)) to))
						(setq period to)
						(scatter chan :pdir 0 :fx (setq fx (logand fx (lognot +fx_porta))))))))
		(. chan :insert :period period)
		;the volume heard: a tremolo, the global volume, and a tremor
		(setq volume (* volume (. chan :find :fade)))
		(when (bits? fx +fx_trem)
			(defq pos (. chan :find :trem_pos) rate (. chan :find :trem_rate))
			(if (> rate 0)
				(setq volume (+ volume (/ (* (st3-wave (. chan :find :trem_wave) pos) (. chan :find :trem_depth)) 64))))
			(unless tick0 (. chan :insert :trem_pos (logand (+ pos rate) 63))))
		(setq volume (* (max 0 (min 64 volume)) (. player :find :gvol)))
		(when (bits? fx +fx_tremor)
			(defq count (. chan :find :tremor_count))
			(cond
				((= count 0) (setq count (logior (. chan :find :tremor_up) 128)))
				((= count 128) (setq count (. chan :find :tremor_down))))
			(. chan :insert :tremor_count (setq count (dec count)))
			(if (= (logand count 128) 0) (setq volume 0)))
		;the pitch heard: a vibrato, and an arpeggio
		(when (bits? fx +fx_vib)
			(defq pos (. chan :find :vib_pos) rate (. chan :find :vib_rate))
			(if (> rate 0)
				(setq period (+ period (/ (* (st3-wave (. chan :find :vib_wave) pos) (. chan :find :vib_depth)) 128))))
			(unless tick0 (. chan :insert :vib_pos (logand (+ pos rate) 63))))
		(defq arp (. chan :find :arp) at (. chan :find :arp_at) up (elem-get arp at))
		(. chan :insert :arp_at (% (inc at) (length arp)))
		(if (> up 0) (setq period (>> (* period (elem-get +semitones up)) 16)))
		(cond
			((< (. chan :find :period) 1) (scatter chan :stop :t :period_out 0))
			((. chan :insert :period_out (max +st3_period_lo period))))
		(. chan :insert :volume_out volume)))

(defun flow-advance (player)
	; (flow-advance player)
	;on to the next row, or where a jump or break says, past the orders
	;that mark, 254, to the song's end, 255. A row played before, not by a
	;loop, means it has gone round: it has ended
	(defq song (. player :find :song) orders (. song :find :orders)
		order (. player :find :order) row (. player :find :row) next :nil)
	(cond
		((. player :find :pbreak)
			(if (>= (defq to (. player :find :jump_to)) 0) (setq order (dec to)))
			(setq next :t))
		((>= (defq loop (. player :find :loop_to)) 0)
			(setq row loop)
			(defq visited (. player :find :visited))
			(each! (# (. visited :erase (+ (* order 256) (!)))) '() row 256))
		((>= (setq row (inc row)) (elem-get (. song :find :rows) (elem-get orders order)))
			(setq next :t)))
	(when next
		(setq row (. player :find :jumpline))
		(while (progn
				(setq order (inc order))
				(when (or (>= order (length orders)) (= (elem-get orders order) 255))
					(. player :insert :ended :t)
					(setq order (if (< (. song :find :restart) (length orders)) (. song :find :restart) 0)))
				(>= (elem-get orders order) (length (. song :find :patterns)))))
		(if (>= row (elem-get (. song :find :rows) (elem-get orders order))) (setq row 0)))
	(defq visited (. player :find :visited))
	(if (. visited :find (defq key (+ (* order 256) row))) (. player :insert :ended :t) (. visited :insert key))
	(scatter player :order order :row row :pbreak :nil :jump_to -1 :jumpline 0
		:loop_to -1 :frame 0 :pdelay 0))

(defun flow-start (player)
	; (flow-start player)
	;the first order of the song that is a pattern
	(defq song (. player :find :song) orders (. song :find :orders) order 0)
	(while (and (< order (length orders)) (>= (elem-get orders order) (length (. song :find :patterns)))
			(/= (elem-get orders order) 255))
		(setq order (inc order)))
	(if (or (>= order (length orders)) (>= (elem-get orders order) (length (. song :find :patterns))))
		(. player :insert :ended :t)
		(. (. player :find :visited) :insert (* order 256)))
	(. player :insert :order order))

(defun st3-tick (player)
	; (st3-tick player)
	;one tick of an S3M: a row read at its first, every channel played,
	;the row long so many ticks as the speed says, times a pattern delay
	(defq frame (. player :find :frame) speed (. player :find :speed))
	(if (= frame 0) (st3-row player))
	(defq tick0 (= (% frame (. player :find :speed)) 0))
	(each (# (st3-play player %0 tick0)) (. player :find :chans))
	(tracker-voices player)
	(. player :insert :frame (setq frame (inc frame)))
	(if (>= frame (* (. player :find :speed) (inc (. player :find :pdelay))))
		(flow-advance player)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FastTracker 2, the effects of an XM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ft2-channel (pan)
	; (ft2-channel pan) -> channel
	(scatter (Emap) :ins -1 :sample -1 :key 0 :note 0 :finetune 0 :volume 64 :pan pan :period 0
		:fx 0 :vslide 0 :fvslide 0 :vslide2 0 :bend 0 :fbend 0 :target 0 :porta 0 :pdir 0
		:pslide 0 :gslide 0 :gfslide 0 :new_vol :nil
		:vol_mem 0 :freq_mem 0 :porta_mem 0 :vib_mem 0 :trem_mem 0 :offset_mem 0 :gvol_mem 0
		:pan_mem 0 :tremor_mem 0 :fine_up_mem 0 :fine_down_mem 0 :fvol_up_mem 0 :fvol_down_mem 0
		:vib_pos 0 :vib_rate 0 :vib_depth 0 :vib_wave 0 :trem_pos 0 :trem_rate 0 :trem_depth 0
		:trem_wave 0 :ivib_pos 0 :ivib_rate 0 :ivib_depth 0 :ivib_wave 0 :ivib_sweep 0
		:arp (list 0) :arp_at 0 :tremor_up 0 :tremor_down 0 :tremor_count 0
		:retrig_val 0 :retrig_count 0 :retrig_type 0 :offset 0 :delay 0 :delayed :nil
		:loop_row 0 :loop_count 0 :keyoff 0 :venv -1 :penv -1 :release :nil :fading :nil
		:fadeout 65536 :fade 0 :period_out 0 :volume_out 0 :pan_out pan
		:trigger :nil :start 0 :stop :nil))

(defun ft2-period (player note finetune)
	; (ft2-period player note finetune) -> period
	;the period of a note, 0 C-0 and on, at a finetune, -128 to 127: linear,
	;64 a semitone, or the Amiga's, 1712 at C-4
	(if (eql (. player :find :pitch) :linear)
		(- 7680 (* note 64) (/ finetune 2))
		(n2i (* (const (n2d 27392)) (pow (const (n2d 2))
			(/ (n2d (neg (+ (* note 128) finetune))) (const (n2d 1536))))))))

(defun ft2-wave (wave pos vibrato)
	; (ft2-wave wave pos vibrato) -> num
	;a wave of FastTracker's at pos, 0 to 63, -255 to 255: sine, a ramp, up
	;for a vibrato and down for a tremolo, square, or random
	(case (logand wave 3)
		(1 (if vibrato (- (* (% (+ pos 32) 64) 8) 255) (- 255 (* pos 8))))
		(2 (if (< pos 32) 255 -255))
		(3 (- (random 512) 256))
		(:t (if (< pos 32) (elem-get +tracker_sine pos) (neg (elem-get +tracker_sine (- pos 32)))))))

(defun ft2-sample (player chan key)
	; (ft2-sample player chan key) -> -1 | sample
	;which sample of the channel's instrument plays a note, 1 to 96, its
	;first if no note
	(cond
		((< (defq ins (. chan :find :ins)) 0) -1)
		((<= 1 key 96)
			(ifn (elem-get (. (elem-get (. (. player :find :song) :find :instruments) ins) :find :map) (dec key)) -1))
		((. (elem-get (. (. player :find :song) :find :instruments) ins) :find :first))))

(defun ft2-env-step (env x release)
	; (ft2-env-step env x release) -> x
	;an envelope's tick on one, held at its sustain point till let go, and
	;round its loop
	(setq x (inc x))
	(when (. env :find :on)
		(defq points (. env :find :points) n (length points) sus (. env :find :sustain)
			loop (. env :find :loop) has_sus (< -1 sus n)
			has_loop (and loop (< (elem-get loop 0) n) (< (elem-get loop 1) n)))
		(and has_loop has_sus (= sus (elem-get loop 1)) (not release) (setq has_sus :nil))
		(cond
			((and has_loop (> x (inc (elem-get (elem-get points (elem-get loop 1)) 0)))) (setq release :t))
			((and has_sus (> x (inc (elem-get (elem-get points sus) 0)))) (setq release :t)))
		(and has_sus (not release) (>= x (defq at (elem-get (elem-get points sus) 0))) (setq x at))
		(and has_loop (>= x (elem-get (elem-get points (elem-get loop 1)) 0))
			(not (and release has_sus (= sus (elem-get loop 1))))
			(setq x (elem-get (elem-get points (elem-get loop 0)) 0))))
	x)

(defun ft2-env-value (env x default)
	; (ft2-env-value env x default) -> num
	;an envelope's value at its tick, between its points
	(cond
		((or (< x 0) (not (. env :find :on))) default)
		(:t (defq points (. env :find :points) i (dec (length points)))
			(bind '(lx ly) (elem-get points i))
			(cond
				((or (>= x lx) (= i 0)) ly)
				(:t (setq i (dec i))
					(while (and (> i 0) (> (elem-get (elem-get points i) 0) x)) (setq i (dec i)))
					(bind '(x1 y1) (elem-get points i))
					(bind '(x2 y2) (elem-get points (inc i)))
					(cond
						((or (< x x1) (< x2 x1)) y1)
						((= x2 x1) y2)
						((+ y1 (/ (* (- y2 y1) (- x x1)) (- x2 x1))))))))))

(defun ft2-toneporta (player chan note)
	; (ft2-toneporta player chan note)
	;a note, if there is one, to slide to, and which way
	(when (<= 1 note 96)
		(when (>= (defq s (ft2-sample player chan (. chan :find :key))) 0)
			(. chan :insert :target (ft2-period player
				(+ (dec note) (. (elem-get (. (. player :find :song) :find :samples) s) :find :relative))
				(. chan :find :finetune)))))
	(. chan :insert :pdir (if (< (. chan :find :period) (. chan :find :target)) 1 -1))
	(chan-fx chan +fx_porta))

(defun ft2-mem (chan key param)
	; (ft2-mem chan key param) -> param
	;an effect's own memory of its number: 0 the last that was not
	(if (= param 0) (. chan :find key) (. chan :insert key param) param))

(defun ft2-effect (player chan effect param note row)
	; (ft2-effect player chan effect param note row)
	;what an effect of a row does at once, and sets going for its ticks
	(defq x (>> param 4) y (logand param 15))
	(case effect
		(0 (if (> param 0) (. chan :insert :arp (list 0 x y))))
		((1 2)
			;1 up, 2 down, xx each tick, Fx fine and Ex extra fine at once
			(setq param (ft2-mem chan :freq_mem param) x (>> param 4) y (logand param 15))
			(defq way (if (= effect 1) -1 1))
			(cond
				((= x 15) (when (> y 0) (. chan :insert :fbend (* way 4 y)) (chan-fx chan +fx_fbend)))
				((= x 14) (when (> y 0) (. chan :insert :fbend (* way y)) (chan-fx chan +fx_fbend)))
				(:t (. chan :insert :bend (* way 4 param)) (chan-fx chan +fx_bend))))
		(3 (if (> (setq param (ft2-mem chan :porta_mem param)) 0) (. chan :insert :porta (* 4 param)))
			(if (>= (. chan :find :ins) 0) (ft2-toneporta player chan note)))
		(4 (setq param (ft2-mem chan :vib_mem param) x (>> param 4) y (logand param 15))
			(if (> y 0) (. chan :insert :vib_depth (* y 4)))
			(if (> x 0) (. chan :insert :vib_rate x))
			(chan-fx chan +fx_vib))
		(5 (if (>= (. chan :find :ins) 0) (ft2-toneporta player chan note))
			(tracker-vslide chan param :vol_mem :nil))
		(6 (chan-fx chan +fx_vib) (tracker-vslide chan param :vol_mem :nil))
		(7 (setq param (ft2-mem chan :trem_mem param))
			(if (> (logand param 15) 0) (. chan :insert :trem_depth (logand param 15)))
			(if (> (>> param 4) 0) (. chan :insert :trem_rate (>> param 4)))
			(chan-fx chan +fx_trem))
		(8 (. chan :insert :pan param))
		(9 (setq param (ft2-mem chan :offset_mem param))
			(if (> note 0) (. chan :insert :offset (<< param 8))))
		(10 (tracker-vslide chan param :vol_mem :nil))
		(11 (scatter player :pbreak :t :jump_to param :jumpline 0))
		(12 (scatter chan :volume (min 64 param) :new_vol :t))
		(13 (scatter player :pbreak :t :jumpline (+ (* 10 x) y)))
		(14 (case x
			(1 (when (> (setq y (ft2-mem chan :fine_up_mem y)) 0)
					(. chan :insert :fbend (* -4 y)) (chan-fx chan +fx_fbend)))
			(2 (when (> (setq y (ft2-mem chan :fine_down_mem y)) 0)
					(. chan :insert :fbend (* 4 y)) (chan-fx chan +fx_fbend)))
			(4 (. chan :insert :vib_wave (logand y 3)))
			(5 (when (> note 0)
					(defq tune (<< (logand (- y 8) 15) 4))
					(. chan :insert :finetune (if (> tune 127) (- tune 256) tune))))
			(6 (cond
				((= y 0)
					(. chan :insert :loop_row row)
					(if (. (. player :find :song) :find :ft2bugs) (. player :insert :jumpline row)))
				((> (defq count (. chan :find :loop_count)) 0)
					(. chan :insert :loop_count (setq count (dec count)))
					(if (> count 0) (scatter player :loop_to (. chan :find :loop_row) :looping :t)))
				(:t (. chan :insert :loop_count y)
					(scatter player :loop_to (. chan :find :loop_row) :looping :t))))
			(7 (. chan :insert :trem_wave (logand y 3)))
			(8 (. chan :insert :pan (<< y 4)))
			(9 (scatter chan :retrig_val y :retrig_count (inc y) :retrig_type 0)
				(chan-fx chan +fx_retrig))
			(10 (setq y (ft2-mem chan :fvol_up_mem y))
				(. chan :insert :fvslide y) (chan-fx chan +fx_fvslide))
			(11 (setq y (ft2-mem chan :fvol_down_mem y))
				(. chan :insert :fvslide (neg y)) (chan-fx chan +fx_fvslide))
			(12 (scatter chan :retrig_val (inc y) :retrig_count (inc y) :retrig_type 16)
				(chan-fx chan +fx_retrig))
			(14 (. player :insert :pdelay y))))
		(15 (cond
			((= param 0))
			((< param 32) (. player :insert :speed param))
			((. player :insert :bpm param))))
		(16 (. player :insert :gvol (min 64 param)))
		(17 (setq param (ft2-mem chan :gvol_mem param) x (>> param 4) y (logand param 15))
			(cond
				((and (= y 15) (/= x 0)) (scatter chan :gslide 0 :gfslide x))
				((and (= x 15) (/= y 0)) (scatter chan :gslide 0 :gfslide (neg y)))
				(:t (scatter chan :gslide (if (/= x 0) x (neg y)) :gfslide 0)))
			(chan-fx chan +fx_gslide))
		(20 (. chan :insert :keyoff (inc param)))
		(21 (scatter chan :venv param :penv param))
		(25 (setq param (ft2-mem chan :pan_mem param))
			(. chan :insert :pslide (- (logand param 15) (>> param 4)))
			(chan-fx chan +fx_pslide))
		(27 (if (> param 0) (scatter chan :retrig_val param :retrig_type x))
			(if (> note 0) (. chan :insert :retrig_count (inc (logand (. chan :find :retrig_val) 15))))
			(chan-fx chan +fx_retrig))
		(29 (setq param (ft2-mem chan :tremor_mem param))
			(scatter chan :tremor_up (>> param 4) :tremor_down (logand param 15)
				:tremor_count (logior (. chan :find :tremor_count) 128))
			(chan-fx chan +fx_tremor))
		(33 (case x
			(1 (. chan :insert :fbend (neg y)) (chan-fx chan +fx_fbend))
			(2 (. chan :insert :fbend y) (chan-fx chan +fx_fbend))))))

(defun ft2-volcol (player chan vol note row)
	; (ft2-volcol player chan vol note row)
	;what the volume column does, other than set the volume
	(defq x (logand vol 15))
	(case (>> vol 4)
		(6 (. chan :insert :vslide2 (neg x)) (chan-fx chan +fx_vslide2))
		(7 (. chan :insert :vslide2 x) (chan-fx chan +fx_vslide2))
		(8 (ft2-effect player chan 14 (+ 0xb0 x) note row))
		(9 (ft2-effect player chan 14 (+ 0xa0 x) note row))
		(10 (ft2-effect player chan 4 (<< x 4) note row))
		(11 (ft2-effect player chan 4 x note row))
		(12 (. chan :insert :pan (<< x 4)))
		(13 (. chan :insert :pslide (neg x)) (chan-fx chan +fx_pslide))
		(14 (. chan :insert :pslide x) (chan-fx chan +fx_pslide))
		(15 (ft2-effect player chan 3 (<< x 4) note row))))

(defun ft2-event (player chan cell row)
	; (ft2-event player chan cell row)
	;a channel's cell read: its note, its instrument, its volume column and
	;effect, as FastTracker 2 has them
	(bind '(note ins vol effect param) cell)
	(defq song (. player :find :song) samples (. song :find :samples)
		instruments (. song :find :instruments) key note set_vol (<= 0x10 vol 0x50)
		porta (or (find effect '(3 5)) (= (>> vol 4) 15))
		k00 (and (= effect 20) (= param 0)) played :nil use_volume :nil
		fade_key (and (= note +note_off) (> ins 0) (not (and (= effect 14) (= (>> param 4) 13)))))
	(if set_vol (. chan :insert :tremor_count (logand (. chan :find :tremor_count) 0x7f)))
	(scatter chan :fx 0 :new_vol :nil)
	(if k00 (setq key 0))
	(if (> ins (length instruments)) (setq ins 0))
	;an instrument with no note, or a key off, its sample's volume and pan
	(and (> ins 0) (or (= key 0) (>= key +note_cut))
		(>= (defq s (ft2-sample player chan (. chan :find :key))) 0)
		(scatter chan :volume (. (elem-get samples s) :find :volume) :pan (. (elem-get samples s) :find :pan)
			:new_vol :t :fade (. (elem-get instruments (. chan :find :ins)) :find :fade)))
	(when (and (> ins 0) (not fade_key))
		(setq use_volume :t)
		(scatter chan :release :nil :fadeout 65536 :tremor_count 0x20)
		(unless k00 (. chan :insert :fading :nil))
		(unless porta (. chan :insert :ins (dec ins))))
	;a note with an instrument, the volume and pan of its sample
	(when (and (> ins 0) (<= 1 key 96))
		(if (>= (defq s (ft2-sample player chan key)) 0)
			(scatter chan :volume (. (elem-get samples s) :find :volume) :pan (. (elem-get samples s) :find :pan)
				:fade (. (elem-get instruments (. chan :find :ins)) :find :fade))
			(. chan :insert :volume 0))
		(. chan :insert :new_vol :t))
	;a key let go, faded, or a note slid to
	(cond
		(fade_key (. chan :insert :fading :t))
		((= key +note_off)
			(if (or (and (>= (. chan :find :ins) 0)
						(. (. (elem-get instruments (. chan :find :ins)) :find :volume) :find :on))
					(not (or set_vol (= effect 12))))
				(progn (. chan :insert :release :t) (setq use_volume :nil))
				(. chan :insert :fading :t)))
		(porta (setq key 0)))
	;a note out of its sample's range not played
	(and (<= 1 key 96) (>= (defq s (ft2-sample player chan key)) 0)
		(not (<= 0 (+ (dec key) (. (elem-get samples s) :find :relative)) 118))
		(setq key 0))
	(when (<= 1 key 96)
		(scatter chan :key key :fadeout 65536)
		(unless (and (>= (. chan :find :ins) 0)
				(. (. (elem-get instruments (. chan :find :ins)) :find :volume) :find :on))
			(scatter chan :release :nil :fading :nil))
		(cond
			((>= (defq s (ft2-sample player chan key)) 0)
				(defq smp (elem-get samples s))
				(setq played (+ (dec key) (. smp :find :relative)))
				(scatter chan :sample s :finetune (. smp :find :finetune)))
			(:t (setq use_volume :nil))))
	(when played
		(defq inst (elem-get instruments (. chan :find :ins)))
		(scatter chan :ivib_depth (<< (. inst :find :vib_depth) 2) :ivib_rate (>> (+ (. inst :find :vib_rate) 2) 2)
			:ivib_wave (. inst :find :vib_wave) :ivib_sweep (. inst :find :vib_sweep) :vib_pos 0 :trem_pos 0))
	(scatter chan :tremor_up 0 :tremor_down 0 :arp (list 0) :arp_at 0 :offset 0)
	;a new instrument, its envelopes from their start
	(and (> ins 0) (not k00) (>= (ft2-sample player chan (. chan :find :key)) 0)
		(scatter chan :venv -1 :penv -1))
	(if set_vol (scatter chan :volume (- vol 0x10) :new_vol :t))
	(if (>= vol 0x60) (ft2-volcol player chan vol note row))
	(ft2-effect player chan effect param note row)
	(when played
		(defq period (ft2-period player played (. chan :find :finetune)))
		(if (or (< (. chan :find :period) 1) (not porta)) (. chan :insert :period period))
		(. chan :insert :note played)
		;an offset past its sample's end is no sound
		(if (>= (. chan :find :offset) (. (elem-get samples (. chan :find :sample)) :find :length))
			(. chan :insert :stop :t)
			(scatter chan :trigger :t :start (. chan :find :offset))))
	(and use_volume (not (. chan :find :new_vol)) (>= (defq s (ft2-sample player chan (. chan :find :key))) 0)
		(. chan :insert :volume (. (elem-get samples s) :find :volume))))

(defun ft2-row (player)
	; (ft2-row player)
	;the first tick of a row: each channel's cell, or held back by a note
	;delay, that plays the last note again if it has none
	(defq song (. player :find :song) channels (. player :find :channels) row (. player :find :row)
		cells (elem-get (. song :find :patterns) (elem-get (. song :find :orders) (. player :find :order)))
		at (* row channels +cell_size))
	(each (lambda (chan)
			(defq c (+ at (* (!) +cell_size)) cell (slice cells c (+ c +cell_size)))
			(bind '(note ins vol effect param) cell)
			;a speed is had first, a delay is of it
			(and (= effect 15) (< 0 param 32) (. player :insert :speed param))
			(cond
				((and (= effect 14) (= (>> param 4) 13) (> (logand param 15) 0))
					(and (= vol 0) (= ins 0) (/= note +note_off)
						(setq vol (+ 0x10 (. chan :find :volume))))
					(if (= note 0) (setq note (. chan :find :key)))
					(if (= ins 0) (setq ins (inc (. chan :find :ins))))
					(scatter chan :delay (inc (logand param 15)) :delayed (list note ins vol effect param)))
				((ft2-event player chan cell row))))
		(. player :find :chans)))

(defun ft2-play (player chan frame)
	; (ft2-play player chan frame)
	;a tick of a channel: its volume, pan and pitch as its effects, its
	;instrument's envelopes and its fade move them
	(defq speed (. player :find :speed) tick0 (= frame 0) row0 (= (% frame speed) 0)
		fx (. chan :find :fx))
	(when (> (. chan :find :delay) 0)
		(. chan :insert :delay (dec (. chan :find :delay)))
		(when (and (= (. chan :find :delay) 0) (< frame speed))
			(ft2-event player chan (. chan :find :delayed) (. player :find :row))
			(setq fx (. chan :find :fx))))
	(when (>= (. chan :find :ins) 0)
		(defq volume (. chan :find :volume) period (. chan :find :period) gvol (. player :find :gvol)
			ins (elem-get (. (. player :find :song) :find :instruments) (. chan :find :ins)))
		;a retrig, or a cut, every so many ticks
		(when (bits? fx +fx_retrig)
			(. chan :insert :retrig_count (defq count (dec (. chan :find :retrig_count))))
			(when (= count 0)
				(if (< (. chan :find :retrig_type) 16) (scatter chan :trigger :t :start 0))
				(bind '(add mult by) (elem-get +st3_retrig (. chan :find :retrig_type)))
				(setq volume (/ (* (+ volume add) mult) by))
				(. chan :insert :retrig_count (logand (. chan :find :retrig_val) 15))))
		;a key let go so many ticks on
		(when (> (defq keyoff (. chan :find :keyoff)) 0)
			(. chan :insert :keyoff (setq keyoff (dec keyoff)))
			(if (= keyoff 0) (. chan :insert :release :t)))
		;slides of the volumes, each tick but a row's first, fine at its first
		(cond
			(row0 (if (bits? fx +fx_fvslide) (setq volume (+ volume (. chan :find :fvslide))))
				(if (bits? fx +fx_gslide) (setq gvol (+ gvol (. chan :find :gfslide)))))
			(:t (if (bits? fx +fx_gslide) (setq gvol (+ gvol (. chan :find :gslide))))
				(if (bits? fx +fx_vslide) (setq volume (+ volume (. chan :find :vslide))))
				(if (bits? fx +fx_vslide2) (setq volume (+ volume (. chan :find :vslide2))))))
		(. chan :insert :volume (setq volume (max 0 (min 64 volume))))
		(. player :insert :gvol (setq gvol (max 0 (min 64 gvol))))
		;slides of the pitch
		(cond
			(tick0 (if (bits? fx +fx_fbend) (setq period (+ period (. chan :find :fbend)))))
			(:t (if (bits? fx +fx_bend) (setq period (+ period (. chan :find :bend))))
				(when (and (bits? fx +fx_porta) (> (defq to (. chan :find :target)) 0))
					(defq by (. chan :find :porta))
					(when (if (> (. chan :find :pdir) 0) (>= (setq period (+ period by)) to)
							(<= (setq period (- period by)) to))
						(setq period to)
						(scatter chan :pdir 0 :fx (setq fx (logand fx (lognot +fx_porta))))))))
		(. chan :insert :period (setq period (max 1 period)))
		(and (bits? fx +fx_pslide) (not tick0)
			(. chan :insert :pan (max 0 (min 255 (+ (. chan :find :pan) (. chan :find :pslide))))))
		;the volume envelope, and the fade once let go
		(defq venv (. ins :find :volume) release (. chan :find :release) fade (. chan :find :fading))
		(when release
			(unless (. venv :find :on) (. chan :insert :fadeout 0))
			(setq fade :t))
		(. chan :insert :venv (defq vx (ft2-env-step venv (. chan :find :venv) release)))
		(if fade (. chan :insert :fadeout (max 0 (- (. chan :find :fadeout) (. chan :find :fade)))))
		;the volume heard: a tremolo, the envelope, the global volume, the
		;fade, and a tremor
		(when (bits? fx +fx_trem)
			(defq pos (. chan :find :trem_pos) rate (. chan :find :trem_rate))
			(if (> rate 0)
				(setq volume (+ volume (/ (* (ft2-wave (. chan :find :trem_wave) pos :nil) (. chan :find :trem_depth)) 64))))
			(unless tick0 (. chan :insert :trem_pos (logand (+ pos rate) 63))))
		(setq volume (>> (* (max 0 (min 64 volume)) (ft2-env-value venv vx 64) gvol (. chan :find :fadeout)) 22))
		(when (bits? (defq count (. chan :find :tremor_count)) 128)
			(when (and (bits? fx +fx_tremor) (not tick0))
				(setq count (logand count (lognot 32)))
				(cond
					((= count 128) (setq count (logior (. chan :find :tremor_up) 192)))
					((= count 192) (setq count (logior (. chan :find :tremor_down) 128)))
					((setq count (dec count))))
				(. chan :insert :tremor_count count))
			(if (= (logand count 224) 128) (setq volume 0)))
		;the pitch heard: the instrument's vibrato, a vibrato, an arpeggio
		(defq rate (. chan :find :ivib_rate) sweep (. chan :find :ivib_sweep))
		(when (> rate 0)
			(setq period (+ period (/ (* (ft2-wave (. chan :find :ivib_wave) (. chan :find :ivib_pos) :t)
				(. chan :find :ivib_depth)) (* 1024 (inc sweep))))))
		(scatter chan :ivib_pos (logand (+ (. chan :find :ivib_pos) rate) 63)
			:ivib_sweep (if (> sweep 1) (- sweep 2) 0))
		(when (bits? fx +fx_vib)
			(defq pos (. chan :find :vib_pos) rate (. chan :find :vib_rate))
			(if (> rate 0)
				(setq period (+ period (/ (* (ft2-wave (. chan :find :vib_wave) pos :t) (. chan :find :vib_depth)) 128))))
			(unless tick0 (. chan :insert :vib_pos (logand (+ pos rate) 63))))
		(defq arp (. chan :find :arp) up 0)
		(when (= (length arp) 3)
			(cond
				((. (. player :find :song) :find :ft2bugs)
					;FastTracker's own, its table read from the end of the row
					(defq i (- speed (% frame speed)))
					(setq up (cond ((= frame 0) 0) ((= i 16) 0) ((> i 16) (elem-get arp 2)) ((elem-get arp (% i 3))))))
				(:t (setq up (elem-get arp (. chan :find :arp_at)))
					(. chan :insert :arp_at (% (inc (. chan :find :arp_at)) 3)))))
		(if (> up 0) (setq period (if (eql (. player :find :pitch) :linear) (- period (* 64 up))
			(>> (* period (elem-get +semitones up)) 16))))
		;the pan heard, its envelope round it
		(. chan :insert :penv (defq px (ft2-env-step (. ins :find :pan) (. chan :find :penv) release)))
		(defq pan (. chan :find :pan) pe (ft2-env-value (. ins :find :pan) px 32))
		(scatter chan :pan_out (max 0 (min 255 (+ pan (/ (* (- pe 32) (- 128 (abs (- pan 128)))) 32))))
			:period_out (max 1 period) :volume_out volume)))

(defun ft2-tick (player)
	; (ft2-tick player)
	;one tick of an XM: a row read at its first, every channel played,
	;the row long so many ticks as the speed says, times a pattern delay
	(defq frame (. player :find :frame))
	(if (= frame 0) (ft2-row player))
	(each (# (ft2-play player %0 frame)) (. player :find :chans))
	(tracker-voices player)
	(. player :insert :frame (setq frame (inc frame)))
	(if (>= frame (* (. player :find :speed) (inc (. player :find :pdelay))))
		(flow-advance player)))

;;;;;;;;;;;;;;;;;;;;
; the song, played
;;;;;;;;;;;;;;;;;;;;

(defun tracker-player (song rate)
	; (tracker-player song rate) -> player
	;a player of a song at a rate of frames a second
	(defq channels (. song :find :channels) mode (. song :find :mode)
		player (scatter (Emap) :song song :rate rate :mode mode :channels channels
			:chans (map (case mode (:st3 (const st3-channel)) (:ft2 (const ft2-channel)) (:t (const pt-channel)))
				(. song :find :pans))
			:voices (apply (const nums) (apply (const cat) (map! (lambda () '(-1 0 0 0 0 -1 0 0)) '() 0 channels)))
			:samples (map (# (. %0 :find :data)) (. song :find :samples))
			:pitch (. song :find :pitch)
			:clock (case mode (:st3 +st3_clock) (:ft2 +ft2_clock) (:t +amiga_clock))
			:scale (if (eql mode :mod) 1 64)
			:speed (. song :find :speed) :bpm (. song :find :tempo) :gvol (. song :find :global)
			:tick 0 :frame 0 :row 0 :order 0 :jump :nil :jump_order 0 :jump_row 0 :delay 0
			:repeat :nil :pbreak :nil :jump_to -1 :jumpline 0 :loop_to -1 :pdelay 0
			:carry 0 :visited (Fset 101) :ended :nil :acc (nums)
			:gain (/ (* 512 (ifn (. song :find :mix) 48)) 48)))
	(if (eql mode :mod) (. (. player :find :visited) :insert 0) (flow-start player))
	;2^(n/768) of 16.16, for linear periods
	(if (eql (. song :find :pitch) :linear)
		(. player :insert :exp2 (apply (const nums) (map! (# (n2i (* (const (n2d 65536))
			(pow (const (n2d 2)) (/ (n2d (!)) (const (n2d 768))))))) '() 0 768))))
	(if (eql mode :mod) (. player :insert :tables (pt-tables)))
	player)

(defun tracker-tick (player)
	; (tracker-tick player) -> frames
	;one tick of the song, and how many frames it lasts
	;2.5 / bpm seconds: for a MOD what is over a whole frame carried to the
	;next, as the Amiga's timer, the others whole frames, as their trackers
	(cond
		((eql (defq mode (. player :find :mode)) :mod)
			(pt-tick player)
			(defq rate (* 5 (. player :find :rate)) bpm (* 2 (. player :find :bpm))
				n (+ rate (. player :find :carry)))
			(. player :insert :carry (% n bpm))
			(/ n bpm))
		(:t (if (eql mode :st3) (st3-tick player) (ft2-tick player))
			(/ (* 5 (. player :find :rate)) (* 2 (. player :find :bpm))))))

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
	;round its loop, back and forth along it, or ended
	(defq voices (. player :find :voices))
	(each! (lambda ()
			(defq at (* (!) +mix_size))
			(when (>= (elem-get voices at) 0)
				(defq pos (elem-get voices (+ at 1)) by (* (elem-get voices (+ at 2)) frames)
					ls (elem-get voices (+ at 5)) endq (<< (elem-get voices (+ at 6)) 16)
					flags (elem-get voices (+ at 7)) back (bits? flags +mix_back))
				(cond
					((and (not back) (< (+ pos by) endq)) (setq pos (+ pos by)))
					((< ls 0) (elem-set voices at -1))
					((not (bits? flags +mix_pingpong))
						(setq pos (+ pos by) pos (+ (<< ls 16) (% (- pos endq) (- endq (<< ls 16))))))
					(:t ;back and forth: where it is along the loop there and back
						(defq lsq (<< ls 16) len (- endq lsq)
							along (% (+ by (if back (- (* 2 len) (- pos lsq)) (- pos lsq))) (* 2 len)))
						(if (< along len)
							(setq pos (+ lsq along) flags (logand flags (lognot +mix_back)))
							(setq pos (+ lsq (- (* 2 len) along)) flags (logior flags +mix_back)))
						(elem-set voices (+ at 7) flags)))
				(elem-set voices (+ at 1) pos)))
		'() 0 (/ (length voices) +mix_size)))

(defun tracker-seek (player frames &optional from)
	; (tracker-seek player frames [from]) -> frames
	;a player played on to the first tick at or past so many frames, from
	;its start, or from where it has got to, from, its rows as they are
	;played and its voices moved on, nothing mixed, and where that is
	(defq at (ifn from 0))
	(while (and (< at frames) (not (. player :find :ended)))
		(defq n (tracker-tick player))
		(tracker-skip player n)
		(setq at (+ at n)))
	at)
