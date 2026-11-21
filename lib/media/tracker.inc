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
; and for an XM's Amiga periods, or an XM's linear period, 64 a semitone.
; Those of Impulse Tracker, :it, the it- functions, are as libxmp 4.7
; plays them, a period 64 times its, and a channel gives the mixer its step
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/media/mod.inc")

(ffi "lib/media/media_mix" media-mix)

(defq +mix_size 13 +mix_pingpong 1 +mix_back 2
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
	+ft2_clock 14317456
	;and of Impulse Tracker
	+fx_fvslide2 4096 +fx_tempo 8192 +fx_tvslide 16384 +fx_tfvslide 32768 +fx_panbrello 65536
	+fx_new_vol 131072 +fx_new_ins 262144 +fx_new_note 524288 +fx_key_off 1048576
	+fx_offset 2097152 +fx_macro 4194304
	;what an IT channel's note is doing, :notes bits, and what is paused, :pers
	+itn_fade 1 +itn_env_release 2 +itn_end 4 +itn_cut 8 +itn_env_end 16 +itn_sample_end 32
	+itn_set 64 +itn_susexit 128 +itn_key_cut 256 +itn_gliss 512 +itn_sample_release 1024
	+itn_release 1026 +itp_venv 1 +itp_penv 2 +itp_fenv 4 +rowdelay_on 1 +rowdelay_first 2
	;the volume column's slides to a note, by its number
	+it_gporta ''(0 1 4 8 16 32 64 96 128 255)
	;the voices of an IT's mixer, its channels' and its past notes'
	+it_voices 128
	;the filter's coefficients are of 22 bits, as libxmp's
	+filter_shift 22)

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
		linear (eql (. player :find :pitch) :linear) stepped (. player :find :stepped)
		chans (. player :find :chans) n (length chans))
	;a channel's voice is its place, an IT's past note's its :slot
	(each (lambda (chan)
			(defq at (* (if (< (!) n) (!) (. chan :find :slot)) +mix_size) sample (. chan :find :sample))
			(when (. chan :find :stop)
				(scatter chan :stop :nil :trigger :nil)
				(elem-set voices at -1)
				;a voice let go of, its filter from nothing when next it is
				(elem-set voices (+ at 11) 0)
				(elem-set voices (+ at 12) 0))
			(when (and (. chan :find :trigger) (>= sample 0))
				;a note played: its sample from the start, or the offset, and
				;its loop
				(defq s (elem-get (. song :find :samples) sample) len (. s :find :length)
					ls (. s :find :loop_start) le (. s :find :loop_end) pingpong (. s :find :pingpong)
					start (. chan :find :start))
				;a channel's loop of its own, an IT's sustain loop
				(if (defq loop (. chan :find :loop)) (bind '(ls le pingpong) loop))
				(cond
					((< start len))
					((>= ls 0) (setq start ls))
					((setq sample -1)))
				(elem-set voices at sample)
				(elem-set voices (+ at 1) (<< start 16))
				(elem-set voices (+ at 5) ls)
				(elem-set voices (+ at 6) (if (>= ls 0) le len))
				(elem-set voices (+ at 7) (if (and (>= ls 0) pingpong) +mix_pingpong 0)))
			(when (. chan :find :loop_change)
				;a sustain loop let go, the voice on round the loop
				(. chan :insert :loop_change :nil)
				(when (>= (elem-get voices at) 0)
					(bind '(ls le pingpong) (. chan :find :loop))
					(elem-set voices (+ at 5) ls)
					(elem-set voices (+ at 6) (if (>= ls 0) le (. (elem-get (. song :find :samples) sample) :find :length)))
					(elem-set voices (+ at 7) (if (and (>= ls 0) pingpong) +mix_pingpong 0))))
			(. chan :insert :trigger :nil)
			(defq period (. chan :find :period_out) pan (ifn (. chan :find :pan_out) (. chan :find :pan))
				weight (/ (* (. chan :find :volume_out) gain) scale))
			(elem-set voices (+ at 2) (cond
				((<= period 0) 0)
				(stepped period)
				(linear (ft2-step player period))
				((/ clock (* period rate)))))
			(elem-set voices (+ at 3) (/ (* weight (- 256 pan)) 256))
			(elem-set voices (+ at 4) (/ (* weight pan) 256))
			;an IT's filter, a0 0 none
			(when stepped
				(elem-set voices (+ at 8) (. chan :find :fa0))
				(elem-set voices (+ at 9) (. chan :find :fb0))
				(elem-set voices (+ at 10) (. chan :find :fb1))))
		(if (defq past (. player :find :past)) (cat chans past) chans)))

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
			(each! (# (. visited :erase (+ (* order 1024) (!)))) '() row 1024))
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
	(if (. visited :find (defq key (+ (* order 1024) row))) (. player :insert :ended :t) (. visited :insert key))
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
		(. (. player :find :visited) :insert (* order 1024)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Impulse Tracker, the effects of an IT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun it-channel (pan vol mute index)
	; (it-channel pan vol mute index) -> channel
	(scatter (Emap) :index index :ins -1 :old_ins 1 :key -1 :key_porta 0 :note 0 :smp -1
		:sample -1 :period 0 :finetune 0 :volume 64 :mastervol vol :pan pan :mute mute :gvl 64
		:fx 0 :notes 0 :pers 0 :ins_fade 0 :fadeout 65536 :venv -1 :penv -1 :fenv -1
		:vslide 0 :fvslide 0 :vslide2 0 :fvslide2 0 :mem 0 :vol_mem2 0
		:bend 0 :fbend 0 :freq_mem 0 :porta 0 :porta_mem 0 :target 0 :pdir 0
		:vib (nums 0 0 0 0) :vib_mem 0 :trem (nums 0 0 0 0) :trem_mem 0
		:ivib (nums 0 0 0 0) :ivib_sweep 0 :panbrello (nums 0 0 0 0)
		:pslide 0 :pfslide 0 :gslide 0 :gfslide 0 :gvol_mem 0
		:tslide 0 :tfslide 0 :trk_mem 0 :tempo 0
		:retrig_val 0 :retrig_count 0 :retrig_type 0
		:tremor_up 0 :tremor_down 0 :tremor_count 0 :tremor_mem 0
		:offset 0 :offset_mem 0 :arp (list 0) :arp_at 0 :arp_mem 0
		:delay 0 :delayed :nil :delayed_ins 0 :rvv 0 :rpv 0 :loop_row 0 :loop_count 0
		:voice :nil :released :nil :loop :nil :loop_change :nil
		:period_out 0 :volume_out 0 :pan_out pan :trigger :nil :start 0 :stop :nil
		:act 0 :v_ins -1 :v_smp -1 :v_key -1
		:f_cutoff 255 :f_res 0 :f_env 256 :f_can :nil :fa0 0 :fb0 0 :fb1 0
		:m_active 0 :m_val 0 :m_slide 0 :m_target 0))

(defun it-flag (chan key bits)
	; (it-flag chan key bits)
	(. chan :insert key (logior (. chan :find key) bits)))

(defun it-unflag (chan key bits)
	; (it-unflag chan key bits)
	(. chan :insert key (logand (. chan :find key) (lognot bits))))

(defun it-lfo (lfo)
	; (it-lfo lfo) -> num
	;an lfo of (phase rate depth wave) now, Scream Tracker's waves times its
	;depth, 0 if it does not move
	(if (= (elem-get lfo 1) 0) 0 (* (elem-get lfo 2) (st3-wave (elem-get lfo 3) (elem-get lfo 0)))))

(defun it-lfo-step (lfo)
	; (it-lfo-step lfo)
	(elem-set lfo 0 (% (+ (elem-get lfo 0) (elem-get lfo 1)) 64)))

(defun it-lfo-set (lfo depth rate)
	; (it-lfo-set lfo depth rate)
	;an lfo's depth and rate, each if not 0
	(if (/= depth 0) (elem-set lfo 2 depth))
	(if (/= rate 0) (elem-set lfo 1 rate)))

(defun it-ins (song ins)
	; (it-ins song ins) -> :nil | instrument
	;an instrument that has a sample to play
	(defq instruments (. song :find :instruments))
	(and (<= 0 ins) (< ins (length instruments))
		(>= (. (defq i (elem-get instruments ins)) :find :first) 0) i))

(defun it-mapped (song ins key)
	; (it-mapped song ins key) -> -1 | sample
	;the sample an instrument's map has for a key, as it is
	(defq s (if (<= 0 key 119) (first (elem-get (. (elem-get (. song :find :instruments) ins) :find :map) key)) -1))
	(if (< s (length (. song :find :samples))) s -1))

(defun it-sub (song ins key)
	; (it-sub song ins key) -> -1 | sample
	;the sample an instrument plays for a key, its first if no key
	(cond
		((not (defq i (it-ins song ins))) -1)
		((<= 0 key 120) (if (< key 120) (first (elem-get (. i :find :map) key)) -1))
		((. i :find :first))))

(defun it-xpo (song ins key)
	; (it-xpo song ins key) -> num
	;the semitones an instrument's map moves a key
	(if (<= 0 key 119) (second (elem-get (. (elem-get (. song :find :instruments) ins) :find :map) key)) 0))

(defun it-same (song chan ins key)
	; (it-same song chan ins key) -> :t | :nil
	;the same sample for a key of an instrument as the channel's now
	(and (>= (defq s (it-sub song ins key)) 0) (= s (it-sub song (. chan :find :ins) (. chan :find :key)))))

(defun it-period (player note fin)
	; (it-period player note fin) -> period
	;the period of a note, 0 C-0 and on, and so many 128ths more, 64 times
	;libxmp's: linear, 1024 a semitone, or the Amiga's, 13696 at C-0
	(if (eql (. player :find :pitch) :linear)
		(* 8 (- 30720 (* note 128) fin))
		(n2i (* (const (n2d 876544)) (pow (const (n2d 2)) (/ (n2d (neg (+ (* note 128) fin))) (const (n2d 1536))))))))

(defun it-pitch (player period)
	; (it-pitch player period) -> pitch
	;the pitch of a period, in 12800ths of a semitone over C-0
	(if (eql (. player :find :pitch) :linear)
		(- 3072000 (/ (* 25 period) 2))
		(n2i (* (const (n2d 153600)) (/ (log (/ (const (n2d 876544)) (n2d period))) (const (log (n2d 2))))))))

(defun it-step (player pitch)
	; (it-step player pitch) -> step
	;a frame's step of a pitch, 16.16: 8363 a second at C-5, by 2^(n/768)
	;between its 64ths of a semitone. 0, no sound, when out of the mixer's
	;reach
	(defq y (- pitch 768000) e (if (>= y 0) (/ y 200) (dec (/ (inc y) 200))) frac (- y (* e 200))
		q (if (>= e 0) (/ e 768) (dec (/ (inc e) 768))) i (- e (* q 768)) exp2 (. player :find :exp2)
		f (+ (elem-get exp2 i) (/ (* (- (elem-get exp2 (inc i)) (elem-get exp2 i)) frac) 200))
		n (* 8363 f) sh (+ q 16)
		step (>> (/ (if (>= sh 0) (<< n sh) (>> n (neg sh))) (. player :find :rate)) 16))
	(if (<= 66 step (const (* 32767 65536))) step 0))

(defun it-env-x (envl i)
	; (it-env-x envl i) -> tick
	;the tick of an envelope's point, 0 past its last
	(if (< i (length (defq points (. envl :find :points)))) (first (elem-get points i)) 0))

(defun it-env-step (envl x release key_off)
	; (it-env-step envl x release key_off) -> x
	;an envelope's tick on one: round its sustain loop till let go, once
	;more if let go at its end, and round its loop
	(if (< x 0xffff) (setq x (inc x)))
	(cond
		((< x 0) -1)
		((not (. envl :find :on)) x)
		(:t (defq sus (. envl :find :sustain) loop (. envl :find :loop))
			(cond
				((and sus key_off (= x (inc (it-env-x envl (second sus))))) (it-env-x envl (first sus)))
				((and sus (not release))
					(if (= x (inc (it-env-x envl (second sus)))) (it-env-x envl (first sus)) x))
				((and loop (> x (it-env-x envl (second loop)))) (it-env-x envl (first loop)))
				(x)))))

(defun it-env-end (envl x)
	; (it-env-end envl x) -> :t | :nil
	;an envelope with no loop at its last point
	(and (. envl :find :on) (not (. envl :find :loop))
		(or (= (length (defq points (. envl :find :points))) 1) (>= x (first (last points))))))

(defun it-env-fade (envl x)
	; (it-env-fade envl x) -> -1 | 0 | 1
	;past an envelope's end: -1 at 0, the note ended, 1 not, it fades
	(cond
		((not (. envl :find :on)) 0)
		((> x (first (defq p (last (. envl :find :points))))) (if (= (second p) 0) -1 1))
		(0)))

(defun it-env-reset (song chan how)
	; (it-env-reset song chan how)
	;the envelopes of the channel's instrument from their start: :all,
	;:volume, or :carry, those that do not carry on from the last note
	(when (defq ins (it-ins song (. chan :find :ins)))
		(it-unflag chan :notes +itn_env_end)
		(each (lambda (key envl)
				(if (or (eql how :all) (and (eql how :volume) (eql key :venv))
						(and (eql how :carry) (not (. (. ins :find envl) :find :carry))))
					(. chan :insert key -1)))
			'(:venv :penv :fenv) '(:volume :panning :pitch))))

(defun it-sus-check (envl x)
	; (it-sus-check envl x) -> :t | :nil
	;a volume envelope held at its sustain point, with no loop
	(and envl (. envl :find :on) (defq sus (. envl :find :sustain)) (not (. envl :find :loop))
		(= x (it-env-x envl (first sus)))))

(defun it-voicepos (chan pos)
	; (it-voicepos chan pos)
	;the channel's voice played again from a place, if it has one
	(if (. chan :find :voice) (scatter chan :trigger :t :start pos)))

(defun it-loop (player chan)
	; (it-loop player chan)
	;the loop the voice plays: its sample's sustain loop till let go, then
	;its loop, none if it has no sustain loop
	(defq s (elem-get (. (. player :find :song) :find :samples) (. chan :find :sample))
		released (bits? (. chan :find :notes) +itn_sample_release))
	(scatter chan :released released :loop (when (>= (. s :find :sus_start) 0)
		(if released (list (. s :find :loop_start) (. s :find :loop_end) (. s :find :pingpong))
			(list (. s :find :sus_start) (. s :find :sus_end) (. s :find :sus_pingpong))))))

(defun it-patch (player chan s)
	; (it-patch player chan s)
	;a sample put on the channel's voice from its start, or no voice
	(cond
		((< s 0) (scatter chan :voice :nil :stop :t :trigger :nil))
		(:t (scatter chan :sample s :voice :t :trigger :t :start 0)
			(it-unflag chan :notes +itn_sample_end)
			(it-loop player chan))))

(defun it-past-drop (player past)
	; (it-past-drop player past)
	;a past note let go of, its voice stopped and its slot free
	(elem-set (. player :find :voices) (* (. past :find :slot) +mix_size) -1)
	(push (. player :find :free) (. past :find :slot))
	(. player :insert :past (filter (# (not (eql %0 past))) (. player :find :past))))

(defun it-voice-cut (player chan)
	; (it-voice-cut player chan)
	;a voice stopped, a channel's own or a past note's
	(if (. chan :find :slot) (it-past-drop player chan) (scatter chan :voice :nil :stop :t :trigger :nil)))

(defun it-voice-room (player)
	; (it-voice-room player)
	;room for one more voice of the mixer's: the quietest past note let go
	;if all are playing
	(defq past (. player :find :past))
	(and (nempty? past)
		(>= (reduce (lambda (n c) (if (. c :find :voice) (inc n) n)) (. player :find :chans) (length past))
			+it_voices)
		(it-past-drop player (reduce (lambda (q c) (if (< (. c :find :volume_out) (. q :find :volume_out)) c q))
			past (first past)))))

(defun it-dct (player chan ins smp key nna dct dca)
	; (it-dct player chan ins smp key nna dct dca)
	;a new note's duplicate check, libxmp's check_dct: each voice of the
	;channel's, its own and its past notes', of the instrument, given the
	;new note's action, and one of the same note, sample or instrument as
	;the check is of, the duplicate's action, or cut
	(defq index (. chan :find :index))
	(each (lambda (c)
			(and (. c :find :voice) (= (. c :find :index) index) (= (. c :find :v_ins) ins)
				(cond
					((= nna 0) (it-voice-cut player c))
					(:t (. c :insert :act nna)
						(when (case dct (3 :t) (2 (= (. c :find :v_smp) smp)) (1 (= (. c :find :v_key) key)))
							(cond
								((and (= nna 2) (= dca 3)) (. c :insert :act 2))
								((> dca 0) (. c :insert :act dca))
								((it-voice-cut player c))))))))
		(cat (list chan) (. player :find :past))))

(defun it-nna (player chan)
	; (it-nna player chan)
	;the voice of a note a new one does not cut, a past note's: a copy of
	;the channel that plays on, its voice and action its own, and none of
	;its effects, libxmp's virtual channel
	(when (and (. chan :find :voice) (/= (. chan :find :act) 0) (nempty? (defq free (. player :find :free))))
		(defq past (. chan :deep_copy) slot (pop free) voices (. player :find :voices)
			from (* (. chan :find :index) +mix_size) to (* slot +mix_size))
		(each! (# (elem-set voices (+ to (!)) (elem-get voices (+ from (!))))) '() 0 +mix_size)
		;the channel's new voice, its filter from nothing
		(elem-set voices (+ from 11) 0)
		(elem-set voices (+ from 12) 0)
		(scatter past :slot slot :fx 0)
		(push (. player :find :past) past)))

(defun it-past-act (player chan act)
	; (it-past-act player chan act)
	;the channel's past notes cut, let go or faded, S70 to S72
	(defq index (. chan :find :index))
	(each (lambda (c)
			(when (= (. c :find :index) index)
				(case act
					(0 (it-past-drop player c))
					(2 (it-flag c :notes +itn_release))
					(3 (it-flag c :notes +itn_fade)))))
		(cat (. player :find :past))))

(defun it-filter (player chan cutoff res)
	; (it-filter player chan cutoff res)
	;the coefficients of the channel's filter, libxmp's libxmp_filter_setup:
	;of two poles, cutoff 0 to 255 110 Hz to 7040 Hz, resonance 0 to 255 up
	;to 24 dB. None, a0 0, at 254 and more with no resonance. Kept by the
	;player, each pair once
	(defq cache (. player :find :filters) key (+ (* cutoff 256) res))
	(unless (defq coeffs (. cache :find key))
		(. cache :insert key (setq coeffs (if (and (>= cutoff 254) (= res 0)) '(0 0 0)
				(defq fs (n2d (. player :find :rate))
					fc (min (/ fs (const (n2d 2)))
						(* (const (n2d 110)) (pow (const (n2d 2))
							(+ (/ (* (n2d cutoff) (const (n2d 128))) (const (n2d 6144))) (const (n2d 0.25))))))
					r (/ fs (* (const (/ (n2d 314159265358979) (n2d 50000000000000))) fc))
					d (- (* (pow (const (n2d 10)) (/ (n2d (* -3 (>> res 1))) (const (n2d 320))))
						(+ r (const (n2d 1)))) (const (n2d 1)))
					e (* r r) g (/ (const (n2d 1)) (+ (const (n2d 1)) d e))
					one (const (n2d (<< 1 +filter_shift))))
				(list (n2i (* g one)) (n2i (* (* (+ d e e) g) one)) (n2i (* (* (neg e) g) one)))))))
	(bind '(a0 b0 b1) coeffs)
	(scatter chan :fa0 a0 :fb0 b0 :fb1 b1))

(defun it-midi-nibble (player chan text param at)
	; (it-midi-nibble player chan text param at) -> (nibble at low)
	;the next half byte of a MIDI macro's text, -1 none, libxmp's
	;midi_nibble: hex, and its letters for what the channel has, a byte
	;whose low half is had next, low, -1 if none
	(defq out -1 byte -1)
	(while (and (< out 0) (< byte 0) (< at (length text)))
		(defq c (elem-get text at) at (inc at))
		(cond
			((<= (const (code "0")) (code c) (const (code "9"))) (setq out (- (code c) (const (code "0")))))
			((<= (const (code "A")) (code c) (const (code "F"))) (setq out (+ 10 (- (code c) (const (code "A"))))))
			((eql c "z") (setq byte param))
			((eql c "n") (setq byte (logand (. chan :find :key) 127)))
			((eql c "h") (setq byte (. chan :find :index)))
			((eql c "o") (setq byte (. chan :find :offset_mem)))
			((eql c "v") (setq byte (max 1 (min 127 (>> (* (. player :find :gvol) (. chan :find :volume)
				(. chan :find :mastervol) (. chan :find :gvl)
				(if (defq i (it-ins (. player :find :song) (. chan :find :ins))) (. i :find :vol) 64)) 24)))))
			((eql c "u") (setq byte (max 1 (min 127 (>> (. chan :find :volume_out) 3)))))
			((eql c "x") (setq byte (max 0 (min 127 (>> (. chan :find :pan) 1)))))
			((eql c "y") (setq byte (max 0 (min 127 (>> (. chan :find :pan_out) 1)))))
			((find c "abpsm") (setq byte 0))
			((eql c "c") (setq out 0))))
	(if (>= byte 0) (list (logand (>> byte 4) 15) at (logand byte 15)) (list out at -1)))

(defun it-midi-byte (player chan text param at buffer)
	; (it-midi-byte player chan text param at buffer) -> (byte at buffer)
	;two half bytes of a macro, -1 if not two, the half a letter's byte
	;leaves over kept for the next
	(defq a -1 b -1)
	(if (>= buffer 0) (setq a buffer buffer -1)
		(bind '(a at buffer) (it-midi-nibble player chan text param at)))
	(if (>= buffer 0) (setq b buffer buffer -1)
		(bind '(b at buffer) (it-midi-nibble player chan text param at)))
	(list (if (and (>= a 0) (>= b 0)) (logior (<< a 4) b) -1) at buffer))

(defun it-macro (player chan text param)
	; (it-macro player chan text param)
	;a MIDI macro run, libxmp's execute_midi_macro: of its messages only IT's
	;own, F0 F0 or F0 F1 and what and how much, 0 the cutoff, 1 the
	;resonance; FA, FC and FF put both back
	(defq at 0 buffer -1)
	(while (< at (length text))
		(bind '(byte at buffer) (it-midi-byte player chan text param at buffer))
		(defq cmd -1)
		(when (= byte 0xf0)
			(bind '(byte at buffer) (it-midi-byte player chan text param at buffer))
			(if (or (= byte 0xf0) (= byte 0xf1)) (setq cmd (logand byte 15))))
		(cond
			((< cmd 0)
				(when (find byte '(0xfa 0xfc 0xff))
					(scatter chan :f_cutoff 254 :f_res 0)))
			(:t (bind '(c at buffer) (it-midi-byte player chan text param at buffer))
				(bind '(val at buffer) (it-midi-byte player chan text param at buffer))
				(and (<= 0 c 127) (<= 0 val 127) (<= (setq cmd (logior c (<< cmd 8))) 127)
					(. chan :insert (if (= cmd 0) :f_cutoff :f_res) (<< val 1)))))))

(defun it-macro-tick (player chan frame)
	; (it-macro-tick player chan frame)
	;a Zxx at a row's first tick, or a smooth one each tick on to its
	;target, libxmp's update_midi_macro: with no macros of the song's own
	;Z00 to Z7F the cutoff, Z80 to Z8F the resonance
	(when (bits? (. chan :find :fx) +fx_macro)
		(defq slide (. chan :find :m_slide) val (+ (. chan :find :m_val) slide) target (. chan :find :m_target))
		(cond
			((> slide 0) (if (> val target) (setq val target slide 0)))
			((< slide 0) (if (< val target) (setq val target slide 0))))
		(scatter chan :m_val val :m_slide slide)
		(when (or (/= slide 0) (= frame 0))
			(defq v (>> val 16) macros (. (. player :find :song) :find :macros))
			(cond
				((>= v 128)
					(cond
						(macros (it-macro player chan (elem-get (second macros) (- v 128)) v))
						((< v 144) (. chan :insert :f_res (<< (- v 128) 4)))))
				(macros (it-macro player chan (elem-get (first macros) (. chan :find :m_active)) v))
				((= (. chan :find :m_active) 0) (. chan :insert :f_cutoff (<< v 1)))))))

(defun it-volfx (vol)
	; (it-volfx vol) -> (volume effect param)
	;the volume column as libxmp has it: a volume and 1, 0 none, or an
	;effect of its own, 27 to 30 its slides of the volume
	(cond
		((<= vol 64) (list (inc vol) 0 0))
		((<= vol 74) (list 0 27 (- vol 65)))
		((<= vol 84) (list 0 28 (- vol 75)))
		((<= vol 94) (list 0 29 (- vol 85)))
		((<= vol 104) (list 0 30 (- vol 95)))
		((<= vol 114) (list 0 5 (<< (- vol 105) 2)))
		((<= vol 124) (list 0 6 (<< (- vol 115) 2)))
		((< vol 128) (list 0 0 0))
		((< vol 192) (list 0 24 (<< (- vol 128) 2)))
		((= vol 192) (list 0 24 255))
		((<= vol 202) (list 0 7 (elem-get +it_gporta (- vol 193))))
		((<= vol 212) (list 0 8 (- vol 203)))
		((list 0 0 0))))

(defun it-bend (chan way param fnum unisld)
	; (it-bend chan way param fnum unisld)
	;a slide of the pitch, way 1 down and -1 up: xx each tick but a row's
	;first, and of the effect column Fx fine and Ex extra fine at its first
	(if (= param 0) (setq param (. chan :find :freq_mem)) (. chan :insert :freq_mem param))
	(defq x (>> param 4) y (logand param 15))
	(cond
		((and (= fnum 0) (= x 15))
			(when (> y 0) (. chan :insert :fbend (* way 64 y)) (chan-fx chan +fx_fbend)))
		((and (= fnum 0) (= x 14))
			(. chan :insert :fbend (* way 16 y)) (chan-fx chan +fx_fbend))
		((> param 0)
			(chan-fx chan +fx_bend)
			(. chan :insert :bend (* way 64 param))
			(if unisld (. chan :insert :porta_mem param)))))

(defun it-toneporta (player chan note)
	; (it-toneporta player chan note)
	;a note, if there is one, to slide to, and which way
	(defq song (. player :find :song) ins (. chan :find :ins)
		inst (elem-get (. song :find :instruments) ins) key (. chan :find :key)
		s (if (<= 0 key 119) (first (elem-get (. inst :find :map) key)) -1))
	(if (< s 0) (setq s (. inst :find :first)))
	(when (<= 1 note 121)
		(defq kp (. chan :find :key_porta))
		(. chan :insert :target (it-period player
			(+ (dec note) (. (elem-get (. song :find :samples) s) :find :xpo) (it-xpo song ins kp))
			(. chan :find :finetune))))
	(. chan :insert :pdir (if (< (. chan :find :period) (. chan :find :target)) 1 -1)))

(defun it-tvslide (chan param)
	; (it-tvslide chan param)
	;a slide of the channel's own volume, as a volume slide
	(if (= param 0) (setq param (. chan :find :trk_mem)))
	(when (> param 0)
		(defq x (>> param 4) y (logand param 15))
		(. chan :insert :trk_mem param)
		(cond
			((and (= x 15) (/= y 0)) (. chan :insert :tfslide (neg y)) (chan-fx chan +fx_tfvslide))
			((and (= y 15) (/= x 0)) (. chan :insert :tfslide x) (chan-fx chan +fx_tfvslide))
			(:t (. chan :insert :tslide (if (/= x 0) x (neg y))) (chan-fx chan +fx_tvslide)))))

(defun it-extended (player chan x y)
	; (it-extended player chan x y)
	;the S effects, by their high half
	(case x
		(1 (if (> y 0) (it-flag chan :notes +itn_gliss) (it-unflag chan :notes +itn_gliss)))
		(3 (elem-set (. chan :find :vib) 3 (logand y 3)))
		(4 (elem-set (. chan :find :trem) 3 (logand y 3)))
		(5 (if (<= y 3) (elem-set (. chan :find :panbrello) 3 y)))
		(6 (. player :insert :pdelay y))
		(7 (case y
			((0 1 2) (it-past-act player chan (elem-get '(0 2 3) y)))
			((3 4 5 6) (and (not (. (. player :find :song) :find :sample_mode)) (. chan :find :voice)
				(. chan :insert :act (- y 3))))
			(7 (it-flag chan :pers +itp_venv))
			(8 (it-unflag chan :pers +itp_venv))
			(9 (it-flag chan :pers +itp_penv))
			(10 (it-unflag chan :pers +itp_penv))
			(11 (it-flag chan :pers +itp_fenv))
			(12 (it-unflag chan :pers +itp_fenv))))
		(8 (scatter chan :pan (<< y 4) :rpv 0))
		(10 (. chan :insert :offset (logior (logand (. chan :find :offset) 0xffff) (<< y 16))))
		(11 (defq looped :nil)
			(cond
				((= y 0) (. chan :insert :loop_row (. player :find :row)))
				((> (defq count (. chan :find :loop_count)) 0)
					(. chan :insert :loop_count (setq count (dec count)))
					(if (> count 0) (setq looped :t)
						;after its last time round a loop starts after it
						(. chan :insert :loop_row (inc (. player :find :row)))))
				(:t (. chan :insert :loop_count y)
					(setq looped :t)))
			(when looped
				;a loop's jump undoes a break or a jump before it in the row
				(scatter player :loop_dest (. chan :find :loop_row) :pbreak :nil :jump_to -1)))
		(12 (chan-fx chan +fx_retrig)
			(it-flag chan :notes +itn_cut)
			(scatter chan :retrig_val (inc (max y 1)) :retrig_count (inc (max y 1)) :retrig_type 16))
		(15 (. chan :insert :m_active y))
		(14 (if (= (. player :find :rowdelay_set) 0)
			(scatter player :rowdelay y :rowdelay_set (const (+ +rowdelay_on +rowdelay_first)))))))

(defun it-fx (player chan fx param fnum note ins)
	; (it-fx player chan fx param fnum note ins)
	;what an effect does at once, and sets going for its ticks: of the
	;effect column by its letter, fnum 0, or of the volume column, fnum 1
	(defq song (. player :find :song) x (>> param 4) y (logand param 15)
		unisld (not (. song :find :link_gxx)))
	(case fx
		(1 (if (> param 0) (. player :insert :speed param)))
		(2 (scatter player :pbreak :t :jump_to param))
		(3 (if (< (. player :find :loop_dest) 0) (scatter player :pbreak :t :jumpline param)))
		(4 (tracker-vslide chan param :mem :nil))
		((5 6) (it-bend chan (if (= fx 5) 1 -1) param fnum unisld))
		(7 (if (= param 0) (setq param (. chan :find :porta_mem)) (. chan :insert :porta_mem param))
			(when (> param 0)
				(if unisld (. chan :insert :freq_mem param))
				(. chan :insert :porta (+ (. chan :find :porta) (* 64 param))))
			(and (or (> note 0) (/= (. chan :find :pdir) 0)) (it-ins song (. chan :find :ins))
				(progn (it-toneporta player chan note) (chan-fx chan +fx_porta))))
		((8 21) (if (= param 0) (setq param (. chan :find :vib_mem)) (. chan :insert :vib_mem param))
			(chan-fx chan +fx_vib)
			(it-lfo-set (. chan :find :vib) (if (= fx 8) (<< (logand param 15) 2) (logand param 15)) (>> param 4)))
		(9 (and (. song :find :old_fx) (> param 0)
				(setq param (logand (logior (<< (inc x) 4) (inc y)) 0xff)))
			(if (= param 0) (setq param (. chan :find :tremor_mem)) (. chan :insert :tremor_mem param))
			(scatter chan :tremor_up (max 1 (>> param 4)) :tremor_down (max 1 (logand param 15)))
			(chan-fx chan +fx_tremor))
		(10 (if (= param 0) (setq param (. chan :find :arp_mem)) (. chan :insert :arp_mem param))
			(if (> param 0) (. chan :insert :arp (list 0 (>> param 4) (logand param 15)))))
		(11 (chan-fx chan +fx_vib) (tracker-vslide chan param :mem :nil))
		(12 (. chan :insert :porta (+ (. chan :find :porta) (* 64 (. chan :find :porta_mem))))
			(when (it-ins song (. chan :find :ins))
				(it-toneporta player chan note)
				(chan-fx chan +fx_porta)
				(tracker-vslide chan param :mem :nil)))
		(13 (if (<= param 64) (. chan :insert :mastervol param)))
		(14 (it-tvslide chan param))
		(15 (if (= param 0) (setq param (. chan :find :offset_mem)) (. chan :insert :offset_mem param))
			(chan-fx chan +fx_offset)
			(if (> note 0)
				(. chan :insert :offset (logior (logand (. chan :find :offset) (lognot 0xffff)) (<< param 8)))))
		(16 (chan-fx chan +fx_pslide)
			(cond
				((= param 0))
				((= x 15) (scatter chan :pslide 0 :pfslide y))
				((= y 15) (scatter chan :pslide 0 :pfslide (neg x)))
				((scatter chan :pslide (- y x) :pfslide 0))))
		(17 (if (> param 0) (scatter chan :retrig_val y :retrig_type x))
			(if (> note 0) (. chan :insert :retrig_count (inc (. chan :find :retrig_val))))
			(chan-fx chan +fx_retrig))
		(18 (if (= param 0) (setq param (. chan :find :trem_mem)) (. chan :insert :trem_mem param))
			(chan-fx chan +fx_trem)
			(it-lfo-set (. chan :find :trem) (logand param 15) (>> param 4)))
		(19 (it-extended player chan x y))
		(20 (cond
			((= x 0) (chan-fx chan +fx_tempo) (if (> y 0) (. chan :insert :tempo (neg y))))
			((= x 1) (chan-fx chan +fx_tempo) (. chan :insert :tempo y))
			((. player :insert :bpm param))))
		(22 (if (<= param 128) (. player :insert :gvol param)))
		(23 (if (= param 0) (setq param (. chan :find :gvol_mem)))
			(when (> param 0)
				(setq x (>> param 4) y (logand param 15))
				(. chan :insert :gvol_mem param)
				(chan-fx chan +fx_gslide)
				(cond
					((and (= y 15) (/= x 0)) (scatter chan :gslide 0 :gfslide x))
					((and (= x 15) (/= y 0)) (scatter chan :gslide 0 :gfslide (neg y)))
					((scatter chan :gslide (if (/= x 0) x (neg y)) :gfslide 0)))))
		(24 (scatter chan :pan param :rpv 0))
		(26 (chan-fx chan +fx_macro) (scatter chan :m_val (<< param 16) :m_slide 0))
		(31 (when (and (> (defq speed (. player :find :speed)) 0) (< (>> (. chan :find :m_val) 16) 128))
			(chan-fx chan +fx_macro)
			(scatter chan :m_target (<< param 16)
				:m_slide (/ (- (<< param 16) (. chan :find :m_val)) speed))))
		(25 (chan-fx chan +fx_panbrello)
			(it-lfo-set (. chan :find :panbrello) (<< (logand param 15) 4) (>> param 4)))
		((27 28) (if (= param 0) (setq param (. chan :find :vol_mem2)) (. chan :insert :vol_mem2 param))
			(. chan :insert :fvslide2 (if (= fx 27) param (neg param)))
			(chan-fx chan +fx_fvslide2))
		((29 30) (if (= param 0) (setq param (. chan :find :vol_mem2)) (. chan :insert :vol_mem2 param))
			(. chan :insert :vslide2 (if (= fx 29) param (neg param)))
			(chan-fx chan +fx_vslide2))))

(defun it-event (player chan cell)
	; (it-event player chan cell)
	;a channel's cell read, its note, instrument, volume column and effect,
	;as Impulse Tracker has them, after libxmp's read_event_it
	(bind '(note ins vol fx param) cell)
	(bind '(evvol f2t f2p) (it-volfx vol))
	(defq song (. player :find :song) samples (. song :find :samples)
		sample_mode (. song :find :sample_mode) prenv (. song :find :link_gxx)
		nins (length (. song :find :instruments)))
	;Impulse Tracker's bug of the instrument always read
	(cond
		((> ins 0) (. chan :insert :delayed_ins 0))
		((and (> note 0) (> (. chan :find :delayed_ins) 0))
			(setq ins (. chan :find :delayed_ins))
			(. chan :insert :delayed_ins 0)))
	(. chan :insert :fx 0)
	(defq out -1 key note not_same_ins :nil not_same_smp :nil new_invalid :nil
		is_porta :nil is_release :nil reset_env :nil reset_sus :nil use_ins_vol :nil
		candidate (. chan :find :ins) porta_offset :nil retrig_ins :nil)
	;a key off with an instrument plays it again in the old effects
	(and (. song :find :old_fx) (= note +note_off) (it-ins song (dec ins)) (setq retrig_ins :t))
	;a note of an instrument with no sample for it is had as no event
	(cond
		((> ins 0)
			(and (<= ins nins) (<= 1 note 120) (< (it-mapped song (dec ins) (dec note)) 0)
				(setq candidate (dec ins) note 0 ins 0 evvol 0 fx 0 param 0 f2t 0 f2p 0)))
		((<= 1 note 120)
			(cond
				((not (it-ins song (dec (. chan :find :old_ins)))) (setq new_invalid :t))
				((< (it-mapped song (dec (. chan :find :old_ins)) (dec note)) 0)
					(setq note 0 evvol 0 fx 0 param 0 f2t 0 f2p 0)))))
	(setq is_porta (or (= fx 7) (= fx 12) (= f2t 7))
		is_release (bits? (. chan :find :notes) (const (+ +itn_env_release +itn_fade))))
	(if (or (<= (. chan :find :period) 0) (bits? (. chan :find :notes) +itn_end)) (setq is_porta :nil))
	(when (and is_porta (= fx 15))
		(setq porta_offset :t)
		(unless prenv (it-unflag chan :notes +itn_env_end)))
	;the instrument
	(when (> ins 0)
		(defq i (dec ins) set_new :t)
		(and is_release (= key 0)
			(cond
				((not is_porta) (it-env-reset song chan :carry))
				((or prenv (bits? (. chan :find :notes) +itn_set))
					(setq is_porta :nil)
					(it-env-reset song chan :carry))))
		(and is_porta (= (. chan :find :ins) i) (not prenv)
			(if (it-same song chan i (dec key)) (setq set_new (not is_release))
				(setq not_same_ins :t not_same_smp :t)))
		(when set_new
			(chan-fx chan +fx_new_ins)
			(setq reset_env :t))
		(setq use_ins_vol :t)
		(. chan :insert :pers 0)
		(cond
			((it-ins song i)
				(when (and (= key 0) (not (bits? (. chan :find :notes) +itn_key_cut)))
					(and sample_mode (bits? (. chan :find :notes) +itn_end) (it-voicepos chan 0))
					(cond
						((= (. chan :find :ins) i) (chan-fx chan +fx_new_ins))
						((setq key (inc (. chan :find :key)))))
					(it-unflag chan :notes +itn_set))
				(when (and (/= (. chan :find :ins) i) (or (not is_porta) (not prenv)))
					(setq candidate i)
					(unless (it-same song chan i (dec key))
						(setq not_same_ins :t)
						(when (and is_porta (>= (defq s (it-sub song i key)) 0))
							(. chan :insert :volume (. (elem-get samples s) :find :volume))
							(setq use_ins_vol :nil)))))
			(:t (if sample_mode (. chan :insert :volume 0))
				(. chan :insert :fx 0)
				(setq new_invalid :t use_ins_vol :nil))))
	;the note
	(when (> key 0)
		(chan-fx chan +fx_new_note)
		(it-flag chan :notes +itn_set)
		(cond
			((= key +note_fade)
				(it-flag chan :notes +itn_fade)
				(setq reset_env :nil reset_sus :nil use_ins_vol :nil))
			((= key +note_cut)
				(it-flag chan :notes (const (+ +itn_end +itn_cut +itn_key_cut)))
				(. chan :insert :period 0)
				(it-patch player chan -1))
			((= key +note_off)
				(defq i (it-ins song (. chan :find :ins)))
				(it-flag chan :notes (if (and i (it-sus-check (. i :find :volume) (. chan :find :venv)))
					+itn_susexit +itn_release))
				(chan-fx chan +fx_key_off)
				(setq reset_env :nil reset_sus :nil)
				(if (= ins 0) (setq use_ins_vol :nil)))
			((not new_invalid)
				(unless is_porta (setq reset_env :t reset_sus :t))
				(when is_porta
					(cond
						((or not_same_ins (bits? (. chan :find :notes) +itn_end))
							(chan-fx chan +fx_new_ins)
							(it-unflag chan :notes (const (+ +itn_env_release +itn_susexit +itn_fade))))
						(:t (if (<= 1 key 121) (. chan :insert :key_porta (dec key)))
							(setq key 0)))))))
	;a note played: its sample from the start on the channel's voice
	(when (and (<= 1 key 121) (not new_invalid))
		(if (bits? (. chan :find :notes) +itn_cut) (setq use_ins_vol :t))
		(. chan :insert :key (setq key (dec key)))
		(it-unflag chan :notes +itn_end)
		(cond
			((>= (defq s (it-sub song candidate key)) 0)
				(defq smp (elem-get samples s) inst (elem-get (. song :find :instruments) candidate)
					rv (min 100 (. inst :find :rv)) rp (min 64 (. inst :find :rp)))
				(. chan :insert :delay 0)
				(setq out (+ key (. smp :find :xpo) (it-xpo song candidate key)))
				;a slide to another sample of a note that carries on
				(defq nna (. inst :find :nna) dct (. inst :find :dct) vsmp (if (> (. smp :find :length) 0) s -1))
				(and not_same_smp (= nna 1)
					(. chan :insert :period (it-period player (+ key (. smp :find :xpo)
						(it-xpo song candidate (. chan :find :key_porta))) (. chan :find :finetune))))
				;a slide to a note, done or not, cuts the note it slides from
				(when not_same_smp (. chan :insert :act 0) (setq dct 0))
				;the note it cuts, or that plays on as a past note, and a past
				;note of the same cut, let go or faded, libxmp's virt_setpatch
				(unless sample_mode
					(if (> dct 0) (it-dct player chan candidate vsmp key nna dct (. inst :find :dca)))
					(unless (and (. chan :find :voice) (= (. chan :find :act) 0)) (it-voice-room player))
					(it-nna player chan))
				(it-patch player chan vsmp)
				(scatter chan :act nna :v_ins candidate :v_smp vsmp :v_key key)
				(scatter chan :rvv (if (> rv 0) (random (inc rv)) 0)
					:rpv (if (> rp 0) (- (random (inc rp)) (/ rp 2)) 0))
				(if (> (. smp :find :length) 0) (. chan :insert :smp s)))
			(:t (. chan :insert :fx 0)
				(setq use_ins_vol :nil))))
	(and (or is_porta retrig_ins) prenv (> ins 0) (it-env-reset song chan :carry))
	(when (defq i (it-ins song candidate))
		(if (/= (. chan :find :ins) candidate) (it-env-reset song chan :all))
		(scatter chan :ins candidate :ins_fade (. i :find :fade)))
	;a new instrument when the last envelope has ended
	(when (and (> ins 0) (bits? (. chan :find :notes) +itn_env_end))
		(defq inst (if (< -1 candidate nins) (elem-get (. song :find :instruments) candidate))
			venv (if inst (. inst :find :volume)) rate (. chan :find :ins_fade))
		(if (or (not inst) (not (. venv :find :on)) (not (. venv :find :carry)) (= rate 0)
				(<= (. chan :find :fadeout) rate))
			(it-env-reset song chan :volume)
			(setq reset_env :nil)))
	(when reset_env
		(if (> note 0) (it-unflag chan :notes (const (+ +itn_env_release +itn_susexit +itn_fade))))
		(. chan :insert :fadeout 65536))
	(and reset_sus (> note 0) (it-unflag chan :notes +itn_sample_release))
	(when (and retrig_ins not_same_ins)
		(chan-fx chan +fx_new_ins)
		(it-voicepos chan 0)
		(. chan :insert :fadeout 65536)
		(it-unflag chan :notes (const (+ +itn_release +itn_susexit +itn_fade))))
	;the sample now played, and what a note takes from it
	(defq s (it-sub song (. chan :find :ins) (. chan :find :key)))
	(when (and (>= s 0) (>= out 0))
		(defq smp (elem-get samples s) ivib (. chan :find :ivib))
		(scatter chan :finetune (. smp :find :fin) :gvl (. smp :find :gvl) :ivib_sweep (. smp :find :vib_sweep))
		;the instrument's filter, a cutoff of 127 and no resonance none if not
		;slid to
		(defq inst (elem-get (. song :find :instruments) (. chan :find :ins)) ifc (. inst :find :ifc) ifr (. inst :find :ifr))
		(if (bits? ifc 128) (. chan :insert :f_cutoff (* 2 (- ifc 128))))
		(if (bits? ifr 128) (. chan :insert :f_res (* 2 (- ifr 128))))
		(scatter chan :f_env 256 :f_can (not is_porta))
		(elem-set ivib 1 (>> (+ (. smp :find :vib_rate) 2) 2))
		(elem-set ivib 2 (. smp :find :vib_depth))
		(elem-set ivib 3 (. smp :find :vib_wave))
		(elem-set (. chan :find :vib) 0 0)
		(elem-set (. chan :find :trem) 0 0))
	(scatter chan :delay 0 :tremor_up 0 :tremor_down 0 :arp (list 0) :arp_at 0)
	;a slide to a note's rate is what the effects of this row add to it
	(if is_porta (. chan :insert :porta 0))
	(when (and (>= s 0) (>= out 0))
		(defq pan (. (elem-get samples s) :find :pan))
		(if (< pan 0) (setq pan (. (elem-get (. song :find :instruments) (. chan :find :ins)) :find :pan)))
		(if (>= pan 0) (. chan :insert :pan pan))
		(cond
			((bits? (. chan :find :notes) +itn_cut) (it-env-reset song chan :all))
			((or (not porta_offset) prenv) (it-env-reset song chan :carry)))
		(it-unflag chan :notes +itn_cut))
	(when (and (> evvol 0) (or (not (bits? (. chan :find :notes) +itn_cut)) (> ins 0)))
		(. chan :insert :volume (dec evvol))
		(chan-fx chan +fx_new_vol))
	(. chan :insert :offset (logand (. chan :find :offset) (lognot 0xffff)))
	;the effects, the volume column's after
	(it-fx player chan fx param 0 note ins)
	(it-fx player chan f2t f2p 1 note ins)
	;the period of the note played, or slid to
	(when (and (>= s 0) (>= out 0))
		(defq per (it-period player out (. chan :find :finetune)))
		(. chan :insert :target per)
		(if (or (< (. chan :find :period) 1) (not is_porta)) (. chan :insert :period per)))
	(when (>= s 0)
		(if (>= out 0) (. chan :insert :note out))
		;an offset past its sample's end starts at 0, or at its end in the
		;old effects
		(defq off (. chan :find :offset))
		(and (or (>= out 0) porta_offset)
			(it-voicepos chan (if (and (bits? (. chan :find :fx) +fx_offset)
				(or (. song :find :old_fx) (< off (. (elem-get samples (. chan :find :sample)) :find :length)))) off 0)))
		(and use_ins_vol (not (bits? (. chan :find :fx) +fx_new_vol))
			(. chan :insert :volume (. (elem-get samples s) :find :volume)))))

(defun it-read-event (player chan cell)
	; (it-read-event player chan cell)
	(if (> (second cell) 0) (. chan :insert :old_ins (second cell)))
	(if (bits? (. chan :find :notes) +itn_sample_end) (it-flag chan :notes +itn_end))
	(it-event player chan cell))

(defun it-row (player)
	; (it-row player)
	;the first tick of a row: each channel's cell, or held back by a note
	;delay to a later tick of it, and not again for a row delay
	(defq song (. player :find :song) channels (. player :find :channels)
		cells (elem-get (. song :find :patterns) (elem-get (. song :find :orders) (. player :find :order)))
		at (* (. player :find :row) channels +cell_size))
	(each (lambda (chan)
			(defq c (+ at (* (!) +cell_size)))
			(bind '(note ins vol fx param) (slice cells c (+ c +cell_size)))
			;a speed is had first, a delay is of it
			(and (= fx 1) (> param 0) (. player :insert :speed param))
			(defq rs (. player :find :rowdelay_set))
			(cond
				((and (= fx 19) (= (>> param 4) 13))
					(scatter chan :fx 0 :delay (inc (max 1 (logand param 15)))
						:delayed (list note ins vol fx param))
					(if (> ins 0) (. chan :insert :delayed_ins ins)))
				((or (= rs 0) (and (bits? rs +rowdelay_first) (> (. player :find :rowdelay) 0)))
					(it-read-event player chan (list note ins vol fx param)))))
		(. player :find :chans)))

(defun it-volumes (player chan first_frame)
	; (it-volumes player chan first_frame)
	;the slides of the channel's volumes and the global volume: each tick
	;but a row's first, and fine at its first
	(defq fx (. chan :find :fx) volume (. chan :find :volume) gvol (. player :find :gvol)
		master (. chan :find :mastervol))
	(cond
		(first_frame
			(if (bits? fx +fx_fvslide) (setq volume (+ volume (. chan :find :fvslide))))
			(and (bits? fx +fx_fvslide2)
				(or (= (defq rs (. player :find :rowdelay_set)) 0) (bits? rs +rowdelay_first))
				(setq volume (+ volume (. chan :find :fvslide2))))
			(if (bits? fx +fx_tfvslide) (setq master (+ master (. chan :find :tfslide))))
			(if (bits? fx +fx_gslide) (setq gvol (+ gvol (. chan :find :gfslide)))))
		(:t (if (bits? fx +fx_gslide) (setq gvol (+ gvol (. chan :find :gslide))))
			(if (bits? fx +fx_vslide) (setq volume (+ volume (. chan :find :vslide))))
			(if (bits? fx +fx_vslide2) (setq volume (+ volume (. chan :find :vslide2))))
			(if (bits? fx +fx_tvslide) (setq master (+ master (. chan :find :tslide))))))
	(scatter chan :volume (max 0 (min 64 volume)) :mastervol (max 0 (min 64 master)))
	(. player :insert :gvol (max 0 (min 128 gvol))))

(defun it-play (player chan frame &optional act)
	; (it-play player chan frame [act])
	;a tick of a channel: its volume, pitch and pan as its effects, its
	;instrument's envelopes and its fade move them. A past note's act, 2
	;let go, 3 faded, is as its note's own
	(defq song (. player :find :song) first_frame (= (% frame (. player :find :speed)) 0)
		old_fx (. song :find :old_fx) fx (. chan :find :fx))
	(and (not first_frame) (bits? fx +fx_tempo)
		(. player :insert :bpm (max 32 (min 255 (+ (. player :find :bpm) (. chan :find :tempo))))))
	;a note delayed to this tick
	(when (> (. chan :find :delay) 0)
		(. chan :insert :delay (dec (. chan :find :delay)))
		(when (= (. chan :find :delay) 0)
			(it-read-event player chan (. chan :find :delayed))
			(setq fx (. chan :find :fx))))
	;a MIDI macro, the filter's cutoff or resonance
	(it-macro-tick player chan frame)
	(cond
		((not (. chan :find :voice))
			(it-volumes player chan first_frame)
			(scatter chan :volume_out 0 :period_out 0))
		((defq ins (it-ins song (. chan :find :ins)))
			;a retrig, or a cut, every so many ticks
			(when (bits? fx +fx_retrig)
				(. chan :insert :retrig_count (defq count (dec (. chan :find :retrig_count))))
				(when (<= count 0)
					(defq kind (. chan :find :retrig_type))
					(if (< kind 16) (it-voicepos chan 0) (it-flag chan :notes +itn_end))
					(bind '(add mult by) (elem-get +st3_retrig kind))
					(. chan :insert :volume (/ (* (+ (. chan :find :volume) add) mult) by))
					(. chan :insert :retrig_count (. chan :find :retrig_val))))
			;a sample's sustain loop let go
			(and (. chan :find :loop)
				(not (eql (. chan :find :released) (bits? (. chan :find :notes) +itn_sample_release)))
				(progn (it-loop player chan) (. chan :insert :loop_change :t)))
			(it-volumes player chan first_frame)
			;slides of the pitch
			(defq period (. chan :find :period) linear (eql (. player :find :pitch) :linear))
			(cond
				(first_frame (if (bits? fx +fx_fbend) (setq period (+ period (. chan :find :fbend)))))
				(:t (if (bits? fx +fx_bend) (setq period (+ period (. chan :find :bend))))
					(when (and (bits? fx +fx_porta) (> (defq to (. chan :find :target)) 0))
						(defq by (. chan :find :porta))
						(when (if (> (. chan :find :pdir) 0) (>= (setq period (+ period by)) to)
								(<= (setq period (- period by)) to))
							(setq period to)
							(scatter chan :pdir 0 :fx (setq fx (logand fx (lognot +fx_porta))))))))
			(if linear (setq period (max 0 (min (const (* 0x1e00 64)) period))))
			(. chan :insert :period period)
			;a slide of the pan
			(if (bits? fx +fx_pslide)
				(. chan :insert :pan (max 0 (min 255 (+ (. chan :find :pan)
					(if first_frame (. chan :find :pfslide) (. chan :find :pslide)))))))
			;the volume envelope, and the fade once let go or at its end
			(defq notes (. chan :find :notes) pers (. chan :find :pers) venv (. ins :find :volume) fade :nil
				release (or (bits? notes +itn_env_release) (eql act 2)) key_off (bits? fx +fx_key_off))
			(and release (or (not (. venv :find :on)) (. venv :find :loop)) (setq fade :t))
			(unless (bits? pers +itp_venv)
				(. chan :insert :venv (it-env-step venv (. chan :find :venv) release key_off)))
			(defq vx (. chan :find :venv) ve (ft2-env-value venv vx 64))
			(when (it-env-end venv vx)
				(if (= ve 0) (it-flag chan :notes +itn_end))
				(it-flag chan :notes +itn_env_end))
			(case (it-env-fade venv vx)
				(-1 (it-flag chan :notes +itn_end))
				(1 (it-flag chan :notes +itn_fade)))
			(if (or (bits? (. chan :find :notes) +itn_fade) (eql act 3)) (setq fade :t))
			(when fade
				(cond
					((> (. chan :find :fadeout) (defq rate (. chan :find :ins_fade)))
						(. chan :insert :fadeout (- (. chan :find :fadeout) rate)))
					(:t (. chan :insert :fadeout 0)
						(it-flag chan :notes +itn_end))))
			;the volume heard: a swing, a tremolo, the fade, the envelope, the
			;global, the channel's and the instrument's, and a tremor
			(defq volume (/ (* (. chan :find :volume) (- 100 (. chan :find :rvv))) 100))
			(when (bits? fx +fx_trem)
				(setq volume (+ volume (/ (it-lfo (defq lfo (. chan :find :trem))) 64)))
				(if (or (not first_frame) (not old_fx)) (it-lfo-step lfo)))
			(setq volume (>> (* (max 0 (min 64 volume)) (. chan :find :fadeout)) 6)
				volume (>> (* (/ (* ve (. player :find :gvol) (. chan :find :mastervol)) 128) volume) 18)
				volume (>> (* volume (. ins :find :vol) (. chan :find :gvl)) 12))
			(when (bits? fx +fx_tremor)
				(defq count (. chan :find :tremor_count))
				(cond
					((= count 0) (setq count (logior (. chan :find :tremor_up) 128)))
					((= count 128) (setq count (. chan :find :tremor_down))))
				(. chan :insert :tremor_count (setq count (dec count)))
				(unless (bits? count 128) (setq volume 0)))
			(if (. chan :find :mute) (setq volume 0))
			;the pitch envelope, the instrument's vibrato and a vibrato
			(defq fenv (. ins :find :pitch))
			(unless (bits? pers +itp_fenv)
				(. chan :insert :fenv (it-env-step fenv (. chan :find :fenv) release key_off)))
			(defq fe (ft2-env-value fenv (. chan :find :fenv) 0) ivib (. chan :find :ivib)
				sweep (. chan :find :ivib_sweep) vibrato (/ (it-lfo ivib) (* 64 (inc sweep))))
			(it-lfo-step ivib)
			(. chan :insert :ivib_sweep (if (> sweep 1) (- sweep 2) 0))
			(when (bits? fx +fx_vib)
				(defq lfo (. chan :find :vib) vib (* 64 (/ (it-lfo lfo) (if old_fx 512 1024))))
				(setq vibrato (if old_fx (+ vibrato vib) (- vibrato vib)))
				(if (or (not first_frame) (not old_fx)) (it-lfo-step lfo)))
			;the pitch heard: glissando to semitones, the envelope, an arpeggio
			(defq arp (. chan :find :arp) up (elem-get arp (. chan :find :arp_at)) note (. chan :find :note)
				pitch (it-pitch player (max 6 (+ (max 6 period) vibrato))))
			(. chan :insert :arp_at (% (inc (. chan :find :arp_at)) (length arp)))
			(when (and (bits? (. chan :find :notes) +itn_gliss) (bits? fx +fx_porta))
				(defq bend (- pitch (* note 12800)))
				(setq pitch (+ (* note 12800) (* 12800 (/ (+ bend (if (>= bend 0) 6400 -6400)) 12800)))))
			(and (>= (. chan :find :fenv) 0) (not (. fenv :find :filter)) (setq pitch (+ pitch (<< fe 7))))
			;the filter, its cutoff the envelope's share of it if it is a
			;filter's
			(defq cutoff (. chan :find :f_cutoff) res (. chan :find :f_res))
			(when (and (>= (. chan :find :fenv) 0) (. fenv :find :filter))
				(if (< fe 254) (. chan :insert :f_env fe))
				(setq cutoff (>> (* cutoff (. chan :find :f_env)) 8)))
			(when (or (< (setq cutoff (min cutoff 255)) 254) (> res 0) (. chan :find :f_can))
				(it-filter player chan cutoff res)
				(. chan :insert :f_can :nil))
			(setq pitch (+ pitch (* 12800 up)))
			;the pan envelope, and a panbrello
			(unless (bits? pers +itp_penv)
				(. chan :insert :penv (it-env-step (. ins :find :panning) (. chan :find :penv) release key_off)))
			(defq pe (ft2-env-value (. ins :find :panning) (. chan :find :penv) 32) pb 0 pan (. chan :find :pan))
			(when (bits? fx +fx_panbrello)
				(setq pb (/ (it-lfo (defq lfo (. chan :find :panbrello))) 512))
				(if first_frame (it-lfo-step lfo)))
			(scatter chan :volume_out volume :period_out (it-step player (min pitch (const (* 149 12800))))
				:pan_out (max 0 (min 255 (+ pan pb (/ (* (- pe 32) (- 128 (abs (- pan 128)))) 32)
					(* 4 (. chan :find :rpv))))))
			(if (bits? (. chan :find :notes) +itn_susexit) (it-flag chan :notes +itn_env_release)))))

(defun it-advance (player)
	; (it-advance player)
	;on to the next row, or where a jump or break says, or the row again for
	;a row delay, past the orders that mark, 254, to the song's end, 255. A
	;row played before, not by a loop, means it has gone round: it has ended
	(defq song (. player :find :song) orders (. song :find :orders) order (. player :find :order)
		row (. player :find :row) next :nil again :nil)
	(scatter player :frame 0 :pdelay 0)
	(cond
		((. player :find :pbreak)
			(. player :insert :loop_dest -1)
			(if (>= (defq to (. player :find :jump_to)) 0) (setq order (dec to)))
			(setq next :t))
		(:t (cond
				((= (. player :find :rowdelay) 0)
					(setq row (inc row))
					(. player :insert :rowdelay_set 0))
				(:t (. player :insert :rowdelay (dec (. player :find :rowdelay)))
					(setq again :t)))
			(when (>= (defq to (. player :find :loop_dest)) 0)
				(setq row to again :nil)
				;a pattern's loop plays its rows again, they are not a sign of
				;the song going round
				(defq visited (. player :find :visited))
				(each! (# (. visited :erase (+ (* order 1024) (!)))) '() row 1024))
			(if (>= row (elem-get (. song :find :rows) (elem-get orders order))) (setq next :t))))
	(when next
		(setq row (. player :find :jumpline) again :nil)
		(while (progn
				(setq order (inc order))
				(when (or (>= order (length orders)) (= (elem-get orders order) 255))
					(. player :insert :ended :t)
					(setq order (if (< (. song :find :restart) (length orders)) (. song :find :restart) 0)))
				(>= (elem-get orders order) (length (. song :find :patterns)))))
		(if (>= row (elem-get (. song :find :rows) (elem-get orders order))) (setq row 0)))
	(unless again
		(defq visited (. player :find :visited))
		(if (. visited :find (defq key (+ (* order 1024) row))) (. player :insert :ended :t) (. visited :insert key)))
	(scatter player :order order :row row :pbreak :nil :jump_to -1 :jumpline 0 :loop_dest -1))

(defun it-tick (player)
	; (it-tick player)
	;one tick of an IT, as libxmp plays one: a row's cells read at its first
	;tick, every channel played, and on to the next row at the end of its
	;last
	(defq chans (. player :find :chans) voices (. player :find :voices)
		instrument_mode (not (. (. player :find :song) :find :sample_mode)))
	;a sample come to its end in the mixer, its voice let go of in
	;instrument mode
	(each (lambda (chan)
			(when (and (. chan :find :voice) (not (. chan :find :trigger))
					(< (elem-get voices (* (!) +mix_size)) 0))
				(it-flag chan :notes +itn_sample_end)
				(if instrument_mode (. chan :insert :voice :nil))))
		chans)
	(each (lambda (past)
			(and (not (. past :find :trigger)) (< (elem-get voices (* (. past :find :slot) +mix_size)) 0)
				(it-past-drop player past)))
		(cat (. player :find :past)))
	(defq frame (. player :find :frame) song (. player :find :song))
	(each (# (it-unflag %0 :fx +fx_key_off)) chans)
	(if (= frame 0) (it-row player))
	(each (# (it-play player %0 frame)) chans)
	;the past notes, as they are let go of or fade, let go of at the next
	;row if cut, at their note's end, or once not heard
	(each (lambda (past)
			(defq act (. past :find :act))
			(cond
				((and (= frame 0) (or (= act 0) (not (it-ins song (. past :find :ins)))))
					(it-past-drop player past))
				(:t (it-play player past frame act)
					(if (or (bits? (. past :find :notes) +itn_end) (= (. past :find :volume_out) 0))
						(it-past-drop player past)))))
		(cat (. player :find :past)))
	(. player :insert :rowdelay_set (logand (. player :find :rowdelay_set) (lognot +rowdelay_first)))
	(tracker-voices player)
	(. player :insert :frame (setq frame (inc frame)))
	(if (>= frame (* (. player :find :speed) (inc (. player :find :pdelay)))) (it-advance player)))

;;;;;;;;;;;;;;;;;;;;
; the song, played
;;;;;;;;;;;;;;;;;;;;

(defun tracker-player (song rate)
	; (tracker-player song rate) -> player
	;a player of a song at a rate of frames a second
	(defq channels (. song :find :channels) mode (. song :find :mode)
		player (scatter (Emap) :song song :rate rate :mode mode :channels channels
			:chans (if (eql mode :it)
				(map (# (it-channel %0 %1 %2 (!))) (. song :find :pans) (. song :find :chvols) (. song :find :mutes))
				(map (case mode (:st3 (const st3-channel)) (:ft2 (const ft2-channel)) (:t (const pt-channel)))
					(. song :find :pans)))
			:voices (apply (const nums) (apply (const cat) (map! (lambda () '(-1 0 0 0 0 -1 0 0 0 0 0 0 0))
				'() 0 (+ channels (defq extra (if (and (eql mode :it) (not (. song :find :sample_mode))) +it_voices 0))))))
			;an IT's past notes, and the voices free for them
			:past (list) :free (map! (# (+ channels (!))) '() 0 extra) :filters (Fmap 101)
			:samples (map (# (. %0 :find :data)) (. song :find :samples))
			:pitch (. song :find :pitch)
			:clock (case mode (:st3 +st3_clock) (:ft2 +ft2_clock) (:t +amiga_clock))
			:scale (case mode (:mod 1) (:it 16) (:t 64)) :stepped (eql mode :it)
			:speed (. song :find :speed) :bpm (. song :find :tempo) :gvol (. song :find :global)
			:tick 0 :frame 0 :row 0 :order 0 :jump :nil :jump_order 0 :jump_row 0 :delay 0
			:repeat :nil :pbreak :nil :jump_to -1 :jumpline 0 :loop_to -1 :pdelay 0
			:rowdelay 0 :rowdelay_set 0 :loop_dest -1 :carry 0 :visited (Fset 101) :ended :nil :acc (nums)
			:gain (/ (* 512 (ifn (. song :find :mix) 48)) 48)))
	(if (eql mode :mod) (. (. player :find :visited) :insert 0) (flow-start player))
	;2^(n/768) of 16.16, to 2, for linear periods and the steps of an IT
	(if (or (eql (. song :find :pitch) :linear) (eql mode :it))
		(. player :insert :exp2 (apply (const nums) (map! (# (n2i (* (const (n2d 65536))
			(pow (const (n2d 2)) (/ (n2d (!)) (const (n2d 768))))))) '() 0 769))))
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
		(:t (case mode (:st3 (st3-tick player)) (:it (it-tick player)) (:t (ft2-tick player)))
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
