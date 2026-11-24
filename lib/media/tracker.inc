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
; plays them, a period 64 times its, and a channel gives the mixer its step,
; and so are those of MED and OctaMED, :med, the med- functions. An AHX's
; or a HivelyTracker's, :ahx, is played by its replayer, lib/media/ahx.inc,
; its voices given the mixer here.
;
; What the players have in common is written once, before them: an
; effect's memory, a channel's flags, a note delay, a slide to a note, a
; tremor, a retrig's volume, the row's cells. And S3M, XM, IT and MED
; step through a song's rows by the one (flow-advance)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./mod.inc")
(import "./it.inc")
(import "./med.inc")
(import "./ahx.inc")

(ffi "lib/media/media_mix" media-mix)

;a voice of the mixer, media_mix, so many numbers of the player's :voices,
;as lib/media/lisp_tracker.vp has them: its sample, -1 none, where it is in
;it and its step a frame, 16.16, its weights left and right, its loop's
;start, -1 none, its end, its flags, its filter's a0 b0 b1 and last two
;outputs, what is added to where it is for the sample played, and its
;ring, a sample it is times, where that is and its step
(enums +mix 0
	(enum sample pos step left right loop end flags fa0 fb0 fb1 fy1 fy2 round ring ring_pos ring_step))

(defq +mix_pingpong 1 +mix_back 2
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
	;what a channel does this row, :fx bits, Scream Tracker's first
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

(defun voice-loop (voices at ls le len pingpong)
	; (voice-loop voices at ls le len pingpong)
	;a voice's loop, from ls to le, back and forth if so, or none, ls less
	;than 0, and its end the sample's
	(elem-set voices (+ at +mix_loop) ls)
	(elem-set voices (+ at +mix_end) (if (>= ls 0) le len))
	(elem-set voices (+ at +mix_flags) (if (and (>= ls 0) pingpong) +mix_pingpong 0)))

(defun tracker-voices (player)
	; (tracker-voices player)
	;each channel's voice for the mixer, as the channel is now
	(defq voices (. player :find :voices) song (. player :find :song) rate (. player :find :rate)
		gain (. player :find :gain) scale (. player :find :scale) clock (<< (. player :find :clock) 16)
		linear (eql (. player :find :pitch) :linear) stepped (. player :find :stepped)
		chans (. player :find :chans) n (length chans)
		round (if (eql (. player :find :mode) :mod) 0 0x8000))
	;a channel's voice is its place, an IT's past note's its :slot
	(each (lambda (chan)
			(defq at (* (if (< (!) n) (!) (. chan :find :slot)) +mix_size) sample (. chan :find :sample))
			(when (. chan :find :stop)
				(scatter chan :stop :nil :trigger :nil)
				(elem-set voices at -1)
				;a voice let go of, its filter from nothing when next it is
				(elem-set voices (+ at +mix_fy1) 0)
				(elem-set voices (+ at +mix_fy2) 0))
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
				(elem-set voices (+ at +mix_pos) (<< start 16))
				(voice-loop voices at ls le len pingpong)
				;the nearest sample, but of a MOD the one it is at
				(elem-set voices (+ at +mix_round) round))
			(when (and (. chan :find :switch) (not (. chan :find :trigger)))
				;another sample where the voice is along, a MED synth sound's
				;next waveform: round its loop if past its end
				(. chan :insert :switch :nil)
				(when (and (>= sample 0) (>= (elem-get voices at) 0))
					(defq s (elem-get (. song :find :samples) sample) len (. s :find :length)
						ls (. s :find :loop_start) le (if (>= ls 0) (. s :find :loop_end) len)
						pos (elem-get voices (+ at +mix_pos)))
					(and (>= (>> pos 16) le) (>= ls 0)
						(setq pos (+ (<< ls 16) (% (- pos (<< ls 16)) (<< (- le ls) 16)))))
					(elem-set voices at sample)
					(elem-set voices (+ at +mix_pos) pos)
					(voice-loop voices at ls le len (. s :find :pingpong))))
			(when (. chan :find :loop_change)
				;a sustain loop let go, the voice on round the loop
				(. chan :insert :loop_change :nil)
				(when (>= (elem-get voices at) 0)
					(bind '(ls le pingpong) (. chan :find :loop))
					(voice-loop voices at ls le (. (elem-get (. song :find :samples) sample) :find :length) pingpong)))
			(. chan :insert :trigger :nil)
			(defq period (. chan :find :period_out) pan (ifn (. chan :find :pan_out) (. chan :find :pan))
				weight (/ (* (. chan :find :volume_out) gain) scale))
			(elem-set voices (+ at +mix_step) (cond
				((<= period 0) 0)
				(stepped period)
				(linear (ft2-step player period))
				((/ clock (* period rate)))))
			(elem-set voices (+ at +mix_left) (/ (* weight (- 256 pan)) 256))
			(elem-set voices (+ at +mix_right) (/ (* weight pan) 256))
			;a stepped player's filter, an IT's, a0 0 none
			(when stepped
				(elem-set voices (+ at +mix_fa0) (. chan :find :fa0))
				(elem-set voices (+ at +mix_fb0) (. chan :find :fb0))
				(elem-set voices (+ at +mix_fb1) (. chan :find :fb1))))
		(if (nempty? (defq past (. player :find :past))) (cat chans past) chans)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; what the players share
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun chan-mem (chan key param)
	; (chan-mem chan key param) -> param
	;an effect's memory of its number, under a key of the channel: 0 is the
	;last that was not
	(if (= param 0) (. chan :find key) (. chan :insert key param) param))

(defun chan-flag (chan key bits)
	; (chan-flag chan key bits)
	;bits of a key of a channel, or of the player, set
	(. chan :insert key (logior (. chan :find key) bits)))

(defun chan-unflag (chan key bits)
	; (chan-unflag chan key bits)
	(. chan :insert key (logand (. chan :find key) (lognot bits))))

(defun chan-fx (chan bit)
	; (chan-fx chan bit)
	;what a channel does this row, a bit of its :fx
	(chan-flag chan :fx bit))

(defun chan-delay-due (chan)
	; (chan-delay-due chan) -> :t | :nil
	;a tick of a note delay: :t at the tick its cell is to be read
	(when (> (defq delay (. chan :find :delay)) 0)
		(. chan :insert :delay (setq delay (dec delay)))
		(= delay 0)))

(defun chan-porta-way (chan)
	; (chan-porta-way chan)
	;which way a slide to a note goes, from its :period to its :target
	(. chan :insert :pdir (if (< (. chan :find :period) (. chan :find :target)) 1 -1)))

(defmacro tracker-porta (chan period fx)
	; (tracker-porta chan period fx)
	;a tick of a slide to a note: the period moved by the channel's :porta,
	;the way of its :pdir, to its :target and no further, and the slide
	;over once there. period and fx are the caller's own, and are set
	(static-qq (when (and (bits? ,fx +fx_porta) (> (defq to (. ,chan :find :target)) 0))
		(defq by (. ,chan :find :porta))
		(when (if (> (. ,chan :find :pdir) 0) (>= (setq ,period (+ ,period by)) to)
				(<= (setq ,period (- ,period by)) to))
			(setq ,period to)
			(scatter ,chan :pdir 0 :fx (setq ,fx (logand ,fx (const (lognot +fx_porta)))))))))

(defun chan-tremor-off (chan)
	; (chan-tremor-off chan) -> :t | :nil
	;a tick of a tremor, so many ticks on then so many off, Scream Tracker's
	;and Impulse Tracker's: :t while it is off
	(defq count (. chan :find :tremor_count))
	(cond
		((= count 0) (setq count (logior (. chan :find :tremor_up) 128)))
		((= count 128) (setq count (. chan :find :tremor_down))))
	(. chan :insert :tremor_count (setq count (dec count)))
	(not (bits? count 128)))

(defun chan-gslide (chan param)
	; (chan-gslide chan param)
	;a slide of the global volume, by its number: x0 up and 0y down each
	;tick, xF up and Fy down at a row's first
	(defq x (>> param 4) y (logand param 15))
	(cond
		((and (= y 15) (/= x 0)) (scatter chan :gslide 0 :gfslide x))
		((and (= x 15) (/= y 0)) (scatter chan :gslide 0 :gfslide (neg y)))
		((scatter chan :gslide (if (/= x 0) x (neg y)) :gfslide 0)))
	(chan-fx chan +fx_gslide))

(defun retrig-volume (volume kind)
	; (retrig-volume volume kind) -> volume
	;a volume as a retrig of that kind changes it, +st3_retrig
	(bind '(add mult by) (elem-get +st3_retrig kind))
	(/ (* (+ volume add) mult) by))

(defun tracker-sine (pos)
	; (tracker-sine pos) -> num
	;the sine of a vibrato or a tremolo at pos, 0 to 63, -255 to 255
	(if (< pos 32) (elem-get +tracker_sine pos) (neg (elem-get +tracker_sine (- pos 32)))))

(defun tracker-row (player &optional row)
	; (tracker-row player [row]) -> (cells at)
	;the cells of the pattern the player is at, and where in them its row
	;starts, or that row of it: a channel's cell is +cell_size on from the last
	(defq song (. player :find :song))
	(list (elem-get (. song :find :patterns) (elem-get (. song :find :orders) (. player :find :order)))
		(* (ifn row (. player :find :row)) (. player :find :channels) +cell_size)))

(defun flow-key (order row)
	; (flow-key order row) -> key
	;a row of an order as the rows played are kept, the player's :visited
	(+ (* order 16384) row))

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
			(setq period (pt-period player chan (+ (mod-nearest (. chan :find :period))
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
	(bind '(cells at) (tracker-row player))
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
				(each! (# (. visited :erase (flow-key order (!)))) '() row +mod_rows)))
		((>= (setq row (inc row)) 64) (setq row 0 order (inc order))))
	(when (>= order orders) (setq order (. song :find :restart)) (. player :insert :ended :t))
	(if (. visited :find (defq key (flow-key order row))) (. player :insert :ended :t) (. visited :insert key))
	(scatter player :order order :row row :jump :nil :looping :nil))

(defun pt-tick (player)
	; (pt-tick player)
	;one tick of a MOD
	(defq tick (. player :find :tick) chans (. player :find :chans))
	(cond
		((and (= tick 0) (not (. player :find :repeat))) (pt-row player))
		;a length only: what moves a MOD on is of its rows
		((. player :find :measure))
		((each (# (pt-effect player %0 tick)) chans)))
	(unless (. player :find :measure) (tracker-voices player))
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
	;second at C-4, as Scream Tracker has it, its table 32 times as fine
	;before it is moved down an octave a time, as OpenMPT has it: else the
	;bits lost are 15 cents
	(defq n (dec note))
	(if (> c2spd 0) (/ (* 8363 (>> (<< (elem-get +st3_periods (% n 12)) 5) (/ n 12))) (* 2 c2spd)) 0))

(defun st3-wave (wave pos)
	; (st3-wave wave pos) -> num
	;a vibrato's or tremolo's wave at pos, 0 to 63, -255 to 255: sine, a
	;ramp down, a square of 255 and 0, or random
	(case (logand wave 3)
		(1 (- 255 (* pos 8)))
		(2 (if (< pos 32) 255 0))
		(3 (- (random 512) 256))
		(:t (tracker-sine pos))))

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
	(chan-porta-way chan)
	(chan-fx chan +fx_porta))

(defun st3-effect (player chan effect param note row)
	; (st3-effect player chan effect param note row)
	;what an effect of a row does at once, and sets going for its ticks
	(case effect
		(1 (if (> (setq param (chan-mem chan :mem param)) 0) (. player :insert :speed param)))
		(2 (scatter player :pbreak :t :jump_to param :jumpline 0))
		(3 (scatter player :pbreak :t :jumpline (+ (* 10 (>> param 4)) (logand param 15))))
		(4 (tracker-vslide chan param :mem :t))
		((5 6)
			;E down, F up, xx each tick, Fx fine and Ex extra fine at once
			(defq param (chan-mem chan :mem param) x (>> param 4) y (logand param 15)
				way (if (= effect 5) 1 -1))
			(cond
				((= x 15) (when (> y 0) (. chan :insert :fbend (* way 4 y)) (chan-fx chan +fx_fbend)))
				((= x 14) (when (> y 0) (. chan :insert :fbend (* way y)) (chan-fx chan +fx_fbend)))
				(:t (. chan :insert :bend (* way 4 param)) (chan-fx chan +fx_bend))))
		(7 (setq param (chan-mem chan :porta_mem param))
			(when (> param 0) (scatter chan :mem param :porta (* 4 param)))
			(if (>= (. chan :find :ins) 0) (st3-toneporta player chan note)))
		((8 21)
			;H vibrato, U fine
			(setq param (chan-mem chan :vib_mem param))
			(if (> param 0) (. chan :insert :mem param))
			(defq x (>> param 4) y (logand param 15))
			(if (> y 0) (. chan :insert :vib_depth (if (= effect 8) (* y 4) y)))
			(if (> x 0) (. chan :insert :vib_rate x))
			(chan-fx chan +fx_vib))
		(9 (setq param (chan-mem chan :mem param))
			(scatter chan :tremor_up (max 1 (>> param 4)) :tremor_down (max 1 (logand param 15)))
			(chan-fx chan +fx_tremor))
		(10 (if (> (setq param (chan-mem chan :mem param)) 0)
				(. chan :insert :arp (list 0 (>> param 4) (logand param 15)))))
		(11 (chan-fx chan +fx_vib) (tracker-vslide chan param :mem :t))
		(12 (if (>= (. chan :find :ins) 0) (st3-toneporta player chan note))
			(tracker-vslide chan param :mem :t))
		(15 (setq param (chan-mem chan :mem param))
			(if (> note 0) (. chan :insert :offset (<< param 8))))
		(17 (when (> (setq param (chan-mem chan :mem param)) 0)
				(scatter chan :retrig_val param :retrig_type (>> param 4)))
			(if (> note 0) (. chan :insert :retrig_count (inc (logand (. chan :find :retrig_val) 15))))
			(chan-fx chan +fx_retrig))
		(18 (setq param (chan-mem chan :mem param))
			(if (> (logand param 15) 0) (. chan :insert :trem_depth (logand param 15)))
			(if (> (>> param 4) 0) (. chan :insert :trem_rate (>> param 4)))
			(chan-fx chan +fx_trem))
		(19 (setq param (chan-mem chan :mem param))
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
							(. player :insert :loop_dest (. chan :find :loop_row))
							;after its last time round a loop starts after it
							(. chan :insert :loop_row (inc row))))
					(:t (. chan :insert :loop_count y)
						(. player :insert :loop_dest (. chan :find :loop_row)))))
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
	(defq row (. player :find :row))
	(bind '(cells at) (tracker-row player))
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
	(when (chan-delay-due chan)
		(st3-event player chan (. chan :find :delayed) (. player :find :row))
		(setq fx (. chan :find :fx)))
	(when (>= (. chan :find :ins) 0)
		(defq volume (. chan :find :volume) period (. chan :find :period))
		;a retrig, or a cut, every so many ticks
		(when (bits? fx +fx_retrig)
			(. chan :insert :retrig_count (defq count (dec (. chan :find :retrig_count))))
			(when (<= count 0)
				(if (< (. chan :find :retrig_type) 16) (scatter chan :trigger :t :start 0))
				(setq volume (retrig-volume volume (. chan :find :retrig_type)))
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
				(tracker-porta chan period fx)))
		(. chan :insert :period period)
		;the volume heard: a tremolo, the global volume, and a tremor
		(setq volume (* volume (. chan :find :fade)))
		(when (bits? fx +fx_trem)
			(defq pos (. chan :find :trem_pos) rate (. chan :find :trem_rate))
			(if (> rate 0)
				(setq volume (+ volume (/ (* (st3-wave (. chan :find :trem_wave) pos) (. chan :find :trem_depth)) 64))))
			(unless tick0 (. chan :insert :trem_pos (logand (+ pos rate) 63))))
		(setq volume (* (max 0 (min 64 volume)) (. player :find :gvol)))
		(and (bits? fx +fx_tremor) (chan-tremor-off chan) (setq volume 0))
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
	;on to the next row, or where a jump or break says, or the row again for
	;an IT's row delay, past the orders of no pattern, and, of a song that
	;has them, its :markers, to its end at an order of 255. A row played
	;before, not by a loop, means it has gone round: it has ended
	(defq song (. player :find :song) orders (. song :find :orders) order (. player :find :order)
		row (. player :find :row) rows (. song :find :rows) npat (length (. song :find :patterns))
		markers (. song :find :markers) next :nil again :nil)
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
				(each! (# (. visited :erase (flow-key order (!)))) '() row (elem-get rows (elem-get orders order))))
			(if (>= row (elem-get rows (elem-get orders order))) (setq next :t))))
	(when next
		(setq row (. player :find :jumpline) again :nil)
		(while (progn
				(setq order (inc order))
				(when (or (>= order (length orders)) (and markers (= (elem-get orders order) 255)))
					(. player :insert :ended :t)
					(setq order (if (< (. song :find :restart) (length orders)) (. song :find :restart) 0)))
				(>= (elem-get orders order) npat)))
		(if (>= row (elem-get rows (elem-get orders order))) (setq row 0)))
	(unless again
		(defq visited (. player :find :visited))
		(if (. visited :find (defq key (flow-key order row))) (. player :insert :ended :t) (. visited :insert key)))
	(scatter player :order order :row row :pbreak :nil :jump_to -1 :jumpline 0 :loop_dest -1))

(defun flow-start (player)
	; (flow-start player)
	;the first order of the song that is a pattern
	(defq song (. player :find :song) orders (. song :find :orders) order 0
		npat (length (. song :find :patterns)) markers (. song :find :markers))
	(while (and (< order (length orders)) (>= (elem-get orders order) npat)
			(not (and markers (= (elem-get orders order) 255))))
		(setq order (inc order)))
	(if (or (>= order (length orders)) (>= (elem-get orders order) npat))
		(. player :insert :ended :t)
		(. (. player :find :visited) :insert (flow-key order 0)))
	(. player :insert :order order))

(defun flow-tick-end (player)
	; (flow-tick-end player)
	;a tick of a row done: on to the next row after its last, the speed's
	;ticks times a pattern delay
	(. player :insert :frame (defq frame (inc (. player :find :frame))))
	(if (>= frame (* (. player :find :speed) (inc (. player :find :pdelay)))) (flow-advance player)))

(defun tracker-chans-play (of_player fn)
	; (tracker-chans-play player fn)
	;a tick of each channel, (fn chan), and the voices as they are then.
	;For a length only, of an S3M or an XM, what moves the song on is of
	;its rows, but for a note delayed to a later tick, whose cell, its
	;jump say, is read then, and a global volume slide, the volume an order
	;starts with: the channels with one are played, no others, and no
	;voices
	(if (. of_player :find :measure)
		(each (# (if (or (> (. %0 :find :delay) 0) (bits? (. %0 :find :fx) +fx_gslide)) (fn %0)))
			(. of_player :find :chans))
		(each fn (. of_player :find :chans))
		(tracker-voices of_player)))

(defun st3-tick (player)
	; (st3-tick player)
	;one tick of an S3M: a row read at its first, every channel played,
	;the row long so many ticks as the speed says, times a pattern delay
	(defq frame (. player :find :frame))
	(if (= frame 0) (st3-row player))
	(defq tick0 (= (% frame (. player :find :speed)) 0))
	(tracker-chans-play player (# (st3-play player %0 tick0)))
	(flow-tick-end player))

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
		(:t (tracker-sine pos))))

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
	(chan-porta-way chan)
	(chan-fx chan +fx_porta))

(defun ft2-effect (player chan effect param note row)
	; (ft2-effect player chan effect param note row)
	;what an effect of a row does at once, and sets going for its ticks
	(defq x (>> param 4) y (logand param 15))
	(case effect
		(0 (if (> param 0) (. chan :insert :arp (list 0 x y))))
		((1 2)
			;1 up, 2 down, xx each tick, Fx fine and Ex extra fine at once
			(setq param (chan-mem chan :freq_mem param) x (>> param 4) y (logand param 15))
			(defq way (if (= effect 1) -1 1))
			(cond
				((= x 15) (when (> y 0) (. chan :insert :fbend (* way 4 y)) (chan-fx chan +fx_fbend)))
				((= x 14) (when (> y 0) (. chan :insert :fbend (* way y)) (chan-fx chan +fx_fbend)))
				(:t (. chan :insert :bend (* way 4 param)) (chan-fx chan +fx_bend))))
		(3 (if (> (setq param (chan-mem chan :porta_mem param)) 0) (. chan :insert :porta (* 4 param)))
			(if (>= (. chan :find :ins) 0) (ft2-toneporta player chan note)))
		(4 (setq param (chan-mem chan :vib_mem param) x (>> param 4) y (logand param 15))
			(if (> y 0) (. chan :insert :vib_depth (* y 4)))
			(if (> x 0) (. chan :insert :vib_rate x))
			(chan-fx chan +fx_vib))
		(5 (if (>= (. chan :find :ins) 0) (ft2-toneporta player chan note))
			(tracker-vslide chan param :vol_mem :nil))
		(6 (chan-fx chan +fx_vib) (tracker-vslide chan param :vol_mem :nil))
		(7 (setq param (chan-mem chan :trem_mem param))
			(if (> (logand param 15) 0) (. chan :insert :trem_depth (logand param 15)))
			(if (> (>> param 4) 0) (. chan :insert :trem_rate (>> param 4)))
			(chan-fx chan +fx_trem))
		(8 (. chan :insert :pan param))
		(9 (setq param (chan-mem chan :offset_mem param))
			(if (> note 0) (. chan :insert :offset (<< param 8))))
		(10 (tracker-vslide chan param :vol_mem :nil))
		(11 (scatter player :pbreak :t :jump_to param :jumpline 0))
		(12 (scatter chan :volume (min 64 param) :new_vol :t))
		(13 (scatter player :pbreak :t :jumpline (+ (* 10 x) y)))
		(14 (case x
			(1 (when (> (setq y (chan-mem chan :fine_up_mem y)) 0)
					(. chan :insert :fbend (* -4 y)) (chan-fx chan +fx_fbend)))
			(2 (when (> (setq y (chan-mem chan :fine_down_mem y)) 0)
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
					(if (> count 0) (. player :insert :loop_dest (. chan :find :loop_row))))
				(:t (. chan :insert :loop_count y)
					(. player :insert :loop_dest (. chan :find :loop_row)))))
			(7 (. chan :insert :trem_wave (logand y 3)))
			(8 (. chan :insert :pan (<< y 4)))
			(9 (scatter chan :retrig_val y :retrig_count (inc y) :retrig_type 0)
				(chan-fx chan +fx_retrig))
			(10 (setq y (chan-mem chan :fvol_up_mem y))
				(. chan :insert :fvslide y) (chan-fx chan +fx_fvslide))
			(11 (setq y (chan-mem chan :fvol_down_mem y))
				(. chan :insert :fvslide (neg y)) (chan-fx chan +fx_fvslide))
			(12 (scatter chan :retrig_val (inc y) :retrig_count (inc y) :retrig_type 16)
				(chan-fx chan +fx_retrig))
			(14 (. player :insert :pdelay y))))
		(15 (cond
			((= param 0))
			((< param 32) (. player :insert :speed param))
			((. player :insert :bpm param))))
		(16 (. player :insert :gvol (min 64 param)))
		(17 (chan-gslide chan (chan-mem chan :gvol_mem param)))
		(20 (. chan :insert :keyoff (inc param)))
		(21 (scatter chan :venv param :penv param))
		(25 (setq param (chan-mem chan :pan_mem param))
			(. chan :insert :pslide (- (logand param 15) (>> param 4)))
			(chan-fx chan +fx_pslide))
		(27 (if (> param 0) (scatter chan :retrig_val param :retrig_type x))
			(if (> note 0) (. chan :insert :retrig_count (inc (logand (. chan :find :retrig_val) 15))))
			(chan-fx chan +fx_retrig))
		(29 (setq param (chan-mem chan :tremor_mem param))
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

(defun ft2-skip-event (player chan cell)
	; (ft2-skip-event player chan cell)
	;a cell not read, for a song's length only, as far as one that is read
	;later needs it: the channel's instrument, without which its global
	;volume slide is not had, as (ft2-event) gives it, and its effects
	;are of it alone
	(bind '(note ins vol effect &ignore) cell)
	(and (<= 1 ins (length (. (. player :find :song) :find :instruments))) (/= note +note_off)
		(not (find effect '(3 5))) (/= (>> vol 4) 15)
		(. chan :insert :ins (dec ins)))
	(. chan :insert :fx 0))

(defun ft2-row (player)
	; (ft2-row player)
	;the first tick of a row: each channel's cell, or held back by a note
	;delay, that plays the last note again if it has none
	(defq row (. player :find :row) measure (. player :find :measure))
	(bind '(cells at) (tracker-row player))
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
				;for a length only a cell of an effect that moves the song on, or
				;sets the global volume an order starts with, is read, B D E F G
				;H: a cell's effect is had whatever its note
				((or (not measure) (find effect '(11 13 14 15 16 17))) (ft2-event player chan cell row))
				((ft2-skip-event player chan cell))))
		(. player :find :chans)))

(defun ft2-play (player chan frame)
	; (ft2-play player chan frame)
	;a tick of a channel: its volume, pan and pitch as its effects, its
	;instrument's envelopes and its fade move them
	(defq speed (. player :find :speed) tick0 (= frame 0) row0 (= (% frame speed) 0)
		fx (. chan :find :fx))
	(when (and (chan-delay-due chan) (< frame speed))
		(ft2-event player chan (. chan :find :delayed) (. player :find :row))
		(setq fx (. chan :find :fx)))
	(when (>= (. chan :find :ins) 0)
		(defq volume (. chan :find :volume) period (. chan :find :period) gvol (. player :find :gvol)
			ins (elem-get (. (. player :find :song) :find :instruments) (. chan :find :ins)))
		;a retrig, or a cut, every so many ticks
		(when (bits? fx +fx_retrig)
			(. chan :insert :retrig_count (defq count (dec (. chan :find :retrig_count))))
			(when (= count 0)
				(if (< (. chan :find :retrig_type) 16) (scatter chan :trigger :t :start 0))
				(setq volume (retrig-volume volume (. chan :find :retrig_type)))
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
				(tracker-porta chan period fx)))
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
	(tracker-chans-play player (# (ft2-play player %0 frame)))
	(flow-tick-end player))

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

(defun amiga-period (note fin)
	; (amiga-period note fin) -> period
	;the Amiga's period of a note, 0 C-0 and on, and so many 128ths more, 64
	;times libxmp's, 13696 at C-0
	(n2i (* (const (n2d 876544)) (pow (const (n2d 2)) (/ (n2d (neg (+ (* note 128) fin))) (const (n2d 1536)))))))

(defun it-period (player note fin)
	; (it-period player note fin) -> period
	;the period of a note, 0 C-0 and on, and so many 128ths more, 64 times
	;libxmp's: linear, 1024 a semitone, or the Amiga's
	(if (eql (. player :find :pitch) :linear)
		(* 8 (- 30720 (* note 128) fin))
		(amiga-period note fin)))

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
		(chan-unflag chan :notes +itn_env_end)
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
			(chan-unflag chan :notes +itn_sample_end)
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
		(elem-set voices (+ from +mix_fy1) 0)
		(elem-set voices (+ from +mix_fy2) 0)
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
					(2 (chan-flag c :notes +itn_release))
					(3 (chan-flag c :notes +itn_fade)))))
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
	(setq param (chan-mem chan :freq_mem param))
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
	(chan-porta-way chan))

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
		(1 (if (> y 0) (chan-flag chan :notes +itn_gliss) (chan-unflag chan :notes +itn_gliss)))
		(3 (elem-set (. chan :find :vib) 3 (logand y 3)))
		(4 (elem-set (. chan :find :trem) 3 (logand y 3)))
		(5 (if (<= y 3) (elem-set (. chan :find :panbrello) 3 y)))
		(6 (. player :insert :pdelay y))
		(7 (case y
			((0 1 2) (it-past-act player chan (elem-get '(0 2 3) y)))
			((3 4 5 6) (and (not (. (. player :find :song) :find :sample_mode)) (. chan :find :voice)
				(. chan :insert :act (- y 3))))
			(7 (chan-flag chan :pers +itp_venv))
			(8 (chan-unflag chan :pers +itp_venv))
			(9 (chan-flag chan :pers +itp_penv))
			(10 (chan-unflag chan :pers +itp_penv))
			(11 (chan-flag chan :pers +itp_fenv))
			(12 (chan-unflag chan :pers +itp_fenv))))
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
			(chan-flag chan :notes +itn_cut)
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
		(7 (setq param (chan-mem chan :porta_mem param))
			(when (> param 0)
				(if unisld (. chan :insert :freq_mem param))
				(. chan :insert :porta (+ (. chan :find :porta) (* 64 param))))
			(and (or (> note 0) (/= (. chan :find :pdir) 0)) (it-ins song (. chan :find :ins))
				(progn (it-toneporta player chan note) (chan-fx chan +fx_porta))))
		((8 21) (setq param (chan-mem chan :vib_mem param))
			(chan-fx chan +fx_vib)
			(it-lfo-set (. chan :find :vib) (if (= fx 8) (<< (logand param 15) 2) (logand param 15)) (>> param 4)))
		(9 (and (. song :find :old_fx) (> param 0)
				(setq param (logand (logior (<< (inc x) 4) (inc y)) 0xff)))
			(setq param (chan-mem chan :tremor_mem param))
			(scatter chan :tremor_up (max 1 (>> param 4)) :tremor_down (max 1 (logand param 15)))
			(chan-fx chan +fx_tremor))
		(10 (setq param (chan-mem chan :arp_mem param))
			(if (> param 0) (. chan :insert :arp (list 0 (>> param 4) (logand param 15)))))
		(11 (chan-fx chan +fx_vib) (tracker-vslide chan param :mem :nil))
		(12 (. chan :insert :porta (+ (. chan :find :porta) (* 64 (. chan :find :porta_mem))))
			(when (it-ins song (. chan :find :ins))
				(it-toneporta player chan note)
				(chan-fx chan +fx_porta)
				(tracker-vslide chan param :mem :nil)))
		(13 (if (<= param 64) (. chan :insert :mastervol param)))
		(14 (it-tvslide chan param))
		(15 (setq param (chan-mem chan :offset_mem param))
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
		(18 (setq param (chan-mem chan :trem_mem param))
			(chan-fx chan +fx_trem)
			(it-lfo-set (. chan :find :trem) (logand param 15) (>> param 4)))
		(19 (it-extended player chan x y))
		(20 (cond
			((= x 0) (chan-fx chan +fx_tempo) (if (> y 0) (. chan :insert :tempo (neg y))))
			((= x 1) (chan-fx chan +fx_tempo) (. chan :insert :tempo y))
			((. player :insert :bpm param))))
		(22 (if (<= param 128) (. player :insert :gvol param)))
		(23 (if (> (setq param (chan-mem chan :gvol_mem param)) 0) (chan-gslide chan param)))
		(24 (scatter chan :pan param :rpv 0))
		(26 (chan-fx chan +fx_macro) (scatter chan :m_val (<< param 16) :m_slide 0))
		(31 (when (and (> (defq speed (. player :find :speed)) 0) (< (>> (. chan :find :m_val) 16) 128))
			(chan-fx chan +fx_macro)
			(scatter chan :m_target (<< param 16)
				:m_slide (/ (- (<< param 16) (. chan :find :m_val)) speed))))
		(25 (chan-fx chan +fx_panbrello)
			(it-lfo-set (. chan :find :panbrello) (<< (logand param 15) 4) (>> param 4)))
		((27 28) (setq param (chan-mem chan :vol_mem2 param))
			(. chan :insert :fvslide2 (if (= fx 27) param (neg param)))
			(chan-fx chan +fx_fvslide2))
		((29 30) (setq param (chan-mem chan :vol_mem2 param))
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
		(unless prenv (chan-unflag chan :notes +itn_env_end)))
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
					(chan-unflag chan :notes +itn_set))
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
		(chan-flag chan :notes +itn_set)
		(cond
			((= key +note_fade)
				(chan-flag chan :notes +itn_fade)
				(setq reset_env :nil reset_sus :nil use_ins_vol :nil))
			((= key +note_cut)
				(chan-flag chan :notes (const (+ +itn_end +itn_cut +itn_key_cut)))
				(. chan :insert :period 0)
				(it-patch player chan -1))
			((= key +note_off)
				(defq i (it-ins song (. chan :find :ins)))
				(chan-flag chan :notes (if (and i (it-sus-check (. i :find :volume) (. chan :find :venv)))
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
							(chan-unflag chan :notes (const (+ +itn_env_release +itn_susexit +itn_fade))))
						(:t (if (<= 1 key 121) (. chan :insert :key_porta (dec key)))
							(setq key 0)))))))
	;a note played: its sample from the start on the channel's voice
	(when (and (<= 1 key 121) (not new_invalid))
		(if (bits? (. chan :find :notes) +itn_cut) (setq use_ins_vol :t))
		(. chan :insert :key (setq key (dec key)))
		(chan-unflag chan :notes +itn_end)
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
		(if (> note 0) (chan-unflag chan :notes (const (+ +itn_env_release +itn_susexit +itn_fade))))
		(. chan :insert :fadeout 65536))
	(and reset_sus (> note 0) (chan-unflag chan :notes +itn_sample_release))
	(when (and retrig_ins not_same_ins)
		(chan-fx chan +fx_new_ins)
		(it-voicepos chan 0)
		(. chan :insert :fadeout 65536)
		(chan-unflag chan :notes (const (+ +itn_release +itn_susexit +itn_fade))))
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
		(chan-unflag chan :notes +itn_cut))
	(when (and (> evvol 0) (or (not (bits? (. chan :find :notes) +itn_cut)) (> ins 0)))
		(. chan :insert :volume (dec evvol))
		(chan-fx chan +fx_new_vol))
	(chan-unflag chan :offset 0xffff)
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
	(if (bits? (. chan :find :notes) +itn_sample_end) (chan-flag chan :notes +itn_end))
	(it-event player chan cell))

(defun it-skip-event (chan note ins)
	; (it-skip-event chan note ins)
	;a cell not read, for a song's length only, as far as one that is read
	;later needs it: whether its note has a sample, and so its effect is
	;had, is of the instrument last given and the one Impulse Tracker
	;holds over from a note delay, and its effects are of it alone
	(cond
		((> ins 0) (scatter chan :old_ins ins :delayed_ins 0))
		((> note 0) (. chan :insert :delayed_ins 0)))
	(. chan :insert :fx 0))

(defun it-row (player)
	; (it-row player)
	;the first tick of a row: each channel's cell, or held back by a note
	;delay to a later tick of it, and not again for a row delay
	(defq measure (. player :find :measure))
	(bind '(cells at) (tracker-row player))
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
				((not (or (= rs 0) (and (bits? rs +rowdelay_first) (> (. player :find :rowdelay) 0)))))
				;for a length only a cell of an effect that moves the song on,
				;or keeps a memory one may use, is read, A B C S T, and the
				;global volume's an order starts with, V W
				((or (not measure) (find fx '(1 2 3 19 20 22 23)))
					(it-read-event player chan (list note ins vol fx param)))
				((it-skip-event chan note ins))))
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
	(when (chan-delay-due chan)
		(it-read-event player chan (. chan :find :delayed))
		(setq fx (. chan :find :fx)))
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
					(if (< kind 16) (it-voicepos chan 0) (chan-flag chan :notes +itn_end))
					(. chan :insert :volume (retrig-volume (. chan :find :volume) kind))
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
					(tracker-porta chan period fx)))
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
				(if (= ve 0) (chan-flag chan :notes +itn_end))
				(chan-flag chan :notes +itn_env_end))
			(case (it-env-fade venv vx)
				(-1 (chan-flag chan :notes +itn_end))
				(1 (chan-flag chan :notes +itn_fade)))
			(if (or (bits? (. chan :find :notes) +itn_fade) (eql act 3)) (setq fade :t))
			(when fade
				(cond
					((> (. chan :find :fadeout) (defq rate (. chan :find :ins_fade)))
						(. chan :insert :fadeout (- (. chan :find :fadeout) rate)))
					(:t (. chan :insert :fadeout 0)
						(chan-flag chan :notes +itn_end))))
			;the volume heard: a swing, a tremolo, the fade, the envelope, the
			;global, the channel's and the instrument's, and a tremor
			(defq volume (/ (* (. chan :find :volume) (- 100 (. chan :find :rvv))) 100))
			(when (bits? fx +fx_trem)
				(setq volume (+ volume (/ (it-lfo (defq lfo (. chan :find :trem))) 64)))
				(if (or (not first_frame) (not old_fx)) (it-lfo-step lfo)))
			(setq volume (>> (* (max 0 (min 64 volume)) (. chan :find :fadeout)) 6)
				volume (>> (* (/ (* ve (. player :find :gvol) (. chan :find :mastervol)) 128) volume) 18)
				volume (>> (* volume (. ins :find :vol) (. chan :find :gvl)) 12))
			(and (bits? fx +fx_tremor) (chan-tremor-off chan) (setq volume 0))
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
			(if (bits? (. chan :find :notes) +itn_susexit) (chan-flag chan :notes +itn_env_release)))))

(defun it-measure-tick (player)
	; (it-measure-tick player)
	;one tick of an IT for its length only: a row's cells read at its first
	;tick, and of the channels only those with a tempo slide, which moves
	;the song on by its ticks, a note delayed to a later tick, whose cell,
	;its jump say, is read then, or a global volume slide, the volume an
	;order starts with. No past notes, no voices
	(defq chans (. player :find :chans) frame (. player :find :frame))
	(each (# (chan-unflag %0 :fx +fx_key_off)) chans)
	(if (= frame 0) (it-row player))
	(each (# (if (or (bits? (. %0 :find :fx) (const (+ +fx_tempo +fx_gslide))) (> (. %0 :find :delay) 0))
			(it-play player %0 frame)))
		chans)
	(chan-unflag player :rowdelay_set +rowdelay_first)
	(flow-tick-end player))

(defun it-play-tick (player)
	; (it-play-tick player)
	;one tick of an IT played
	(defq chans (. player :find :chans) voices (. player :find :voices)
		instrument_mode (not (. (. player :find :song) :find :sample_mode)))
	;a sample come to its end in the mixer, its voice let go of in
	;instrument mode
	(each (lambda (chan)
			(when (and (. chan :find :voice) (not (. chan :find :trigger))
					(< (elem-get voices (* (!) +mix_size)) 0))
				(chan-flag chan :notes +itn_sample_end)
				(if instrument_mode (. chan :insert :voice :nil))))
		chans)
	(each (lambda (past)
			(and (not (. past :find :trigger)) (< (elem-get voices (* (. past :find :slot) +mix_size)) 0)
				(it-past-drop player past)))
		(cat (. player :find :past)))
	(defq frame (. player :find :frame) song (. player :find :song))
	(each (# (chan-unflag %0 :fx +fx_key_off)) chans)
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
	(chan-unflag player :rowdelay_set +rowdelay_first)
	(tracker-voices player)
	(flow-tick-end player))


(defun it-tick (player)
	; (it-tick player)
	;one tick of an IT, as libxmp plays one: a row's cells read at its first
	;tick, every channel played, and on to the next row at the end of its
	;last
	(if (. player :find :measure) (it-measure-tick player) (it-play-tick player)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MED and OctaMED, the effects of an MMD, as libxmp 4.7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;a MED command as libxmp has it, of its own effects or those of the
;ProTracker's, by number: the ProTracker's 0 to 15, and these
(defq +medfx_vib2 16 +medfx_finetune 17 +medfx_fvup 18 +medfx_fvdn 19 +medfx_fpup 20 +medfx_fpdn 21
	+medfx_pdelay 22 +medfx_bpm 23 +medfx_retrig 24 +medfx_hold 25 +medfx_setpan 26
	;the half sine of a synth sound's vibrato
	+med_sine ''(0 49 97 141 180 212 235 250 255 250 235 212 180 141 97 49
		0 -49 -97 -141 -180 -212 -235 -250 -255 -250 -235 -212 -180 -141 -97 -49))

(defun med-channel (pan vol index)
	; (med-channel pan vol index) -> channel
	(scatter (Emap) :index index :ins -1 :key -1 :note 0 :smp -1 :sample -1 :voice :nil :period 0 :per_adj 1
		:finetune 0 :volume 64 :mastervol vol :pan pan :fx 0 :notes 0
		:delay 0 :delayed :nil :vslide 0 :fvslide 0 :vol_mem 0
		:bend 0 :fbend 0 :porta 0 :porta_mem 0 :target 0 :pdir 0
		:vib (nums 0 0 0 0) :vib_mem 0 :trem (nums 0 0 0 0) :trem_mem 0
		:retrig_val 0 :retrig_count 0 :retrig_type 0 :retrig_limit 0
		:offset 0 :offset_mem 0 :arp (list 0) :arp_at 0 :loop_row 0 :loop_count 0
		;a note's hold and decay
		:hold_count 0 :hold_active :nil :hold_sustained :nil :decay_value 0
		;a synth sound's tables as they play, the volume it gives, its
		;arpeggio and vibrato
		:s_volume 0 :vp 0 :vc 0 :vw 0 :vs 0 :vv 0 :wp 0 :wc 0 :ww 0 :ws 0 :wv 0 :s_period 0
		:s_arp 0 :s_aidx 0 :vib_depth 0 :vib_speed 0 :vib_idx 0 :env_wav 0 :env_idx 0 :env_loop :nil
		;no filter, a0 of 0 off
		:fa0 0 :fb0 0 :fb1 0
		:period_out 0 :volume_out 0 :pan_out pan :trigger :nil :switch :nil :start 0 :stop :nil))

(defun med-xlat (note cmd param song)
	; (med-xlat note cmd param song) -> (fx param)
	;a MED command as libxmp has it, mmd_xlat_fx
	(defq none '(-1 0))
	(case cmd
		((0 3 5 6 7 10 11 12) (list cmd param))
		((1 2) (if (= param 0) none (list cmd param)))
		(4 (list +medfx_vib2 param))
		(8 (list +medfx_hold param))
		(9 (if (<= 1 param 0x20) (list 15 param) none))
		(13 (list 10 param))
		(15 (cond
			((= param 0) '(13 0))
			((<= param 0xf0) (list +medfx_bpm (med-tempo param (. song :find :bpm_on) (. song :find :med_8ch))))
			((= param 0xf1) '(14 0x93))
			((= param 0xf2) '(14 0xd3))
			((= param 0xf3) (list +medfx_retrig 2))
			(none)))
		(0x11 (list +medfx_fpup param))
		(0x12 (list +medfx_fpdn param))
		(0x14 (list 4 param))
		(0x15 (list +medfx_finetune (logand (<< (+ param 8) 4) 0xff)))
		(0x16 (list 14 (logior 0x60 (min param 15))))
		(0x18 (list 14 (logior 0xc0 (min param 15))))
		(0x19 (list 9 param))
		(0x1a (if (= param 0) none (list +medfx_fvup param)))
		(0x1b (if (= param 0) none (list +medfx_fvdn param)))
		(0x1d (list 13 param))
		(0x1e (list +medfx_pdelay param))
		(0x1f (if (and (/= param 0) (> note 0)) (list +medfx_retrig param) none))
		(0x2e (if (or (>= param 0xf0) (<= param 0x10))
			(list +medfx_setpan (min 255 (<< (+ (if (> param 127) (- param 256) param) 16) 3))) none))
		(:t none)))

(defun med-valid (song ins)
	; (med-valid song ins) -> :nil | instrument
	;an instrument that has something to play
	(defq instruments (. song :find :instruments))
	(and (<= 0 ins) (< ins (length instruments))
		(> (. (defq i (elem-get instruments ins)) :find :nsm) 0) i))

(defun med-sub (song ins key)
	; (med-sub song ins key) -> :nil | (sample transpose volume finetune)
	;the sub an instrument plays for a key, its first if no key
	(when (defq i (med-valid song ins))
		(defq subs (. i :find :subs))
		(if (<= 0 key 120)
			(if (< (defq m (first (elem-get (. i :find :map) key))) (length subs)) (elem-get subs m))
			(first subs))))

(defun med-period (note fin adj)
	; (med-period note fin adj) -> period
	;the Amiga's period of a note, as an instrument of octaves has it twice
	(* adj (amiga-period note fin)))

(defun med-hold-decay (chan hold decay)
	; (med-hold-decay chan hold decay)
	(if (<= hold 0) (setq hold -1 decay -1))
	(scatter chan :hold_active (> hold 0) :hold_count hold :decay_value decay))

(defun med-toneporta (player chan note)
	; (med-toneporta player chan note)
	;a note, if there is one, to slide to, and which way
	(defq song (. player :find :song) inst (elem-get (. song :find :instruments) (. chan :find :ins))
		key (. chan :find :key) subs (. inst :find :subs)
		m (if (<= 0 key 120) (first (elem-get (. inst :find :map) key)) 0)
		sub (elem-get subs (if (< m (length subs)) m 0)))
	(when (<= 1 note 121)
		(. chan :insert :target (med-period (+ (dec note) (second sub) (if (<= 0 key 120) (second (elem-get (. inst :find :map) key)) 0))
			(. chan :find :finetune) (. chan :find :per_adj))))
	(chan-porta-way chan))

(defun med-vslide (chan param)
	; (med-vslide chan param)
	;a volume slide, its last if 0: up if it has one, else down
	(if (= param 0) (setq param (. chan :find :vol_mem)))
	(when (/= param 0)
		(chan-fx chan +fx_vslide)
		(defq x (>> param 4) y (logand param 15))
		(scatter chan :vol_mem param :vslide (if (/= x 0) x (neg y)))))

(defun med-loop (player chan param)
	; (med-loop player chan param)
	;a pattern's loop: 0 where it starts, else so many times back to it
	(cond
		((= param 0) (. chan :insert :loop_row (. player :find :row)))
		((> (defq count (. chan :find :loop_count)) 0)
			(. chan :insert :loop_count (setq count (dec count)))
			(if (> count 0) (. player :insert :loop_dest (. chan :find :loop_row))))
		(:t (. chan :insert :loop_count param)
			(. player :insert :loop_dest (. chan :find :loop_row)))))

(defun med-fx (player chan fx param note ins)
	; (med-fx player chan fx param note ins)
	;what an effect does at once, and sets going for its ticks
	(defq song (. player :find :song) x (>> param 4) y (logand param 15))
	(case fx
		(0 (. chan :insert :arp (list 0 x y)))
		((1 2) (chan-fx chan +fx_bend)
			(. chan :insert :bend (* 64 (if (= fx 1) (neg param) param))))
		(3 (setq param (chan-mem chan :porta_mem param))
			(if (/= param 0) (. chan :insert :porta (+ (. chan :find :porta) (* 64 param))))
			(when (med-valid song (. chan :find :ins))
				(med-toneporta player chan note)
				(chan-fx chan +fx_porta)))
		(4 (setq param (chan-mem chan :vib_mem param))
			(chan-fx chan +fx_vib)
			(it-lfo-set (. chan :find :vib) (<< (logand param 15) 2) (>> param 4)))
		(16 (chan-fx chan +fx_vib)
			(it-lfo-set (. chan :find :vib) (<< y 3) x))
		(5 (. chan :insert :porta (+ (. chan :find :porta) (* 64 (. chan :find :porta_mem))))
			(when (med-valid song (. chan :find :ins))
				(med-toneporta player chan note)
				(chan-fx chan +fx_porta)
				(med-vslide chan param)))
		(6 (chan-fx chan +fx_vib) (med-vslide chan param))
		(7 (setq param (chan-mem chan :trem_mem param))
			(chan-fx chan +fx_trem)
			(it-lfo-set (. chan :find :trem) (logand param 15) (>> param 4)))
		(26 (. chan :insert :pan param))
		(9 (setq param (chan-mem chan :offset_mem param))
			(chan-fx chan +fx_offset)
			(if (> note 0) (. chan :insert :offset (<< param 8))))
		(10 (med-vslide chan param))
		(11 (scatter player :pbreak :t :jump_to param :jumpline 0))
		(12 (chan-fx chan +fx_new_vol) (. chan :insert :volume param))
		(13 (scatter player :pbreak :t :jumpline (+ (* 10 x) y)))
		(14 (case x
			(9 (chan-fx chan +fx_retrig)
				(scatter chan :retrig_val y :retrig_count (inc y) :retrig_type 0 :retrig_limit 1))
			(6 (med-loop player chan y))
			(12 (chan-fx chan +fx_retrig)
				(scatter chan :retrig_val (inc y) :retrig_count (inc y) :retrig_type 16))))
		(15 (cond
			((< param 0x20) (if (> param 0) (. player :insert :speed param)))
			((. player :insert :bpm (max (. player :find :min_bpm) param)))))
		(17 (. chan :insert :finetune (- param 0x80)))
		((18 19) (chan-fx chan +fx_fvslide)
			(. chan :insert :fvslide (if (= fx 18) param (neg param))))
		((20 21) (when (/= param 0)
			(chan-fx chan +fx_fbend)
			(. chan :insert :fbend (* 64 (if (= fx 20) (neg param) param)))))
		(22 (. player :insert :pdelay param))
		(23 (. player :insert :bpm (max (. player :find :min_bpm) param)))
		(24 (chan-fx chan +fx_retrig)
			(scatter chan :retrig_val y :retrig_count (inc y) :retrig_type 0 :retrig_limit 0))
		(25 (and (<= 1 note 121) (med-valid song (dec ins)) (med-hold-decay chan y x)))))

(defun med-next-cell (player chan_at)
	; (med-next-cell player chan_at) -> :nil | (note ins vol cmd param)
	;the channel's cell of the next row of the pattern, if it has one
	(defq song (. player :find :song) row (inc (. player :find :row)))
	(when (< row (elem-get (. song :find :rows) (elem-get (. song :find :orders) (. player :find :order))))
		(bind '(cells at) (tracker-row player row))
		(defq c (+ at (* chan_at +cell_size)))
		(slice cells c (+ c +cell_size))))

(defun med-event (player chan cell)
	; (med-event player chan cell)
	;a channel's cell read, its note, instrument and command, as libxmp's
	;read_event_med
	(bind '(note ins vol cmd param) cell)
	(bind '(fx fxp) (med-xlat note cmd param (defq song (. player :find :song))))
	(defq out -1 new_invalid :nil is_porta (or (= fx 3) (= fx 5)) version (. song :find :version))
	(. chan :insert :fx 0)
	(when (and (> ins 0) (> note 0))
		(chan-fx chan +fx_new_ins)
		(. chan :insert :offset 0)
		(cond
			((defq i (med-valid song (dec ins)))
				(unless is_porta
					(. chan :insert :ins (dec ins))
					(med-hold-decay chan (. i :find :hold) (. i :find :decay))))
			(:t (setq new_invalid :t)
				(scatter chan :voice :nil :stop :t :trigger :nil)))
		(scatter chan :s_arp 0 :s_aidx 0))
	;an instrument's volume, of a note or from OctaMED 3 a hold too
	(and (> ins 0) (or (<= 1 note 121) (>= version +med_ver_octamed_300))
		(defq sub (med-sub song (dec ins) (dec note)))
		(progn (. chan :insert :volume (third sub)) (chan-fx chan +fx_new_vol)))
	(when (> note 0)
		(chan-fx chan +fx_new_note)
		(cond
			((= note +note_cut)
				(chan-flag chan :notes +itn_end)
				(. chan :insert :period 0)
				(scatter chan :voice :nil :stop :t :trigger :nil))
			((and (not is_porta) (defq i (med-valid song (. chan :find :ins))) (<= 1 note 121))
				(defq key (dec note))
				(. chan :insert :key key)
				(chan-unflag chan :notes +itn_end)
				;a sample of octaves, and a synth sound of no speeds, at half the
				;rate, as libxmp has them
				(. chan :insert :per_adj (if (and (> (. i :find :nsm) 1) (= (. i :find :vts) 0) (= (. i :find :wts) 0)) 2 1))
				(when (and (not new_invalid) (defq sub (med-sub song (. chan :find :ins) key)))
					(setq out (+ key (second sub) (second (elem-get (. i :find :map) key))))
					(when (> (. (elem-get (. song :find :samples) (first sub)) :find :length) 0)
						(scatter chan :sample (first sub) :voice :t :trigger :t :start 0 :smp (first sub)))))))
	;the sub now played, which a slide to a note may have kept
	(defq sub (med-sub song (. chan :find :ins) (. chan :find :key)) finetune (. chan :find :finetune))
	(when (and sub (>= out 0))
		(. chan :insert :finetune (elem-get sub 3))
		(elem-set (. chan :find :vib) 0 0)
		(elem-set (. chan :find :trem) 0 0))
	(scatter chan :delay 0 :arp (list 0) :arp_at 0)
	(if is_porta (. chan :insert :porta 0))
	(if (= ins 0) (. chan :insert :finetune finetune))
	(if (>= fx 0) (med-fx player chan fx fxp note ins))
	;the period of the note played, or slid to
	(when (and sub (>= out 0))
		(defq per (med-period out (. chan :find :finetune) (. chan :find :per_adj)))
		(. chan :insert :target per)
		(if (or (< (. chan :find :period) 64) (not is_porta)) (. chan :insert :period per)))
	;a hold kept on by an instrument with no note on the next row, or a
	;slide to a note there
	(. chan :insert :hold_sustained :nil)
	(and (>= version +med_ver_300) (> (. chan :find :hold_count) 0)
		(defq next (med-next-cell player (. chan :find :index)))
		(bind '(n_note n_ins & n_cmd &ignore) next)
		(if (<= 1 n_note 121) (= n_cmd 3) (> n_ins 0))
		(. chan :insert :hold_sustained :t))
	(when (and sub (>= out 0))
		(. chan :insert :note out)
		(if (. chan :find :voice) (scatter chan :trigger :t :start (. chan :find :offset)))))

(defun med-row (player)
	; (med-row player)
	;the first tick of a row: each channel's cell, or held back by a note
	;delay to a later tick of it. For a length only, the cells of the
	;commands that move the song on
	(defq song (. player :find :song) measure (. player :find :measure))
	(bind '(cells at) (tracker-row player))
	(each (lambda (chan)
			(defq c (+ at (* (!) +cell_size)) cell (slice cells c (+ c +cell_size)))
			(bind '(note ins vol cmd param) cell)
			(bind '(fx fxp) (med-xlat note cmd param song))
			;a speed is had first, a delay is of it
			(and (= fx 15) (< 0 fxp 0x20) (. player :insert :speed fxp))
			(cond
				((and (= fx 14) (= (>> fxp 4) 13) (> (logand fxp 15) 0))
					(scatter chan :delay (inc (logand fxp 15)) :delayed cell))
				((and (= fx +medfx_retrig) (> (>> fxp 4) 0))
					(scatter chan :delay (inc (>> fxp 4)) :delayed cell)
					(chan-unflag chan :fx +fx_retrig))
				((not measure) (med-event player chan cell))
				((or (find fx '(11 13 15 22 23)) (and (= fx 14) (= (>> fxp 4) 6)))
					(med-fx player chan fx fxp note ins))))
		(. player :find :chans)))

(defun med-table-next (chan key table)
	; (med-table-next chan key table) -> num
	;the next of a synth sound's table, its volumes' or its waveforms', where
	;the channel is in it under key, 255 its end past it
	(defq at (. chan :find key))
	(cond ((< -1 at (length table)) (. chan :insert key (inc at)) (code table 1 at)) (255)))

(defun med-vt (chan vtbl)
	; (med-vt chan vtbl) -> num
	(med-table-next chan :vp vtbl))

(defun med-wt (chan wtbl)
	; (med-wt chan wtbl) -> num
	(med-table-next chan :wp wtbl))

(defun med-synth (player chan inst)
	; (med-synth player chan inst)
	;a tick of a synth sound's tables, its volume's and its waveform's:
	;each a list of commands and values played at its speed, after
	;libxmp's med_extras
	(bind '(vtbl wtbl) (. inst :find :synth))
	(defq frame (. player :find :frame) fx (. chan :find :fx) subs (. inst :find :subs)
		samples (. (. player :find :song) :find :samples) jws 0 jvs 0)
	(when (and (= frame 0) (bits? fx +fx_new_note))
		(. chan :insert :s_period (. chan :find :period))
		(if (bits? fx +fx_new_ins)
			(scatter chan :s_arp 0 :s_aidx 0 :vp 0 :vc 0 :vw 0 :wp 0 :wc 0 :ww 0 :env_wav -1 :env_idx 0
				:env_loop :nil :vv 0 :wv 0 :vs (. inst :find :vts) :ws (. inst :find :wts))))
	;a table's count only goes down while it has a speed
	(when (and (> (. chan :find :vs) 0) (= (defq vc (. chan :find :vc)) (progn (. chan :insert :vc (dec vc)) 0)))
		(. chan :insert :vc (dec (. chan :find :vs)))
		(cond
			((> (. chan :find :vw) 0) (. chan :insert :vw (dec (. chan :find :vw))))
			(:t (defq again :t looped :nil)
				(while again
					(setq again :nil)
					(case (defq b (med-vt chan vtbl))
						((255 251) (. chan :insert :vp (dec (. chan :find :vp))))
						(254 (unless looped
							(. chan :insert :vp (med-vt chan vtbl))
							(setq looped :t again :t)))
						(250 (setq jws (med-vt chan vtbl)))
						(245 (scatter chan :env_wav (med-vt chan vtbl) :env_loop :t))
						(244 (. chan :insert :env_wav (med-vt chan vtbl)))
						(243 (. chan :insert :vv (med-vt chan vtbl)))
						(242 (. chan :insert :vv (neg (med-vt chan vtbl))))
						(241 (. chan :insert :vw (med-vt chan vtbl)))
						(240 (. chan :insert :vs (med-vt chan vtbl)))
						(:t (if (<= b 64) (. chan :insert :s_volume b)))))))
		;a volume's envelope, a waveform of 128 played as one
		(when (< -1 (defq ew (. chan :find :env_wav)) (length subs))
			(defq s (elem-get samples (first (elem-get subs ew))))
			(when (= (. s :find :length) 128)
				(defq ei (. chan :find :env_idx) v (code (. s :find :data) 1 (inc (* 2 ei))))
				(. chan :insert :s_volume (>> (+ (if (> v 127) (- v 256) v) 128) 2))
				(. chan :insert :env_idx (setq ei (inc ei)))
				(when (>= ei 128)
					(unless (. chan :find :env_loop) (. chan :insert :env_wav -1))
					(. chan :insert :env_idx 0))))
		(. chan :insert :s_volume (max 0 (min 64 (+ (. chan :find :s_volume) (. chan :find :vv))))))
	(when (and (> (. chan :find :ws) 0) (= (defq wc (. chan :find :wc)) (progn (. chan :insert :wc (dec wc)) 0)))
		(. chan :insert :wc (dec (. chan :find :ws)))
		(cond
			((> (. chan :find :ww) 0) (. chan :insert :ww (dec (. chan :find :ww))))
			(:t (defq again :t looped :nil)
				(while again
					(setq again :nil)
					(case (defq b (med-wt chan wtbl))
						((255 251) (. chan :insert :wp (dec (. chan :find :wp))))
						(254 (unless looped
							(cond
								((= (defq to (med-wt chan wtbl)) 255) (. chan :insert :wp (dec (. chan :find :wp))))
								(:t (. chan :insert :wp to)
									(setq looped :t again :t)))))
						(253)
						(252 (defq at (. chan :find :wp))
							(scatter chan :s_arp at :s_aidx at :wp (inc at))
							(while (not (find b '(253 255))) (setq b (med-wt chan wtbl))))
						(250 (setq jvs (med-wt chan wtbl)))
						(247 (med-wt chan wtbl))
						(246 (. chan :insert :period (. chan :find :s_period)))
						(245 (. chan :insert :vib_speed (med-wt chan wtbl)))
						(244 (. chan :insert :vib_depth (med-wt chan wtbl)))
						(243 (. chan :insert :wv (neg (med-wt chan wtbl))))
						(242 (. chan :insert :wv (med-wt chan wtbl)))
						(241 (. chan :insert :ww (med-wt chan wtbl)))
						(240 (. chan :insert :ws (med-wt chan wtbl)))
						(:t ;another waveform, the voice where it was along
							(and (< b (length subs)) (/= (defq s (first (elem-get subs b))) (. chan :find :smp))
								(scatter chan :smp s :sample s :switch :t)))))))
		(. chan :insert :period (+ (. chan :find :period) (* 64 (. chan :find :wv)))))
	(if (/= jws 0) (. chan :insert :wp jws))
	(if (/= jvs 0) (. chan :insert :vp jvs)))

(defun med-extras (player chan inst)
	; (med-extras player chan inst)
	;a tick of a note's hold and decay, and of a synth sound
	(defq song (. player :find :song))
	(when (= (. chan :find :hold_count) 0)
		;its decay, at once if 0
		(defq decay (. chan :find :decay_value) vol (. chan :find :volume))
		(. chan :insert :volume (setq vol (max 0 (min 64 (- vol (if (/= decay 0) decay vol))))))
		(if (= vol 0) (scatter chan :hold_count -1 :decay_value -1))
		(. chan :insert :hold_active :nil))
	(and (. chan :find :hold_sustained) (< (. song :find :version) +med_ver_ss_2)
		(>= (. player :find :frame) (. player :find :speed))
		(. chan :insert :hold_sustained :nil))
	(and (> (. chan :find :hold_count) 0) (not (. chan :find :hold_sustained))
		(. chan :insert :hold_count (dec (. chan :find :hold_count))))
	(if (. inst :find :synth) (med-synth player chan inst) (. chan :insert :s_volume 64)))

(defun med-arp-at (inst i)
	; (med-arp-at inst i) -> num
	;the waveform table's at i, 253 the end of an arpeggio past it
	(defq wtbl (second (. inst :find :synth)))
	(if (< i (length wtbl)) (code wtbl 1 i) 253))

(defun med-synth-arp (chan inst)
	; (med-synth-arp chan inst) -> semitones
	;the note a synth sound's arpeggio moves it this tick
	(defq at (. chan :find :s_arp))
	(cond
		((not (. inst :find :synth)) 0)
		((= at 0) 0)
		((= (med-arp-at inst at) 253) 0)
		(:t (defq i (. chan :find :s_aidx) v (med-arp-at inst i))
			(when (= v 253) (setq i at v (med-arp-at inst i)))
			(. chan :insert :s_aidx (inc i))
			v)))

(defun med-lfo (lfo)
	; (med-lfo lfo) -> num
	;an lfo of (phase rate depth wave) now, as libxmp's: FastTracker's waves
	;of a tremolo, (ft2-wave), times its depth, 0 if it does not move
	(if (= (elem-get lfo 1) 0) 0
		(* (elem-get lfo 2) (ft2-wave (elem-get lfo 3) (elem-get lfo 0) :nil))))

(defun med-play (player chan)
	; (med-play player chan)
	;a tick of a channel, libxmp's play_channel: a delayed note, the hold,
	;a retrig, the slides, and its volume, pitch and pan heard
	(defq song (. player :find :song) frame (. player :find :frame))
	(if (chan-delay-due chan) (med-event player chan (. chan :find :delayed)))
	(defq fx (. chan :find :fx) first_row (= (% frame (. player :find :speed)) 0)
		all (. song :find :slides_all))
	;the slides of the volume: each tick but a row's first, or each of
	;them all, and fine at the first
	(defq volume (. chan :find :volume))
	(cond
		((not (. chan :find :voice))
			(and (or (not first_row) all) (bits? fx +fx_vslide) (setq volume (+ volume (. chan :find :vslide))))
			(and first_row (bits? fx +fx_fvslide) (setq volume (+ volume (. chan :find :fvslide))))
			(scatter chan :volume (max 0 (min 64 volume)) :volume_out 0 :period_out 0))
		((defq inst (med-valid song (. chan :find :ins)))
			(med-extras player chan inst)
			(setq volume (. chan :find :volume))
			(when (bits? fx +fx_retrig)
				(. chan :insert :retrig_count (defq count (dec (. chan :find :retrig_count))))
				(when (= count 0)
					(defq kind (. chan :find :retrig_type))
					(cond
						((< kind 16)
							(scatter chan :trigger :t :start 0)
							(and (>= (. song :find :version) +med_ver_octamed_300) (. chan :find :hold_active)
								(. chan :insert :hold_count (+ (. chan :find :hold_count) frame))))
						((chan-flag chan :notes +itn_end)))
					(setq volume (retrig-volume volume kind))
					(. chan :insert :retrig_count (. chan :find :retrig_val))
					(when (> (defq limit (. chan :find :retrig_limit)) 0)
						(. chan :insert :retrig_limit (setq limit (dec limit)))
						(if (= limit 0) (. chan :insert :fx (setq fx (logand fx (const (lognot +fx_retrig)))))))))
			(and (or (not first_row) all) (bits? fx +fx_vslide) (setq volume (+ volume (. chan :find :vslide))))
			(and first_row (bits? fx +fx_fvslide) (setq volume (+ volume (. chan :find :fvslide))))
			(. chan :insert :volume (setq volume (max 0 (min 64 volume))))
			;the slides of the pitch
			(defq period (. chan :find :period))
			(when (or (/= frame 0) all)
				(if (bits? fx +fx_bend) (setq period (+ period (. chan :find :bend))))
				(tracker-porta chan period fx))
			(and (= frame 0) (bits? fx +fx_fbend) (setq period (+ period (. chan :find :fbend))))
			(. chan :insert :period period)
			;the volume heard: the synth's share, a tremolo, the channel's
			(defq vol (/ (* (. chan :find :s_volume) volume) 64))
			(when (bits? fx +fx_trem)
				(setq vol (+ vol (/ (med-lfo (defq lfo (. chan :find :trem))) 64)))
				(if (/= frame 0) (it-lfo-step lfo)))
			(setq vol (>> (* (/ (* 64 (. player :find :gvol) (. chan :find :mastervol)) 64) (<< (max 0 (min 64 vol)) 10)) 18))
			;the pitch heard: a vibrato, the synth's, an arpeggio
			(defq vibrato 0)
			(when (bits? fx +fx_vib)
				(setq vibrato (* 64 (/ (med-lfo (defq lfo (. chan :find :vib))) 512)))
				(if (/= frame 0) (it-lfo-step lfo)))
			(defq vi (. chan :find :vib_idx) svib (>>> (* (elem-get +med_sine (>> vi 5)) (. chan :find :vib_depth)) 10))
			(. chan :insert :vib_idx (% (+ vi (. chan :find :vib_speed)) 1024))
			(defq arp (. chan :find :arp) up (elem-get arp (. chan :find :arp_at))
				p (max 6 (+ period (* 64 svib) vibrato)) adj (. chan :find :per_adj)
				pitch (+ (it-pitch player (/ p adj)) (* 12800 (+ up (med-synth-arp chan inst)))))
			(. chan :insert :arp_at (% (inc (. chan :find :arp_at)) (length arp)))
			(scatter chan :volume_out vol :period_out (it-step player (min pitch (const (* 149 12800))))
				:pan_out (. chan :find :pan)))
		((scatter chan :volume_out 0))))

(defun med-tick (player)
	; (med-tick player)
	;one tick of a MED song, as libxmp plays one: a row's cells read at its
	;first tick, every channel played, and on to the next row at the end
	;of its last. For a length only, no channel is played
	(defq chans (. player :find :chans) voices (. player :find :voices) frame (. player :find :frame))
	(unless (. player :find :measure)
		;a sample come to its end in the mixer, its voice let go of
		(each (lambda (chan)
				(and (. chan :find :voice) (not (. chan :find :trigger)) (< (elem-get voices (* (!) +mix_size)) 0)
					(. chan :insert :voice :nil)))
			chans))
	(if (= frame 0) (med-row player))
	(unless (. player :find :measure)
		(each (# (med-play player %0)) chans)
		(tracker-voices player))
	(flow-tick-end player))

(defun med-frames (player)
	; (med-frames player) -> frames
	;how long a tick is, as libxmp works it out in doubles: the rate, times
	;the song's time factor and 250, over the bpm and 1000
	(n2i (/ (/ (* (* (n2d (. player :find :rate)) (. player :find :tf)) (const (n2d 250)))
		(n2d (max 1 (. player :find :bpm)))) (const (n2d 1000)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AHX, its replayer's voices given the mixer, lib/media/ahx.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ahx-player (player song)
	; (ahx-player player song) -> player
	;a player of an AHX or a HivelyTracker's: a voice each of its channels, a
	;buffer each of the mixer's, and one each for their rings after them
	(defq n (. song :find :channels))
	(scatter player :chans (map (# (ahx-voice (!) %0)) (. song :find :pans))
		:samples (map! (# (pad "" (const (* 2 +ahx_buffer)) "\x00")) '() 0 (* 2 n))
		:pan_tables (ahx-pans) :order 0 :row 0 :speed 6 :wait 0 :get_new :t :pos_jump 0 :pos_jump_note 0
		:pattern_break :nil :end_reached :nil :frames_done 0)
	;each voice on its buffer from the start, the one it is at, no ring
	(defq voices (. player :find :voices))
	(each! (lambda ()
			(defq at (* (!) +mix_size))
			(elem-set voices at (!))
			(elem-set voices (+ at +mix_pos) 0)
			(elem-set voices (+ at +mix_loop) 0)
			(elem-set voices (+ at +mix_end) +ahx_buffer)
			(elem-set voices (+ at +mix_flags) 0)
			(elem-set voices (+ at +mix_round) 0)
			(elem-set voices (+ at +mix_ring) -1))
		'() 0 n)
	player)

(defun ahx-voices (player)
	; (ahx-voices player)
	;each voice's place in the mixer: its buffer round and round, from its
	;start when an instrument began it, its step, its share each side of
	;the song's gain, and its ring if it has one
	(defq voices (. player :find :voices) pans (. player :find :pan_tables)
		chans (. player :find :chans) n (length chans) gain (. (. player :find :song) :find :mixgain))
	(each (lambda (voice)
			(defq at (* (!) +mix_size) vol (. voice :find :voice_volume) pan (. voice :find :pan))
			(when (. voice :find :restart)
				(. voice :insert :restart :nil)
				(elem-set voices (+ at +mix_pos) 0))
			(when (. voice :find :ring_restart)
				(. voice :insert :ring_restart :nil)
				(elem-set voices (+ at +mix_ring_pos) 0))
			(elem-set voices (+ at +mix_step) (. voice :find :delta))
			(elem-set voices (+ at +mix_left) (/ (* vol (elem-get (first pans) pan) gain) 128))
			(elem-set voices (+ at +mix_right) (/ (* vol (elem-get (second pans) pan) gain) 128))
			(elem-set voices (+ at +mix_ring) (if (. voice :find :ring_mix) (+ n (!)) -1))
			(elem-set voices (+ at +mix_ring_step) (. voice :find :ring_delta))
			(scatter voice :volume_out vol :period_out (. voice :find :delta) :pan_out pan))
		chans))

;;;;;;;;;;;;;;;;;;;;
; the song, played
;;;;;;;;;;;;;;;;;;;;

(defun tracker-player (song rate)
	; (tracker-player song rate) -> player
	;a player of a song at a rate of frames a second
	(defq channels (. song :find :channels) mode (. song :find :mode)
		player (scatter (Emap) :song song :rate rate :mode mode :channels channels
			:chans (case mode
				(:it (map (# (it-channel %0 %1 %2 (!))) (. song :find :pans) (. song :find :chvols) (. song :find :mutes)))
				(:med (map (# (med-channel %0 %1 (!))) (. song :find :pans) (. song :find :chvols)))
				;an AHX's voices are its replayer's, (ahx-player)
				(:ahx (list))
				(:t (map (case mode (:st3 (const st3-channel)) (:ft2 (const ft2-channel)) (:t (const pt-channel)))
					(. song :find :pans))))
			:voices (apply (const nums) (apply (const cat) (map! (lambda () '(-1 0 0 0 0 -1 0 0 0 0 0 0 0 0 -1 0 0)) ;+mix_sample, +mix_loop and +mix_ring none
				'() 0 (+ channels (defq extra (if (and (eql mode :it) (not (. song :find :sample_mode))) +it_voices 0))))))
			;an IT's past notes, and the voices free for them
			:past (list) :free (map! (# (+ channels (!))) '() 0 extra) :filters (Fmap 101)
			:samples (map (# (. %0 :find :data)) (. song :find :samples))
			:pitch (. song :find :pitch)
			:clock (case mode (:st3 +st3_clock) (:ft2 +ft2_clock) (:t +amiga_clock))
			:scale (case mode (:mod 1) ((:it :med) 16) (:t 64)) :stepped (find mode '(:it :med))
			:speed (. song :find :speed) :bpm (. song :find :tempo) :gvol (. song :find :global)
			:tick 0 :frame 0 :row 0 :order 0 :jump :nil :jump_order 0 :jump_row 0 :delay 0
			:repeat :nil :pbreak :nil :jump_to -1 :jumpline 0 :pdelay 0
			:rowdelay 0 :rowdelay_set 0 :loop_dest -1 :carry 0 :visited (Fset 101) :ended :nil :acc (nums)
			:gain (/ (* 512 (ifn (. song :find :mix) 48)) 48)))
	(case mode
		(:mod (. (. player :find :visited) :insert 0))
		(:ahx)
		(:med (flow-start player)
			;a MED's tick by its time factor, in doubles as libxmp has it, and
			;the slowest tempo that allows
			(bind '(num den) (. song :find :time_factor))
			(defq tf (/ (n2d num) (n2d den)))
			(scatter player :tf tf :min_bpm (n2i (+ (const (n2d 0.5)) (/ (* tf (const (n2d 20))) (const (n2d 10)))))))
		(:t (flow-start player)))
	;2^(n/768) of 16.16, to 2, for linear periods and the steps of an IT or a MED
	(if (or (eql (. song :find :pitch) :linear) (find mode '(:it :med)))
		(. player :insert :exp2 (apply (const nums) (map! (# (n2i (* (const (n2d 65536))
			(pow (const (n2d 2)) (/ (n2d (!)) (const (n2d 768))))))) '() 0 769))))
	(if (eql mode :mod) (. player :insert :tables (pt-tables)))
	(if (eql mode :ahx) (ahx-player player song) player))

(defun tracker-tick (player)
	; (tracker-tick player) -> frames
	;one tick of the song, and how many frames it lasts
	;an AHX's a frame of its replayer, a MED's by its time factor, else
	;2.5 / bpm seconds: for a MOD what is over a whole frame carried to the
	;next, as the Amiga's timer, the others whole frames, as their trackers
	(cond
		((eql (defq mode (. player :find :mode)) :mod)
			(pt-tick player)
			(defq rate (* 5 (. player :find :rate)) bpm (* 2 (. player :find :bpm))
				n (+ rate (. player :find :carry)))
			(. player :insert :carry (% n bpm))
			(/ n bpm))
		((eql mode :med) (med-tick player) (med-frames player))
		((eql mode :ahx)
			(ahx-tick player)
			(unless (. player :find :measure) (ahx-voices player))
			(ahx-frames player))
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

(defun tracker-playing (player)
	; (tracker-playing player) -> (sample ...)
	;the samples of the song that sound now, each once: those of the voices
	;the mixer plays to either side, or an AHX's voices' instruments
	(defq playing (list))
	(cond
		((eql (. player :find :mode) :ahx)
			(each (lambda (voice)
					(defq s (dec (. voice :find :ins_at)))
					(and (>= s 0) (> (. voice :find :volume_out) 0) (not (find s playing)) (push playing s)))
				(. player :find :chans)))
		(:t (defq voices (. player :find :voices))
			(each! (lambda ()
					(defq at (* (!) +mix_size) s (elem-get voices at))
					(and (>= s 0) (or (> (elem-get voices (+ at +mix_left)) 0) (> (elem-get voices (+ at +mix_right)) 0))
						(not (find s playing)) (push playing s)))
				'() 0 (/ (length voices) +mix_size))))
	playing)

(defun tracker-start (player frames)
	; (tracker-start player frames) -> start
	;where an order starts, as a player comes to it: its first frame, the
	;order and row, the speed, tempo and global volume, and a MOD's frames
	;carried over, (frames order row speed bpm gvol carry)
	(list frames (. player :find :order) (. player :find :row) (. player :find :speed)
		(. player :find :bpm) (. player :find :gvol) (. player :find :carry)))

(defun tracker-at (song rate start)
	; (tracker-at song rate start) -> player
	;a player of a song from where an order starts, (tracker-start), as
	;libxmp seeks: its speed, tempo and global volume as they were there,
	;its channels as at the song's start, a note already sounding then
	;not heard. The rows come to before are not known to it, so its end is
	;known by the song's length
	(bind '(& order row speed bpm gvol carry) start)
	(defq player (tracker-player song rate))
	(scatter player :order order :row row :speed speed :bpm bpm :gvol gvol :carry carry
		:visited (Fset 101))
	(. (. player :find :visited) :insert (flow-key order row))
	player)

(defun tracker-measure (song rate)
	; (tracker-measure song rate) -> measure
	;a player of a song that only finds how long it is: no past notes, as
	;voices are not what that is of, so they do not pile up unmixed. And
	;no channel's ticks but those that move the song on, a delayed note's
	;and an IT's tempo slide, (tracker-chans-play) and (it-measure-tick)
	;and where each order starts as it plays, (tracker-start)
	(defq measure (scatter (tracker-player song rate) :free (list) :measured 0 :measure :t))
	(. measure :insert :starts (list (tracker-start measure 0)))
	measure)

(defun tracker-measure-step (measure time)
	; (tracker-measure-step measure time) -> :nil | frames
	;its ticks played for so long, and how long the song is once ended
	(defq at (pii-time) frames (. measure :find :measured) starts (. measure :find :starts))
	(while (and (not (. measure :find :ended)) (< (- (pii-time) at) time))
		(setq frames (+ frames (tracker-tick measure)))
		;another order come to, not the one it goes round to at its end
		(and (not (. measure :find :ended)) (/= (. measure :find :order) (second (last starts)))
			(push starts (tracker-start measure frames))))
	(. measure :insert :measured frames)
	(if (. measure :find :ended) frames))

(defun tracker-length (song rate)
	; (tracker-length song rate) -> frames
	;how long a song plays for, to its end, its ticks played and not mixed
	(defq measure (tracker-measure song rate) frames :nil)
	(until (setq frames (tracker-measure-step measure 1000000)) (task-slice))
	frames)

(defun tracker-skip (player frames)
	; (tracker-skip player frames)
	;each voice moved on so many frames, as the mixer would, nothing mixed:
	;round its loop, back and forth along it, or ended
	(defq voices (. player :find :voices))
	(each! (lambda ()
			(defq at (* (!) +mix_size))
			(when (>= (elem-get voices at) 0)
				(defq pos (elem-get voices (+ at +mix_pos)) by (* (elem-get voices (+ at +mix_step)) frames)
					ls (elem-get voices (+ at +mix_loop)) endq (<< (elem-get voices (+ at +mix_end)) 16)
					flags (elem-get voices (+ at +mix_flags)) back (bits? flags +mix_back))
				(cond
					((and (not back) (< (+ pos by) endq)) (setq pos (+ pos by)))
					((< ls 0) (elem-set voices at -1))
					((not (bits? flags +mix_pingpong))
						(setq pos (+ pos by) pos (+ (<< ls 16) (% (- pos endq) (- endq (<< ls 16))))))
					(:t ;back and forth: where it is along the loop there and back
						(defq lsq (<< ls 16) len (- endq lsq)
							along (% (+ by (if back (- (* 2 len) (- pos lsq)) (- pos lsq))) (* 2 len)))
						(if (< along len)
							(setq pos (+ lsq along) flags (logand flags (const (lognot +mix_back))))
							(setq pos (+ lsq (- (* 2 len) along)) flags (logior flags +mix_back)))
						(elem-set voices (+ at +mix_flags) flags)))
				(elem-set voices (+ at +mix_pos) pos)
				;its ring, a HivelyTracker's, on round the loop as the mixer has it
				(when (and (>= (elem-get voices (+ at +mix_ring)) 0) (>= ls 0))
					(defq lsq (<< ls 16) ring (+ (elem-get voices (+ at +mix_ring_pos)) (* (elem-get voices (+ at +mix_ring_step)) frames)))
					(elem-set voices (+ at +mix_ring_pos) (if (>= ring endq) (+ lsq (% (- ring lsq) (- endq lsq))) ring)))))
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
