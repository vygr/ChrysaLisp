;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space's boom, a buoy gone nuclear, made and kept: data/boom.wav, a crack,
; a deep thump falling away, and a long rumble rolling off:
;
;	lisp -r (import "apps/demos/space/bake_boom.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/vector.inc")
(import "lib/audio/wav.inc")

;its rate and how long; the crack, how long it lasts and how loud; the
;thump, the pitch it starts at and falls to, how fast it falls, how long
;it lasts and how loud; the rumble, how high it goes, how soon it comes
;up, how long it takes to die away and how loud, how fast it rolls and how
;much; how hard it is clipped; and the least peak it is made as loud as,
;and how loud that is
(defq +boom_rate 22050 +boom_long (n2d 3.6)
	+crack_long (n2d 0.03) +crack_loud (n2d 0.6)
	+thump_from (n2d 90) +thump_to (n2d 28) +thump_fall (n2d 6) +thump_long (n2d 0.5) +thump_loud (n2d 1.1)
	+rumble_hz (n2d 160) +rumble_up (n2d 0.06) +rumble_long (n2d 1.1) +rumble_loud (n2d 1.6)
	+roll_hz (n2d 3) +roll_much (n2d 0.5) +boom_drive (n2d 1.6)
	+least_peak (n2d 0.0001) +boom_peak (n2d 28000) +noise_steps (n2d 1000000))

(defun noise ()
	; (noise) -> double
	;white noise, -1 to 1
	(- (* +double_2 (/ (n2d (random 1000000)) +noise_steps)) +double_1))

(defun boom ()
	; (boom) -> doubles
	;the samples of a boom, -1 to 1: a crack of noise, gone in a few
	;hundredths of a second; a thump, a sine falling from a low pitch to a
	;lower, dying in half a second; and a rumble, noise low passed twice,
	;up quickly and dying away over seconds, its loudness wobbling slowly
	;so it rolls; clipped softly, for weight
	(defq rate (n2d +boom_rate) count (n2i (* +boom_long rate)) out (fill (doubles) count +double_0)
		phase +double_0 k (/ (* +double_2pi +rumble_hz) (+ rate (* +double_2pi +rumble_hz)))
		r (/ (* +double_2pi +roll_hz) (+ rate (* +double_2pi +roll_hz)))
		l1 +double_0 l2 +double_0 w1 +double_0 w2 +double_0)
	(each! (lambda ()
			(defq t (/ (n2d (!)) rate)
				hz (+ +thump_to (* (- +thump_from +thump_to) (exp (* (neg +thump_fall) t)))))
			(setq phase (+ phase (/ hz rate)) phase (- phase (floor phase))
				;the rumble's noise low passed twice, and the roll's slow wobble
				l1 (+ l1 (* k (- (noise) l1))) l2 (+ l2 (* k (- l1 l2)))
				w1 (+ w1 (* r (- (noise) w1))) w2 (+ w2 (* r (- w1 w2))))
			(defq crack (* +crack_loud (noise) (exp (neg (/ t +crack_long))))
				thump (* +thump_loud (sin (* +double_2pi phase)) (exp (neg (/ t +thump_long))))
				rumble (* +rumble_loud l2 (min +double_1 (/ t +rumble_up)) (exp (neg (/ t +rumble_long)))
					(+ +double_1 (* +roll_much (* w2 +double_4)))))
			(elem-set out (!) (tanh (* +boom_drive (+ crack thump rumble)))))
		'() 0 count)
	out)

(defun boom-save (samples file)
	; (boom-save samples file)
	;the samples made as loud as they go, a little under, and written as a
	;16 bit mono WAV
	(defq most (reduce (lambda (m v) (max m (abs v))) samples +least_peak)
		pcm (reduce (lambda (out v) (push out (n2i (* +boom_peak (/ v most))))) samples (shorts))
		bytes (* (length pcm) 2) stream (file-stream file +file_open_write))
	(WAV-head stream +boom_rate)
	(write-blk stream (obj-set (str-alloc bytes) +str_data bytes pcm))
	(WAV-tail stream +boom_rate bytes))

(time-it "space boom baked" (boom-save (boom) "apps/demos/space/data/boom.wav"))
