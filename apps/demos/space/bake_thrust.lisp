;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space's thrust, the roar of the ship's boost, made and kept:
; data/thrust.wav, four seconds of it that go round without a seam, long
; enough that the ear does not hear it go round, the app
; writing it to a stream as loud as the boost is. At random, so a bake of
; it is a new roar:
;
;	lisp -r (import "apps/demos/space/bake_thrust.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/vector.inc")
(import "lib/audio/wav.inc")

;its rate, how long the loop is, seconds, and how long its end is faded
;into its start; the hum goes round a whole number of times in the loop;
;the steps of (rnd); how hard the noise is smoothed for the rumble, the
;deep rumble and the hiss, and how loud each is and the hum; the least
;peak it is made as loud as, how loud that is, and how softly it is
;clipped
(defq +thrust_rate 22050 +thrust_long 4 +thrust_join +double_1/2 +thrust_hum 55
	+rnd_steps (n2d 1000000) +rumble_smooth (n2d 0.015) +deep_smooth (n2d 0.004)
	+hiss_smooth (n2d 0.45) +rumble_loud (n2d 3.5) +deep_loud +double_6 +hiss_loud (n2d 0.35)
	+hum_loud (n2d 0.08) +least_peak (n2d 0.0001) +thrust_peak (n2d 30000) +thrust_drive (n2d 1.4))

(defun rnd ()
	; (rnd) -> double
	;0 to 1
	(/ (n2d (random 1000000)) +rnd_steps))

(defun noise ()
	; (noise) -> double
	;-1 to 1
	(- (* +double_2 (rnd)) +double_1))

(defun thrust ()
	; (thrust) -> doubles
	;the samples of the roar, -1 to 1, the loop and its join more: a deep
	;rumble, noise smoothed hard; the hiss of the exhaust, noise between a
	;high and a low smoothing of it; and a faint low hum. Nothing that beats
	;in time, so nothing in it marks where the loop goes round
	(defq rate (n2d +thrust_rate) count (n2i (+ (* rate (n2d +thrust_long)) (* rate +thrust_join)))
		out (fill (doubles) count +double_0) low +double_0 high +double_0 deep +double_0)
	(each! (lambda ()
			(defq t (/ (n2d (!)) rate) n (noise))
			(setq low (+ low (* +rumble_smooth (- n low))) deep (+ deep (* +deep_smooth (- n deep)))
				high (+ high (* +hiss_smooth (- n high))))
			(elem-set out (!) (+ (* +rumble_loud low) (* +deep_loud deep) (* +hiss_loud (- high low))
				(* +hum_loud (sin (* +double_2pi (n2d +thrust_hum) t))))))
		'() 0 count)
	out)

(defun thrust-loop (samples)
	; (thrust-loop samples) -> doubles
	;the loop, its first seconds, its start faded in from what came after
	;its end, so its end goes on into its start as it did into what came
	;after
	(defq loop (n2i (* (n2d +thrust_rate) (n2d +thrust_long)))
		join (n2i (* (n2d +thrust_rate) +thrust_join)) out (slice samples 0 loop))
	(each! (lambda (after was)
			(defq k (/ (n2d (!)) (n2d join)))
			(elem-set out (!) (+ (* k was) (* (- +double_1 k) after))))
		(list (slice samples loop -1) out))
	out)

(defun thrust-save (samples file)
	; (thrust-save samples file)
	;the samples made as loud as they go, a little under, clipped softly,
	;and written as a 16 bit mono WAV
	(defq most (reduce (lambda (m v) (max m (abs v))) samples +least_peak)
		pcm (reduce (lambda (out v)
				(push out (n2i (* +thrust_peak (/ (tanh (* +thrust_drive (/ v most))) (tanh +thrust_drive))))))
			samples (shorts))
		bytes (* (length pcm) 2) stream (file-stream file +file_open_write))
	(WAV-head stream +thrust_rate)
	(write-blk stream (obj-set (str-alloc bytes) +str_data bytes pcm))
	(WAV-tail stream +thrust_rate bytes))

(time-it "space thrust baked" (thrust-save (thrust-loop (thrust)) "apps/demos/space/data/thrust.wav"))
