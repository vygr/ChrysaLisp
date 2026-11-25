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
;into its start; the hum goes round a whole number of times in the loop
(defq +thrust_rate 22050 +thrust_long 4 +thrust_join (n2d 0.5) +thrust_hum 55)

(defun rnd ()
	; (rnd) -> double
	;0 to 1
	(/ (n2d (random 1000000)) (const (n2d 1000000))))

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
			(setq low (+ low (* (const (n2d 0.015)) (- n low))) deep (+ deep (* (const (n2d 0.004)) (- n deep)))
				high (+ high (* (const (n2d 0.45)) (- n high))))
			(elem-set out (!) (+ (* (const (n2d 3.5)) low) (* (const (n2d 6)) deep) (* (const (n2d 0.35)) (- high low))
				(* (const (n2d 0.08)) (sin (* +double_2pi (n2d +thrust_hum) t))))))
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
	(defq most (reduce (lambda (m v) (max m (abs v))) samples (const (n2d 0.0001)))
		pcm (reduce (lambda (out v)
				(push out (n2i (* (const (n2d 30000)) (/ (tanh (* (const (n2d 1.4)) (/ v most))) (tanh (const (n2d 1.4))))))))
			samples (shorts))
		bytes (* (length pcm) 2) stream (file-stream file +file_open_write))
	(WAV-head stream +thrust_rate)
	(write-blk stream (obj-set (str-alloc bytes) +str_data bytes pcm))
	(WAV-tail stream +thrust_rate bytes))

(time-it "space thrust baked" (thrust-save (thrust-loop (thrust)) "apps/demos/space/data/thrust.wav"))
