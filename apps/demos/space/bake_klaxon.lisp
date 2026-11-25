;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space's klaxon, the warning of a buoy armed to go nuclear, made and kept:
; data/klaxon.wav, one a-oo-gah of it, the old horn's, a submarine's
; diving alarm's:
;
;	lisp -r (import "apps/demos/space/bake_klaxon.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/vector.inc")
(import "lib/audio/wav.inc")

;its rate and how long; the pitch it starts at, swoops up to by the end
;of the a-oo, and drops to for the gah, and when; and the two pitches its
;horn rings at, and how sharply
(defq +klaxon_rate 22050 +klaxon_long (n2d 0.9)
	+klaxon_from (n2d 130) +klaxon_top (n2d 390) +klaxon_end (n2d 270) +klaxon_oo (n2d 0.5)
	+klaxon_ring_1 (n2d 650) +klaxon_ring_2 (n2d 1700) +klaxon_sharp (n2d 5))

(defun ring (hz)
	; (ring hz) -> (b0 b2 a1 a2)
	;a resonance at a pitch, as sharp as +klaxon_sharp, the numbers of its
	;filter, a band pass of two poles, its peak as loud as what goes in
	(defq w (/ (* +double_2pi hz) (n2d +klaxon_rate)) alpha (/ (sin w) (* +double_2 +klaxon_sharp))
		a0 (+ +double_1 alpha))
	(list (/ alpha a0) (/ (neg alpha) a0) (/ (* (const (n2d -2)) (cos w)) a0) (/ (- +double_1 alpha) a0)))

(defun klaxon ()
	; (klaxon) -> doubles
	;the samples of an a-oo-gah, -1 to 1: a diaphragm buzzing, a narrow
	;pulse, its pitch swooping up through the a-oo and dropping for the
	;gah; rung by the horn at two pitches, the nasal honk of it, with a
	;little of the buzz itself; clipped hard, as a horn is harsh; quick on,
	;louder on the gah, and cut off short
	(defq rate (n2d +klaxon_rate) count (n2i (* +klaxon_long rate)) out (fill (doubles) count +double_0)
		phase +double_0 one (ring +klaxon_ring_1) two (ring +klaxon_ring_2)
		x1 +double_0 x2 +double_0 p1 +double_0 p2 +double_0 q1 +double_0 q2 +double_0)
	(bind '(b0 b2 a1 a2) one)
	(bind '(c0 c2 e1 e2) two)
	(each! (lambda ()
			(defq t (/ (n2d (!)) rate)
				up (min +double_1 (/ t +klaxon_oo))
				hz (if (< t +klaxon_oo)
					(+ +klaxon_from (* (- +klaxon_top +klaxon_from) (- +double_1 (* (- +double_1 up) (- +double_1 up)))))
					(- +klaxon_top (* (- +klaxon_top +klaxon_end)
						(min +double_1 (/ (- t +klaxon_oo) (const (n2d 0.12))))))))
			(setq phase (+ phase (/ hz rate)) phase (- phase (floor phase)))
			;the buzz, a pulse a quarter of its cycle, its middle taken off
			(defq x (if (< phase (const (n2d 0.25))) (const (n2d 0.75)) (const (n2d -0.25)))
				p (- (+ (* b0 x) (* b2 x2)) (+ (* a1 p1) (* a2 p2)))
				q (- (+ (* c0 x) (* c2 x2)) (+ (* e1 q1) (* e2 q2)))
				env (* (min +double_1 (/ t (const (n2d 0.02))))
					(if (< t +klaxon_oo) (const (n2d 0.8)) +double_1)
					(min +double_1 (/ (- +klaxon_long t) (const (n2d 0.06))))))
			(setq x2 x1 x1 x p2 p1 p1 p q2 q1 q1 q)
			(elem-set out (!) (* env (tanh (* (const (n2d 3)) (+ p (* (const (n2d 0.7)) q) (* (const (n2d 0.15)) x)))))))
		'() 0 count)
	out)

(defun klaxon-save (samples file)
	; (klaxon-save samples file)
	;the samples made as loud as they go, a little under, and written as a
	;16 bit mono WAV
	(defq most (reduce (lambda (m v) (max m (abs v))) samples (const (n2d 0.0001)))
		pcm (reduce (lambda (out v) (push out (n2i (* (const (n2d 28000)) (/ v most))))) samples (shorts))
		bytes (* (length pcm) 2) stream (file-stream file +file_open_write))
	(WAV-head stream +klaxon_rate)
	(write-blk stream (obj-set (str-alloc bytes) +str_data bytes pcm))
	(WAV-tail stream +klaxon_rate bytes))

(time-it "space klaxon baked" (klaxon-save (klaxon) "apps/demos/space/data/klaxon.wav"))
