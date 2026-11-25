;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space's crunches, the sound of rock hitting rock, made and kept: the near
; ones, data/crunch_1.wav and on, and the far one, data/crunch_far.wav. At
; random, so a bake of it is new sounds; bake_levels.lisp makes the quieter
; copies of whatever crunches there are, run it after:
;
;	lisp -r (import "apps/demos/space/bake_crunch.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/audio/wav.inc")

(defun rnd ()
	; (rnd) -> double
	;0 to 1
	(/ (n2d (random 1000000)) (const (n2d 1000000))))

;the crunch of rock on rock, as a hit is heard, made: a thump, a low tone
;falling as it dies away; the crunch, short bursts of noise, each filtered
;to a pitch of its own and as loud as it is, thinning out; and a rumble,
;deep noise smoothed and fading; mixed and clipped softly. Some of them,
;so a hit is not the same each time, and one as heard from far off: its
;highs smoothed away, as a sample taken down to a lower rate has, and
;clipped harder, so it is dull and gritty. 16 bit mono WAVs
(defq +crunch_rate 22050 +crunch_long (n2d 0.7) +crunch_count 3)

(defun noise ()
	; (noise) -> double
	;-1 to 1
	(- (* +double_2 (rnd)) +double_1))

(defun crunch (grit)
	; (crunch grit) -> doubles
	;the samples of a crunch, -1 to 1, of grit 1 to 2: how much more crunch
	;there is to its thump, its bursts the more, louder and brighter, and
	;sharp cracks among them, rock splitting
	(defq count (n2i (* +crunch_long (n2d +crunch_rate))) out (fill (doubles) count +double_0)
		rate (n2d +crunch_rate) phase +double_0 low +double_0)
	;the thump
	(each! (lambda ()
			(defq t (/ (n2d (!)) rate)
				hz (+ (const (n2d 38)) (* (const (n2d 32)) (exp (neg (* t (const (n2d 14)))))))
				env (* (min +double_1 (/ t (const (n2d 0.004)))) (exp (neg (/ t (const (n2d 0.12)))))))
			(setq phase (+ phase (/ (* +double_2pi hz) rate)))
			(elem-set out (!) (* (- (const (n2d 1.1)) (* (const (n2d 0.3)) grit)) env (sin phase))))
		'() 0 count)
	;the rumble, noise made deep by a slow follower, fading
	(each! (lambda (thump)
			(defq t (/ (n2d (!)) rate))
			(setq low (+ low (* (const (n2d 0.02)) (- (noise) low))))
			(elem-set out (!) (+ thump (* (const (n2d 2.2)) low (exp (neg (/ t (const (n2d 0.3)))))))))
		(list out))
	;the crunch, bursts of filtered noise, more of them early
	(times (n2i (* grit (n2d (+ 15 (random 11)))))
		(defq at (n2i (* rate (* (const (n2d 0.25)) (+ grit (const (n2d 0.4)))) (rnd) (rnd)))
			long (n2i (* rate (+ (const (n2d 0.004)) (* (const (n2d 0.02)) (rnd)))))
			follow (min (const (n2d 0.95)) (* (+ (const (n2d 0.08)) (* (const (n2d 0.5)) (rnd))) grit))
			loud (* grit (+ (const (n2d 0.2)) (* (const (n2d 0.8)) (rnd))) (exp (neg (/ (n2d at) (* rate (const (n2d 0.15)))))))
			v +double_0)
		(each! (lambda ()
				(defq i (+ at (!)))
				(when (< i count)
					(setq v (+ v (* follow (- (noise) v))))
					(elem-set out i (+ (elem-get out i) (* loud v (- +double_1 (/ (n2d (!)) (n2d long))))))))
			'() 0 long))
	;the cracks, a grit of more than 1 has, clicks of bright noise a
	;millisecond or two long
	(times (n2i (* (- grit +double_1) (const (n2d 14))))
		(defq at (n2i (* rate (const (n2d 0.3)) (rnd) (rnd))) long (+ 10 (random 35))
			loud (* (const (n2d 1.6)) (+ (const (n2d 0.4)) (* (const (n2d 0.6)) (rnd)))))
		(each! (lambda ()
				(defq i (+ at (!)))
				(if (< i count) (elem-set out i (+ (elem-get out i) (* loud (noise) (- +double_1 (/ (n2d (!)) (n2d long))))))))
			'() 0 long))
	out)

(defun crunch-far (near)
	; (crunch-far near) -> doubles
	;a crunch as heard far off: its highs smoothed away, a follower of it
	;twice, and clipped harder
	(defq out (cat near) v +double_0)
	(times 2
		(setq v +double_0)
		(each! (lambda (e) (setq v (+ v (* (const (n2d 0.3)) (- e v)))) (elem-set out (!) v))
			(list out)))
	(map (# (tanh (* %0 (const (n2d 2.5))))) out))

(defun crunch-save (samples file drive)
	; (crunch-save samples file drive)
	;the samples made as loud as they go, a little under, clipped softly,
	;as hard as drive, and written as a 16 bit mono WAV
	(defq most (reduce (lambda (m v) (max m (abs v))) samples (const (n2d 0.0001)))
		pcm (reduce (lambda (out v)
				(push out (n2i (* (const (n2d 30000)) (/ (tanh (* drive (/ v most))) (tanh drive))))))
			samples (shorts))
		bytes (* (length pcm) 2) stream (file-stream file +file_open_write))
	(WAV-head stream +crunch_rate)
	(write-blk stream (obj-set (str-alloc bytes) +str_data bytes pcm))
	(WAV-tail stream +crunch_rate bytes))

(defun bake-crunches ()
	; (bake-crunches) -> :t
	;near, crunchy and driven hard; far, of a crunch of less grit, as it was
	(each! (lambda () (crunch-save (crunch (const (n2d 2.2)))
			(cat "apps/demos/space/data/crunch_" (str (inc (!))) ".wav") (const (n2d 2.0))))
		'() 0 +crunch_count)
	(crunch-save (crunch-far (crunch +double_1)) "apps/demos/space/data/crunch_far.wav" (const (n2d 1.3)))
	:t)

(time-it "space crunches baked" (bake-crunches))
