;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space's crunches, the sound of rock hitting rock, made and kept: the near
; ones, data/crunch_1.wav and on, and the far one, data/crunch_far.wav. At
; random, so a bake of it is new sounds; bake_levels.lisp makes the quieter
; copies of whatever crunches there are, run it after:
;
;	lisp -r (import "apps/demos/space/bake_crunch.lisp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/audio/wav.inc")

;the steps of (rnd). The thump, its low pitch, how far above it it starts
;and how fast it falls, how long it takes to come on and to die away, how
;loud it is and how much less with grit; the rumble, how hard its noise is
;smoothed, how loud it is and how long; the bursts, how late in it they
;come past its grit, their least length and spread, the least and the
;most of how each follows its pitch, the spread of their loudness and
;their fading; the grains, how many to a grit, how late they come, how
;loud and the least and the spread of it; the far one, how hard it is
;smoothed and clipped; the least peak a crunch is made as loud as, and
;how loud that is; and the grit of a near one, and how hard the far one
;is driven as it is kept
(defq +rnd_steps (n2d 1000000) +thump_low (n2d 38) +thump_fall (n2d 32) +thump_fall_rate (n2d 14)
	+thump_on (n2d 0.004) +thump_long (n2d 0.12) +thump_loud (n2d 1.1) +thump_grit (n2d 0.3)
	+rumble_smooth (n2d 0.02) +rumble_loud (n2d 2.2) +rumble_long (n2d 0.3) +burst_late (n2d 0.4)
	+burst_least (n2d 0.004) +burst_spread (n2d 0.02) +follow_least (n2d 0.08)
	+follow_most (n2d 0.95) +burst_loud_spread (n2d 0.8) +burst_fade (n2d 0.15)
	+grains_per_grit (n2d 14) +grains_late (n2d 0.3) +grain_loud (n2d 1.6)
	+grain_loud_least (n2d 0.4) +grain_loud_spread (n2d 0.6) +far_smooth (n2d 0.3)
	+far_drive (n2d 2.5) +least_peak (n2d 0.0001) +crunch_peak (n2d 30000) +near_grit (n2d 2.2)
	+far_drive_save (n2d 1.3))

(defun rnd ()
	; (rnd) -> double
	;0 to 1
	(/ (n2d (random 1000000)) +rnd_steps))

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
				hz (+ +thump_low (* +thump_fall (exp (neg (* t +thump_fall_rate)))))
				env (* (min +double_1 (/ t +thump_on)) (exp (neg (/ t +thump_long)))))
			(setq phase (+ phase (/ (* +double_2pi hz) rate)))
			(elem-set out (!) (* (- +thump_loud (* +thump_grit grit)) env (sin phase))))
		'() 0 count)
	;the rumble, noise made deep by a slow follower, fading
	(each! (lambda (thump)
			(defq t (/ (n2d (!)) rate))
			(setq low (+ low (* +rumble_smooth (- (noise) low))))
			(elem-set out (!) (+ thump (* +rumble_loud low (exp (neg (/ t +rumble_long)))))))
		(list out))
	;the crunch, bursts of filtered noise, more of them early
	(times (n2i (* grit (n2d (+ 15 (random 11)))))
		(defq at (n2i (* rate (* +double_1/4 (+ grit +burst_late)) (rnd) (rnd)))
			long (n2i (* rate (+ +burst_least (* +burst_spread (rnd)))))
			follow (min +follow_most (* (+ +follow_least (* +double_1/2 (rnd))) grit))
			loud (* grit (+ +double_1/5 (* +burst_loud_spread (rnd))) (exp (neg (/ (n2d at) (* rate +burst_fade)))))
			v +double_0)
		(each! (lambda ()
				(defq i (+ at (!)))
				(when (< i count)
					(setq v (+ v (* follow (- (noise) v))))
					(elem-set out i (+ (elem-get out i) (* loud v (- +double_1 (/ (n2d (!)) (n2d long))))))))
			'() 0 long))
	;the cracks, a grit of more than 1 has, clicks of bright noise a
	;millisecond or two long
	(times (n2i (* (- grit +double_1) +grains_per_grit))
		(defq at (n2i (* rate +grains_late (rnd) (rnd))) long (+ 10 (random 35))
			loud (* +grain_loud (+ +grain_loud_least (* +grain_loud_spread (rnd)))))
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
		(each! (lambda (e) (setq v (+ v (* +far_smooth (- e v)))) (elem-set out (!) v))
			(list out)))
	(map (# (tanh (* %0 +far_drive))) out))

(defun crunch-save (samples file drive)
	; (crunch-save samples file drive)
	;the samples made as loud as they go, a little under, clipped softly,
	;as hard as drive, and written as a 16 bit mono WAV
	(defq most (reduce (lambda (m v) (max m (abs v))) samples +least_peak)
		pcm (reduce (lambda (out v)
				(push out (n2i (* +crunch_peak (/ (tanh (* drive (/ v most))) (tanh drive))))))
			samples (shorts))
		bytes (* (length pcm) 2) stream (file-stream file +file_open_write))
	(WAV-head stream +crunch_rate)
	(write-blk stream (obj-set (str-alloc bytes) +str_data bytes pcm))
	(WAV-tail stream +crunch_rate bytes))

(defun bake-crunches ()
	; (bake-crunches) -> :t
	;near, crunchy and driven hard; far, of a crunch of less grit, as it was
	(each! (lambda () (crunch-save (crunch +near_grit)
			(cat "apps/demos/space/data/crunch_" (str (inc (!))) ".wav") +double_2))
		'() 0 +crunch_count)
	(crunch-save (crunch-far (crunch +double_1)) "apps/demos/space/data/crunch_far.wav" +far_drive_save)
	:t)

(time-it "space crunches baked" (bake-crunches))
