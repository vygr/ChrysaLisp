(report-header "Streams: mp3, MPEG audio layer III, our own decoder, against what was coded")

(import "lib/media/mp3.inc")

;tone_mp3.mp3 is half a second of 440 a second on the left and 660 on the
;right, 0.25 high, at 44100, coded by LAME at 128 kbit/s, stereo.
;clicks_mp3.mp3 is 0.6 seconds of near the same on both sides, 440 and
;1320 and a little 2000 on the right, and every quarter second a click
;of 3000 for 2ms, joint stereo: its middle and side frames and its short
;blocks. tone22_mp3.mp3 is the tone of the first at 22050, MPEG-2, 48
;kbit/s. The samples come 2257 late, LAME's first frame of its tag, 1152
;of nothing, and its delay of 1105; of MPEG-2 1105 late, 576 and 529
(defq m3_late 2257 m3_rate 44100 m3_two_pi (* (n2r 2) (n2r 3.14159265)))

(defun m3-decode (file)
	; (m3-decode file) -> (frames bad left right)
	(defq s (load file) frames (mp3-frames s) d (mp3-decoder) left (list) right (list) bad 0)
	(each (lambda ((at size))
			(defq got (catch (mp3-frame d (slice s at (+ at size))) (progn (++ bad) :t)))
			(when (list? got) (push left (first got)) (push right (second got))))
		frames)
	(list (length frames) bad (apply (const cat) left) (apply (const cat) right)))

(defun m3-ratio (got want lo hi)
	; (m3-ratio samples want lo hi) -> real
	;the signal to the error, from lo to hi of the signal, want of a time
	(defq sig (n2r 0) err (n2r 0))
	(each (lambda (i)
			(defq w (* (n2r 32767) (want (/ (n2r i) (n2r m3_rate)))) x (elem-get got (+ i m3_late)))
			(setq sig (+ sig (* w w)) err (+ err (* (- x w) (- x w)))))
		(range lo hi))
	(/ sig err))

(defun m3-click (t)
	;the click of every quarter second, 2ms of 3000
	(if (< (- t (* (n2r 0.25) (n2r (n2i (* t (n2r 4)))))) (n2r 0.002)) (sin (* m3_two_pi (n2r 3000) t)) (n2r 0)))

(bind '(frames bad left right) (m3-decode "tests/streams/data/tone_mp3.mp3"))
(assert-eq "tone: every frame decodes" 0 bad)
(assert-eq "tone: 1152 samples a frame a side" (* frames 1152) (length left))
(assert-true "tone: the left is the 440 it was"
	(> (m3-ratio left (# (* (n2r 0.25) (sin (* m3_two_pi (n2r 440) %0)))) 4000 10000) (n2r 100)))
(assert-true "tone: the right is the 660 it was"
	(> (m3-ratio right (# (* (n2r 0.25) (sin (* m3_two_pi (n2r 660) %0)))) 4000 10000) (n2r 100)))

(bind '(frames bad left right) (m3-decode "tests/streams/data/clicks_mp3.mp3"))
(assert-eq "clicks: every frame decodes, middle and side and short blocks" 0 bad)
(assert-true "clicks: the left between the clicks is as it was"
	(> (m3-ratio left (# (+ (* (n2r 0.3) (sin (* m3_two_pi (n2r 440) %0))) (* (n2r 0.1) (sin (* m3_two_pi (n2r 1320) %0))))) 4000 10000) (n2r 100)))
(assert-true "clicks: the left's click at a quarter second is the click it was"
	(> (m3-ratio left (# (+ (* (n2r 0.3) (sin (* m3_two_pi (n2r 440) %0))) (* (n2r 0.1) (sin (* m3_two_pi (n2r 1320) %0)))
		(* (n2r 0.8) (m3-click %0)))) 10825 11425) (n2r 100)))
(assert-true "clicks: the right, of its own 2000 too, is as it was"
	(> (m3-ratio right (# (+ (* (n2r 0.28) (sin (* m3_two_pi (n2r 440) %0))) (* (n2r 0.1) (sin (* m3_two_pi (n2r 1320) %0)))
		(* (n2r 0.03) (sin (* m3_two_pi (n2r 2000) %0))))) 4000 10000) (n2r 100)))

(setq m3_late 1105 m3_rate 22050)
(bind '(frames bad left right) (m3-decode "tests/streams/data/tone22_mp3.mp3"))
(assert-eq "MPEG-2: every frame decodes, its scale factors" 0 bad)
(assert-eq "MPEG-2: 576 samples a frame a side" (* frames 576) (length left))
(assert-true "MPEG-2: the left is the 440 it was"
	(> (m3-ratio left (# (* (n2r 0.25) (sin (* m3_two_pi (n2r 440) %0)))) 2000 5000) (n2r 100)))
(assert-true "MPEG-2: the right is the 660 it was"
	(> (m3-ratio right (# (* (n2r 0.25) (sin (* m3_two_pi (n2r 660) %0)))) 2000 5000) (n2r 100)))
