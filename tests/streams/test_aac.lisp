(report-header "Streams: aac, the sound of a film, our own decoder, against what was coded")

(import "lib/media/mp4.inc")
(import "lib/media/aac.inc")

;tone.m4a is half a second of 440 a second on the left and 660 on the
;right, 8000 high, at 48000, made AAC-LC by afconvert at 128 kbit/s. It
;starts after what its coder put first, 2112, which it says in metadata
;of iTunes', not an edit list
(defq ac_reel (mp4-reel (file-stream "tests/streams/data/tone.m4a"))
	ac_track (some (# (if (eql (. %0 :find :kind) "soun") %0)) (. ac_reel :find :tracks)))
(assert-true "a sound track" ac_track)
(mp4-tables ac_track)
(assert-list-eq "AAC-LC, 48000, stereo, by its esds" '(2 3 48000 2 :nil :nil) (aac-track-config ac_track))

(defq ac_decoder (aac-decoder (aac-track-config ac_track)) ac_stream (. ac_reel :find :stream)
	ac_left (list) ac_right (list) ac_bad 0)
(each (lambda (i)
		(stream-seek ac_stream (elem-get (. ac_track :find :offsets) i) 0)
		(defq chans (catch (aac-decode ac_decoder (read-blk ac_stream (elem-get (. ac_track :find :sizes) i))) (progn (++ ac_bad) :t)))
		(when (list? chans)
			(each (# (push ac_left %0)) (first chans))
			(each (# (push ac_right %0)) (second chans))))
	(range 0 (. ac_track :find :samples)))
(assert-eq "every packet decodes" 0 ac_bad)

;each side against the tone it was, from the start the edit list says,
;a little in from either end: what was coded and what is heard, more than
;30dB apart
(defun ac-ratio (got hz)
	(defq start 2112 sig (n2r 0) err (n2r 0)
		w (/ (* (n2r 2) (n2r 3.14159265) (n2r hz)) (n2r 48000)))
	(each (lambda (i)
			(defq want (* (n2r 8000) (sin (* w (n2r i)))) x (elem-get got (+ i start)))
			(setq sig (+ sig (* want want)) err (+ err (* (- x want) (- x want)))))
		(range 4800 19200))
	(/ sig err))
(assert-true "the left is the tone of 440 it was" (> (ac-ratio ac_left 440) (n2r 1000)))
(assert-true "the right is the tone of 660 it was" (> (ac-ratio ac_right 660) (n2r 1000)))

;he_aac.m4a is 0.6 seconds of 440 on the left and 660 on the right, 8192
;high, at 44100, the left with white noise above 9000 too, made HE-AAC by
;afconvert at 48 kbit/s: a core of AAC-LC at 22050, said by the sync
;extension of its config, and SBR in its fill elements, which makes the
;band above the core again of the band below. Of the second differences
;of the samples, which the 660 a second barely has, ffmpeg's decode gives
;9421 on the left and 55 on the right
(defq he_reel (mp4-reel (file-stream "tests/streams/data/he_aac.m4a"))
	he_track (some (# (if (eql (. %0 :find :kind) "soun") %0)) (. he_reel :find :tracks)))
(mp4-tables he_track)
(assert-list-eq "HE-AAC: a core at 22050, stereo, SBR, by its sync extension" '(2 7 22050 2 :t :nil) (aac-track-config he_track))
(defq he_decoder (aac-decoder (aac-track-config he_track)) he_stream (. he_reel :find :stream)
	he_left (list) he_right (list) he_sizes (list))
(each (lambda (i)
		(stream-seek he_stream (elem-get (. he_track :find :offsets) i) 0)
		(defq chans (aac-decode he_decoder (read-blk he_stream (elem-get (. he_track :find :sizes) i))))
		(push he_sizes (length (first chans)))
		(push he_left (first chans))
		(push he_right (second chans)))
	(range 0 (. he_track :find :samples)))
(assert-true "HE-AAC: every packet 2048 samples a side, at 44100" (every (# (= %0 2048)) he_sizes))
(setq he_left (apply (const cat) he_left) he_right (apply (const cat) he_right))
(defun he-second (x)
	; (he-second samples) -> real
	;the rms of the second differences, from 6000 to 24000
	(defq a (slice x 5999 23999) b (slice x 6000 24000) c (slice x 6001 24001)
		d (nums-add (nums-sub c b) (nums-sub a b)))
	(sqrt (/ (nums-dot d d) (n2r 18000))))
(defun he-tone (x hz)
	; (he-tone samples hz) -> real
	;the level of a tone, from 6000 to 24000
	(defq w (/ (* (n2r 2) (n2r 3.14159265) (n2r hz)) (n2r 44100)) c (n2r 0) s (n2r 0))
	(each! (lambda (v)
			(defq a (* w (n2r (!))))
			(setq c (+ c (* v (cos a))) s (+ s (* v (sin a)))))
		(list x) 6000 24000)
	(/ (* (n2r 2) (sqrt (+ (* c c) (* s s)))) (n2r 18000)))
(assert-true "HE-AAC: the left's 440 as it was coded" (< (n2r 7500) (he-tone he_left 440) (n2r 8800)))
(assert-true "HE-AAC: the right's 660 as it was coded" (< (n2r 7500) (he-tone he_right 660) (n2r 8800)))
(assert-true "HE-AAC: the left's band above the core made again, as ffmpeg makes it" (< (n2r 7500) (he-second he_left) (n2r 11300)))
(assert-true "HE-AAC: the right with none made, it had none" (< (he-second he_right) (n2r 200)))

;he_ps.m4a is half a second of 440 on the left and 660 on the right, made
;HE-AAC v2 by afconvert at 24 kbit/s: a core of one channel, SBR, and
;parametric stereo, which makes the two of the one by how they differ
;band by band. Each side has its own tone and little of the other's, as
;ffmpeg makes them, 7618 and 302 on the left, 8211 and 375 on the right
(defq ps_reel (mp4-reel (file-stream "tests/streams/data/he_ps.m4a"))
	ps_track (some (# (if (eql (. %0 :find :kind) "soun") %0)) (. ps_reel :find :tracks)))
(mp4-tables ps_track)
(assert-list-eq "HE-AAC v2: a core of one channel, SBR and PS, by its sync extensions" '(2 7 22050 1 :t :t) (aac-track-config ps_track))
(defq ps_decoder (aac-decoder (aac-track-config ps_track)) ps_stream (. ps_reel :find :stream) ps_left (list) ps_right (list))
(each (lambda (i)
		(stream-seek ps_stream (elem-get (. ps_track :find :offsets) i) 0)
		(defq chans (aac-decode ps_decoder (read-blk ps_stream (elem-get (. ps_track :find :sizes) i))))
		(push ps_left (first chans))
		(push ps_right (second chans)))
	(range 0 (. ps_track :find :samples)))
(setq ps_left (apply (const cat) ps_left) ps_right (apply (const cat) ps_right))
(assert-true "HE-AAC v2: two channels of the one" (and (= (length ps_left) (length ps_right)) (> (length ps_left) 30000)))
(assert-true "HE-AAC v2: the left's 440" (< (n2r 7000) (he-tone ps_left 440) (n2r 8500)))
(assert-true "HE-AAC v2: the left with little of the right's 660" (< (he-tone ps_left 660) (n2r 1000)))
(assert-true "HE-AAC v2: the right's 660" (< (n2r 7500) (he-tone ps_right 660) (n2r 9000)))
(assert-true "HE-AAC v2: the right with little of the left's 440" (< (he-tone ps_right 440) (n2r 1000)))
