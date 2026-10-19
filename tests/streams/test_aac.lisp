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
(assert-list-eq "AAC-LC, 48000, stereo, by its esds" '(2 3 48000 2) (aac-track-config ac_track))

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
