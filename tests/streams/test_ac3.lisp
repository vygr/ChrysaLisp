(report-header "Streams: ac3, the sound of films of AC-3 and E-AC-3, our own decoder, against what was coded")

(import "lib/media/mp4.inc")
(import "lib/media/ac3.inc")

;six_eac3.mp4 and six_ac3.mp4 are half a second of 5.1, a tone on each
;channel, 6000 high, at 48000, made E-AC-3 and AC-3 by ffmpeg at 256
;kbit/s. The tones of the full channels, in AC-3's order, L C R Ls Rs
(defq a3_tones '(440 660 550 880 110))

(defun a3-decode (file)
	; (a3-decode file) -> (header frames bad (reals ...))
	(defq reel (mp4-reel (file-stream file)) stream (. reel :find :stream)
		track (some (# (and (eql (. %0 :find :kind) "soun") %0)) (. reel :find :tracks)))
	(mp4-tables track)
	(defq packet (lambda (i) (stream-seek stream (elem-get (. track :find :offsets) i) 0)
			(read-blk stream (elem-get (. track :find :sizes) i)))
		h (ac3-header (bits-of (packet 0))) d (ac3-decoder h) chans (map (lambda (_) (list)) (range 0 (+ (. h :find :fbw) (. h :find :lfe)))) bad 0)
	(each (lambda (i)
			(defq got (catch (ac3-frame d (packet i)) (progn (++ bad) :t)))
			(if (list? got) (each (# (push (elem-get chans (!)) %0)) got)))
		(range 0 (. track :find :samples)))
	(list h (. track :find :samples) bad (map (# (apply (const cat) %0)) chans) d))

(defun a3-ratio (got hz)
	;each channel against the tone it was, at the best of a few starts, a
	;block of the decoder late and the coder's own before it; in from the
	;ends, the signal to the error
	(defq best (n2r 0) w (/ (* (n2r 2) (n2r 3.14159265) (n2r hz)) (n2r 48000)))
	(each (lambda (start)
			(defq sig (n2r 0) err (n2r 0))
			(each (lambda (i)
					(defq want (* (n2r 6000) (sin (* w (n2r i)))) x (* (n2r -16777216) (elem-get got (+ i start))))
					(setq sig (+ sig (* want want)) err (+ err (* (- x want) (- x want)))))
				(range 4800 19200))
			(setq best (max best (/ sig err))))
		'(256 512 1792 2048))
	best)

(each (lambda ((file id what))
		(bind '(h frames bad chans d) (a3-decode file))
		(assert-list-eq (cat what ": its stream id, 5.1 with its LFE, 48000") (list id 7 1 48000)
			(list (. h :find :id) (. h :find :mode) (. h :find :lfe) (. h :find :rate)))
		(assert-eq (cat what ": every frame decodes") 0 bad)
		(assert-eq (cat what ": 1536 samples a frame a channel") (* frames 1536) (length (first chans)))
		(each (lambda (hz)
				(assert-true (cat what ": channel " (str (!)) " is the tone of " (str hz) " it was")
					(> (a3-ratio (elem-get chans (!)) hz) (n2r 1000))))
			a3_tones)
		(assert-true (cat what ": made two channels, both heard")
			(every (# (> (nums-sum (nums-mul %0 %0)) (n2r 0))) (ac3-stereo d (map (# (slice %0 4800 6336)) chans)))))
	'(("tests/streams/data/six_eac3.mp4" 16 "E-AC-3") ("tests/streams/data/six_ac3.mp4" 8 "AC-3")))

;short_ac3.mp4 is half a second of stereo at 48000, AC-3 by Aften at 256
;kbit/s with block switching on, from 0.4s of a signal: a tone of 0.01 on
;each, 440 and 550, and every quarter second a click of 0.9 for 2ms, 3000
;and 2000, the right's 0.1s before the left's. The right's click in frame 7
;is coded in short blocks
(defun a3-click (ch i)
	;what the signal was at sample i of channel ch, out of 16 bits
	(defq at (+ i 19200 (if (= ch 0) 0 4800)) w (/ (* (n2r 2) (n2r 3.14159265)) (n2r 48000)))
	(* (n2r 32767) (if (< (% at 12000) 96)
		(* (n2r 0.9) (sin (* w (n2r (if (= ch 0) 3000 2000)) (n2r (+ i 19200)))))
		(* (n2r 0.01) (sin (* w (n2r (if (= ch 0) 440 550)) (n2r (+ i 19200))))))))

(defun a3-click-ratio (got ch lo hi)
	;the signal to the error, samples lo to hi, a block of the decoder late
	(defq sig (n2r 0) err (n2r 0))
	(each (lambda (i)
			(defq want (a3-click ch i) x (* (n2r -16777216) (elem-get got (+ i 256))))
			(setq sig (+ sig (* want want)) err (+ err (* (- x want) (- x want)))))
		(range lo hi))
	(/ sig err))

(bind '(h frames bad chans d) (a3-decode "tests/streams/data/short_ac3.mp4"))
(assert-eq "short blocks: every frame decodes" 0 bad)
(assert-true "short blocks: some were decoded" (> (. d :find :shorts) 0))
(assert-true "short blocks: the tones between the clicks are as they were"
	(every (# (> (a3-click-ratio (elem-get chans %0) %0 (* 5 1536) (* 7 1536)) (n2r 10000))) '(0 1)))
(assert-true "short blocks: the right's click in short blocks is the click it was"
	(> (a3-click-ratio (second chans) 1 (* 7 1536) (* 8 1536)) (n2r 100)))
