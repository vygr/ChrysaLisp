(report-header "Streams: flac, lossless, our own decoder, each sample as it was coded")

(import "lib/media/flac.inc")

;each file coded by the reference flac from a source of samples, which
;comes back exactly. Of the samples one channel after another a frame at
;a time, their sum, and their sum each by its place from 1, of the
;source: flac_lpc LPC at 16 bits, stereo; flac_24 LPC of order 32 at 24
;bits, one channel; flac_fixed the fixed predictors and frames of one
;number, its silence; flac_wasted 8 bit samples at 16, 8 bits a sample
;wasted; flac_verbatim noise, the samples as they are; flac_stereo the
;left and side, the right and side, and the mid and side
(defq fl_files '(("flac_lpc" 44100 2 16 22050 5036 -7524371853)
	("flac_24" 48000 1 24 4800 -2479503 -91223653820)
	("flac_fixed" 22050 2 16 11026 -70 -609100600)
	("flac_wasted" 16000 1 16 3200 0 -151961600)
	("flac_verbatim" 44100 2 16 4410 -236261 209520494)
	("flac_stereo" 44100 2 16 26460 5156 -5473313300)))

(defun fl-walk (file)
	; (fl-walk file) -> (info frames count sum placed)
	;the frames one after another, (at bytes) each, and the samples' sums
	(defq s (load file) info (flac-info (file-stream file)) at (. info :find :start) frames (list)
		count 0 sum 0 placed 0 nch (. info :find :channels))
	(while (and (< at (length s)) (defq got (flac-frame info (slice s at (min (length s) (+ at 65536))))))
		(bind '(chans used) got)
		(defq n (length (first chans)) places (apply (const nums) (range 0 n)))
		;the place of sample j of channel c is count, of all before, then j by
		;the channels and c, from 1
		(each (lambda (ch c)
				(defq total (nums-sum ch))
				(setq sum (+ sum total) placed (+ placed (* nch (nums-dot places ch)) (* (+ count c 1) total))))
			chans (range 0 nch))
		(push frames (list at used))
		(setq count (+ count (* n nch)) at (+ at used)))
	(list info frames count sum placed))

(each (lambda ((name rate nch bps count sum placed))
		(defq file (cat "tests/streams/data/" name ".flac"))
		(bind '(info frames got_count got_sum got_placed) (fl-walk file))
		(assert-list-eq (cat name ": STREAMINFO, its rate, channels and bits") (list rate nch bps)
			(list (. info :find :rate) (. info :find :channels) (. info :find :bps)))
		(assert-eq (cat name ": every sample") count got_count)
		(assert-eq (cat name ": the samples' sum, as the source's") sum got_sum)
		(assert-eq (cat name ": their sum by place, each where it was") placed got_placed)
		;the file as a reel: each packet the frame the walk found, in order and
		;by a seek, counted by STREAMINFO, none after the last
		(defq reel (reel-open file) s (load file)
			frame (lambda (n) (bind '(at len) (elem-get frames n)) (slice s at (+ at len))))
		(assert-eq (cat name ": its frames counted") (length frames) (. reel :find :frames))
		(assert-true (cat name ": each packet the frame, in order") (every (lambda (_) (eql (reel-packet reel (!)) (frame (!)))) frames))
		(assert-true (cat name ": each packet the frame, by a seek")
			(every (# (eql (reel-packet reel %0) (frame %0))) (cat (reverse (range 0 (length frames))) (list 0))))
		(assert-eq (cat name ": none after the last") :nil (reel-packet reel (length frames))))
	fl_files)
