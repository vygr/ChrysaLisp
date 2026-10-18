(import "lib/options/options.inc")
(import "service/audio/app.inc")

(defq usage `(
(("-h" "--help")
"Usage: mic [options]

    options:
        -h --help: this help info.
        -s --seconds num: how long to listen, default 3.
        -o --out path: what was heard written to a .wav file.
        -p --play: what was heard played back, as a stream is.

    Listen to the microphone of the Audio service of this machine, and
    say what was heard: how many pieces, how many samples, any lost or
    out of order, and how loud, at its loudest and on the whole. What is
    heard is 16 bit mono at 16000 a second. A desktop that is never seen,
    -hidden, hears a tone of 440 a second, not a device.

    On macOS the program the host runs under, the terminal, must be let
    use the microphone, or it hears nothing.")
(("-s" "--seconds") ,(opt-num 'opt_s))
(("-o" "--out") ,(opt-str 'opt_o))
(("-p" "--play") ,(opt-flag 'opt_p))
))

(defun sample-at (data i)
	; (sample-at data index) -> num
	;the 16 bit sample at index, signed
	(defq v (code data 2 (* i 2)))
	(if (> v 32767) (- v 65536) v))

(defun wav-text (data)
	; (wav-text data) -> str
	;16 bit mono samples at the rate of a voice, as a .wav file
	(cat "RIFF" (char (+ 36 (length data)) 4) "WAVE"
		"fmt " (char 16 4) (char 1 2) (char 1 2) (char +audio_rate 4)
		(char (* +audio_rate 2) 4) (char 2 2) (char 16 2)
		"data" (char (length data) 4) data))

(defun play-back (data)
	;what was heard, as a stream, a piece of 20ms at a time and at the pace
	;it is played, a little ahead
	(when (defq handle (audio-stream-open-rpc))
		(defq piece (* (/ +audio_rate 50) 2) at 0 t0 (pii-time))
		(while (< at (length data))
			(audio-stream-write-rpc handle (slice data at (min (length data) (+ at piece))))
			(setq at (+ at piece))
			;20ms a piece, kept 100ms ahead of what has been played
			(defq ahead (- (* (/ at piece) 20000) (- (pii-time) t0)))
			(if (> ahead 100000) (task-sleep (- ahead 100000))))
		(task-sleep 300000)
		(audio-stream-close-rpc handle)
		:t))

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq opt_s 3 opt_o :nil opt_p :nil args (options stdio usage)))
		(defq who (mail-mbox) t0 (pii-time) renewed t0 pieces (list) seqs (list))
		(cond
			((not (audio-service)) (print "mic: no Audio service, a desktop has one"))
			((not (audio-listen-rpc who)) (print "mic: no microphone, or it can not be opened"))
			(:t (while (< (- (pii-time) t0) (* opt_s 1000000))
					;the lease is said again well before it is out
					(when (> (- (pii-time) renewed) (/ +audio_lease 2))
						(setq renewed (pii-time))
						(audio-listen-rpc who))
					(when (defq msg (mail-read-timeout who 100000))
						(push seqs (getf msg +audio_heard_seq))
						(push pieces (list (getf msg +audio_heard_seq) (slice msg +audio_heard_data -1)))))
				(audio-hush-rpc who)
				;in the order they were heard
				(sort pieces (# (- (first %0) (first %1))))
				(defq data (apply (const cat) (cat (list "") (map (const second) pieces)))
					samples (/ (length data) 2) peak 0 sum 0
					lost (if (nempty? seqs)
						(- (inc (- (reduce max seqs) (reduce min seqs))) (length seqs)) 0)
					late (reduce (lambda (n i) (if (< (elem-get seqs i) (elem-get seqs (dec i))) (inc n) n))
						(range 1 (max 1 (length seqs))) 0))
				(defq i 0)
				(while (< i samples)
					(defq v (abs (sample-at data i)))
					(setq peak (max peak v) sum (+ sum (* v v)) i (inc i)))
				(print "mic: " (length pieces) " pieces, " samples " samples, "
					(/ (* samples 1000) +audio_rate) "ms, " lost " lost, " late " out of order")
				(print "mic: loudest " peak ", on the whole " (if (> samples 0) (n2i (sqrt (n2r (/ sum samples)))) 0)
					" of 32767")
				(when opt_o
					(save (wav-text data) opt_o)
					(print "mic: written to " opt_o))
				(when opt_p
					(print (if (play-back data) "mic: played back" "mic: no stream could be opened")))))))
