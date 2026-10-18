;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/audio/stages.inc - the stages of a sound pipeline
;
; Each is a stage of lib/streams/stages.inc, (fn in out arg ...), a task
; that reads the samples of one stream and writes the next, 16 bit mono,
; in pieces of 20ms, and knows nothing of what is either side of it. A
; source has no in, :nil, a sink no out. Made with (sound-stage fn arg ...)
; and run with (stages-run stages [done]):
;
;	(stages-run (list (sound-stage 'sound-mic (stage-mbox ctl))
;		(sound-stage 'sound-gain 200) (sound-stage 'sound-to-file "a.wav")) done)
;
; Sources: sound-mic, what the microphone hears till its ctl mailbox is
; sent anything; sound-tone, a tone; sound-from-file, a sound file.
; Filters: sound-gain, sound-lowpass, sound-level, which tells a mailbox
; how loud each piece is and passes it on. Sinks: sound-to-file, as it
; comes, and sound-speaker, played at the pace it is heard.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././streams/stages.inc")
(import "service/audio/app.inc")
(import "./wav.inc")

;a piece, 20ms of samples, in bytes
(defq +sound_piece (* (/ +sound_rate 50) 2))

(defun sound-stage (fn &rest args)
	; (sound-stage fn [arg ...]) -> stage
	;a stage of this module
	(apply (const stage) (cat (list "lib/audio/stages.inc" fn) args)))

(defun sound-map (data fn)
	; (sound-map data fn) -> data
	;each sample of the piece made new, (fn sample) -> sample, kept in range
	(defq n (/ (length data) 2) out (list) i 0)
	(while (< i n)
		(push out (char (max -32768 (min 32767 (fn (sound-sample data i)))) 2))
		(setq i (inc i)))
	(apply (const cat) (cat (list "") out)))

(defun sound-add (datas)
	; (sound-add datas) -> data
	;pieces added, sample by sample, as long as the first, kept in range: a
	;mix. One shorter than the first is quiet after its end
	(cond
		((= (length datas) 1) (first datas))
		(:t (defq n (/ (length (first datas)) 2) out (list) i 0)
			(while (< i n)
				(defq v 0 at (* i 2))
				(each (# (if (< at (length %0)) (setq v (+ v (sound-sample %0 i))))) datas)
				(push out (char (clamp v -32768 32767) 2))
				(setq i (inc i)))
			(apply (const cat) (cat (list "") out)))))

(defun sound-peak (data)
	; (sound-peak data) -> num
	;the loudest of a piece, every other sample looked at
	(defq n (/ (length data) 2) peak 0 i 0)
	(while (< i n)
		(setq peak (max peak (abs (sound-sample data i))) i (+ i 2)))
	peak)

(defun sound-pieces (in fn)
	; (sound-pieces in fn)
	;each piece of the in stream, to its end, given to fn
	(while (and (defq data (read-blk in +sound_piece)) (nempty? data))
		(fn data)
		(task-slice)))

;what comes by mail, which is not kept in order, put back in it

(defun sound-order (seq)
	; (sound-order seq) -> order
	;the place in what comes: the number of the piece to be had next, and
	;what has come before it is wanted
	(list seq (Fmap 11)))

(defun sound-take (order seq data)
	; (sound-take order seq data) -> datas
	;a piece come, and what can be had now, in order: one that is not there
	;when more than two after it are is lost, and one that comes after
	;that is let go
	(bind '(next early) order)
	(if (>= seq next) (. early :insert seq data))
	(defq out (list))
	(while (or (defq data (. early :find next)) (> (. early :size) 2))
		(when data
			(. early :erase next)
			(push out data))
		(setq next (inc next)))
	(elem-set order 0 next)
	out)

;sources

(defun sound-mic (in out ctl)
	; (sound-mic in out ctl)
	;what the microphone hears, in the order it was heard, till ctl is sent
	;anything. A piece that is not there when two after it are is lost
	(defq heard (mail-mbox) select (list heard ctl) running :t
		renewed (pii-time) order :nil)
	(when (audio-listen-rpc heard)
		(while running
			(when (> (- (pii-time) renewed) (/ +audio_lease 2))
				(setq renewed (pii-time))
				(audio-listen-rpc heard))
			(defq idx (mail-select select) msg (mail-read (elem-get select idx)))
			(cond
				((= idx 1) (setq running :nil))
				(:t (defq seq (getf msg +audio_heard_seq))
					(unless order (setq order (sound-order seq)))
					(each (# (write-blk out %0)) (sound-take order seq (slice msg +audio_heard_data -1))))))
		(audio-hush-rpc heard)))

(defun sound-stopped? (ctl)
	; (sound-stopped? ctl) -> :t | :nil
	;has a source been told to stop, ctl sent anything, if it has a ctl
	(and ctl (mail-poll (list ctl)) :t))

(defun sound-tone (in out hz ms &optional level ctl)
	; (sound-tone in out hz ms [level ctl])
	;a tone of hz a second for ms, at level of 32767, or till ctl is sent
	;anything
	(defq n (/ (* +sound_rate ms) 1000) level (ifn level 8192) at 0
		step (/ (* (n2r hz) (const (n2r +fp_2pi))) (n2r +sound_rate)))
	(while (and (< at n) (not (sound-stopped? ctl)))
		(defq piece (list) end (min n (+ at (/ +sound_piece 2))))
		(while (< at end)
			(push piece (char (n2i (* (n2r level) (sin (* step (n2r at))))) 2))
			(setq at (inc at)))
		(write-blk out (apply (const cat) (cat (list "") piece)))
		(task-slice)))

(defun sound-from-file (in out path &optional ctl)
	; (sound-from-file in out path [ctl])
	;the samples of a sound file, by its format, or till ctl is sent anything
	(when (and (defq kind (sound-loader-of path)) (defq stream (file-stream path)))
		(bind '(rate & & bytes) ((second kind) stream))
		(while (and (> bytes 0) (not (sound-stopped? ctl))
				(defq data (read-blk stream (min bytes +sound_piece))) (nempty? data))
			(write-blk out data)
			(setq bytes (- bytes (length data)))
			(task-slice))))

;filters

(defun sound-gain (in out percent)
	; (sound-gain in out percent)
	;louder or quieter, 100 as it is
	(sound-pieces in (lambda (data)
		(write-blk out (sound-map data (# (/ (* %0 percent) 100)))))))

(defun sound-lowpass (in out hz)
	; (sound-lowpass in out hz)
	;what is above hz let through less, a pole of its own
	(defq k (n2i (* (n2r 65536) (/ (* (const (n2r +fp_2pi)) (n2r hz)) (+ (n2r +sound_rate) (* (const (n2r +fp_2pi)) (n2r hz))))))
		y 0)
	(sound-pieces in (lambda (data)
		(write-blk out (sound-map data (lambda (x)
			(setq y (+ y (/ (* k (- x y)) 65536)))))))))

(defun sound-level (in out who)
	; (sound-level in out who)
	;each piece passed on, and how loud it was sent to who, a number
	(sound-pieces in (lambda (data)
		(mail-send who (str (sound-peak data)))
		(write-blk out data))))

;sinks

(defun sound-to-file (in out path)
	; (sound-to-file in out path)
	;the samples to a sound file by its format, written as they come, its
	;length put in at the end
	(when (and (defq kind (sound-loader-of path))
			(defq stream (file-stream path +file_open_write)))
		((third kind) stream +sound_rate)
		(defq bytes 0)
		(sound-pieces in (lambda (data)
			(write-blk stream data)
			(setq bytes (+ bytes (length data)))))
		((elem-get kind 3) stream +sound_rate bytes)))

(defun sound-speaker (in out)
	; (sound-speaker in out)
	;played on the Audio service, at the pace it is heard, 100ms ahead
	(when (defq handle (audio-stream-open-rpc))
		(defq t0 (pii-time) sent 0)
		(sound-pieces in (lambda (data)
			(audio-stream-write-rpc handle data)
			(setq sent (+ sent (length data)))
			(defq ahead (- (/ (* sent 500000) +sound_rate) (- (pii-time) t0)))
			(if (> ahead 100000) (task-sleep (- ahead 100000)))))
		;what is left to play, played
		(defq left (- (/ (* sent 500000) +sound_rate) (- (pii-time) t0)))
		(if (> left 0) (task-sleep left))
		(task-sleep 100000)
		(audio-stream-close-rpc handle)))
