(report-header "Audio: the microphone heard in pieces, and a stream played as it comes")

;A release test, tests -a. A desktop of its own is started, hidden, and its
;Audio service's microphone hears the tone of src/host/mic.h, 440 a second,
;not a device. What is asked of it is asked by a task on its node, so it is
;that desktop's service and not one of a desktop of someone's

(import "lib/files/files.inc")
(import "service/audio/app.inc")

(defq am_before (lisp-nodes :t) am_pid (first (node-spawn 1 :gui_hidden "service/gui/app.lisp"))
	am_t0 (pii-time) am_gui :nil am_node :nil)

(defun am-ask (form)
	; (am-ask form) -> str
	;form run on the desktop's node, what it gives back as text
	(defq reply (mail-mbox))
	;the structures are imported first, a form apart, as the one after is
	;made with them
	(open-task (cat "(import {lib/audio/stages.inc})" "\n"
			(str `(mail-send (hex-decode ,(hex-encode reply)) (str ,form))))
		am_node +kn_call_pin 0 (mail-mbox))
	(mail-read reply))

;its own desktop, of a node that was not there before it was started, not
;one of someone at the machine. A host with no display never has one:
;that ten seconds is how it is told
(while (and (empty? (setq am_gui (filter (# (not (find (slice (hex-decode (second (split %0 ","))) +long_size -1) am_before))) (mail-enquire "@Gui,")))) (< (- (pii-time) am_t0) (task-timeout 10)))
	(task-sleep 50000))
(cond
	((starts-with "obj/vp64" (load-path))
		(test-skip "the microphone" "the emulator, the host drivers are the native hosts'"))
	((empty? am_gui)
		(test-skip "the microphone" "no desktop could be started on this host"))
	(:t (setq am_node (slice (hex-decode (second (split (first am_gui) ","))) +long_size -1))
		;its Audio service, on its node
		(until (eql (am-ask '(if (audio-service) "yes" "no")) "yes")
			(task-sleep 100000))
		;a second of it: the pieces in order, the samples, the loudest, and
		;how many times it crossed nought, twice for each of its waves
		(defq am_got (am-ask '(progn
			(defq who (mail-mbox) t0 (pii-time) seqs (list) data (list))
			(cond
				((audio-listen-rpc who)
					(while (< (- (pii-time) t0) 1000000)
						(when (defq msg (mail-read-timeout who 100000))
							(push seqs (getf msg +audio_heard_seq))
							(push data (slice msg +audio_heard_data -1))))
					(audio-hush-rpc who)
					(defq all (apply (const cat) (cat (list "") data)) n (/ (length all) 2)
						peak 0 crosses 0 was 0 i 0)
					(while (< i n)
						(defq v (code all 2 (* i 2)) v (if (> v 32767) (- v 65536) v))
						(setq peak (max peak (abs v)))
						(if (and (< was 0) (>= v 0)) (setq crosses (inc crosses)))
						(setq was v i (inc i)))
					(join (map (const str) (list (length seqs)
						(if (nempty? seqs) (- (inc (- (reduce max seqs) (reduce min seqs))) (length seqs)) 0)
						n peak crosses)) " "))
				("-1")))))
		(defq am_res (map (const str-as-num) (split (ifn am_got "-1") " ")))
		(assert-true "it listens" (> (first am_res) 0))
		(when (> (first am_res) 0)
			(bind '(pieces lost samples peak crosses) am_res)
			(assert-true "in pieces, a few to the second" (<= 10 pieces 60))
			(assert-eq "none lost" 0 lost)
			(assert-true "a second of samples, near enough" (<= 12000 samples 20000))
			(assert-true "the tone at its level" (<= 8000 peak 8192))
			;440 waves in a second, of what came in it
			(assert-true "the tone is 440 a second" (<= 400 (/ (* crosses 16000) samples) 480)))
		;a stream: opened, a second written to it in pieces, closed
		(defq am_stream (am-ask '(progn
			(defq handle (audio-stream-open-rpc))
			(cond
				(handle
					(defq piece (apply (const cat) (map (# (char (* (% %0 40) 400) 2)) (range 0 320))))
					(times 50 (audio-stream-write-rpc handle piece))
					(defq one (audio-stream-close-rpc handle) two (audio-stream-close-rpc handle))
					(cat "1 " (if (and one (= (getf one +audio_ret_retval) 0)) "1" "0")
						" " (if (and two (/= (getf two +audio_ret_retval) 0)) "1" "0")))
				("0")))))
		(defq am_s (map (const str-as-num) (split (ifn am_stream "0") " ")))
		(assert-eq "a stream is opened" 1 (first am_s))
		(when (= (first am_s) 1)
			(assert-eq "and closed" 1 (second am_s))
			(assert-eq "and closed again, it is not there" 1 (third am_s)))
		;a stream of a film's sound, 48000 stereo: half a second of silence
		;written, what is still to play of it goes down as it is heard
		(defq am_film (am-ask '(progn
			(defq handle (audio-stream-open-rpc 48000 2))
			(cond
				(handle
					(defq piece (str-alloc (* 960 4)))
					(times 25 (audio-stream-write-rpc handle piece))
					(defq one (progn (task-sleep 150000) (audio-stream-left-rpc handle))
						two (progn (task-sleep 200000) (audio-stream-left-rpc handle)))
					(audio-stream-close-rpc handle)
					(join (map (const str) (list 1 (ifn one -1) (ifn two -1))) " "))
				("0")))))
		(defq am_f (map (const str-as-num) (split (ifn am_film "0") " ")))
		(assert-eq "a stream of 48000 stereo is opened" 1 (first am_f))
		(when (= (first am_f) 1)
			(assert-true "what is left of it is less than was written" (< 0 (second am_f) 24000))
			(assert-true "and less again a little later, it is being heard" (< (third am_f) (second am_f))))
		;a listener that does not say so again is let go when its lease is out
		(defq am_lease (am-ask '(progn
			(defq who (mail-mbox) t0 (pii-time) last 0)
			(audio-listen-rpc who)
			(while (< (- (pii-time) t0) (+ +audio_lease 3000000))
				(if (mail-read-timeout who 100000) (setq last (- (pii-time) t0))))
			last)))
		(defq am_last (if am_lease (str-as-num am_lease) 0))
		(assert-true "a listener whose lease is out is sent no more" (< am_last (+ +audio_lease 2000000)))
		(assert-true "but is sent till then" (> am_last (- +audio_lease 1000000)))
		;a pipeline, the microphone to a file for a second, then the file to
		;the speaker, each run on the desktop's node
		(defq am_pipe (am-ask '(progn
			(defq ctl (mail-mbox) done (mail-mbox) file "tests/scratch/test_mic.wav" peak 0)
			(stages-run (list (sound-stage 'sound-mic (stage-mbox ctl)) (sound-stage 'sound-to-file file)) done)
			(task-sleep 1000000)
			(mail-send ctl "")
			(defq saved (mail-read done) info (sound-info file)
				stream (file-stream file))
			(WAV-info stream)
			(while (and (defq data (read-blk stream 4096)) (nempty? data))
				(setq peak (max peak (sound-peak data))))
			(stages-run (list (sound-stage 'sound-from-file file) (sound-stage 'sound-speaker)) done)
			(defq t0 (pii-time) played (mail-read done) took (- (pii-time) t0))
			(pii-remove file)
			(join (map (const str) (list (if saved 1 0) (if info (elem-get info 3) 0) peak
				(if played 1 0) took)) " "))))
		(defq am_p (map (const str-as-num) (split (ifn am_pipe "0 0 0 0 0") " ")))
		(assert-eq "the microphone to a file, it says it is done" 1 (first am_p))
		(assert-true "about a second of it in the file" (<= 24000 (second am_p) 36000))
		(assert-true "the tone in the file" (<= 8000 (third am_p) 8192))
		(assert-eq "the file to the speaker, it says it is done" 1 (elem-get am_p 3))
		(assert-true "at the pace it plays, about a second" (<= 900000 (elem-get am_p 4) 2000000))))

;the desktop goes, and the network is as it was
(when am_pid
	(when (defq am_new (some (# (unless (find %0 am_before) %0)) (lisp-nodes :t)))
		(open-task "(pii-exit)" am_new +kn_call_pin 0 (mail-mbox)))
	(setq am_t0 (pii-time))
	(while (and (pii-alive am_pid) (< (- (pii-time) am_t0) (task-timeout 5)))
		(task-sleep 10000))
	(if (pii-alive am_pid) (pii-kill am_pid))
	(while (/= (length (lisp-nodes :t)) (length am_before))
		(task-sleep 10000)))
