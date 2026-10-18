(report-header "Audio: the microphone heard in pieces, and a stream played as it comes")

;A release test, tests -a. A desktop of its own is started, hidden, and its
;Audio service's microphone hears the tone of src/host/mic.h, 440 a second,
;not a device. What is asked of it is asked by a task on its node, so it is
;that desktop's service and not one of a desktop of someone's

(import "lib/files/files.inc")
(import "service/audio/app.inc")

(defq am_before (lisp-nodes) am_pid (first (node-spawn 1 :gui_hidden "service/gui/app.lisp"))
	am_t0 (pii-time) am_gui :nil am_node :nil)

(defun am-ask (form &optional wait)
	; (am-ask form [wait]) -> :nil | str
	;form run on the desktop's node, what it gives back as text
	(defq reply (mail-mbox))
	;the structures are imported first, a form apart, as the one after is
	;made with them
	(open-task (cat "(import {service/audio/app.inc})" (ascii-char 10)
			(str `(mail-send (hex-decode ,(hex-encode reply)) (str ,form))))
		am_node +kn_call_pin 0 (mail-mbox))
	(mail-read-timeout reply (task-timeout (ifn wait 10))))

(while (and (empty? (setq am_gui (mail-enquire "@Gui,"))) (< (- (pii-time) am_t0) (task-timeout 10)))
	(task-sleep 50000))
(cond
	((starts-with "obj/vp64" (load-path))
		(test-skip "the microphone" "the emulator, the host drivers are the native hosts'"))
	((empty? am_gui)
		(test-skip "the microphone" "no desktop could be started on this host"))
	(:t (setq am_node (slice (hex-decode (second (split (first am_gui) ","))) +long_size -1))
		;its Audio service, on its node
		(defq am_t0 (pii-time))
		(while (not (or (eql (am-ask '(if (audio-service) "yes" "no")) "yes") (> (- (pii-time) am_t0) (task-timeout 10))))
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
		;a listener that does not say so again is let go when its lease is out
		(defq am_lease (am-ask '(progn
			(defq who (mail-mbox) t0 (pii-time) last 0)
			(audio-listen-rpc who)
			(while (< (- (pii-time) t0) (+ +audio_lease 3000000))
				(if (mail-read-timeout who 100000) (setq last (- (pii-time) t0))))
			last) 15))
		(defq am_last (if am_lease (str-as-num am_lease) 0))
		(assert-true "a listener whose lease is out is sent no more" (< am_last (+ +audio_lease 2000000)))
		(assert-true "but is sent till then" (> am_last (- +audio_lease 1000000)))))

;the desktop goes, and the network is as it was
(when am_pid
	(when (defq am_new (some (# (unless (find %0 am_before) %0)) (lisp-nodes)))
		(open-task "(pii-exit)" am_new +kn_call_pin 0 (mail-mbox)))
	(setq am_t0 (pii-time))
	(while (and (pii-alive am_pid) (< (- (pii-time) am_t0) (task-timeout 5)))
		(task-sleep 10000))
	(if (pii-alive am_pid) (pii-kill am_pid))
	(setq am_t0 (pii-time))
	(while (and (/= (length (lisp-nodes)) (length am_before)) (< (- (pii-time) am_t0) (task-timeout 5)))
		(task-sleep 10000)))
