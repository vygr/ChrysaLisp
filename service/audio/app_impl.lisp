(import "./app.inc")

(defmacro audio-raw (func &rest args)
	(setq func (cat "service/audio/lisp_" (str func)))
	`((const (ffi ,func)) ~args))

(defmacro audio-reply (result)
	`(mail-send reply_id (setf-> (str-alloc +audio_ret_size)
		(+audio_ret_retval ,result))))

(defmacro audio-call (func &rest args)
	(setq func (cat "service/audio/lisp_" (str func)))
	`(mail-send reply_id (setf-> (str-alloc +audio_ret_size)
		(+audio_ret_retval ((const (ffi ,func)) ~args)))))

(defun main ()
	;the service is only there if the host of this node has an audio
	;driver, and it starts. The TUI host has none, nor has a GUI host
	;built for the frame buffer, and with no service an app's sounds are
	;quietly not played.
	(when (= ((ffi "service/audio/lisp_init")) 0)
		; Declare the audio service
		(defq audio_service (mail-declare (task-mbox) "@Audio" "Audio Service 0.1")
			file_map (Fmap 11) handle_map (Fmap 11) refs_map (Fmap)
			;who listens, by mailbox, and when their lease is out; the streams,
			;by handle, and when they were last written to. What is heard is
			;read into the one buffer, a second of it
			listeners (Fmap 11) streams (Fmap 11) heard_seq 0
			heard_buf (str-alloc (* +audio_rate 2)) looked (pii-time))
		; Main loop
		(while :t
			;while anyone listens what is heard is read each 20ms, and sent
			(when (not (. listeners :empty?))
				(when (> (defq n (audio-raw mic_read heard_buf)) 0)
					(defq piece (slice heard_buf 0 (* n 2)))
					(. listeners :each (lambda (who &ignore)
						(mail-send (hex-decode who) (cat (setf-> (str-alloc +audio_heard_size)
							(+audio_heard_seq heard_seq)) piece))))
					(++ heard_seq)))
			;those whose lease is out are let go, and with the last listener
			;the microphone is closed
			(when (> (- (defq now (pii-time)) looked) 1000000)
				(setq looked now)
				(defq gone (list))
				(. listeners :each (lambda (who until) (if (< until now) (push gone who))))
				(each (# (. listeners :erase %0)) gone)
				(and (nempty? gone) (. listeners :empty?) (audio-raw mic_close))
				(defq gone (list))
				(. streams :each (lambda (handle last) (if (> (- now last) +audio_lease) (push gone handle))))
				(each (# (. streams :erase %0) (audio-raw stream_close %0)) gone))
			(let* ((msg (ifn (. listeners :empty?) (mail-read-timeout (task-mbox) 20000) (mail-read-timeout (task-mbox) 1000000)))
					(reply_id (and msg (getf msg +audio_rpc_reply_id))))
				(case (and msg (getf msg +audio_rpc_type))
					(+audio_type_add
						; Add sound effect
						(defq file (slice msg +audio_add_size -1))
						(cond
							((defq handle (. file_map :find file))
								(. refs_map :update handle (# (inc %0))))
							(:t ;not shared
								(defq handle (audio-raw add file))
								(. file_map :insert file handle)
								(. handle_map :insert handle file)
								(. refs_map :insert handle 1)))
						(audio-reply handle))
					(+audio_type_play
						; Play sound effect
						(audio-call play (getf msg +audio_play_handle)
										(getf msg +audio_play_pan)))
					(+audio_type_change
						; Change playing state
						(audio-call change (getf msg +audio_change_handle)
										(getf msg +audio_change_state)))
					(+audio_type_remove
						; Remove sound effect
						(defq handle (getf msg +audio_remove_handle))
						(audio-reply (cond
							((<= (. refs_map :update handle (# (dec %0))) 0)
								;last ref has gone, and with it all that is kept of it
								(. file_map :erase (. handle_map :find handle))
								(. handle_map :erase handle)
								(. refs_map :erase handle)
								(audio-raw remove handle))
							(0))))
					(+audio_type_listen
						;the microphone opened by the first
						(defq who (hex-encode (getf msg +audio_listen_who)))
						(audio-reply (cond
							((. listeners :find who)
								(. listeners :insert who (+ (pii-time) +audio_lease)) 0)
							((or (not (. listeners :empty?)) (= (audio-raw mic_open) 0))
								(. listeners :insert who (+ (pii-time) +audio_lease)) 0)
							(-1))))
					(+audio_type_hush
						(when (. listeners :find (defq who (hex-encode (getf msg +audio_listen_who))))
							(. listeners :erase who)
							(if (. listeners :empty?) (audio-raw mic_close)))
						(audio-reply 0))
					(+audio_type_stream_open
						(defq handle (audio-raw stream_open))
						(if (/= handle 0) (. streams :insert handle (pii-time)))
						(audio-reply handle))
					(+audio_type_stream_write
						;no answer, a piece each 20ms
						(when (. streams :find (defq handle (getf msg +audio_stream_handle)))
							(. streams :insert handle (pii-time))
							(audio-raw stream_write handle (slice msg +audio_stream_data -1))))
					(+audio_type_stream_close
						(defq handle (getf msg +audio_remove_handle))
						(audio-reply (cond
							((. streams :find handle)
								(. streams :erase handle)
								(audio-raw stream_close handle))
							(-1)))))))
		(audio-raw mic_close)
		((ffi "service/audio/lisp_deinit"))
		; Forget the audio service
		(mail-forget audio_service)))
