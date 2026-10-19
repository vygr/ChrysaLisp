;audio messages

; define the message types for the service
(enums +audio_type 0
	(enum add play change remove listen hush stream_open stream_write stream_close stream_left))

; common header for all messages
(structure +audio_rpc 0
	(netid reply_id)
	(uint type))

; audio_add - params and return
(structure +audio_add +audio_rpc_size)

(structure +audio_ret 0
	(uint retval))

; audio_play - params
; pan is -255 (full left) to 255 (full right), 0 is centre
(structure +audio_play +audio_rpc_size
	(uint handle)
	(int pan))

; audio_change - params
(structure +audio_change +audio_rpc_size
	(uint handle)
	(int state))

; audio_remove - params
(structure +audio_remove +audio_rpc_size
	(uint handle))

;the microphone and streams. What is heard, and what a stream plays, is 16
;bit mono at +audio_rate, the rate of a voice. A listener is sent each
;piece as it is heard, +audio_heard, numbered one more each, so a piece lost or
;come out of order is known, mail is not kept in order. A listener says
;listen again before +audio_lease is out, or is let go, and a stream not
;written to for that long is closed: what is on another node can not be
;asked if it is still there, so it has to say so
(defq +audio_rate 16000 +audio_lease 5000000)

; audio_listen, audio_hush - params, who is to be sent what is heard
(structure +audio_listen +audio_rpc_size
	(netid who))

; what a listener is sent
(structure +audio_heard 0
	(uint seq)
	(offset data))

; audio_stream_write - params, no reply
(structure +audio_stream +audio_rpc_size
	(uint handle)
	(offset data))

; audio_stream_open - params and return, the rate and channels of a
; stream, a request without them is of 16000 mono
(structure +audio_stream_open +audio_rpc_size
	(uint rate channels))

(defun audio-service ()
	; (audio-service) -> :nil | net_id
	;the audio service to use. Each desktop has one of its own, on its
	;node, sound is played by the host of the node. An app with a window
	;is on its desktop's node and has that one, so a desktop that quits
	;takes its own sound with it and nobody else's. A task on a node with
	;no desktop has any one there is on the machine
	(mail-service "@Audio"))

(defun audio-add-rpc (file_path)
	; (audio-add-rpc file_path) -> handle
	(defq handle :nil)
	(when (defq service (audio-service))
		(defq mbox (mail-mbox))
		(mail-send service (setf-> (cat (str-alloc +audio_add_size) file_path)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_add)))
		(when (defq reply (mail-read-timeout mbox))
			(setq handle (getf reply +audio_ret_retval))))
	handle)

(defun audio-play-rpc (handle &optional pan)
	; (audio-play-rpc handle [pan])
	(when (defq service (audio-service))
		(defq mbox (mail-mbox))
		(mail-send service (setf-> (str-alloc +audio_play_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_play)
			(+audio_play_handle handle)
			(+audio_play_pan (ifn pan 0))))
		(mail-read-timeout mbox)))

(defun audio-change-rpc (handle state)
	; (audio-change-rpc handle state)
	(when (defq service (audio-service))
		(defq mbox (mail-mbox))
		(mail-send service (setf-> (str-alloc +audio_change_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_change)
			(+audio_change_handle handle)
			(+audio_change_state state)))
		(mail-read-timeout mbox)))

(defun audio-remove-rpc (handle)
	; (audio-remove-rpc handle)
	(when (defq service (audio-service))
		(defq mbox (mail-mbox))
		(mail-send service (setf-> (str-alloc +audio_remove_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_remove)
			(+audio_remove_handle handle)))
		(mail-read-timeout mbox)))

(defun audio-listen-rpc (who)
	; (audio-listen-rpc who) -> :t | :nil
	;who is to be sent what the microphone hears, again before the lease is
	;out to go on being. :nil if there is no microphone
	(when (defq service (audio-service))
		(defq mbox (mail-mbox))
		(mail-send service (setf-> (str-alloc +audio_listen_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_listen)
			(+audio_listen_who who)))
		(and (defq reply (mail-read-timeout mbox)) (= (getf reply +audio_ret_retval) 0))))

(defun audio-hush-rpc (who)
	; (audio-hush-rpc who)
	(when (defq service (audio-service))
		(defq mbox (mail-mbox))
		(mail-send service (setf-> (str-alloc +audio_listen_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_hush)
			(+audio_listen_who who)))
		(mail-read-timeout mbox)))

(defun audio-stream-open-rpc (&optional rate channels)
	; (audio-stream-open-rpc [rate channels]) -> :nil | handle
	;a stream to play as it comes, :nil if there is no room or no sound. Of
	;16 bit samples, 1 or 2 channels, left first, at a rate of 8000 to
	;48000; 16000 mono, +sound_rate, if not said
	(when (defq service (audio-service))
		(defq mbox (mail-mbox))
		(mail-send service (setf-> (str-alloc +audio_stream_open_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_stream_open)
			(+audio_stream_open_rate (ifn rate 16000))
			(+audio_stream_open_channels (ifn channels 1))))
		(and (defq reply (mail-read-timeout mbox))
			(/= (defq handle (getf reply +audio_ret_retval)) 0) handle)))

(defun audio-stream-left-rpc (handle)
	; (audio-stream-left-rpc handle) -> :nil | frames
	;how many frames of a stream are still to be played, what was written
	;less what has been heard: a player keeps its pictures to its sound so
	(when (defq service (audio-service))
		(defq mbox (mail-mbox))
		(mail-send service (setf-> (str-alloc +audio_remove_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_stream_left)
			(+audio_remove_handle handle)))
		;the answer is a uint, no such stream, -1, is all ones
		(and (defq reply (mail-read-timeout mbox))
			(/= (defq left (getf reply +audio_ret_retval)) 0xffffffff) left)))

(defun audio-stream-write-rpc (handle data)
	; (audio-stream-write-rpc handle data)
	;the next of a stream, 16 bit samples, a frame each of its channels,
	;no answer
	(when (defq service (audio-service))
		(mail-send service (cat (setf-> (str-alloc +audio_stream_size)
			(+audio_rpc_type +audio_type_stream_write)
			(+audio_stream_handle handle)) data))))

(defun audio-stream-close-rpc (handle)
	; (audio-stream-close-rpc handle)
	(when (defq service (audio-service))
		(defq mbox (mail-mbox))
		(mail-send service (setf-> (str-alloc +audio_remove_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_stream_close)
			(+audio_remove_handle handle)))
		(mail-read-timeout mbox)))
