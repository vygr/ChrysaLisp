;audio messages

; define the message types for the service
(enums +audio_type 0
	(enum add play change remove))

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

(defun audio-service ()
	; (audio-service) -> :nil | net_id
	;the audio service to use. Each desktop has one of its own, on its
	;node, sound is played by the host of the node. An app with a window
	;is on its desktop's node and has that one, so a desktop that quits
	;takes its own sound with it and nobody else's. A task on a node with
	;no desktop has any one there is on the machine
	(defq me (slice (task-mbox) +mailbox_id_size -1) found :nil)
	(each (lambda (entry)
		(defq id (hex-decode (second (split entry ","))))
		(if (or (not found) (eql (slice id +mailbox_id_size -1) me)) (setq found id)))
		(mail-enquire "@Audio,"))
	found)

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
