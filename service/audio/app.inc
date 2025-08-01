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
(structure +audio_play +audio_rpc_size
	(uint handle))

; audio_change - params
(structure +audio_change +audio_rpc_size
	(uint handle)
	(int state))

; audio_remove - params
(structure +audio_remove +audio_rpc_size
	(uint handle))

(defun audio-add-rpc (file_path)
	; (audio-add-rpc file_path) -> handle
	(defq handle :nil)
	(when (nempty? (defq service (mail-enquire "Audio,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-mbox))
		(mail-send service (setf-> (cat (str-alloc +audio_add_size) file_path)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_add)))
		(setq handle (getf (mail-read mbox) +audio_ret_retval)))
	handle)

(defun audio-play-rpc (handle)
	; (audio-play-rpc handle)
	(when (nempty? (defq service (mail-enquire "Audio,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-mbox))
		(mail-send service (setf-> (str-alloc +audio_play_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_play)
			(+audio_play_handle handle)))
		(mail-read mbox)))

(defun audio-change-rpc (handle state)
	; (audio-change-rpc handle state)
	(when (nempty? (defq service (mail-enquire "Audio,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-mbox))
		(mail-send service (setf-> (str-alloc +audio_change_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_change)
			(+audio_change_handle handle)
			(+audio_change_state state)))
		(mail-read mbox)))

(defun audio-remove-rpc (handle)
	; (audio-remove-rpc handle)
	(when (nempty? (defq service (mail-enquire "Audio,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-mbox))
		(mail-send service (setf-> (str-alloc +audio_remove_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_remove)
			(+audio_remove_handle handle)))
		(mail-read mbox)))
