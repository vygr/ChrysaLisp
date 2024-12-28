;audio messages

; define the message types for the service
(enums +audio_type 0
	(enum add_sfx play_sfx change_sfx remove_sfx))

; common header for all messages
(structure +audio_rpc 0
	(netid reply_id)
	(uint type))

; audio_add_sfx - params and return
(structure +audio_add_sfx +audio_rpc_size)

(structure +audio_sfx_ret 0
	(uint retval))

; audio_play_sfx - params
(structure +audio_play_sfx +audio_rpc_size
	(uint handle))

; audio_change_sfx - params
(structure +audio_change_sfx +audio_rpc_size
	(uint handle)
	(int state))

; audio_remove_sfx - params
(structure +audio_remove_sfx +audio_rpc_size
	(uint handle))

(defun audio-add-sfx-rpc (file_path)
	; (audio-add-sfx-rpc file_path) -> handle
	(defq handle :nil)
	(when (nempty? (defq service (mail-enquire "Audio,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-alloc-mbox))
		(mail-send service (setf-> (cat (str-alloc +audio_add_sfx_size) file_path)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_add_sfx)))
		(setq handle (getf (mail-read mbox) +audio_sfx_ret_retval))
		(mail-free-mbox mbox))
	handle)

(defun audio-play-sfx-rpc (handle)
	; (audio-play-sfx-rpc handle)
	(when (nempty? (defq service (mail-enquire "Audio,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-alloc-mbox))
		(mail-send service (setf-> (str-alloc +audio_play_sfx_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_play_sfx)
			(+audio_play_sfx_handle handle)))
		(mail-read mbox)
		(mail-free-mbox mbox)))

(defun audio-change-sfx-rpc (handle state)
	; (audio-change-sfx-rpc handle state)
	(when (nempty? (defq service (mail-enquire "Audio,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-alloc-mbox))
		(mail-send service (setf-> (str-alloc +audio_change_sfx_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_change_sfx)
			(+audio_change_sfx_handle handle)
			(+audio_change_sfx_state state)))
		(mail-read mbox)
		(mail-free-mbox mbox)))

(defun audio-remove-sfx-rpc (handle)
	; (audio-remove-sfx-rpc handle)
	(when (nempty? (defq service (mail-enquire "Audio,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-alloc-mbox))
		(mail-send service (setf-> (str-alloc +audio_remove_sfx_size)
			(+audio_rpc_reply_id mbox)
			(+audio_rpc_type +audio_type_remove_sfx)
			(+audio_remove_sfx_handle handle)))
		(mail-read mbox)
		(mail-free-mbox mbox)))
