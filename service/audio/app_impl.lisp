(import "./app.inc")

;lisp bindings
(ffi audio-init "service/audio/lisp_init" 0)
(ffi audio-deinit "service/audio/lisp_deinit" 0)
(ffi audio-add-sfx "service/audio/lisp_add_sfx" 0)
(ffi audio-play-sfx "service/audio/lisp_play_sfx" 0)
(ffi audio-change-sfx "service/audio/lisp_change_sfx" 0)
(ffi audio-remove-sfx "service/audio/lisp_remove_sfx" 0)

(defun main ()
	; Declare the audio service
	(defq audio_service (mail-declare (task-netid) "Audio" "Audio Service 0.1"))
	(audio-init)
	; Main loop
	(while :t
		(let ((msg (mail-read (task-netid))))
			(defq reply_id (getf msg +audio_rpc_reply_id)
				  type (getf msg +audio_rpc_type))
			(case type
				(+audio_type_add_sfx
					; Add sound effect
					(mail-send reply_id (setf-> (str-alloc +audio_sfx_ret_size)
						(+audio_sfx_ret_retval (audio-add-sfx (slice msg +audio_add_sfx_size -1))))))
				(+audio_type_play_sfx
					; Play sound effect
					(mail-send reply_id (setf-> (str-alloc +audio_sfx_ret_size)
						(+audio_sfx_ret_retval (audio-play-sfx (getf msg +audio_play_sfx_handle))))))
				(+audio_type_change_sfx
					; Change playing state
					(mail-send reply_id (setf-> (str-alloc +audio_sfx_ret_size)
						(+audio_sfx_ret_retval (audio-change-sfx
							(getf msg +audio_change_sfx_handle)
							(getf msg +audio_change_sfx_state))))))
				(+audio_type_remove_sfx
					; Remove sound effect
					(mail-send reply_id (setf-> (str-alloc +audio_sfx_ret_size)
						(+audio_sfx_ret_retval (audio-remove-sfx (getf msg +audio_remove_sfx_handle)))))))))
	(audio-deinit)
	; Forget the audio service
	(mail-forget audio_service))
