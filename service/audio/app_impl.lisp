(import "./app.inc")

(defmacro audio-call (func &rest args)
	(setq func (cat "service/audio/lisp_" (str func) "_sfx"))
	`(mail-send reply_id (setf-> (str-alloc +audio_sfx_ret_size)
		(+audio_sfx_ret_retval ((const (ffi _ ,func 0)) ~args)))))

(defun main ()
	; Declare the audio service
	(defq audio_service (mail-declare (task-netid) "Audio" "Audio Service 0.1"))
	((ffi _ "service/audio/lisp_init" 0))
	; Main loop
	(while :t
		(let ((msg (mail-read (task-netid))))
			(defq reply_id (getf msg +audio_rpc_reply_id)
				  type (getf msg +audio_rpc_type))
			(case type
				(+audio_type_add_sfx
					; Add sound effect
					(audio-call add (slice msg +audio_add_sfx_size -1)))
				(+audio_type_play_sfx
					; Play sound effect
					(audio-call play (getf msg +audio_play_sfx_handle)))
				(+audio_type_change_sfx
					; Change playing state
					(audio-call change (getf msg +audio_change_sfx_handle) (getf msg +audio_change_sfx_state)))
				(+audio_type_remove_sfx
					; Remove sound effect
					(audio-call remove (getf msg +audio_remove_sfx_handle))))))
	((ffi _ "service/audio/lisp_deinit" 0))
	; Forget the audio service
	(mail-forget audio_service))
