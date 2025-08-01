(import "./app.inc")

(defmacro audio-call (func &rest args)
	(setq func (cat "service/audio/lisp_" (str func)))
	`(mail-send reply_id (setf-> (str-alloc +audio_ret_size)
		(+audio_ret_retval ((const (ffi ,func)) ~args)))))

(defun main ()
	; Declare the audio service
	(defq audio_service (mail-declare (task-mbox) "Audio" "Audio Service 0.1"))
	((ffi "service/audio/lisp_init"))
	; Main loop
	(while :t
		(let* ((msg (mail-read (task-mbox))) (reply_id (getf msg +audio_rpc_reply_id)))
			(case (getf msg +audio_rpc_type)
				(+audio_type_add
					; Add sound effect
					(audio-call add (slice msg +audio_add_size -1)))
				(+audio_type_play
					; Play sound effect
					(audio-call play (getf msg +audio_play_handle)))
				(+audio_type_change
					; Change playing state
					(audio-call change (getf msg +audio_change_handle) (getf msg +audio_change_state)))
				(+audio_type_remove
					; Remove sound effect
					(audio-call remove (getf msg +audio_remove_handle))))))
	((ffi "service/audio/lisp_deinit"))
	; Forget the audio service
	(mail-forget audio_service))
