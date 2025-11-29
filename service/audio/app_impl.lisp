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
	; Declare the audio service
	(defq audio_service (mail-declare (task-mbox) "Audio" "Audio Service 0.1")
		file_map (Fmap 11) handle_map (Fmap 11) refs_map (Fmap))
	((ffi "service/audio/lisp_init"))
	; Main loop
	(while :t
		(let* ((msg (mail-read (task-mbox))) (reply_id (getf msg +audio_rpc_reply_id)))
			(case (getf msg +audio_rpc_type)
				(+audio_type_add
					; Add sound effect
					(defq file (slice msg +audio_add_size -1))
					(cond
						((defq handle (. file_map :find file))
							(. refs_map :update handle (# (inc %0))))
						(:t	;not shared
							(defq handle (audio-raw add file))
							(. file_map :insert file handle)
							(. handle_map :insert handle file)
							(. refs_map :insert handle 1)))
					(audio-reply handle))
				(+audio_type_play
					; Play sound effect
					(audio-call play (getf msg +audio_play_handle)))
				(+audio_type_change
					; Change playing state
					(audio-call change (getf msg +audio_change_handle)
									(getf msg +audio_change_state)))
				(+audio_type_remove
					; Remove sound effect
					(defq handle (getf msg +audio_remove_handle))
					(audio-reply (cond
						((<= (. refs_map :update handle (# (dec %0))) 0)
							;last ref has gone
							(. file_map :erase (. handle_map :find handle))
							(. handle_map :erase handle)
							(audio-raw remove handle))
						(0)))))))
	((ffi "service/audio/lisp_deinit"))
	; Forget the audio service
	(mail-forget audio_service))
