;;;;;;;;;;;;;;
; undo actions
;;;;;;;;;;;;;;

(defun action-undo ()
	(.-> *edit* :get_buffer :undo)
	(. *edit* :clear_selection)
	(refresh))

(defun action-redo ()
	(.-> *edit* :get_buffer :redo)
	(. *edit* :clear_selection)
	(refresh))

(defun action-rewind ()
	(defq buffer (. *edit* :get_buffer))
	(while (> (length (get :undo_stack buffer)) 0)
		(. buffer :undo))
	(. *edit* :clear_selection)
	(refresh))

(defun action-undo-global ()
	(when (defq exit :nil here *current_file*)
		(push *refresh_mode* 1)
		(until exit
			(action-undo)
			(update-meta-data)
			(action-next-buffer)
			(setq exit (eql *current_file* here)))
		(pop *refresh_mode*)
		(refresh)))

(defun action-redo-global ()
	(when (defq exit :nil here *current_file*)
		(push *refresh_mode* 1)
		(until exit
			(action-redo)
			(update-meta-data)
			(action-next-buffer)
			(setq exit (eql *current_file* here)))
		(pop *refresh_mode*)
		(refresh)))
