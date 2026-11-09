;;;;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././edit/ui.inc")

(redefun action-file-leaf-action ()
	;load the file selected
	(switch-file (defq file (. *file_selector* :get_route
				(. *window* :find_id (getf *msg* +ev_msg_action_source_id))))))

(redefun action-pop ()
	(when (defq pos (pop *cursor_stack*))
		(bind '(file cx cy) pos)
		(when (/= 0 (age file))
			;file exists
			(switch-file file)
			(. *edit* :set_cursor cx cy)
			(refresh))))
