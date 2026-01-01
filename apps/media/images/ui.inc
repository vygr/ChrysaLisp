;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun action-file-leaf-action ()
	;load the file selected
	(win-refresh (. *file_selector* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
