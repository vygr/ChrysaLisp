;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "././edit/ui.inc")

(defun action-file-leaf-action ()
	;load up the file selected
	(defq file (. *file_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id))))
	(select-node file)
	(populate-vdu file))
