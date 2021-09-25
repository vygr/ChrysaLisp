;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "././edit/ui.inc")

;overload Edit app ui.inc
(defun action-file-leaf-action ()
	;load up the file selected
	(defq file (. *file_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id))))
	(select-node file)
	(populate-vdu file))

;;;;;;;;;;;;;;;;;;;
; main vdu subclass
;;;;;;;;;;;;;;;;;;;

(defclass Viewer-vdu () (Edit-vdu)
	(defmethod :mouse_move (this event)
		; (. vdu :mouse_move event) -> vdu
		(bind '(x y) (vdu-char-pos this event))
		(bind '(x y) (. *current_buffer* :constrain x y))
		(. *current_buffer* :set_cursor x y)
		(refresh) this)

	(defmethod :mouse_wheel (this event)
		; (. vdu :mouse_wheel event) -> vdu
		(bind '(x y ax ay sx sy) (. *meta_map* :find *current_file*))
		(setq sx (+ *scroll_x* (getf event +ev_msg_wheel_x))
			sy (- *scroll_y* (getf event +ev_msg_wheel_y)))
		(. *meta_map* :insert *current_file* (list x y ax ay sx sy))
		(set-sliders) (load-display) this)
	)
