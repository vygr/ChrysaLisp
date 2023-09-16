;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "././edit/ui.inc")

(defun action-file-leaf-action ()
	;load the file selected
	(switch-file (defq file (. *file_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id))))))

;;;;;;;;;;;;;;;;;;;;
; main edit subclass
;;;;;;;;;;;;;;;;;;;;

(defclass Viewer-edit () (Edit)
	(def this :min_width +vdu_min_width :min_height +vdu_min_height
		:vdu_width +vdu_min_width :vdu_height +vdu_min_height
		:font *env_editor_font* :ink_color +argb_white)

	(defmethod :mouse_down (event)
		; (. vdu :mouse_down event) -> vdu
		(.super this :mouse_down event)
		(refresh) this)

	(defmethod :mouse_up (event)
		; (. vdu :mouse_up event) -> vdu
		(case (getf event +ev_msg_mouse_count)
			(2 (action-select-word))
			(3 (action-select-line))
			(4 (action-select-paragraph)))
		(refresh) this)

	(defmethod :mouse_move (event)
		; (. vdu :mouse_move event) -> vdu
		(.super this :mouse_move event)
		(refresh) this)

	(defmethod :mouse_wheel (event)
		; (. vdu :mouse_wheel event) -> vdu
		(.super this :mouse_wheel event)
		(defq meta (.-> *meta_map* (:find :files) (:find (str *current_file*))))
		(bind '(sx sy) (. this :get_scroll))
		(scatter meta :sx sx :sy sy)
		(refresh-sliders) (refresh-display) this)
	)
