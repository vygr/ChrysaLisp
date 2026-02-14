;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "././edit/ui.inc")

(redefun action-file-leaf-action ()
	;load the file selected
	(switch-file (defq file (. *file_selector* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id))))))

(redefun action-whole-words ()
	(setq *whole_words* (not *whole_words*))
	(update-find-toolbar)
	(refresh-display))

(redefun action-regexp ()
	(setq *regexp* (not *regexp*))
	(update-find-toolbar)
	(refresh-display))

(redefun action-region ()
	(if (nql (. *edit* :get_focus) +invalid_focus)
		(. *edit* :set_focus +invalid_focus)
		(progn
			(bind '(& sy & sy1) (. *edit* :get_selected_extent))
			(. *edit* :set_focus (nums 0 sy 0 sy1))))
	(update-find-toolbar)
	(refresh-display))

(redefun action-pop ()
	(when (defq pos (pop *cursor_stack*))
		(bind '(file cx cy) pos)
		(when (/= 0 (age file))
			;file exists
			(switch-file file)
			(. *edit* (:set_cursor cx cy))
			(refresh))))

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
		(.super this :mouse_up event)
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
