;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "././edit/ui.inc")

(redefun action-file-leaf-action ()
	;load the file selected
	(switch-file (defq file (. *file_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id))))))

(redefun action-whole-words ()
	(setq *whole_words* (not *whole_words*))
	(bind '(fx fy fx1 fy1) (. *edit* :get_find))
	(toolbar-states *find_toolbar* (list (> fy1 fy) *whole_words* *regexp* :nil :nil))
	(refresh-display))

(redefun action-regexp ()
	(setq *regexp* (not *regexp*))
	(bind '(fx fy fx1 fy1) (. *edit* :get_find))
	(toolbar-states *find_toolbar* (list (> fy1 fy) *whole_words* *regexp* :nil :nil))
	(refresh-display))

(redefun action-region ()
	(bind '(sy sy1) (select-lines))
	(bind '(fx fy fx1 fy1) (. *edit* :get_find))
	(if (> fy1 fy) (setq fy 0 fy1 0) (setq fy sy fy1 sy1))
	(. *edit* :set_find fx fy fx1 fy1)
	(toolbar-states *find_toolbar* (list (> fy1 fy) *whole_words* *regexp* :nil :nil))
	(refresh-display))

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
