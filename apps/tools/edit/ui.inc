;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-minimise ()
	(vdu-resize +vdu_min_width +vdu_min_height))

(defun action-maximise ()
	(vdu-resize +vdu_max_width +vdu_max_height))

(defun action-close ()
	(setq *running* :nil))

(defun action-layout ()
	;user window resize
	(window-resize))

(defun action-xscroll ()
	;user xscroll bar
	(bind '(sx sy) (. *edit* :get_scroll))
	(. *edit* :set_scroll (get :value *xslider*) sy)
	(refresh-display))

(defun action-yscroll ()
	;user yscroll bar
	(bind '(sx sy) (. *edit* :get_scroll))
	(. *edit* :set_scroll sx (get :value *yslider*))
	(refresh-display))

(defun action-open-leaf-action ()
	;switch to the file selected
	(switch-file (. *open_files_selector* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))

(defun action-file-folder-action ()
	;load all files in this folder
	(defq folder (. *file_selector* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id))))
	(open-files (defq files (sort (files-all folder +file_types))))
	(switch-file (first files)))

(defun action-file-leaf-action ()
	;load the file selected
	(open-file (defq file (. *file_selector* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
	(.-> *name_text* (:set_text file) :layout :dirty))

(defun action-scale-up ()
	;scale font up
	(setq *page_scale* (min 1.5 (+ (defq old_size *page_scale*) 0.125)))
	(when (/= old_size *page_scale*)
		(set *edit* :font (create-font +edit_font (page-scale +edit_size)))
		(set *vdu_lines* :font (get :font *edit*))
		(.-> *scale_flow* :constrain :dirty_all)
		(window-resize)))

(defun action-scale-down ()
	;scale font down
	(setq *page_scale* (max 0.75 (- (defq old_size *page_scale*) 0.125)))
	(when (/= old_size *page_scale*)
		(set *edit* :font (create-font +edit_font (page-scale +edit_size)))
		(set *vdu_lines* :font (get :font *edit*))
		(.-> *scale_flow* :constrain :dirty_all)
		(window-resize)))

;;;;;;;;;;;;;;;;;;;;
; main edit subclass
;;;;;;;;;;;;;;;;;;;;

(defclass Editor-edit () (Edit)
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
		(clear-matches)
		(refresh) this)

	(defmethod :mouse_wheel (event)
		; (. vdu :mouse_wheel event) -> vdu
		(.super this :mouse_wheel event)
		(clear-matches)
		(defq meta (.-> *meta_map* (:find :files) (:find (str *current_file*))))
		(bind '(sx sy) (. this :get_scroll))
		(scatter meta :sx sx :sy sy)
		(refresh-sliders) (refresh-display) this)
	)
