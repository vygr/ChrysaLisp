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
	(load-display))

(defun action-yscroll ()
	;user yscroll bar
	(bind '(sx sy) (. *edit* :get_scroll))
	(. *edit* :set_scroll sx (get :value *yslider*))
	(load-display))

(defun action-tree-action ()
	;any tree mutate action
	(.-> (penv (. *window* :find_id (getf *msg* +ev_msg_action_source_id)))
		:layout :dirty_all))

(defun action-file-leaf-action ()
	;load up the file selected
	(. *open_tree* :add_route (defq file (. *file_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
	(select-node file)
	(populate-vdu file))

(defun action-open-leaf-action ()
	;switch to the file selected
	(. *file_tree* :add_route (defq file (. *open_tree* :get_route
		(. *window* :find_id (getf *msg* +ev_msg_action_source_id)))))
	(select-node file)
	(populate-vdu file))

;;;;;;;;;;;;;;;;;;;;
; main edit subclass
;;;;;;;;;;;;;;;;;;;;

(defclass Editor-edit () (Edit)
	(def this :min_width +vdu_min_width :min_height +vdu_min_height
		:vdu_width +vdu_min_width :vdu_height +vdu_min_height
		:font *env_terminal_font* :ink_color +argb_white)

	(defmethod :mouse_down (this event)
		; (. vdu :mouse_down event) -> vdu
		(.super this :mouse_down event)
		(refresh) this)

	(defmethod :mouse_up (this event)
		; (. vdu :mouse_up event) -> vdu
		(case (getf event +ev_msg_mouse_count)
			(2 (action-select-word))
			(3 (action-select-line))
			(4 (action-select-paragraph)))
		(refresh) this)

	(defmethod :mouse_move (this event)
		; (. vdu :mouse_move event) -> vdu
		(.super this :mouse_move event)
		(clear-matches)
		(refresh) this)

	(defmethod :mouse_wheel (this event)
		; (. vdu :mouse_wheel event) -> vdu
		(.super this :mouse_wheel event)
		(clear-matches)
		(bind '(x y ax ay sx sy _ buffer) (. *meta_map* :find *current_file*))
		(bind '(sx sy) (. this :get_scroll))
		(. *meta_map* :insert *current_file* (list x y ax ay sx sy _ buffer))
		(set-sliders) (load-display) this)
	)
