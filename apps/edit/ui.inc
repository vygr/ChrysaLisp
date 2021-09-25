;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-minimise ()
	(vdu-resize +vdu_min_width +vdu_min_height))

(defun action-maximise ()
	(vdu-resize +vdu_max_width +vdu_max_height))

(defun action-close ()
	(setq *running* nil))

(defun action-layout ()
	;user window resize
	(window-resize))

(defun action-xscroll ()
	;user xscroll bar
	(setq *scroll_x* (get :value *xslider*))
	(load-display))

(defun action-yscroll ()
	;user yscroll bar
	(setq *scroll_y* (get :value *yslider*))
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

;;;;;;;;;;;;;;;;;;;
; main vdu subclass
;;;;;;;;;;;;;;;;;;;

(defun vdu-char-pos (this event)
	(bind '(w h) (. this :char_size))
	(defq x (getf event +ev_msg_mouse_rx) y (getf event +ev_msg_mouse_ry))
	(setq x (if (>= x 0) x (- x w)) y (if (>= y 0) y (- y h)))
	(setq x (+ *scroll_x* (/ x w)) y (+ *scroll_y* (/ y h)))
	(list x y))

(defclass Edit-vdu () (Vdu)
	(def this :min_width +vdu_min_width :min_height +vdu_min_height
		:vdu_width +vdu_min_width :vdu_height +vdu_min_height
		:font (get :font *vdu_underlay*)
		:ink_color (get :ink_color *vdu_underlay*))

	(defmethod :mouse_down (this event)
		; (. vdu :mouse_down event) -> vdu
		(bind '(x y) (vdu-char-pos this event))
		(bind '(x y) (. *current_buffer* :constrain x y))
		(. *current_buffer* :set_cursor x y)
		(setq *anchor_x* x *anchor_y* y)
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
		(clear-matches)
		(bind '(x y) (vdu-char-pos this event))
		(bind '(x y) (. *current_buffer* :constrain x y))
		(. *current_buffer* :set_cursor x y)
		(refresh) this)

	(defmethod :mouse_wheel (this event)
		; (. vdu :mouse_wheel event) -> vdu
		(clear-matches)
		(bind '(x y ax ay sx sy _ buffer) (. *meta_map* :find *current_file*))
		(setq sx (+ *scroll_x* (getf event +ev_msg_wheel_x))
			sy (- *scroll_y* (getf event +ev_msg_wheel_y)))
		(. *meta_map* :insert *current_file* (list x y ax ay sx sy _ buffer))
		(set-sliders) (load-display) this)
	)
