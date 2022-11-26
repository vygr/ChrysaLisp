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

(defun action-font-up ()
	;scale font up
	(bind '(name size) (font-info (get :font *edit*)))
	(set *edit* :font (defq font (create-font name (min (inc size) 48))))
	(.-> *main_flow* :layout :dirty_all)
	(window-resize))

(defun action-font-down ()
	;scale font up
	(bind '(name size) (font-info (get :font *edit*)))
	(set *edit* :font (defq font (create-font name (max (dec size) 8))))
	(.-> *main_flow* :layout :dirty_all)
	(window-resize))

;;;;;;;;;;;;;;;;;;;;
; main edit subclass
;;;;;;;;;;;;;;;;;;;;

(defclass Viewer-edit () (Editor-edit)
	(defmethod :mouse_move (this event)
		; (. edit :mouse_move event) -> edit
		(raise :buffer)
		(bind '(x y) (. this :char_pos event))
		(bind '(x y) (. buffer :constrain x y))
		(. buffer :set_cursor x y)
		(refresh) this)

	(defmethod :mouse_wheel (this event)
		; (. edit :mouse_wheel event) -> edit
		(raise :scroll_x :scroll_y)
		(setq scroll_x (+ scroll_x (getf event +ev_msg_wheel_x))
			scroll_y (- scroll_y (getf event +ev_msg_wheel_y)))
		(lower :scroll_x :scroll_y)
		(bind '(x y ax ay sx sy) (. *meta_map* :find *current_file*))
		(setq sx (+ sx (getf event +ev_msg_wheel_x))
			sy (- sy (getf event +ev_msg_wheel_y)))
		(. *meta_map* :insert *current_file* (list x y ax ay sx sy))
		(set-sliders) (load-display) this)
	)
