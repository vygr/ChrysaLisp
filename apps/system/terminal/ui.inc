;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/tools/viewer/ui.inc")

(redefun action-scale-up ()
	;scale font up
	(defq old_size (get :zoom *window*)
		new_size (min 1.5 (+ old_size 0.125)))
	(when (/= old_size new_size)
		(def *window* :zoom new_size)
		(set *edit* :font (create-font +edit_font (page-scale +edit_size)))
		(.-> *edit_flow* :layout :dirty_all)
		(window-resize)))

(redefun action-scale-down ()
	;scale font down
	(defq old_size (get :zoom *window*)
		new_size (max 0.75 (- old_size 0.125)))
	(when (/= old_size new_size)
		(def *window* :zoom new_size)
		(set *edit* :font (create-font +edit_font (page-scale +edit_size)))
		(.-> *edit_flow* :layout :dirty_all)
		(window-resize)))

;;;;;;;;;;;;;;;;;;;;
; main edit subclass
;;;;;;;;;;;;;;;;;;;;

(defclass Terminal-edit () (Viewer-edit)
	(def this :ink_color +argb_green)

	(defmethod :mouse_wheel (event)
		; (. edit :mouse_wheel event) -> edit
		(raise :scroll_x :scroll_y)
		(setq scroll_x (+ scroll_x (getf event +ev_msg_wheel_x))
			scroll_y (- scroll_y (getf event +ev_msg_wheel_y)))
		(lower :scroll_x :scroll_y)
		(refresh-sliders) (refresh-display) this)
	)
