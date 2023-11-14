;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "././viewer/ui.inc")

(defun action-scale-up ()
	;scale font up
	(setq *page_scale* (min 1.5 (+ (defq old_size *page_scale*) 0.125)))
	(when (/= old_size *page_scale*)
		(set *edit* :font (create-font +edit_font (page-scale +edit_size)))
		(.-> *edit_flow* :layout :dirty_all)
		(window-resize)))

(defun action-scale-down ()
	;scale font down
	(setq *page_scale* (max 0.75 (- (defq old_size *page_scale*) 0.125)))
	(when (/= old_size *page_scale*)
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
