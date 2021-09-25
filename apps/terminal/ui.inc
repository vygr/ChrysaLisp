;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(import "././viewer/ui.inc")

;;;;;;;;;;;;;;;;;;;
; main vdu subclass
;;;;;;;;;;;;;;;;;;;

(defclass Terminal-vdu () (Viewer-vdu)
	(defmethod :mouse_wheel (this event)
		; (. vdu :mouse_wheel event) -> vdu
		(setq *scroll_x* (+ *scroll_x* (getf event +ev_msg_wheel_x))
			*scroll_y* (- *scroll_y* (getf event +ev_msg_wheel_y)))
		(set-sliders) (load-display) this)
	)
