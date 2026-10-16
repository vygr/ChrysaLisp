;;;;;;;;;;;;;
; mask widget
;;;;;;;;;;;;;

(import "././view/lisp.inc")

(defclass Mask () (View)
	; (Mask) -> mask

	(defmethod :draw ()
		; (. mask :draw) -> mask
		(.-> this
			(:ctx_set_color (get :paper this))
			(:ctx_filled_region +view_opaque_region)))

	(defmethod :layout ()
		; (. mask :layout) -> mask
		this)
	)
