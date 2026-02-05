;;;;;;;;;
; texture
;;;;;;;;;

(import "class/obj/lisp.inc")
(import "./struct.inc")

(defun texture-metrics (this)
	; (texture-metrics texture) -> (handle width height)
	(list (getf this +texture_handle 0)
		(getf this +texture_width 0)
		(getf this +texture_height 0)))
