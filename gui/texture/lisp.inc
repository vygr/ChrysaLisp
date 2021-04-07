;;;;;;;;;
; texture
;;;;;;;;;

(import "class/obj/lisp.inc")

(structure +texture +obj_size
	(ulong handle)
	(uint width height))

(defun texture-metrics (this)
	; (texture-metrics texture) -> (handle width height)
	(list (getf this +texture_handle 0)
		(getf this +texture_width 0)
		(getf this +texture_height 0)))
