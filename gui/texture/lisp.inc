;;;;;;;;;
; texture
;;;;;;;;;

(import "class/obj/lisp.inc")

(structure +texture +obj_size
	(ulong handle)
	(uint width height))

(defun texture-metrics (this)
	; (texture-metrics texture) -> (handle width height)
	(list (get-field this +texture_handle +long_size)
		(get-field this +texture_width (const (neg +int_size)))
		(get-field this +texture_height (const (neg +int_size)))))
