;;;;;;;;;
; texture
;;;;;;;;;

(import "./struct.inc")

(defun texture-metrics (this)
	; (texture-metrics texture) -> (handle width height)
	(getf-> this (+texture_handle 0) (+texture_width 0) (+texture_height 0)))

(defun texture-sized (this)
	; (texture-sized texture) -> (texture width height)
	;what a view's :ctx_blit is given, the texture itself, which the view's
	;list of draws keeps while it has it, and its size
	(list this (getf this +texture_width 0) (getf this +texture_height 0)))
