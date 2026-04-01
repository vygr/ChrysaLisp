;;;;;;;;;
; texture
;;;;;;;;;

(import "./struct.inc")

(defun texture-metrics (this)
	; (texture-metrics texture) -> (handle width height)
	(getf-> this (+texture_handle 0) (+texture_width 0) (+texture_height 0)))
