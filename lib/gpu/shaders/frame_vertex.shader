;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The corners of a frame, a vertex shader. It goes with any pixel
; shader that is drawn over all of a frame: two triangles that cover
; it, each corner where it is given, -1 to 1 across and up, and the
; pixel shader is run for every pixel between.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defattr corner :vec2)

(defun main :vec4 ()
	(vec4 (:x corner) (:y corner) 0.0 1.0))
