;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The sky, a vertex shader, for a pixel shader of a sky.
;
; Two triangles over all of the frame, as frame_vertex.shader, but at
; the far end of the depth, so whatever else is drawn is in front of
; them. Each corner is handed on, the pixel shader makes the way it
; looks from it. (sky-verts), lib/gpu/shapes.inc, is the mesh.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defattr corner :vec2)

(defvarying screen :vec2)

(defun main :vec4 ()
	(setq screen corner)
	(vec4 (:x corner) (:y corner) 0.9999 1.0))
