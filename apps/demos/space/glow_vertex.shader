;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, the glow round a buoy's lamp, a vertex shader, for
; glow_lit.shader.
;
; Two triangles, a square of corners -1 to 1, (sky-verts), turned to
; face the eye where the lamp is, as big as the glow. The corner is
; handed on.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definput middle :vec3)
(definput size :float)
(definput right :vec3)
(definput up :vec3)
(definput lens :mat4)

(defattr corner :vec2)

(defvarying square :vec2)

(defun main :vec4 ()
	(setq square corner)
	(* lens (vec4 (+ middle (* (+ (* right (:x corner)) (* up (:y corner))) size)) 1.0)))
