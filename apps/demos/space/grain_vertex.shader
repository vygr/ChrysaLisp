;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, the grains a hit throws out, a vertex shader, for
; grain_lit.shader.
;
; All the grains of a hit are the one mesh, a little square of two
; triangles a grain, each corner of it with the way and speed of its
; grain, the app's (spray-verts): so a hit is one draw, and the GPU moves
; them. Each grain is where its way has taken it since the hit, at the
; speed the hit gave it, straight, there being no air, its square
; turned to face the eye.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;where the hit was, how long ago, seconds, how hard it threw them, how
;big a grain is, the eye's across and up, how the world is seen
(definput middle :vec3)
(definput seconds :float)
(definput throw :float)
(definput grain :float)
(definput right :vec3)
(definput up :vec3)
(definput lens :mat4)

(defattr corner :vec2)
(defattr way :vec3)

(defvarying square :vec2)
(defvarying shade :float)

(defun main :vec4 ()
	(setq square corner
		;each grain a little its own shade, of its way
		shade (+ 0.6 (* 0.4 (fract (* 37.0 (+ (:x way) (:y way) (:z way)))))))
	(* lens (vec4 (+ middle (* way (* throw seconds))
		(* (+ (* right (:x corner)) (* up (:y corner))) grain)) 1.0)))
