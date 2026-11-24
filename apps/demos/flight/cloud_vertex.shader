;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The clouds, a vertex shader, for cloud_lit.shader.
;
; One sheet, flat, at the clouds' height, a square over the eye moved
; with it, a corner -1 to 1 each way. Where on the sheet a pixel is, in
; the world, is handed on, and how far into the haze it is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/flight/land.shader")

;how the world is seen and where from
(definput lens :mat4)
(definput eye :vec3)

(defattr corner :vec2)

(defvarying place :vec3)
(defvarying fade :float)

;how high the clouds are, how far the sheet reaches, and the haze on them
(defconst cloud_base 150.0)
(defconst reach 650.0)
(defconst haze_near 250.0)
(defconst haze_far 620.0)

(defun main :vec4 ()
	(setq place (vec3 (+ (:x eye) (* (:x corner) reach)) cloud_base (+ (:z eye) (* (:y corner) reach)))
		fade (ease haze_near haze_far (length (- (:xz place) (:xz eye)))))
	(* lens (vec4 place 1.0)))
