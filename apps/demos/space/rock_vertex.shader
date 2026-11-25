;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, a rock, a vertex shader, for rock_lit.shader.
;
; A vertex where it is on the rock and its normal, rock.inc, placed by
; the rock's matrix, how it is turned, how big and where, and then by
; that of the view. Where it is on the rock itself is handed on, for
; what is drawn on it to stay put on it as it turns; and what is the
; rock's own, the same over all of it, so the pixel shader's inputs are
; the same for all rocks and are packed the once a frame.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the rock in the world, how the world is seen, and where from
(definput model :mat4)
(definput lens :mat4)
(definput eye :vec3)
;where on the moon's map the rock is, how big, and the lamp lighting it
(definput rock_patch :vec3)
(definput rock_bulk :float)
(definput rock_lamp_at :vec3)
(definput rock_lamp :vec3)

(defattr position :vec4)
(defattr normal :vec3)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying local :vec3)
(defvarying far :float)
(defvarying patch :vec3)
(defvarying bulk :float)
(defvarying buoy_at :vec3)
(defvarying buoy_light :vec3)

(defun main :vec4 ()
	(defq at (* model position))
	(setq place (:xyz at) local (:xyz position)
		facing (:xyz (* model (vec4 normal 0.0)))
		far (length (- place eye))
		patch rock_patch bulk rock_bulk buoy_at rock_lamp_at buoy_light rock_lamp)
	(* lens at))
