;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, a rock, a vertex shader, for rock_lit.shader.
;
; A vertex where it is on the rock and its normal, rock.inc, placed by
; the rock's matrix, how it is turned, how big and where, and then by
; that of the view. Where it is on the rock itself is handed on, for
; what is drawn on it to stay put on it as it turns.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the rock in the world, how the world is seen, and where from
(definput model :mat4)
(definput lens :mat4)
(definput eye :vec3)

(defattr position :vec4)
(defattr normal :vec3)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying local :vec3)
(defvarying far :float)

(defun main :vec4 ()
	(defq at (* model position))
	(setq place (:xyz at) local (:xyz position)
		facing (:xyz (* model (vec4 normal 0.0)))
		far (length (- place eye)))
	(* lens at))
