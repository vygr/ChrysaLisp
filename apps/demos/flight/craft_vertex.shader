;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A craft, a vertex shader, for craft_lit.shader.
;
; A vertex of a part, where it is and its normal, smooth, as
; (mesh-corners-smooth) makes them. It is placed in the world by the
; part's matrix, which is that of the craft and of each part it is in,
; a scale the same every way, so the normal is turned by it too. The
; place, the normal and how far into the haze it is are handed on, as
; land_vertex.shader hands on the land's, and where it is on the part,
; for what is drawn on it to stay put on it as it turns.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/flight/land.shader")

;the part in the world, how the world is seen, and where from
(definput model :mat4)
(definput lens :mat4)
(definput eye :vec3)

(defattr position :vec4)
(defattr normal :vec3)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying fade :float)
(defvarying local :vec3)

(defconst haze_near 200.0)
(defconst haze_far 560.0)

(defun main :vec4 ()
	(defq at (* model position))
	(setq place (:xyz at) local (:xyz position)
		facing (:xyz (* model (vec4 normal 0.0)))
		fade (ease haze_near haze_far (length (- place eye))))
	(* lens at))
