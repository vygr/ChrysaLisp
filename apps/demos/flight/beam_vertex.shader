;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A craft's beam, a vertex shader, for beam_lit.shader.
;
; An open tube, a column of light from the craft down to the land: a
; vertex of a tube of radius 1, its top at 0 and its foot at -1, put in
; the world by the beam's matrix, its radius and length. Handed on: how
; far down it is, its normal, out from its middle, where it is, and how
; far into the haze.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/flight/land.shader")

;the beam in the world, how the world is seen and where from
(definput model :mat4)
(definput lens :mat4)
(definput eye :vec3)

(defattr position :vec3)

(defvarying along :float)
(defvarying facing :vec3)
(defvarying place :vec3)
(defvarying fade :float)

(defconst haze_near 200.0)
(defconst haze_far 560.0)

(defun main :vec4 ()
	(defq at (* model (vec4 position 1.0)))
	(setq along (- (:y position))
		facing (normalize (vec3 (:x position) 0.0 (:z position)))
		place (:xyz at)
		fade (ease haze_near haze_far (length (- place eye))))
	(* lens at))
