;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Space, a visitor's beam, a vertex shader, for Flight's beam_lit.shader.
;
; Flight's beam is a tube hanging straight down, its sides' ways the
; world's; a visitor's points any way, at a rock, so the way its side
; faces is turned by its matrix into the world's, as the pixel shader
; wants it, and the way along it, its axis. How far along it, from the
; visitor to the rock, and its fade into the dark with distance, as
; Flight's.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/gpu/shaders/noise.shader")

(definput model :mat4)
(definput lens :mat4)
(definput eye :vec3)

(defattr position :vec3)

(defvarying along :float)
(defvarying facing :vec3)
(defvarying place :vec3)
(defvarying fade :float)
(defvarying axis :vec3)

(defconst haze_near 200.0)
(defconst haze_far 560.0)

(defun main :vec4 ()
	(defq at (* model (vec4 position 1.0)))
	(setq along (- (:y position))
		facing (normalize (:xyz (* model (vec4 (:x position) 0.0 (:z position) 0.0))))
		axis (normalize (:xyz (* model (vec4 0.0 -1.0 0.0 0.0))))
		place (:xyz at)
		fade (ease haze_near haze_far (length (- place eye))))
	(* lens at))
