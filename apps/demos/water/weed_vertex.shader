;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, a clump of weed, a vertex shader, for weed_lit.shader.
;
; A vertex where it is on the clump and its normal, weeds.inc, placed
; by the clump's matrix, upright on the floor under it, turned and
; sized, and then by that of the view. It sways in the swell: the
; higher up a blade, the more, the square of how high, as a wave that
; travels over the seabed, so clumps near each other sway together a
; little apart, a slower roll under it. As the stones do, it rises out
; of the floor as it comes nearer than how far off it is drawn, all
; under the floor at that, where the water hides it anyway.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the clump in the world, how the world is seen, and where from
(definput model :mat4)
(definput lens :mat4)
(definput eye :vec3)
;the sun's and the sky's light where it grows, 0 kelp or 1 grass, and
;how high the floor is under it
(definput stone :vec4)
;how far off it is drawn at all, and the time, seconds, the swell goes by
(definput stone_reach :float)
(definput time :float)

(defattr position :vec4)
(defattr normal :vec3)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying far :float)
(defvarying lit :vec2)
(defvarying grain :vec2)

;the way the swell goes, across the seabed
(defconst swell_way (vec3 0.8 0.0 0.6))

(defun main :vec4 ()
	(defq centre (:xyz (* model (vec4 0.0 0.0 0.0 1.0))) up_way (:xyz (* model (vec4 0.0 1.0 0.0 0.0)))
		tall (length up_way) h (:y position)
		;the swell where the clump is, and the roll under it
		phase (- (* (dot centre swell_way) 0.12) (* time 0.9))
		sway (* (+ (sin phase) (* 0.35 (sin (+ (* phase 2.3) 1.7)))) h h tall 0.22)
		roll (* (sin (+ (* time 0.6) (* (:x centre) 0.3) (* (:z centre) 0.2))) h h tall 0.08)
		sunk (clamp (/ (- (length (- centre eye)) (* 0.9 stone_reach)) (* 0.1 stone_reach)) 0.0 1.0)
		at (+ (* model position)
			(vec4 (- (+ (* swell_way sway) (vec3 (* roll (:z swell_way)) 0.0 (* roll (- (:x swell_way)))))
				(* up_way (* 1.2 sunk))) 0.0)))
	(setq place (:xyz at)
		facing (:xyz (* model (vec4 normal 0.0)))
		far (length (- place eye))
		lit (:xy stone) grain (vec2 (:z stone) h))
	(* lens at))
