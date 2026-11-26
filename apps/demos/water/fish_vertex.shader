;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, a fish, a vertex shader, for fish_lit.shader.
;
; A vertex where it is on the fish and its normal, fish.inc, its nose
; at x 1/2 and its tail at -1/2, beating: a wave down the body from
; behind the head, side to side, the more the nearer the tail, as fast
; as the fish swims and in a time of its own. Then placed by the fish's
; matrix, the way it swims, its length, and by that of the view.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the fish in the world, how the world is seen, and where from
(definput model :mat4)
(definput lens :mat4)
(definput eye :vec3)
;its kind, its own time in the beat, how fast it beats, and how far its
;tail swings, of its length; and the time, seconds
(definput fish :vec4)
(definput time :float)

(defattr position :vec4)
(defattr normal :vec3)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying far :float)
(defvarying local :vec3)
(defvarying kind :float)

(defun main :vec4 ()
	(defq back (clamp (/ (- 0.3 (:x position)) 0.8) 0.0 1.0)
		swing (* (sin (+ (* time (:z fish)) (:y fish) (* (:x position) 6.0))) (:w fish) (+ (* back back) (* 0.15 back)))
		bent (vec4 (:x position) (:y position) (+ (:z position) swing) 1.0)
		at (* model bent))
	(setq place (:xyz at) local (:xyz position) kind (:x fish)
		facing (:xyz (* model (vec4 normal 0.0)))
		far (length (- place eye)))
	(* lens at))
