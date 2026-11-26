;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, a stone, a vertex shader, for stone_lit.shader.
;
; A vertex where it is on the stone and its normal, stones.inc, placed
; by the stone's matrix, how it lies on the floor under it, how it is
; turned about that floor's up, how big and where, and then by that of
; the view. Where it is on the stone itself is handed on, for its beds
; to stay put on it, how high in its beds along its own up, so they
; tilt with it, from a bed of its own, so each shows its own on top; and what is the stone's own, the light where it
; lies and what rock it is, so the pixel shader's inputs are the same
; for all stones and are packed the once a frame.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the stone in the world, how the world is seen, and where from
(definput model :mat4)
(definput lens :mat4)
(definput eye :vec3)
;the sun's and the sky's light where it lies, 0 sandstone or 1 basalt,
;and how high the floor is under it
(definput stone :vec4)
;how far off it is drawn at all: it rises out of the floor as it comes
;nearer than that, all under the floor at it, so it never pops into sight
(definput stone_reach :float)

(defattr position :vec4)
(defattr normal :vec3)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying far :float)
(defvarying lit :vec2)
(defvarying grain :vec2)
(defvarying bedding :float)

(defun main :vec4 ()
	(defq centre (:xyz (* model (vec4 0.0 0.0 0.0 1.0))) up_way (:xyz (* model (vec4 0.0 1.0 0.0 0.0)))
		sunk (clamp (/ (- (length (- centre eye)) (* 0.9 stone_reach)) (* 0.1 stone_reach)) 0.0 1.0)
		at (- (* model position) (vec4 (* up_way (* 2.2 sunk)) 0.0)))
	;how high in its beds, along its own up, so a level slab's beds are
	;level and a tilted piece's tilt with it, from a bed of its own, so
	;which bed is its top is its own too
	(setq bedding (+ (* (fract (* (sin (dot centre (vec3 12.9898 78.233 37.719))) 43758.5453)) 1.1)
			(* (:y position) (length up_way)))
		place (:xyz at)
		facing (:xyz (* model (vec4 normal 0.0)))
		far (length (- place eye))
		lit (:xy stone) grain (:zw stone))
	(* lens at))
