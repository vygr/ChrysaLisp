;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, the water round the diver, a pixel shader. It goes with
; lib/gpu/shaders/sky_vertex.shader, at the back of the depth, and is
; drawn after the reef, so it is worked out only where it is seen.
;
; The way a pixel looks is made from the eye's forward, right and up
; and where on the frame it is. Looking up is the surface, its window
; of the sky and the sun, look.shader; every other way the water, dark
; down and deep, bright up and near the surface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "apps/demos/water/look.shader")

(definput forward :vec3)
(definput right :vec3)
(definput up :vec3)
(definput spread :vec2)
(definput eye :vec3)
(definput time :float)
(definput murk :float)
(definput open :float)
(definput lamp :float)

(defvarying screen :vec2)

(defun main :vec4 ((frag :vec2))
	(defq d (normalize (+ forward (* right (* (:x screen) (:x spread))) (* up (* (:y screen) (:y spread))))))
	;as lit as the eye is, little in the caves, and the lamp's glow down
	;its beam
	(vec4 (+ (water_seen eye d time murk open) (lamp_glow d forward lamp 1.0)) 1.0))
