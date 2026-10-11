;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A lit face of a mesh that does not shine, a pixel shader. It goes
; with any vertex shader that has a facing and a fade,
; mesh_vertex.shader has.
;
; The color of the object and nothing but, for a thing whose color
; is what it says: less of it the further away and the more it is
; turned from the light, a little of it whatever the light. No
; highlight, which is white whatever the color under it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the color of the object, and how much of it there is, 1 is solid
(definput color :vec4)

(defvarying facing :vec3)
(defvarying fade :float)

;the way to the light, it is up, to the left, and on our side of the scene
(defconst light (vec3 -0.55 0.55 0.55))
(defconst ambient 0.25)

(defun main :vec4 ((frag :vec2))
	(defq tint (:xyz color)
		turn (max (dot (normalize facing) light) 0.0)
		lit (+ ambient (* fade (+ 0.5 (* turn 0.5)))))
	(vec4 (min (* tint lit) (vec3 1.0)) (:w color)))
