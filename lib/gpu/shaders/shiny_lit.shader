;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A surface that shines, a pixel shader, the lighting of Phong as
; Blinn has it. It goes with shiny_vertex.shader.
;
; The color of the object, a little of it whatever the light, more
; of it the more the surface is turned to the light, and a white
; highlight where it is turned half way between the light and the
; eye. With normals that are smooth, one a vertex, a ball is round
; and has a spot of light on it, that stays put as the ball turns.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the color of the object, and how much of it there is, 1 is solid
(definput color :vec4)

(defvarying facing :vec3)
(defvarying eye :vec3)

;the way to the light, it is up, to the left, and on our side of the scene
(defconst light (vec3 -0.57735 0.57735 0.57735))
(defconst ambient 0.22)
;how much of the color the light brings out, how bright the highlight
;is, and how small, a bigger power is a smaller spot
(defconst diffuse 0.8)
(defconst gleam 0.9)
(defconst tight 40.0)

(defun main :vec4 ((frag :vec2))
	(defq tint (:xyz color)
		n (normalize facing)
		lit (max (dot n light) 0.0)
		half (normalize (+ light (normalize eye)))
		spot (* gleam (pow (max (dot n half) 0.0) tight)))
	(vec4 (min (+ (* tint (+ ambient (* diffuse lit))) (vec3 spot)) (vec3 1.0)) (:w color)))
