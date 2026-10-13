;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The surface of a globe, a pixel shader. It goes with
; globe_vertex.shader.
;
; The color is that of the map at the place, a little of it
; whatever the light, and more of it the more the surface is
; turned to the sun, so the globe has a day side and a night side.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the map of the globe, east to the right and north at the top
(deftexture surface)

;the way to the sun
(definput sun :vec3)

(defvarying uv :vec2)
(defvarying facing :vec3)

(defconst ambient 0.06)
(defconst diffuse 1.1)

(defun main :vec4 ((frag :vec2))
	(defq tint (:xyz (texture surface uv))
		lit (max (dot (normalize facing) sun) 0.0))
	(vec4 (min (* tint (+ ambient (* diffuse lit))) (vec3 1.0)) 1.0))
