;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The surface of a model, a pixel shader. It goes with
; model_vertex.shader.
;
; The colour is that of the material's picture at the place, times
; its tint, a little of it whatever the light and more of it the
; more the surface is turned to the light. The back of a face is
; lit as its front, a model need not be closed.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the picture of the material, its colour
(deftexture surface)

;the colour the picture is multiplied by, and the way to the light
(definput tint :vec3)
(definput sun :vec3)

(defvarying uv :vec2)
(defvarying facing :vec3)

(defconst ambient 0.25)
(defconst diffuse 0.9)

(defun main :vec4 ((frag :vec2))
	(defq colour (* (:xyz (texture surface uv)) tint)
		lit (abs (dot (normalize facing) sun)))
	(vec4 (min (* colour (+ ambient (* diffuse lit))) (vec3 1.0)) 1.0))
