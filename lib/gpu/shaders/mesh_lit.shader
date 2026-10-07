;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A lit face of a mesh, a pixel shader. It goes with any vertex
; shader that has a facing and a fade, mesh_vertex.shader has.
;
; The color of the object, less of it the further away, a little of
; it whatever the light, and a highlight where the face is turned
; to the light.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;the color of the object
(definput color :vec3)

(defvarying facing :vec3)
(defvarying fade :float)

;the way to the light, it is up, to the left, and on our side of the scene
(defconst light (vec3 -0.55 0.55 0.55))
(defconst ambient 0.25)

(defun main :vec4 ((frag :vec2))
	(defq turn (max (dot (normalize facing) light) 0.0)
		turn2 (* turn turn)
		shine (* turn2 turn2 turn2))
	(vec4 (min (+ (* color fade) (* color ambient) (vec3 shine)) (vec3 1.0)) 1.0))
