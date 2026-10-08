;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The vertices of a mesh that is to shine, for shiny_lit.shader.
;
; As mesh_vertex.shader, a vertex is where it is in its object and
; a normal, placed by the matrix of its object and then by that of
; the view. The pixel shader is handed which way the surface is
; turned there, and the way from there back to the eye, which a
; highlight is found by.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;where the object is in the scene, and how the scene is seen
(definput model :mat4)
(definput lens :mat4)

(defattr position :vec4)
(defattr normal :vec3)

;the normal, turned as the object is
(defvarying facing :vec3)
;from the surface to the eye. The eye is where the scene is seen from, 0
(defvarying eye :vec3)

(defglobal both (* lens model))

(defun main :vec4 ()
	(setq facing (* model normal)
		eye (* (:xyz (* model position)) -1.0))
	(* both position))
