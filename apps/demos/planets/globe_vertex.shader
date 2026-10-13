;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The vertices of a globe, for globe_lit.shader.
;
; A vertex is where it is on a ball of radius 1, and where on the
; map of the globe that place is. It is placed by the matrix of
; the globe, how it is turned and where it is, and then by that
; of the view. A ball's normal at a place is the place, so the
; pixel shader is handed which way the surface is turned from it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;how the globe is turned and where it is, and how the scene is seen
(definput model :mat4)
(definput lens :mat4)

(defattr position :vec3)
(defattr coord :vec2)

;where on the map, across and down, 0 to 1
(defvarying uv :vec2)
;the normal, turned as the globe is
(defvarying facing :vec3)

(defglobal both (* lens model))

(defun main :vec4 ()
	(setq uv coord
		facing (* model position))
	(* both (vec4 position 1.0)))
