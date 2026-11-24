;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The moon, a vertex shader, for moon_lit.shader.
;
; A vertex is where it is on a ball of radius 1, and where on the map
; of the moon that place is. It is placed by the matrix of the moon,
; how it is turned, how big and where in the sky, and then by that of
; the view. A ball's normal at a place is the place, so the pixel
; shader is handed which way the surface is turned from it.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;how the moon is turned and where it is, and how the world is seen
(definput model :mat4)
(definput spin :mat4)
(definput lens :mat4)

(defattr position :vec3)
(defattr coord :vec2)

;where on the map, across and down, 0 to 1
(defvarying uv :vec2)
;the normal, turned as the moon is
(defvarying facing :vec3)

(defglobal both (* lens model))

(defun main :vec4 ()
	(setq uv coord
		facing (* spin position))
	(* both (vec4 position 1.0)))
