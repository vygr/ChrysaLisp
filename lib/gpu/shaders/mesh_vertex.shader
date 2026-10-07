;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The vertices of a mesh of a scene, lib/math/scene.inc.
;
; A vertex is where it is in its object, and the normal of the face
; it is a corner of. It is placed by the matrix of its object, then
; by that of the view. What a pixel shader is handed is which way
; the face is turned, and how much light there is that far away.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;where the object is in the scene, and how the scene is seen
(definput model :mat4)
(definput lens :mat4)

(defattr position :vec4)
(defattr normal :vec3)

;the normal, turned as the object is
(defvarying facing :vec3)
;less light the further away
(defvarying fade :float)

;the two as one, worked out once for all the vertices of the object
(defglobal both (* lens model))

(defun main :vec4 ()
	(defq p (* both position))
	(setq facing (* model normal)
		fade (/ 1.0 (+ (/ (:z p) (:w p)) 2.0)))
	p)
