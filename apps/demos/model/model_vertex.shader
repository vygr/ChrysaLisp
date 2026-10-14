;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The vertices of a model, for model_lit.shader.
;
; A vertex is where it is, where on its material's picture it is,
; and the way its surface faces. It is placed by the matrix of the
; model, how it is turned and where it is, and then by that of the
; view. The pixel shader is handed where on the picture, and the
; way the surface faces, turned as the model is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;how the model is turned and where it is, and how the scene is seen
(definput model :mat4)
(definput lens :mat4)

(defattr position :vec3)
(defattr coord :vec2)
(defattr normal :vec3)

;where on the picture, across and down, 0 to 1, repeated past it
(defvarying uv :vec2)
;the way the surface faces, turned as the model is
(defvarying facing :vec3)

(defglobal both (* lens model))

(defun main :vec4 ()
	(setq uv coord
		facing (* model normal))
	(* both (vec4 position 1.0)))
