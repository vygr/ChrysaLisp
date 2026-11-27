;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Water, a chunk of the reef, a vertex shader, for reef_lit.shader.
;
; A vertex where it is in its chunk, its normal and its light, the
; sun's and the sky's, chunk.inc, placed by where the chunk is nearest
; the eye, from the eye, and then by the view's turn: small numbers, so
; singles keep the depth exact anywhere round the world, the place the
; world's again for the pixel shader. A chunk is only moved, never
; turned, so its normals are the world's.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definput model :mat4)
(definput lens :mat4)
(definput eye :vec3)

(defattr position :vec4)
(defattr normal :vec3)
(defattr light :vec2)

(defvarying place :vec3)
(defvarying facing :vec3)
(defvarying far :float)
(defvarying lit :vec2)

(defun main :vec4 ()
	(defq at (* model position))
	(setq place (+ (:xyz at) eye) facing normal far (length (:xyz at)) lit light)
	(* lens at))
