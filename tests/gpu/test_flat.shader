;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; A pixel shader of one colour, for test_render_pairs.lisp, with
; frame_vertex.shader: a draw whose pair and vertex are not those of
; the render it is drawn by.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(definput tint :vec3)

(defun main :vec4 ((frag :vec2))
	(vec4 tint 1.0))
