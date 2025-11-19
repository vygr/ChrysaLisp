;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; OpenGL Demo Application
; Demonstrates basic OpenGL integration with ChrysaLisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "sys/opengl/class.inc")
(include "sys/opengl/lisp.inc")

(defun main ()
	; OpenGL Demo - Rotating Triangle
	(defq
		running :t
		angle 0.0
		width 800
		height 600)

	(print "ChrysaLisp OpenGL Demo Starting...")
	(print (cat "Window size: " width "x" height))

	; Main render loop
	(while running
		; Clear the color and depth buffers
		(call 'host_gl :gl_clear (list (logior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT)))

		; Set viewport
		(call 'host_gl :gl_viewport (list 0 0 width height))

		; Draw a simple triangle
		(call 'host_gl :gl_begin (list GL_TRIANGLES))

		; First vertex (red)
		(call 'host_gl :gl_color3f (list 1.0 0.0 0.0))
		(call 'host_gl :gl_vertex3f (list 0.0 1.0 0.0))

		; Second vertex (green)
		(call 'host_gl :gl_color3f (list 0.0 1.0 0.0))
		(call 'host_gl :gl_vertex3f (list -1.0 -1.0 0.0))

		; Third vertex (blue)
		(call 'host_gl :gl_color3f (list 0.0 0.0 1.0))
		(call 'host_gl :gl_vertex3f (list 1.0 -1.0 0.0))

		(call 'host_gl :gl_end (list))

		; Flush OpenGL commands
		(call 'host_gl :gl_flush (list))

		; Update angle for rotation
		(setq angle (+ angle 0.01))
		(if (> angle 360.0) (setq angle 0.0))

		; Simple exit condition (for now, runs 1000 frames)
		(if (> angle 10.0) (setq running :nil)))

	(print "OpenGL Demo Complete!"))

; Run the demo
(main)
