(report-header "Eyes: the shader of the Eyes app, two balls that turn to look")

(import "gui/lisp.inc")
(import "lib/gpu/vp.inc")

(defq ey_program (shader-load "apps/desktop/eyes/eyes.shader") ey_native (shader-vp ey_program)
	ey_w 128 ey_h 64)

(defun ey-look (x y)
	;a gaze, so far to the right and so far down, as the app works it out
	(defq len (sqrt (+ (* x x) (* y y) 1.44)))
	(map (# (n2r (/ %0 len))) (list x y -1.2)))

(defun ey-draw (left right)
	; (ey-draw left right) -> pixels
	;both eyes, as the app draws them, and the pixels as they are saved
	(defq canvas (Canvas ey_w ey_h 1) pixmap (getf canvas +canvas_pixmap 0)
		stream (memory-stream))
	(shader-vp-draw ey_native
		(shader-vp-frame ey_program ey_native (list
			(list 'resolution (list ey_w ey_h))
			(list 'look_left left) (list 'look_right right)
			(list 'iris_color (map (const n2r) '(0.0 1.0 0.0)))
			(list 'iris_size 0.49) (list 'pupil_size 0.4)))
		pixmap 0 0 ey_w ey_h ey_h :t)
	(pixmap-write pixmap stream 32)
	(stream-seek stream 0 0)
	(slice (read-blk stream 1000000) 0 (* ey_w ey_h 4)))

(defun ey-at (pixels x y)
	; (ey-at pixels x y) -> (alpha red green blue)
	(defq p (get-uint pixels (* (+ x (* y ey_w)) 4)))
	(map (# (logand (>> p %0) 0xff)) '(24 16 8 0)))

;looking straight out
(defq ey_out (ey-draw (ey-look 0.0 0.0) (ey-look 0.0 0.0)))
(assert-eq "the corner of the frame is clear" "(0 0 0 0)" (str (ey-at ey_out 0 0)))
(assert-eq "so is the gap between the eyes, top and middle" "(0 0 0 0)" (str (ey-at ey_out 64 1)))
(bind '(a r g b) (ey-at ey_out 32 32))
(assert-true "the middle of an eye that looks at us is its pupil, solid and dark"
	(and (= a 255) (< (max r g b) 40)))
(bind '(a r g b) (ey-at ey_out 43 32))
(assert-true "next to it is the iris, the green it was given" (and (= a 255) (> g (* 2 (max r b)))))
(bind '(a r g b) (ey-at ey_out 32 58))
(assert-true "and out by the edge is the white" (and (= a 255) (> (min r g b) 100) (< (- (max r g b) (min r g b)) 60)))
(assert-eq "the two eyes that look the same way are the same"
	(str (map (# (ey-at ey_out %0 20)) (range 4 60))) (str (map (# (ey-at ey_out (+ %0 64) 20)) (range 4 60))))

;looking away
(defq ey_side (ey-draw (ey-look 2.4 0.0) (ey-look 0.0 0.0)))
(bind '(a r g b) (ey-at ey_side 32 32))
(assert-true "an eye that looks to the side has the white in its middle" (and (= a 255) (> (min r g b) 150)))
(bind '(a r g b) (ey-at ey_side 58 32))
(assert-true "and its iris or pupil over at that side" (< (min r b) 100))
(assert-eq "the other eye is as it was"
	(str (map (# (ey-at ey_out %0 32)) (range 64 128))) (str (map (# (ey-at ey_side %0 32)) (range 64 128))))
