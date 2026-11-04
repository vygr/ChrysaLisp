(report-header "GPU: a scene, an object with shaders of its own, lit smooth, that shines")

(import "gui/lisp.inc")
(import "lib/math/mesh.inc")
(import "lib/math/scene.inc")

(defun sc-pixels (canvas size)
	;the pixels of a canvas, as bytes
	(defq stream (memory-stream))
	(pixmap-write (getf canvas +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(slice (read-blk stream 1000000) 0 (* size size 4)))

(defun sc-lit (pixels)
	;how many pixels are not black, and how many are near white
	(defq lit 0 white 0)
	(each (lambda (i)
		(defq p (get-uint pixels i))
		(if (/= (logand p 0xffffff) 0) (setq lit (inc lit)))
		(and (> (logand p 0xff) 230) (> (logand (>> p 8) 0xff) 230) (> (logand (>> p 16) 0xff) 230)
			(setq white (inc white))))
		(range 0 (length pixels) 4))
	(list lit white))

(defun sc-frame (smooth files)
	;a red ball in the middle of a frame, drawn here
	(defq size 128 canvas (Canvas size size 1) scene (Scene "root")
		ball (Scene-object (Mesh-sphere +real_1 12) (fixeds 1.0 0.8 0.1 0.1)))
	(if smooth (def ball :smooth :t))
	(if files (def ball :shaders files))
	(. ball :set_translation +real_0 +real_0 (const (n2r -4)))
	(. scene :add_node ball)
	(defq draws (. scene :draws (const (n2r -1)) +real_1 +real_1 (const (n2r -1)) +real_2 (const (n2r 6)) size))
	(. scene :draw canvas draws)
	(list draws (sc-pixels canvas size)))

(defq sc_files '("lib/gpu/shaders/shiny_vertex.shader" "lib/gpu/shaders/shiny_lit.shader"))
(bind '(sc_draws sc_flat) (sc-frame :nil :nil))
(assert-eq "a draw of the scene's own shaders is as it was, five things" 5 (length (first sc_draws)))
(assert-true "the ball is drawn" (> (first (sc-lit sc_flat)) 1000))

(bind '(sc_draws sc_smooth) (sc-frame :t :nil))
(assert-eq "lit smooth it covers the same pixels" (first (sc-lit sc_flat)) (first (sc-lit sc_smooth)))
(assert-true "and is not the same picture" (nql sc_flat sc_smooth))

(bind '(sc_draws sc_shiny) (sc-frame :t sc_files))
(assert-eq "a draw with shaders of its own has their files on the end" sc_files (last (first sc_draws)))
(assert-eq "it covers the same pixels" (first (sc-lit sc_flat)) (first (sc-lit sc_shiny)))
(assert-true "it has a highlight, a spot that is white" (> (second (sc-lit sc_shiny)) 5))
(assert-true "that is small, a spot and not the ball"
	(< (second (sc-lit sc_shiny)) (/ (first (sc-lit sc_shiny)) 8)))
(assert-eq "the same scene drawn again is the same to the bit" sc_shiny (second (sc-frame :t sc_files)))

;one that is not to shine: its color is what it says, a link of the
;Network Map, whose color is how hot it is
(bind '(sc_draws sc_matte) (sc-frame :t +scene_matte_files))
(assert-eq "an object that is not to shine has its files on the end" +scene_matte_files (last (first sc_draws)))
(assert-eq "it covers the same pixels" (first (sc-lit sc_flat)) (first (sc-lit sc_matte)))
(assert-eq "and none of it is white, there is no highlight" 0 (second (sc-lit sc_matte)))
(defun sc-red? (pixels)
	;no pixel has more green or blue than a fifth of its red
	(every (lambda (i) (defq p (get-uint pixels i) r (logand (>> p 16) 0xff))
		(and (<= (* 5 (logand (>> p 8) 0xff)) (+ r 5)) (<= (* 5 (logand p 0xff)) (+ r 5))))
		(range 0 (length pixels) 4)))
(assert-true "a red ball that does not shine is red all over" (sc-red? sc_matte))
(assert-true "with the scene's own shaders it is not, where it is turned to the light it is washed out" (not (sc-red? sc_smooth)))
