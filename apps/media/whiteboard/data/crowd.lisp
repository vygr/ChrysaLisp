;a board of very many shapes, crowd.cwb beside this, to see the nodes of
;the machine draw it in stripes, lib/cwb/stripes.inc, as it is moved,
;zoomed and turned. Made again by: lisp apps/media/whiteboard/data/crowd.lisp -r :t
(import "gui/lisp.inc")
(import "lib/cwb/board.inc")

(defq doc (cwb-doc 3200 2400) count 6000 cx 1600.0 cy 1200.0)
(. doc :insert :style :plain)
(each (lambda (i)
		;out along a spiral, every colour of the wheel once round it
		(defq a (* (n2f i) 0.0523) r (+ 40.0 (* (n2f i) 0.19))
			x (n2i (+ cx (* r (cos a)))) y (n2i (+ cy (* r (sin a))))
			h (% (* i 7) 360) col (+ 0xff000000 (cond
				((< h 60) (+ 0xff0000 (* (/ (* h 255) 60) 0x100)))
				((< h 120) (+ 0x00ff00 (* (- 255 (/ (* (- h 60) 255) 60)) 0x10000)))
				((< h 180) (+ 0x00ff00 (/ (* (- h 120) 255) 60)))
				((< h 240) (+ 0x0000ff (* (- 255 (/ (* (- h 180) 255) 60)) 0x100)))
				((< h 300) (+ 0x0000ff (* (/ (* (- h 240) 255) 60) 0x10000)))
				((+ 0xff0000 (- 255 (/ (* (- h 300) 255) 60)))))))
		(cwb-add doc (case (% i 5)
			(0 (cwb-shape (cwb-d-rect x y (+ x 40) (+ y 30) 6) :fill col :stroke 0xff000000 :width 2))
			(1 (cwb-shape (cwb-d-ellipse (+ x 20) (+ y 20) 18 12) :stroke col :width 3))
			(2 (cwb-shape (cwb-d-line x y (+ x 50) (+ y 35)) :stroke col :width 4 :cap2 :arrow))
			(3 (cwb-shape (cwb-d-arc (+ x 20) (+ y 20) 16 0.3 4.0 :t) :fill col))
			(:t (cwb-shape (board-pen-d (list x y (+ x 15) (+ y 25) (+ x 30) y (+ x 45) (+ y 25))) :stroke col :width 3)))))
	(range 0 count))
(cwb-add doc (cwb-text-mid (cat (str count) " shapes, drawn by the nodes") 1600 120 :font_size 64))
(cwb-save doc (file-stream "apps/media/whiteboard/data/crowd.cwb" +file_open_write))
(print "crowd.cwb " (str count) " shapes")
