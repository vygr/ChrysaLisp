(report-header "Whiteboard bite: an eraser that takes out just what it covers, of anything")

(import "gui/lisp.inc")
(import "lib/cwb/board.inc")

(defun bt-board ()
	;a box with a line round it, a line, and a box out of the way
	(defq doc (cwb-doc 400 300))
	(cwb-add doc (cwb-shape (cwb-d-rect 50 50 250 150) :fill 0xffffd070 :stroke 0xff000000 :width 4))
	(cwb-add doc (cwb-shape (cwb-d "M" 20 200 "L" 380 200) :stroke 0xff0000ff :width 6))
	(cwb-add doc (cwb-shape (cwb-d-rect 300 20 380 80) :fill 0xff00ff00 :stroke 0))
	(Board doc))
(defun bt-said (board)
	; what is on it: the id, fill and stroke of each
	(str (map (# (list (elem-get %0 +cwb_id) (elem-get %0 +cwb_fill) (elem-get %0 +cwb_stroke))) (cwb-items (. board :get_doc)))))
(defun bt-area (board color)
	; the area that is filled with a colour
	(defq sum 0.0)
	(cwb-walk (cwb-items (. board :get_doc)) :nil (lambda (shape m)
		(if (= (elem-get shape +cwb_fill) color)
			(setq sum (+ sum (poly-area (poly-union (cwb-mat-paths m (first (cwb-flat shape))) (list) 0)))))))
	sum)
(defun bt-in? (board x y) (if (cwb-hit (. board :get_doc) x y 0) :t :nil))

(defq bt (bt-board))
(assert-eq "a board rubs out as it did unless it is told" :part (get :rub_mode bt))
(def bt :rub_mode :bite)
(defq bt_was (bt-said bt))
(. bt :snapshot)
(assert-true "an eraser 20 wide drawn down through the box and the line bites" (. bt :rub_along 150 0 150 280 10.0))
(assert-eq "the box is two things now, what is left of its fill and of its line, and the line one, each filled and with no line"
	(str (list (list 4 0xffffd070 0) (list 5 0xff000000 0) (list 6 0xff0000ff 0) (list 3 0xff00ff00 0))) (bt-said bt))
(assert-true "the fill has lost a strip 20 wide and 100 tall, as near as a circle of straight sides is to 20"
	(< 17990.0 (bt-area bt 0xffffd070) 18080.0))
(assert-true "where it went is not there" (not (bt-in? bt 150 100)))
(assert-true "nor is the line there" (not (bt-in? bt 150 200)))
(assert-true "to each side the fill is" (and (bt-in? bt 100 100) (bt-in? bt 200 100)))
(assert-true "and the line is, to each side" (and (bt-in? bt 60 200) (bt-in? bt 300 200)))
(assert-true "and the line round the box, where it was not rubbed" (bt-in? bt 50 100))
(assert-eq "the box that was out of the way is as it was, the same thing" 4800.0 (bt-area bt 0xff00ff00))
(assert-true "a thing that was bitten is told as changed, for what keeps a copy" (>= (length (. bt :take_changes)) 3))
(. bt :undo)
(assert-eq "undo, and they are what they were, a box and a line" bt_was (bt-said bt))
(assert-true "with nothing gone" (and (bt-in? bt 150 100) (bt-in? bt 150 200)))

;where nothing is, nothing is done
(defq bt (bt-board))
(def bt :rub_mode :bite)
(defq bt_was (bt-said bt))
(assert-true "an eraser drawn where nothing is bites nothing" (not (. bt :rub_along 10 270 390 290 5.0)))
(assert-eq "and nothing is flat that was not" bt_was (bt-said bt))
(assert-true "a small one in the green box" (. bt :rub 340 50 2.0))
(assert-true "does not take it whole, as the other eraser takes a thing that is not a line: it has a small hole"
	(< 4780.0 (bt-area bt 0xff00ff00) 4795.0))

;a dot is a hole
(assert-true "a dot of it in the middle of the box" (. bt :rub 100 100 15.0))
(assert-true "is a hole" (not (bt-in? bt 100 100)))
(assert-true "of a circle of 15" (< 19200.0 (bt-area bt 0xffffd070) 19320.0))
(assert-true "with the box all round it" (and (bt-in? bt 100 80) (bt-in? bt 100 120) (bt-in? bt 80 100) (bt-in? bt 120 100)))

;an eraser of a shape
(defq bt (bt-board))
(def bt :rub_mode :bite)
(assert-true "a cloth 40 by 10 drawn across the box" (. bt :bite (path 100.0 100.0 200.0 100.0) 0.0 (path -20.0 -5.0 20.0 -5.0 20.0 5.0 -20.0 5.0)))
(assert-eq "takes a strip 140 by 10 out of it, to the pixel" 18600.0 (bt-area bt 0xffffd070))

;a thing in a group that is turned, and a group with nothing left
(defq bt (bt-board) bt_doc (. bt :get_doc))
(def bt :rub_mode :bite)
(defq bt_g (cwb-group (list (cwb-shape (cwb-d-rect 300 150 360 190) :fill 0xffff8080 :stroke 0))))
(cwb-set bt_g :m (Mat3x2-rot 0.5 330 170))
(cwb-add bt_doc bt_g)
(defq bt_all (bt-area bt 0xffff8080))
(assert-true "the box in the group is 60 by 40 where it lies turned" (< 2399.9 bt_all 2400.1))
(assert-true "an eraser drawn through it" (. bt :rub_along 290 170 370 170 5.0))
(assert-true "takes a strip out of it" (< 1500.0 (bt-area bt 0xffff8080) (- bt_all 500.0)))
(assert-true "the group is there, with what is left in it"
	(and (find bt_g (cwb-items bt_doc)) (= 1 (length (elem-get bt_g +cwb_items)))))
(assert-true "an eraser big enough to take all of it" (. bt :rub 330 170 60.0))
(assert-true "and the group has gone with the last thing in it" (not (find bt_g (cwb-items bt_doc))))

;words
(defq bt (Board (cwb-doc 400 300)))
(def bt :rub_mode :bite)
(cwb-add (. bt :get_doc) (cwb-text {Hello} 50 100 :font_size 40))
(defq bt_ink (elem-get (first (cwb-items (. bt :get_doc))) +cwb_fill) bt_all (bt-area bt bt_ink))
(assert-true "words have an area" (> bt_all 500.0))
(assert-true "an eraser drawn through them" (. bt :rub_along 40 85 200 85 4.0))
(assert-true "leaves the most of them, as a shape" (and (< (* 0.4 bt_all) (bt-area bt bt_ink) bt_all)
	(not (eql (elem-get (first (cwb-items (. bt :get_doc))) +cwb_kind) :text))))

;the picture of it in the docs is made by this, so it is what a board does
(assert-true "the docs have a picture of it" (nempty? (cwb-items (cwb-load (file-stream "docs/diagrams/wb_bite.cwb")))))
