(report-header "Whiteboard bite: an eraser that takes out just what it covers, of anything")

(import "gui/lisp.inc")
(import "lib/cwb/board.inc")

(defun bt-board ()
	;a box with a line round it, a line, and a box out of the way
	(defq doc (cwb-doc 400 300))
	(cwb-add doc (cwb-shape (cwb-d-rect 50 50 250 150) :fill 0xffffd070 :stroke 0xff000000 :width 4))
	(cwb-add doc (cwb-shape (cwb-d "M" 20 200 "L" 380 200) :stroke 0xff0000ff :width 6))
	(cwb-add doc (cwb-shape (cwb-d-rect 300 20 380 80) :fill 0xff00ff00 :stroke 0))
	(test-release (Board doc)))
(defun bt-said (board)
	; what is on it: the id, fill and stroke of each, and of a group what
	; is in it
	(str (map (# (if (cwb-group? %0)
			(list :group (map (# (list (elem-get %0 +cwb_id) (elem-get %0 +cwb_fill) (elem-get %0 +cwb_stroke))) (elem-get %0 +cwb_items)))
			(list (elem-get %0 +cwb_id) (elem-get %0 +cwb_fill) (elem-get %0 +cwb_stroke))))
		(cwb-items (. board :get_doc)))))
(defun bt-area (board color)
	; the area that is filled with a colour
	(defq sum 0.0)
	(cwb-walk (cwb-items (. board :get_doc)) :nil (lambda (shape m)
		(if (= (elem-get shape +cwb_fill) color)
			(setq sum (+ sum (poly-area (poly-union (cwb-mat-paths m (first (cwb-flat shape))) (list) 0)))))))
	sum)
(defun bt-in? (board x y) (if (cwb-hit (. board :get_doc) x y 0) :t :nil))

(defq bt (bt-board))
(assert-eq "it is how a board rubs out" :bite (get :rub_mode bt))
(def bt :rub_mode :bite)
(defq bt_was (bt-said bt))
(. bt :snapshot)
(assert-true "an eraser 20 wide drawn down through the box and the line bites" (. bt :rub_along 150 0 150 280 10.0))
(assert-eq "the box is two things now, what is left of its fill and of its line, no group of them, and the line one, each filled and with no line"
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
(defq bt (test-release (Board (cwb-doc 400 300))))
(def bt :rub_mode :bite)
(cwb-add (. bt :get_doc) (cwb-text {Hello} 50 100 :font_size 40))
(defq bt_ink (elem-get (first (cwb-items (. bt :get_doc))) +cwb_fill) bt_all (bt-area bt bt_ink))
(assert-true "words have an area" (> bt_all 500.0))
(assert-true "an eraser drawn through them" (. bt :rub_along 40 85 200 85 4.0))
(assert-true "leaves the most of them, as a shape" (and (< (* 0.4 bt_all) (bt-area bt bt_ink) bt_all)
	(not (eql (elem-get (first (cwb-items (. bt :get_doc))) +cwb_kind) :text))))

;the picture of it in the docs is made by this, so it is what a board does
(assert-true "the docs have a picture of it" (nempty? (cwb-items (cwb-load (file-stream "docs/diagrams/wb_bite.cwb")))))

;all but a sliver gone: the shape is out of the document, and undo has it back
(defq bt (test-release (Board (cwb-doc 400 300))) bt_doc (. bt :get_doc))
(. bt :add (cwb-shape (cwb-d-rect 100 100 120 120) :fill 0xff00ff00 :stroke 0))
(. bt :add (cwb-shape (cwb-d-rect 200 100 220 120) :fill 0xff00ff00 :stroke 0))
(. bt :snapshot)
;an eraser that is a box, that leaves a strip of a tenth by twenty of the first
(. bt :bite (path 110.0 110.0) 1.0 (path -10.0 -15.0 9.9 -15.0 9.9 15.0 -10.0 15.0))
(assert-eq "a strip of 2 square units is left, a shape still" 2 (length (cwb-items bt_doc)))
;and of the second a strip of a twenty fifth by twenty, less than one square unit
(. bt :bite (path 210.0 110.0) 1.0 (path -10.0 -15.0 9.96 -15.0 9.96 15.0 -10.0 15.0))
(assert-eq "a sliver of less than one is nothing, the shape has gone from the document" 1 (length (cwb-items bt_doc)))
(. bt :undo)
(assert-eq "and undo has both back, as they were" (str (list (cwb-d-rect 100 100 120 120) (cwb-d-rect 200 100 220 120)))
	(str (map (# (elem-get %0 +cwb_d)) (cwb-items bt_doc))))

;first come first served: what one has hold of is theirs till they let go
(defq bt (test-release (Board (cwb-doc 400 300))) bt_doc (. bt :get_doc))
(. bt :add (cwb-shape (cwb-d-rect 100 100 200 200) :fill 0xff00ff00 :stroke 0))
(def bt :mode :select)
(defun bt-go (&rest events) (. bt :pointers events))
(defun bt-at () (map (const n2i) (cwb-bounds (cwb-items bt_doc))))
;a pen takes hold of the box and moves it 10
(bt-go (ptr-event 0x2000001 :pen 1 150 150)) (bt-go (ptr-event 0x2000001 :pen 1 160 150))
(assert-eq "a pen has hold of the box, and moved it" "(110 100 210 200)" (str (bt-at)))
;another's finger goes down on it and drags: nothing
(bt-go (ptr-event 0x1000000 :touch 1 150 150)) (bt-go (ptr-event 0x1000000 :touch 1 150 250))
(assert-eq "another's finger on it, dragged, does nothing to it" "(110 100 210 200)" (str (bt-at)))
(assert-eq "and has nothing in hand: no box is dragged out by it" 0 (. (get :bands bt) :size))
;an eraser goes through it as it is held: it is bitten, and what is left is in the hand still, and selected
(. bt :bite (path 100.0 150.0 300.0 150.0) 10.0)
(assert-true "held, the eraser takes its bite out of it: its middle is gone, above and below are there"
	(and (not (cwb-hit bt_doc 160 150 0)) (cwb-hit bt_doc 160 120 0) (cwb-hit bt_doc 160 180 0)))
(assert-eq "it is where it was, and what is left of it is what is selected" "((110 100 210 200) 1 :t)"
	(str (list (bt-at) (length (. bt :get_selected)) (eql (str (. bt :get_selected)) (str (map (# (elem-get %0 +cwb_id)) (cwb-items bt_doc)))))))
;the pen that has it goes on moving it, from where it is, with no jump, the finger still down
(bt-go (ptr-event 0x2000001 :pen 1 180 150))
(assert-eq "the one that has it still has it, what is left of it: moved 20 more it is 20 on" "(130 100 230 200)" (str (bt-at)))
(bt-go (ptr-event 0x1000000 :touch 0 150 250)) (bt-go (ptr-event 0x2000001 :pen 0 180 150))
;an eraser that takes all of what is held: the hand has nothing, and moved it does nothing
(defq bt (test-release (Board (cwb-doc 400 300))) bt_doc (. bt :get_doc))
(. bt :add (cwb-shape (cwb-d-rect 100 100 120 120) :fill 0xff00ff00 :stroke 0))
(def bt :mode :select)
(bt-go (ptr-event 0x2000001 :pen 1 110 110))
(. bt :bite (path 110.0 110.0) 40.0)
(bt-go (ptr-event 0x2000001 :pen 1 200 200)) (bt-go (ptr-event 0x2000001 :pen 0 200 200))
(assert-eq "all of it rubbed out of the hand, there is nothing, and no box was dragged out by the hand that had it" "(0 0 0)"
	(str (list (length (cwb-items bt_doc)) (length (. bt :get_selected)) (. (get :bands bt) :size))))
;two fingers turning a thing, and an eraser through it: they go on turning what is left
(defq bt (test-release (Board (cwb-doc 400 300))) bt_doc (. bt :get_doc))
(. bt :add (cwb-shape (cwb-d-rect 100 100 200 200) :fill 0xff00ff00 :stroke 0))
(def bt :mode :select)
(bt-go (ptr-event 0x1000000 :touch 1 120 150)) (bt-go (ptr-event 0x1000001 :touch 1 180 150))
(bt-go (ptr-event 0x1000000 :touch 1 120 160)) (bt-go (ptr-event 0x1000001 :touch 1 180 140))
(defq bt_turned (bt-at))
(. bt :bite (path 150.0 60.0 150.0 240.0) 6.0)
(defq bt_bitten (bt-at))
(bt-go (ptr-event 0x1000000 :touch 1 130 160)) (bt-go (ptr-event 0x1000001 :touch 1 190 140))
(assert-eq "two fingers have turned it, the eraser has cut it in two, it has not jumped, and moved 10 on by both fingers it is 10 on, both halves"
	(str (list bt_turned (+ (first bt_turned) 10) (+ (third bt_turned) 10)))
	(str (list bt_bitten (first (bt-at)) (third (bt-at)))))
;two fingers of the one device on a thing still move it between them
(defq bt (test-release (Board (cwb-doc 400 300))) bt_doc (. bt :get_doc))
(. bt :add (cwb-shape (cwb-d-rect 100 100 200 200) :fill 0xff00ff00 :stroke 0))
(def bt :mode :select)
(bt-go (ptr-event 0x1000000 :touch 1 120 150)) (bt-go (ptr-event 0x1000001 :touch 1 180 150))
(bt-go (ptr-event 0x1000000 :touch 1 90 150)) (bt-go (ptr-event 0x1000001 :touch 1 210 150))
(assert-true "two fingers of the one device on a thing size it between them, as they did" (> (- (third (bt-at)) (first (bt-at))) 150))

;the eraser's way is kept as it goes, and one bite of all of it is taken at a tick, or as it lifts: the hand is not kept waiting
(defq bt (test-release (Board (cwb-doc 400 300))) bt_doc (. bt :get_doc))
(. bt :add (cwb-shape (cwb-d-rect 100 100 300 200) :fill 0xff00ff00 :stroke 0))
(def bt :mode :eraser)
(defun bt-d () (cwb-get (first (cwb-items bt_doc)) :d))
(defq bt_was (bt-d))
(bt-go (ptr-event 0 :mouse +pev_left 150 50)) (bt-go (ptr-event 0 :mouse +pev_left 150 150)) (bt-go (ptr-event 0 :mouse +pev_left 250 150))
(assert-true "the eraser has gone down the box and along it, and nothing is rubbed out yet" (eql bt_was (bt-d)))
(assert-eq "its way is kept, three places, and how far it reaches, three times the width the board draws with" "(6 9.00000)"
	(str (list (length (first (. (get :rub_todo bt) :find 0))) (second (. (get :rub_todo bt) :find 0)))))
(. bt :tick 1000)
(assert-true "at the tick the bite is taken, of all the way at once: down and along are both gone"
	(and (not (eql bt_was (bt-d))) (not (cwb-hit bt_doc 150 120 0)) (not (cwb-hit bt_doc 200 150 0)) (cwb-hit bt_doc 280 120 0)))
(assert-eq "and no way is kept" :nil (. (get :rub_todo bt) :find 0))
(bt-go (ptr-event 0 :mouse +pev_left 250 250)) (bt-go (ptr-event 0 :mouse 0 250 250))
(assert-true "the rest of its way is bitten as it lifts, with no tick" (not (cwb-hit bt_doc 250 180 0)))
(. bt :undo)
(assert-true "and it is all one step, undone it is the box" (eql bt_was (bt-d)))
;a long line of many points is thinned before it is bitten: what is left has far fewer points than what it was drawn with
(defq bt (test-release (Board (cwb-doc 1200 600))) bt_doc (. bt :get_doc) bt_pts (list))
(each (# (push bt_pts (+ 100.0 (* 0.9 (n2f %0))) (+ 300.0 (* 100.0 (sin (* 0.05 (n2f %0))))))) (range 0 1000))
(. bt :add (cwb-shape (board-pen-d bt_pts) :width 3 :kind :pen))
(defq bt_n (/ (length (first (second (cwb-flat (first (cwb-items bt_doc)))))) 2))
(. bt :bite (path 500.0 100.0 500.0 500.0) 8.0)
(defq bt_m (reduce (# (+ %0 (/ (length %1) 2))) (first (cwb-flat (first (cwb-items bt_doc)))) 0))
(assert-true "a line of a thousand points is drawn with nearly eight thousand, and bitten what is left has little more than half of them" (and (> bt_n 5000) (< bt_m (/ (* bt_n 6) 10))))
(assert-true "it is cut where the eraser went and there each side" (and (not (cwb-hit bt_doc 500 (+ 300 (n2i (* 100.0 (sin (* 0.05 444.0))))) 1)) (cwb-hit bt_doc 300 (+ 300 (n2i (* 100.0 (sin (* 0.05 222.2))))) 2)))

;a turned thing bitten keeps its turn: what is left is in its own space,
;with its matrix, so it lies as it did and its handles are turned as it is
(defq bt_doc (cwb-doc 400 300) bt_box (cwb-shape (cwb-d-rect -60 -20 60 20) :fill 0xff0070ff :stroke 0))
(cwb-set bt_box :m (Mat3x2-mul (Mat3x2-translate 200 150) (Mat3x2-rot 0.5)))
(cwb-add bt_doc bt_box)
(defq bt_t (test-release (Board bt_doc)) bt_m (elem-get bt_box +cwb_m))
(def bt_t :rub_mode :bite)
(. bt_t :rub_along 200 0 200 300 6.0)
(defq bt_left (cwb-items (. bt_t :get_doc)))
(assert-true "bitten through, it is one thing of two rings"
	(and (= 1 (length bt_left)) (= 2 (length (first (cwb-flat (first bt_left)))))))
(assert-true "each piece has the matrix the thing had, its turn kept"
	(every (# (eql (str (elem-get %0 +cwb_m)) (str bt_m))) bt_left))
(assert-true "and is where the thing was: its fill either side of the bite"
	(and (bt-in? bt_t 160 130) (bt-in? bt_t 240 170) (not (bt-in? bt_t 200 150))))
