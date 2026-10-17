(report-header "Whiteboard menu: a panel of cells that opens where a hand taps on nothing, for the pointer that opened it")

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/cwb/palette.inc")

(defun pl-tap (board id kind buttons x y)
	;a pointer down and up at a point
	(. board :pointers (list (ptr-event id kind buttons x y)))
	(. board :pointers (list (ptr-event id kind 0 x y))))

(defun pl-drag (board id kind buttons x y x1 y1)
	(. board :pointers (list (ptr-event id kind buttons x y)))
	(. board :pointers (list (ptr-event id kind buttons x1 y1)))
	(. board :pointers (list (ptr-event id kind 0 x1 y1))))

(defun pl-pick (board palette id kind buttons what &optional val)
	;a tap on a cell, where the menu says it is
	(apply (const pl-tap) (cat (list board id kind buttons) (. palette :where what val))))

(defun pl-ink (canvas back)
	;how many pixels are not what was behind
	(defq stream (memory-stream) n 0)
	(pixmap-write (getf canvas +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(defq bytes (read-blk stream 100000000) want (char back 4))
	(each (# (unless (eql (slice bytes %0 (+ %0 4)) want) (++ n))) (range 0 (length bytes) 4))
	n)

;a board with no menu is as it was
(defq pl_board (Board (cwb-doc 800 600)))
(pl-tap pl_board 0 :mouse +pev_right 400 300)
(assert-eq "a tap on nothing does nothing on a board that was not given a palette" 0 (length (palettes pl_board)))

(palette-enable pl_board)
(. pl_board :tick 0)
(pl-tap pl_board 0 :mouse +pev_right 400 300)
(defq pl_pal (first (palettes pl_board)))
(assert-eq "the right button of the mouse, down and up on nothing, opens one" 1 (length (palettes pl_board)))
(assert-list-eq "about where it tapped, and it is the mouse's" '(400 300 :mouse 0)
	(cat (map (const n2i) (get :origin pl_pal)) (list (get :kind pl_pal) (get :id pl_pal))))
(assert-eq "it is at the front of the stage" :t (eql pl_pal (last (. (. pl_board :get_stage) :get_actors))))
(assert-eq "38 cells: 11 tools, 12 colours and 4 widths, 10 things to do, and the cross" 38 (length (. pl_pal :get_cells)))

;time, it opens at once
(assert-eq "nothing of it moves" :nil (. pl_board :tick 60000))

;drawing
(defq pl_canvas (Canvas 800 600 1))
(. pl_canvas :set_canvas_flags +canvas_flag_antialias)
(. pl_canvas :fill 0xffffffff)
(. pl_board :draw_actors pl_canvas)
(defq pl_open (pl-ink pl_canvas 0xffffffff))
(assert-true "open, it is drawn, a panel 220 by 216" (and (> pl_open 42000) (< pl_open 50000)))

;where a cell is, and what is there
(defq pl_at (. pl_pal :where :tool :rect))
(assert-eq "a point of a cell is on that cell" :rect
	(elem-get (apply (# (. pl_pal :cell_at %0 %1)) (apply (# (. pl_pal :to_own %0 %1)) pl_at)) +cell_val))
(assert-eq "every cell is where it says it is" 38
	(length (filter (lambda (cell)
		(eql cell (apply (# (. pl_pal :cell_at %0 %1)) (apply (# (. pl_pal :to_own %0 %1))
			(. pl_pal :where (elem-get cell +cell_what) (elem-get cell +cell_val))))))
		(. pl_pal :get_cells))))
(assert-eq "a point on it is on it" :t (. pl_pal :hit 400 400))
(assert-eq "and one off it is not" :nil (. pl_pal :hit 400 420))

;a pointer over it, not down
(. pl_board :dirty? +board_dirty_overlay)
(. pl_board :pointers (list (apply (const ptr-event) (cat (list 0 :mouse 0) pl_at))))
(assert-eq "the cell the mouse is over is the one drawn so" :rect (elem-get (get :hover pl_pal) +cell_val))
(assert-eq "which is to be drawn" :t (. pl_board :dirty? +board_dirty_overlay))
(. pl_board :pointers (list (ptr-event 0 :mouse 0 700 500)))
(assert-eq "and none when it has gone off" :nil (get :hover pl_pal))

;colour and width, it stays
(pl-pick pl_board pl_pal 0 :mouse +pev_left :color 0xffe03131)
(pl-pick pl_board pl_pal 0 :mouse +pev_left :width 6.0)
(assert-list-eq "a tap on a colour and one on a width set the board's" '(0xffe03131 6)
	(list (get :color pl_board) (n2i (get :width pl_board))))
(assert-eq "and it is still open" 1 (length (palettes pl_board)))
;a tool, it goes
(. pl_board :dirty? +board_dirty_overlay)
(pl-pick pl_board pl_pal 0 :mouse +pev_left :tool :rect)
(assert-eq "a tap on a tool sets what the board draws" :rect (get :mode pl_board))
(assert-eq "and it shuts" 0 (length (palettes pl_board)))
(assert-eq "shut, a point on it is not on it" :nil (. pl_pal :hit 400 300))
(assert-eq "it is off the stage at once" :nil (find pl_pal (. (. pl_board :get_stage) :get_actors)))
(assert-eq "and what is over the board is to be drawn again, without it" :t (. pl_board :dirty? +board_dirty_overlay))
;what was picked is what is drawn
(pl-drag pl_board 0 :mouse +pev_left 100 100 220 180)
(defq pl_item (last (cwb-items (. pl_board :get_doc))))
(assert-list-eq "the mouse then draws a box, red, 6 wide" (list (cwb-d-rect 100 100 220 180) 0xffe03131 6)
	(list (cwb-get pl_item :d) (cwb-get pl_item :stroke) (n2i (cwb-get pl_item :width))))

;taps that do not open one
(. pl_board :select (list (elem-get pl_item +cwb_id)))
(pl-tap pl_board 0 :mouse +pev_right 600 500)
(assert-list-eq "with something selected, a tap on nothing lets go of it and opens none" '(0 0)
	(list (length (. pl_board :get_selected)) (length (palettes pl_board))))
(pl-drag pl_board 0 :mouse +pev_right 500 400 700 560)
(assert-eq "a hand dragged on nothing is a box round things, and opens none" 0 (length (palettes pl_board)))
(pl-tap pl_board 0 :mouse +pev_right 600 500)
(defq pl_pal (first (palettes pl_board)))
(assert-eq "the next tap on nothing does" 1 (length (palettes pl_board)))
(pl-tap pl_board 0 :mouse +pev_right 100 500)
(assert-eq "a tap off it puts it away, and opens no other" 0 (length (palettes pl_board)))
(pl-tap pl_board 0 :mouse +pev_right 100 500)
(defq pl_pal (first (palettes pl_board)))
(assert-list-eq "it is about the point it was asked for at, near an edge too: under the hand" '(100 500) (map (const n2i) (get :origin pl_pal)))
;down on its bar and dragged it goes with the pointer, and put down nothing
;is picked: it is still there. Its bar is across the top, 108 above the middle
(. pl_board :pointers (list (ptr-event 0 :mouse +pev_left 100 400)))
(. pl_board :pointers (list (ptr-event 0 :mouse +pev_left 260 280)))
(. pl_board :pointers (list (ptr-event 0 :mouse 0 260 280)))
(assert-list-eq "dragged by its bar it is where it was put, and is still open" '(260 380 1)
	(cat (map (const n2i) (get :origin pl_pal)) (list (length (palettes pl_board)))))
(pl-pick pl_board pl_pal 0 :mouse +pev_left :close)
(assert-eq "a tap on its cross puts it away" 0 (length (palettes pl_board)))
;the left button draws, it is the pen
(. pl_board :tick 2000000)
(def pl_board :mode :select)
(pl-tap pl_board 0 :mouse +pev_left 600 300)
(assert-eq "with the board set to select the left button is a hand too, and opens one" 1 (length (palettes pl_board)))
(. (first (palettes pl_board)) :pick :tool :pen)

;things to do
(. pl_board :tick 3000000)
(defq pl_count (length (cwb-items (. pl_board :get_doc))))
(pl-tap pl_board 0 :mouse +pev_right 400 300)
(defq pl_pal (first (palettes pl_board)))
(pl-pick pl_board pl_pal 0 :mouse +pev_left :action :undo)
(assert-list-eq "undo takes the box out, and the palette stays" (list (dec pl_count) 1)
	(list (length (cwb-items (. pl_board :get_doc))) (length (palettes pl_board))))
(pl-pick pl_board pl_pal 0 :mouse +pev_left :action :redo)
(assert-eq "redo puts it back" pl_count (length (cwb-items (. pl_board :get_doc))))
(pl-pick pl_board pl_pal 0 :mouse +pev_left :action :snap)
(assert-eq "snap is on, to the grid of the document" 32 (n2i (get :snap pl_board)))
(pl-pick pl_board pl_pal 0 :mouse +pev_left :action :snap)
(assert-eq "and off" 0 (n2i (get :snap pl_board)))
(pl-pick pl_board pl_pal 0 :mouse +pev_left :action :ruler)
(defq pl_ruler (some (# (if (Ruler? %0) %0)) (. (. pl_board :get_stage) :get_actors)))
(assert-list-eq "a ruler is put where the palette was, and the palette goes" '(400 300 0)
	(cat (map (const n2i) (get :origin pl_ruler)) (list (length (palettes pl_board)))))
(. (. pl_board :get_stage) :sub pl_ruler)

;a pen has its own
(. pl_board :tick 4000000)
(pl-tap pl_board 9 :pen +pev_right 300 300)
(defq pl_pal (first (palettes pl_board)))
(assert-list-eq "a pen with the button on its side held rubs out, and a tap of it on nothing opens its own" '(:pen 9)
	(list (get :kind pl_pal) (get :id pl_pal)))
(pl-pick pl_board pl_pal 9 :pen +pev_left :color 0xff2f9e44)
(pl-pick pl_board pl_pal 9 :pen +pev_left :width 12.0)
(pl-pick pl_board pl_pal 9 :pen +pev_left :tool :ellipse)
(assert-list-eq "what it picks is not the board's" '(:pen 0xffe03131 6)
	(list (get :mode pl_board) (get :color pl_board) (n2i (get :width pl_board))))
(pl-drag pl_board 9 :pen +pev_left 500 100 600 160)
(defq pl_item (last (cwb-items (. pl_board :get_doc))))
(assert-list-eq "that pen draws an ellipse, green, 12 wide" (list :ellipse 0xff2f9e44 12)
	(list (cwb-get pl_item :kind) (cwb-get pl_item :stroke) (n2i (cwb-get pl_item :width))))
(pl-drag pl_board 8 :pen +pev_left 500 200 600 260)
(defq pl_item (last (cwb-items (. pl_board :get_doc))))
(assert-list-eq "another pen draws what the board is set to, in its colour" (list :pen 0xffe03131)
	(list (cwb-get pl_item :kind) (cwb-get pl_item :stroke)))
(pl-drag pl_board 0 :mouse +pev_left 500 300 600 360)
(assert-eq "and so does the mouse" :pen (cwb-get (last (cwb-items (. pl_board :get_doc))) :kind))
(. pl_board :tick 5000000)
(pl-tap pl_board 9 :pen +pev_right 300 300)
(defq pl_pal (first (palettes pl_board)))
(assert-list-eq "opened again by that pen, it shows what that pen has" (list :ellipse 0xff2f9e44 12)
	(list (. pl_pal :value :mode) (. pl_pal :value :color) (n2i (. pl_pal :value :width))))
(pl-pick pl_board pl_pal 9 :pen +pev_left :tool :line)
(assert-eq "a pen that picks again is bound once, not once more each time" 1
	(length (filter (# (and (eql (first %0) :pen) (eql (second %0) 9))) (get :rules (. pl_board :get_bindings)))))

;two at once
(. pl_board :tick 6000000)
(pl-tap pl_board 0x10001 :touch +pev_left 200 200)
(pl-tap pl_board 9 :pen +pev_right 600 400)
(assert-list-eq "a finger opens one, and a pen another, both are there" '(:touch :pen)
	(map (# (get :kind %0)) (palettes pl_board)))
(pl-drag pl_board 0 :mouse +pev_left 50 500 80 560)
(assert-eq "the mouse drawing elsewhere puts neither away, they are not its" 2 (length (palettes pl_board)))
(pl-tap pl_board 0x10002 :touch +pev_left 700 100)
(assert-list-eq "another finger down off them puts the finger's away" '(:pen) (map (# (get :kind %0)) (palettes pl_board)))
(. (first (palettes pl_board)) :close)

;zoomed, it is the same size on the screen
(. pl_board :tick 7000000)
(defq pl_pal (palette-open pl_board 400 300) pl_w1 (. pl_pal :where :tool :rect))
(def pl_board :zoom 2.0)
(defq pl_w2 (. pl_pal :where :tool :rect))
(assert-list-eq "at twice the size a cell is half as far from the middle in the document"
	(map (# (n2i (* 0.5 (- %0 %1)))) pl_w1 '(400.0 300.0)) (map (# (n2i (- %0 %1))) pl_w2 '(400.0 300.0)))
(assert-list-eq "and the menu is on half as much of it" '(:t :nil) (list (. pl_pal :hit 400 353) (. pl_pal :hit 400 356)))
(. pl_pal :close)
(def pl_board :zoom 1.0)
(. pl_board :tick 8000000)
(assert-eq "all are gone from the stage at the end" 0
	(length (filter (const Palette?) (. (. pl_board :get_stage) :get_actors))))

;a finger that goes down somewhere puts away nobody's palette. A tap of one
;on nothing puts away one that is open, and opens one if there was none
(defq pf_board (palette-enable (Board (cwb-doc 800 600))))
(cwb-add (. pf_board :get_doc) (cwb-shape (cwb-d-rect 600 400 700 500) :fill 0xff0000ff :stroke 0))
(pl-tap pf_board 0x10001 :touch 1 200 200)
(assert-eq "a tap of a finger on nothing opens a palette" 1 (length (palettes pf_board)))
(. pf_board :pointers (list (ptr-event 0x10002 :touch 1 650 450)))
(. pf_board :pointers (list (ptr-event 0x10002 :touch 1 660 460)))
(assert-eq "another finger takes hold of a thing and moves it: the palette is still open" 1 (length (palettes pf_board)))
(. pf_board :pointers (list (ptr-event 0x10003 :touch 1 500 100)))
(. pf_board :pointers (list (ptr-event 0x10003 :touch 1 560 160)))
(assert-eq "and another drags a box out on nothing: still open" 1 (length (palettes pf_board)))
(. pf_board :pointers (list (ptr-event 0x10003 :touch 0 560 160)))
(. pf_board :pointers (list (ptr-event 0x10002 :touch 0 660 460)))
(. pf_board :select (list))
(pl-tap pf_board 0x10004 :touch 1 500 300)
(assert-eq "a tap of a finger on nothing, off it, puts it away, and opens no other" 0 (length (palettes pf_board)))
(pl-tap pf_board 0x10005 :touch 1 500 300)
(assert-eq "and the next tap opens one" 1 (length (palettes pf_board)))

;a thing that covers all there is: nowhere to tap that is nothing. The first tap on it selects it, a tap on what is selected asks for the palette
(defq pl_board (Board (cwb-doc 400 300)) pl_doc (. pl_board :get_doc))
(palette-enable pl_board)
(def pl_board :mode :select)
(. pl_board :add (cwb-shape (cwb-d-rect -1000 -1000 2000 2000) :fill 0xff808080 :stroke 0))
(pl-tap pl_board 0 :mouse +pev_left 200 150)
(assert-list-eq "a tap on a thing selects it, and opens nothing" '(1 0) (list (length (. pl_board :get_selected)) (length (palettes pl_board))))
(pl-tap pl_board 0 :mouse +pev_left 210 160)
(assert-list-eq "a tap on it now it is selected takes it out of select mode, and opens nothing: no menu from a tap, Chris" '(0 0)
	(list (length (. pl_board :get_selected)) (length (palettes pl_board))))
;dragged, it is moved and nothing opens
(pl-drag pl_board 0 :mouse +pev_left 210 160 260 200)
(assert-list-eq "dragged, it is moved, and no palette opens, and a drag selects nothing" '(0 0) (list (length (palettes pl_board)) (length (. pl_board :get_selected))))

;the right button on a thing sends it to the back, and that is all it does, selected or not
(defq pr_board (Board (cwb-doc 400 300)) pr_doc (. pr_board :get_doc))
(palette-enable pr_board)
(. pr_board :add (cwb-shape (cwb-d-rect 50 50 200 150) :fill 0xff808080 :stroke 0))
(. pr_board :add (cwb-shape (cwb-d-rect 100 80 200 150) :fill 0xff404040 :stroke 0))
(defq pr_top (elem-get (last (cwb-items pr_doc)) +cwb_id))
(pl-tap pr_board 0 :mouse +pev_right 150 120)
(assert-list-eq "the one in front, tapped by the right button, is selected and behind, and no palette opens"
	(list (list pr_top) :t 0)
	(list (. pr_board :get_selected) (eql (elem-get (first (cwb-items pr_doc)) +cwb_id) pr_top) (length (palettes pr_board))))
;where only the other is: it is selected, and tapped again now it is selected there is still no palette
(defq pr_low (elem-get (first (filter (# (/= (elem-get %0 +cwb_id) pr_top)) (cwb-items pr_doc))) +cwb_id))
(pl-tap pr_board 0 :mouse +pev_right 60 60)
(assert-list-eq "the other, where it is alone, is selected by the right button as well" (list (list pr_top pr_low) 0)
	(list (. pr_board :get_selected) (length (palettes pr_board))))
(pl-tap pr_board 0 :mouse +pev_right 60 60)
(assert-list-eq "tapped again now it is selected: it is selected still, behind still, and no palette opens"
	(list (list pr_top pr_low) :t 0)
	(list (. pr_board :get_selected) (eql (elem-get (first (cwb-items pr_doc)) +cwb_id) pr_low) (length (palettes pr_board))))

;the button on the side of a pen rubs out, and only a tap of it on nothing is the menu
(defq pb_board (Board (cwb-doc 800 600)))
(palette-enable pb_board)
(. pb_board :tick 0)
(pl-drag pb_board 9 :pen +pev_left 100 300 500 300)
(defq pb_before (length (cwb-items (. pb_board :get_doc)))
	pb_d (str (cwb-get (first (cwb-items (. pb_board :get_doc))) :d)))
(pl-drag pb_board 9 :pen +pev_right 300 250 300 350)
(. pb_board :tick 100000)
(defq pb_items (cwb-items (. pb_board :get_doc)))
(assert-true "dragged across a line it takes a bite out of it"
	(or (/= (length pb_items) pb_before) (not (eql pb_d (str (cwb-get (first pb_items) :d))))))
(assert-eq "and no menu is opened by that" 0 (length (palettes pb_board)))
(pl-tap pb_board 9 :pen +pev_right 600 500)
(assert-list-eq "a tap of it on nothing is that pen's menu" '(1 :pen 9)
	(list (length (palettes pb_board)) (get :kind (first (palettes pb_board))) (get :id (first (palettes pb_board)))))
(. (first (palettes pb_board)) :close)
(pl-tap pb_board 7 :eraser +pev_left 600 100)
(assert-eq "a tap of the other end of a pen on nothing opens none, it is an eraser and no more" 0 (length (palettes pb_board)))

;a floating menu shows a dot of each device on what it has bound. Chris: "on
;the float menus each device showed a little dot (coloured per device, of
;that board) to show what that device had selected as its modes"
(defq pf_board (Board (cwb-doc 400 400)))
(. pf_board :tick 1000000)
(defq pf (palette-float pf_board 200 200))
(. pf_board :tick 3000000)
(assert-eq "a floating menu is no device's, and has no dots till one is bound" 0 (length (. pf :devices)))
(.-> (. pf_board :get_bindings)
	(:set :pen 0x2000000 +pev_left :color 0xffe03131)
	(:set :touch 0x1000000 :nil :mode :rect))
(assert-eq "two devices bound, two dots, each the colour of its place, touch the second, a first pen a student's the fourth"
	(str (list 0xffff0000 0xff000000)) (str (map (const first) (. pf :devices))))
(assert-eq "the mouse's place is the first, the teacher's pen the third, a second student's pen the fifth"
	(str '(0 2 4)) (str (list (. pf_board :device_slot :mouse 0) (progn (. pf_board :appoint (ptr-event 0x3000000 :pen 0 0 0))
		(. pf_board :device_slot :pen 0x3000000)) (. pf_board :device_slot :pen 0x4000000))))
(assert-eq "a second button is the row below, the mouse's right, a pen's barrel and its other end; the middle not shown, touch one row"
	"(6 8 8 :nil 1)" (str (list (. pf_board :device_slot :mouse 0 +pev_right) (. pf_board :device_slot :pen 0x3000000 +pev_right)
		(. pf_board :device_slot :eraser 0x3000000) (. pf_board :device_slot :mouse 0 +pev_middle) (. pf_board :device_slot :touch 0x1000000))))
(assert-eq "a switch is a device's own if bound, else the board's: lock on, snap off, a device's snap on"
	"(:t :nil :t :nil)" (str (list (. pf_board :switch_of '(:tool :pen) :keep_shape) (. pf_board :switch_of '(:tool :pen) :snap)
		(. pf_board :switch_of '(:snap :t) :snap) (. pf_board :switch_of '(:keep_shape :nil) :keep_shape))))
;a tap of the mouse on a floating menu binds the button it went down with,
;not the none it came up with: a rule of buttons 0 matched every press of
;the mouse, and in select mode the mouse drew, Chris: "in select mode, I am
;drawing..."
(pl-pick pf_board pf 0 :mouse +pev_left :tool :select)
(assert-true "a mouse's tap on a floating menu binds its left button"
	(some (lambda ((kind id buttons &ignore)) (and (eql kind :mouse) (eql id 0) (eql buttons +pev_left))) (get :rules (. pf_board :get_bindings))))
(assert-eq "and makes no rule of buttons 0" :nil
	(some (lambda ((kind id buttons &ignore)) (eql buttons 0)) (get :rules (. pf_board :get_bindings))))
(. (. pf_board :get_bindings) :set :mouse 0 0 :width 1.5)
(assert-eq "a :set of buttons 0 is of the left button" :nil
	(some (lambda ((kind id buttons &ignore)) (eql buttons 0)) (get :rules (. pf_board :get_bindings))))
(assert-eq "and in select mode the mouse's left is the hand, not a pen" :select
	(. (. pf_board :get_bindings) :value (ptr-event 0 :mouse +pev_left 0 0) :mode))
;what is selected has its handles in any mode, Chris: "single tap to
;select a shape, in any mode, does not bring up the shape controls ?"
(defq ph_board (Board (cwb-doc 400 400)))
(. ph_board :tick 1000000)
(pl-drag ph_board 0 :mouse +pev_left 10 10 100 100)
(. ph_board :select (map (# (elem-get %0 +cwb_id)) (cwb-items (. ph_board :get_doc))))
(assert-true "nothing in select mode, the handles all the same" (. (get :handles ph_board) :box))
(. (. ph_board :get_bindings) :set :mouse 0 +pev_left :mode :select)
(assert-true "the mouse in select mode, the handles" (. (get :handles ph_board) :box))
;a device's main button's pen outlines and its second's fills, Chris: "we
;use the left button bound pen to outline and the right button bound pen to
;fill". A rect is an outline; a filled rect both; one ink for both, a fill
;as far out as the outline would be
(defq pp_board (Board (cwb-doc 400 400)) pp_b (. pp_board :get_bindings))
(defun pp-last () (last (cwb-items (. pp_board :get_doc))))
(. pp_board :tick 1000000)
(.-> pp_b (:set :mouse 0 +pev_left :color 0xffff0000) (:set :mouse 0 +pev_right :color 0x6000ff00)
	(:set :mouse 0 +pev_left :width 10.0) (:set :mouse 0 +pev_left :mode :rect))
(pl-drag pp_board 0 :mouse +pev_left 10 10 100 100)
(defq pp_rect (pp-last))
(. pp_b :set :mouse 0 +pev_left :mode :frect)
(pl-drag pp_board 0 :mouse +pev_left 150 150 250 250)
(defq pp_frect (pp-last))
(. pp_b :set :mouse 0 +pev_right :mode :frect)
(pl-drag pp_board 0 :mouse +pev_right 150 20 250 120)
(assert-eq "a rect is the main pen's outline and no fill; a filled rect the main pen's outline and the second's fill, whichever button drew it"
	(str (list 0xffff0000 0 0xffff0000 0x6000ff00 0xffff0000 0x6000ff00))
	(str (map (# (cwb-get %0 %1)) (list pp_rect pp_rect pp_frect pp_frect (pp-last) (pp-last)) '(:stroke :fill :stroke :fill :stroke :fill))))
(. pp_b :set :mouse 0 +pev_middle :color 0x600000ff)
(. pp_b :set :mouse 0 +pev_left :mode :frect)
(pl-drag pp_board 0 :mouse +pev_left 20 200 120 300)
(assert-eq "one ink in both, a see through outline over the fill would be darker: a fill only, as far out as the outline would be"
	(str (list 0 0x600000ff (list 15 195 125 305)))
	(str (list (cwb-get (pp-last) :stroke) (cwb-get (pp-last) :fill) (map (const n2i) (cwb-bounds (list (pp-last)))))))
;a pen given to what is selected: the main button the outline, the second
;the fill, the middle both, touch what a thing shows
(. pp_board :select (map (# (elem-get %0 +cwb_id)) (list pp_rect pp_frect (pp-last))))
(. pp_board :paint :t (ptr-part (ptr-event 0 :mouse +pev_left 0 0)) 0xff000000)
(. pp_board :paint :t (ptr-part (ptr-event 0 :mouse +pev_right 0 0)) 0xffffffff)
(defq pp_after (map (# (list (cwb-get %0 :stroke) (cwb-get %0 :fill))) (list pp_rect pp_frect (pp-last))))
(. pp_board :paint :t (ptr-part (ptr-event 0x1000001 :touch 1 0 0)) 0xff123456)
(. pp_board :paint (list (elem-get pp_rect +cwb_id)) (ptr-part (ptr-event 0 :mouse +pev_middle 0 0)) 0)
(assert-eq "left outlines, right fills, as given; then touch the outline, or the fill of a fill only; the middle both, none"
	(str (list (list (list 0xff000000 0xffffffff) (list 0xff000000 0xffffffff) (list 0xff000000 0xffffffff))
		(list (list 0 0) (list 0xff123456 0xffffffff) (list 0xff123456 0xffffffff))))
	(str (list pp_after (map (# (list (cwb-get %0 :stroke) (cwb-get %0 :fill))) (list pp_rect pp_frect (pp-last))))))
;the middle of the mouse binds both its buttons, and is no rule of its own:
;what shows the board pans with it, the board never has it
(defq pm_board (Board (cwb-doc 400 400)))
(. (. pm_board :get_bindings) :set :mouse 0 +pev_middle :mode :rect)
(assert-eq "a middle press binds the left and the right, and makes no rule of the middle"
	"(:rect :rect :nil)" (str (list (. (. pm_board :get_bindings) :value (ptr-event 0 :mouse +pev_left 0 0) :mode)
		(. (. pm_board :get_bindings) :value (ptr-event 0 :mouse +pev_right 0 0) :mode)
		(some (lambda ((& & b &ignore)) (eql b +pev_middle)) (get :rules (. pm_board :get_bindings))))))
;an eraser that lifts takes no more bite: its last step is of the buttons it
;went down with, not of none, which is bound to the board's width, Chris:
;"When I release the button on an erase action, I see a blip of change"
(defq pe_board (Board (cwb-doc 400 400)))
(. pe_board :tick 1000000)
(def pe_board :width 40.0)
(. (. pe_board :get_bindings) :set :mouse 0 +pev_left :mode :pen)
(pl-drag pe_board 0 :mouse +pev_left 20 200 380 200)
(.-> (. pe_board :get_bindings) (:set :mouse 0 +pev_left :mode :eraser) (:set :mouse 0 +pev_left :width 3.0))
(. pe_board :pointers (list (ptr-event 0 :mouse +pev_left 100 200)))
(. pe_board :pointers (list (ptr-event 0 :mouse +pev_left 110 200)))
(. pe_board :tick 2000000)
(defq pe_was (str (cwb-items (. pe_board :get_doc))))
(. pe_board :pointers (list (ptr-event 0 :mouse 0 110 200)))
(. pe_board :tick 3000000)
(assert-eq "nothing more is rubbed out as the eraser lifts" pe_was (str (cwb-items (. pe_board :get_doc))))
;what is selected stays so as an eraser bites it, the left selecting and
;the right rubbing out, Chris: "the select controls vanish as the erase takes
;a bite"
(defq ps_board (Board (cwb-doc 400 400)))
(. ps_board :tick 1000000)
(. (. ps_board :get_bindings) :set :mouse 0 +pev_left :mode :frect)
(pl-drag ps_board 0 :mouse +pev_left 50 50 250 250)
(.-> (. ps_board :get_bindings) (:set :mouse 0 +pev_left :mode :select) (:set :mouse 0 +pev_right :mode :eraser))
(. ps_board :select (map (# (elem-get %0 +cwb_id)) (cwb-items (. ps_board :get_doc))))
(pl-drag ps_board 0 :mouse +pev_right 380 150 20 150)
(. ps_board :tick 2000000)
(assert-true "bitten through, what is left is selected, with its handles"
	(and (nempty? (get :selected ps_board)) (. (get :handles ps_board) :box)))
;rubbed out to nothing, nothing of it is selected, and there are no handles
(defq pz_board (Board (cwb-doc 400 400)))
(. pz_board :tick 1000000)
(. (. pz_board :get_bindings) :set :mouse 0 +pev_left :mode :frect)
(pl-drag pz_board 0 :mouse +pev_left 100 100 140 140)
(.-> (. pz_board :get_bindings) (:set :mouse 0 +pev_left :mode :select) (:set :mouse 0 +pev_right :mode :eraser)
	(:set :mouse 0 +pev_right :width 40.0))
(. pz_board :select (map (# (elem-get %0 +cwb_id)) (cwb-items (. pz_board :get_doc))))
(each (lambda (y) (pl-drag pz_board 0 :mouse +pev_right 20 y 300 y) (. pz_board :tick (* (+ 2 (!)) 1000000))) '(95 110 125 140 155))
(assert-eq "rubbed out to nothing: no items, nothing selected, no handles"
	"(0 0 :nil)" (str (list (length (cwb-items (. pz_board :get_doc))) (length (get :selected pz_board)) (. (get :handles pz_board) :box))))
(assert-true "and it was bitten: a mode bound to the right button is a pen of it, not the hand it was"
	(eql (. (. ps_board :get_bindings) :value (ptr-event 0 :mouse +pev_right 0 0) :tool) :pen))
;the left out of select mode, from a floating menu, and nothing is in it: what
;was selected is let go of, Chris: "I'm not in select mode on left/right
;button and still see selected shapes"
(defq ps_menu (palette-float ps_board 200 200))
(. ps_board :tick 3000000)
(pl-pick ps_board ps_menu 0 :mouse +pev_left :tool :pen)
(assert-eq "nothing in select mode, nothing selected" 0 (length (get :selected ps_board)))
;each user's own: the teacher's mouse out of select mode lets go of what the
;teacher tapped, not what a student's pen, still in select mode, tapped
(defq pu_board (Board (cwb-doc 400 400)))
(. pu_board :tick 1000000)
(. (. pu_board :get_bindings) :set :mouse 0 +pev_left :mode :frect)
(pl-drag pu_board 0 :mouse +pev_left 20 20 120 120)
(pl-drag pu_board 0 :mouse +pev_left 200 200 300 300)
(defq pu_ids (map (# (elem-get %0 +cwb_id)) (cwb-items (. pu_board :get_doc))))
(.-> (. pu_board :get_bindings) (:set :mouse 0 +pev_left :mode :select) (:set :pen 0x2000000 +pev_left :mode :select))
(defq pu_student (. pu_board :user_of (ptr-event 0x2000000 :pen +pev_left 0 0)))
(. pu_board :toggle_select (first pu_ids) +board_teacher)
(. pu_board :toggle_select (second pu_ids) pu_student)
(. (. pu_board :get_bindings) :set :mouse 0 +pev_left :mode :pen)
(. pu_board :settle_select)
(assert-eq "out of select mode, what each tapped stays picked up, the teacher's and the student's"
	(str pu_ids) (str (get :selected pu_board)))
;a box no one tapped is let go of once nothing is in select mode
(. pu_board :select pu_ids)
(. pu_board :select (list)) (. pu_board :select pu_ids)
(. (. pu_board :get_bindings) :set :pen 0x2000000 +pev_left :mode :pen)
(. pu_board :settle_select)
(assert-eq "what a box selected goes once nothing is in select mode" "()" (str (get :selected pu_board)))
(. pu_board :toggle_select (first pu_ids) +board_teacher)
(. pu_board :toggle_select (second pu_ids) pu_student)
;an undo is of the document, and what was selected does not outlast it
(. pu_board :select pu_ids)
(. (. pu_board :get_bindings) :set :mouse 0 +pev_left :mode :frect)
(pl-drag pu_board 0 :mouse +pev_left 300 20 380 100)
(. pu_board :undo)
(assert-eq "an undo of a third rect puts the two down" "()" (str (get :selected pu_board)))
(defq pf_canvas (Canvas 400 400 1))
(. pf_canvas :fill 0)
(. pf :draw pf_canvas (Mat3x2))
(defq pf_stream (memory-stream))
(pixmap-write (getf pf_canvas +canvas_pixmap 0) pf_stream 32)
(stream-seek pf_stream 0 0)
(defq pf_data (read-blk pf_stream 10000000) pf_at (- (length pf_data) (* 400 400 4)))
(defun pf-has (col)
	;is that colour on the canvas, near it, as it is drawn soft at its edges
	(some (lambda (i)
			(defq p (get-uint pf_data (+ pf_at (* 4 i))))
			(and (< (abs (- (logand p 0xff) (logand col 0xff))) 12)
				(< (abs (- (logand (>> p 8) 0xff) (logand (>> col 8) 0xff))) 12)
				(< (abs (- (logand (>> p 16) 0xff) (logand (>> col 16) 0xff))) 12)))
		(range 0 (* 400 400))))
(assert-true "each device's dot is drawn" (and (pf-has 0xffff0000) (pf-has 0xff000000)))
;an ink wholly see through is none, Chris: "a pen can be fully transparent ?
;if that the pen used for the outline it does just fill the inner, if the
;transparent ink is the fill we just fill the outline"
(defq pn_board (Board (cwb-doc 400 400)) pn_b (. pn_board :get_bindings))
(defun pn-last () (last (cwb-items (. pn_board :get_doc))))
(. pn_board :tick 1000000)
(.-> pn_b (:set :mouse 0 +pev_left :width 10.0) (:set :mouse 0 +pev_left :mode :frect)
	(:set :mouse 0 +pev_left :color 0x00ff0000) (:set :mouse 0 +pev_right :color 0x8000ff00))
(pl-drag pn_board 0 :mouse +pev_left 20 20 120 120)
(defq pn_inner (pn-last))
(.-> pn_b (:set :mouse 0 +pev_left :color 0xffff0000) (:set :mouse 0 +pev_right :color 0))
(pl-drag pn_board 0 :mouse +pev_left 200 20 300 120)
(assert-eq "a clear outline pen fills the inside, not grown; a clear fill pen is the outline alone"
	(str (list 0 0x8000ff00 (list 20 20 120 120) 0xffff0000 0))
	(str (list (cwb-get pn_inner :stroke) (cwb-get pn_inner :fill) (map (const n2i) (cwb-bounds (list pn_inner)))
		(cwb-get (pn-last) :stroke) (cwb-get (pn-last) :fill))))
(assert-list-eq "none is any ink with no alpha" '(:t :t :nil) (map (const board-none?) '(0 0x00ff0000 0x01000000)))
;each user's own: a hand on nothing puts down only its user's, a hand on a
;thing picks it up for its user and leaves what others hold, who holds what
;is kept only for what is selected, Chris: "2 users can select seperate item"
(defq pw_board (Board (cwb-doc 600 400)) pw_b (. pw_board :get_bindings))
(. pw_board :tick 1000000)
(.-> pw_b (:set :mouse 0 +pev_left :mode :frect))
(pl-drag pw_board 0 :mouse +pev_left 20 20 100 100)
(pl-drag pw_board 0 :mouse +pev_left 200 20 280 100)
(pl-drag pw_board 0 :mouse +pev_left 400 20 480 100)
(bind '(pw_a pw_c pw_s) (map (# (elem-get %0 +cwb_id)) (cwb-items (. pw_board :get_doc))))
(defq pw_student (. pw_board :user_of (ptr-event 0x2000000 :pen +pev_left 0 0)))
(. pw_board :toggle_select pw_a +board_teacher)
(. pw_board :toggle_select pw_s pw_student)
(.-> pw_b (:set :mouse 0 +pev_left :mode :select))
(pl-tap pw_board 0 :mouse +pev_left 300 300)
(assert-eq "the teacher's hand on nothing puts down the teacher's, the student's stays" (str (list pw_s)) (str (get :selected pw_board)))
(assert-eq "and who held the teacher's is forgotten" :nil (. pw_board :holder pw_a))
(pl-tap pw_board 0 :mouse +pev_left 240 60)
(assert-eq "the teacher's hand on a thing picks it up, the student's stays"
	(str (list pw_s pw_c)) (str (get :selected pw_board)))
(assert-eq "and it is the teacher's" +board_teacher (. pw_board :holder pw_c))
;what draws along an instrument's edge is what does not rub out, its catch
;shows by the same rule
(.-> pw_b (:set :pen 0x2000000 +pev_left :mode :eraser))
(assert-list-eq "a pen bound to rub out, an eraser, rub out; the mouse's hand and a finger draw along an edge"
	'(:t :t :nil :nil)
	(map (# (board-rubs? pw_b %0 :pen)) (list (ptr-event 0x2000000 :pen +pev_left 0 0) (ptr-event 0x4000000 :eraser 1 0 0)
		(ptr-event 0 :mouse +pev_right 0 0) (ptr-event 0x1000001 :touch 1 0 0))))
;what a press's buttons are, worked out in one place
(assert-eq "a finger is all its device, none held is the main, the middle of a mouse both, a pen's barrel its second"
	(str (list (list :nil) (list +pev_left) (list +pev_left +pev_right) (list +pev_right) (list +pev_middle)))
	(str (list (ptr-roles :touch 1) (ptr-roles :mouse 0) (ptr-roles :mouse +pev_middle) (ptr-roles :pen +pev_right) (ptr-roles :pen +pev_middle))))
(assert-list-eq "read by its main role: the middle as the left, none as the left, a finger as it is"
	(list +pev_left +pev_left 1) (map (# (elem-get (ptr-main %0) +pev_buttons))
		(list (ptr-event 0 :mouse +pev_middle 0 0) (ptr-event 0 :mouse 0 0 0) (ptr-event 0x1000001 :touch 1 0 0))))
(assert-list-eq "what a press colours: the main the outline, the second the fill, both, a finger what shows"
	'(:stroke :fill :both :ink) (map (const ptr-part) (list (ptr-event 0 :mouse +pev_left 0 0) (ptr-event 0x2000000 :pen +pev_right 0 0)
		(ptr-event 0 :mouse +pev_middle 0 0) (ptr-event 0x1000001 :touch 1 0 0))))
;a slot holds a pen by its number, the board has the pens' inks: a pen
;filled with another ink is the ink of every slot that holds it, Chris: "the
;ability to fill those pens with any ink"
(defq pk_board (Board (cwb-doc 400 400)) pk_b (. pk_board :get_bindings) pk_pens (list +argb_red +argb_green))
(def pk_board :pens pk_pens :pen 1)
(.-> pk_b (:set :mouse 0 +pev_left :pen 0) (:set :mouse 0 +pev_right :color +argb_blue))
(defun pk-inks () (map (# (. pk_board :ink_of (ptr-event %0 %1 %2 0 0))) '(0 0 0x2000000) '(:mouse :mouse :pen) (list +pev_left +pev_right +pev_left)))
(defq pk_was (pk-inks))
(elem-set pk_pens 0 +argb_yellow)
(assert-eq "the left holds pen 0, the right an ink of its own, a pen bound to nothing the board's pen; pen 0 filled yellow, the left draws yellow"
	(str (list (list +argb_red +argb_blue +argb_green) (list +argb_yellow +argb_blue +argb_green)))
	(str (list pk_was (pk-inks))))
(assert-list-eq "and which pen each holds" '(0 :nil 1)
	(map (# (. pk_board :pen_of (ptr-event %0 %1 %2 0 0))) '(0 0 0x2000000) '(:mouse :mouse :pen) (list +pev_left +pev_right +pev_left)))
(assert-eq "a key is found only where keys are, (:tool :pen :pen 3)" 3 (elem-get '(:tool :pen :pen 3) (ptr-at '(:tool :pen :pen 3) :pen)))
;who holds what is the board's, (:holder) (:pick_up) (:others_of), kept
;only for what is selected, and carried to what a thing becomes
(defq po_board (Board (cwb-doc 400 400)))
(defq po_a (. po_board :add (cwb-shape (cwb-d-rect 10 10 50 50) :fill 0xff000000 :stroke 0))
	po_b (. po_board :add (cwb-shape (cwb-d-rect 60 10 100 50) :fill 0xff000000 :stroke 0)))
(. po_board :pick_up po_a 3) (. po_board :pick_up po_b 3)
(defq po_g (. po_board :group))
(defq po_was (list (. po_board :holder po_g) (. po_board :others_of 4) (. po_board :others_of 3)))
(. po_board :duplicate)
(defq po_dup (first (get :selected po_board)))
(defq po_dupped (. po_board :holder po_dup))
(. po_board :ungroup)
(defq po_kids (get :selected po_board))
(assert-eq "a group of what one held is theirs, as is its copy, and the copy's items once it is ungrouped"
	(str (list 3 (list po_g) (list) 3 (list 3 3)))
	(str (list (first po_was) (second po_was) (third po_was) po_dupped (map (# (. po_board :holder %0)) po_kids))))
(. po_board :delete)
(assert-eq "deleted, no one holds them" "(:nil :nil)" (str (map (# (. po_board :holder %0)) po_kids)))
;one pick, (. board :pick event what val), wherever it was made: to what
;its user holds, else to the slot of what pressed, else, no device, the
;board's own
(defq pt_board (Board (cwb-doc 400 400)) pt_b (. pt_board :get_bindings) pt_pens (list +argb_red +argb_green))
(def pt_board :pens pt_pens :pen 0)
(defq pt_left (ptr-event 0 :mouse +pev_left 0 0) pt_right (ptr-event 0 :mouse +pev_right 0 0))
(. pt_board :pick pt_left :pen 1) (. pt_board :pick pt_left :width 9.0) (. pt_board :pick pt_left :mode :rect)
(. pt_board :pick pt_left :switch :snap) (. pt_board :pick :nil :switch :keep_shape)
(assert-eq "nothing held: the left holds pen 1, width 9, boxes and snap; no device turns over the board's lock"
	"(1 9 :rect :t :nil)" (str (list (. pt_b :value pt_left :pen) (n2i (. pt_b :value pt_left :width)) (. pt_b :value pt_left :mode)
		(. pt_b :value pt_left :snap) (get :keep_shape pt_board))))
(defq pt_it (. pt_board :add (cwb-shape (cwb-d-rect 10 10 50 50) :fill 0xff000000 :stroke 0xff000000 :width 3.0)))
(. pt_board :pick_up pt_it +board_teacher)
(. pt_board :pick pt_right :pen 1) (. pt_board :pick pt_left :width 12.0) (. pt_board :pick pt_left :alpha 0x40)
(defq pt_shape (first (cwb-find (. pt_board :get_doc) pt_it)))
(assert-eq "held: the right's pen is its fill, the width its, a quarter see through its outline, and the slots are as they were"
	(str (list +argb_green 12 0x40000000 9 :nil (first pt_pens)))
	(str (list (cwb-get pt_shape :fill) (n2i (cwb-get pt_shape :width)) (cwb-get pt_shape :stroke)
		(n2i (. pt_b :value pt_left :width)) (. pt_b :value pt_right :pen) (first pt_pens))))
(. pt_board :select (list))
(. pt_board :pick pt_left :ink 0xff0000ff)
(assert-eq "put down, an ink fills the pen the left holds, pen 1" 0xff0000ff (second pt_pens))
;the things pick themselves up, (Things), in front of the canvas, which
;knows nothing of them: a tap of a pen on a thing picks it up, a stroke of
;one that begins on it is the canvas's and draws, a pen on what its user
;holds moves it, and an eraser bites through
(defq px_board (Board (cwb-doc 400 400)) px_b (. px_board :get_bindings))
(. px_board :tick 1000000)
(defq px_it (. px_board :add (cwb-shape (cwb-d-rect 100 100 200 200) :fill 0xff000000 :stroke 0)))
(.-> px_b (:set :mouse 0 +pev_left :mode :line))
(defq px_actor (. (. px_board :get_stage) :hit 150 150 (ptr-event 0 :mouse +pev_left 150 150)))
(assert-true "a pen down on a thing is the things'" (eql px_actor (get :things px_board)))
(assert-true "an eraser down on it is the canvas's"
	(eql (get :surface px_board) (. (. px_board :get_stage) :hit 150 150 (ptr-event 0x4000000 :eraser 1 150 150))))
(pl-drag px_board 0 :mouse +pev_left 150 150 300 300)
(assert-eq "a line begun on it is drawn, and it is not picked up" "(2 ())"
	(str (list (length (cwb-items (. px_board :get_doc))) (get :selected px_board))))
(pl-tap px_board 0 :mouse +pev_left 120 120)
(assert-eq "a tap of the pen on it picks it up" (str (list px_it)) (str (get :selected px_board)))
(bind '(px_x0 & &ignore) (cwb-bounds (list (first (cwb-find (. px_board :get_doc) px_it)))))
(pl-drag px_board 0 :mouse +pev_left 180 120 200 120)
(bind '(px_x1 & &ignore) (cwb-bounds (list (first (cwb-find (. px_board :get_doc) px_it)))))
(assert-eq "held, a drag of the pen on it moves it, and draws nothing" "(20 2)"
	(str (list (n2i (- px_x1 px_x0)) (length (cwb-items (. px_board :get_doc))))))
;no actor holds its board by a count: the board holds its stage and the
;stage its actors, and an actor holds the board by a weak ref, claimed when
;it is used, so letting go of the board lets go of all of it
(defq pr_board (Board (cwb-doc 100 100)))
(. (. pr_board :get_stage) :add (Ruler pr_board 50 50))
(defq pr_actors (. (. pr_board :get_stage) :get_actors))
(assert-eq "each of its four actors has the board by a weak ref"
	"(4 4)" (str (list (length pr_actors) (length (filter (# (eql (get :board %0) (weak-ref pr_board))) pr_actors)))))
;what is done to a selection is done to what one user holds, the others'
;stay as they were, Chris: "2 users can select seperate item"
(defq pg_board (Board (cwb-doc 400 400)))
(defq pg_ids (map (# (. pg_board :add (cwb-shape (cwb-d-rect (* %0 50) 10 (+ (* %0 50) 40) 50) :fill 0xff000000 :stroke 0))) '(0 1 2 3)))
(bind '(pg_a pg_b pg_c pg_d) pg_ids)
(. pg_board :pick_up pg_a 0) (. pg_board :pick_up pg_b 0) (. pg_board :pick_up pg_c 3) (. pg_board :pick_up pg_d 3)
(defq pg_g (. pg_board :group (. pg_board :selected_by 0)))
(assert-eq "the teacher's two are one group, the student's two are as they were, and all still picked up"
	(str (list 3 (list pg_c pg_d pg_g) 0))
	(str (list (length (cwb-items (. pg_board :get_doc))) (get :selected pg_board) (. pg_board :holder pg_g))))
(. pg_board :duplicate :nil :nil (. pg_board :selected_by 3))
(assert-eq "the student's copies are picked up in place of theirs, the teacher's group stays"
	"(5 3 :t)" (str (list (length (cwb-items (. pg_board :get_doc))) (length (get :selected pg_board))
		(if (find pg_g (get :selected pg_board)) :t))))
(. pg_board :delete (. pg_board :selected_by 0))
(assert-eq "the teacher's delete takes the group only" 4 (length (cwb-items (. pg_board :get_doc))))
(. pg_board :delete (list))
(assert-eq "one who holds nothing deletes nothing" 4 (length (cwb-items (. pg_board :get_doc))))
