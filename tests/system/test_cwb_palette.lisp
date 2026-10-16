(report-header "Whiteboard palette: rings of wedges that open where a hand taps on nothing, for the pointer that opened it")

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
	;a tap on a wedge, where the palette says it is
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
(assert-eq "37 wedges: 11 tools, 12 colours and 4 widths, 10 things to do" 37 (length (. pl_pal :get_wedges)))

;time
(assert-eq "at the moment it opens no ring is open at all" 0 (n2i (* 100.0 (. pl_pal :ring_scale 0))))
(assert-eq "and it is moving" :t (. pl_board :tick 60000))
(assert-true "a little after, the inner ring is part open and the outer less so"
	(and (> (. pl_pal :ring_scale 0) 0.2) (< (. pl_pal :ring_scale 2) (. pl_pal :ring_scale 0))))
(assert-eq "what moves is to be drawn again" :t (. pl_board :dirty? +board_dirty_overlay))
(assert-eq "after a second it is still" :nil (. pl_board :tick 1000000))
(assert-list-eq "and all three rings are open" '(100 100 100)
	(map (# (n2i (* 100.0 (. pl_pal :ring_scale %0)))) '(0 1 2)))

;drawing
(defq pl_canvas (Canvas 800 600 1))
(. pl_canvas :set_canvas_flags +canvas_flag_antialias)
(. pl_canvas :fill 0xffffffff)
(. pl_board :draw_actors pl_canvas)
(defq pl_open (pl-ink pl_canvas 0xffffffff))
(assert-true "open, it is drawn, most of a disc 150 across each way" (and (> pl_open 55000) (< pl_open 72000)))

;where a wedge is, and what is there
(assert-list-eq "the middle is the middle" '(400 300) (map (const n2i) (. pl_pal :where :hub)))
(defq pl_at (. pl_pal :where :tool :rect))
(assert-eq "a point of a wedge is on that wedge" :rect
	(elem-get (apply (# (. pl_pal :wedge_at %0 %1)) (apply (# (. pl_pal :to_own %0 %1)) pl_at)) +wedge_val))
(assert-eq "every wedge is where it says it is" 37
	(length (filter (lambda (wedge)
		(eql wedge (apply (# (. pl_pal :wedge_at %0 %1)) (apply (# (. pl_pal :to_own %0 %1))
			(. pl_pal :where (elem-get wedge +wedge_what) (elem-get wedge +wedge_val))))))
		(. pl_pal :get_wedges))))
(assert-eq "a point on it is on it" :t (. pl_pal :hit 400 440))
(assert-eq "and one off it is not" :nil (. pl_pal :hit 400 460))

;a pointer over it, not down
(. pl_board :dirty? +board_dirty_overlay)
(. pl_board :pointers (list (apply (const ptr-event) (cat (list 0 :mouse 0) pl_at))))
(assert-eq "the wedge the mouse is over is the one drawn so" :rect (elem-get (get :hover pl_pal) +wedge_val))
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
(pl-pick pl_board pl_pal 0 :mouse +pev_left :tool :rect)
(assert-eq "a tap on a tool sets what the board draws" :rect (get :mode pl_board))
(assert-eq "and it shuts" 0 (length (palettes pl_board)))
(assert-eq "shut, a point on it is not on it" :nil (. pl_pal :hit 400 300))
(assert-eq "it is on the stage while it shuts" :t (. pl_board :tick 1050000))
(assert-true "smaller" (< (. pl_pal :ring_scale 0) 0.7))
(. pl_board :dirty? +board_dirty_overlay)
(. pl_board :tick 1200000)
(assert-eq "and then is off it" :nil (find pl_pal (. (. pl_board :get_stage) :get_actors)))
(assert-eq "and what is over the board is to be drawn once more, without it: the last of it, small, is not left there"
	:t (. pl_board :dirty? +board_dirty_overlay))
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
;down on its middle and dragged it goes with the pointer, and put down nothing is picked: it is still there
(. pl_board :pointers (list (ptr-event 0 :mouse +pev_left 100 500)))
(. pl_board :pointers (list (ptr-event 0 :mouse +pev_left 102 501)))
(assert-list-eq "a hair of a move on its middle is not a drag" '(100 500) (map (const n2i) (get :origin pl_pal)))
(. pl_board :pointers (list (ptr-event 0 :mouse +pev_left 260 380)))
(. pl_board :pointers (list (ptr-event 0 :mouse 0 260 380)))
(assert-list-eq "dragged by its middle it is where it was put, and is still open" '(260 380 1)
	(cat (map (const n2i) (get :origin pl_pal)) (list (length (palettes pl_board)))))
(pl-pick pl_board pl_pal 0 :mouse +pev_left :hub)
(assert-eq "a tap on its middle puts it away" 0 (length (palettes pl_board)))
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
(def pl_board :zoom 2.0)
(defq pl_pal (palette-open pl_board 400 300))
(assert-list-eq "at twice the size a wedge is half as far from the middle in the document" '(400 327)
	(map (const n2i) (. pl_pal :where :tool :rect)))
(assert-list-eq "and the palette is on half as much of it" '(:t :nil) (list (. pl_pal :hit 400 374) (. pl_pal :hit 400 377)))
(. pl_pal :pick :hub)
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
(assert-list-eq "dragged, it is moved, and no palette opens" '(0 1) (list (length (palettes pl_board)) (length (. pl_board :get_selected))))

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
(assert-list-eq "the other, where it is alone, is selected by the right button" (list (list pr_low) 0)
	(list (. pr_board :get_selected) (length (palettes pr_board))))
(pl-tap pr_board 0 :mouse +pev_right 60 60)
(assert-list-eq "tapped again now it is selected: it is selected still, behind still, and no palette opens"
	(list (list pr_low) :t 0)
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
;what is selected has its handles while a device is in select mode, the
;board's own mode a pen
(defq ph_board (Board (cwb-doc 400 400)))
(. ph_board :tick 1000000)
(pl-drag ph_board 0 :mouse +pev_left 10 10 100 100)
(. ph_board :select (map (# (elem-get %0 +cwb_id)) (cwb-items (. ph_board :get_doc))))
(assert-eq "nothing in select mode, no handles" :nil (. (get :handles ph_board) :box))
(. (. ph_board :get_bindings) :set :mouse 0 +pev_left :mode :select)
(assert-true "the mouse in select mode, the handles" (. (get :handles ph_board) :box))
;a pen's colour is the outline, a brush's the fill, Chris: "brushes set the
;fill", and a shape drawn filled has no outline, its fill takes a pen's
(defq pp_board (Board (cwb-doc 400 400)))
(. pp_board :tick 1000000)
(. (. pp_board :get_bindings) :set :mouse 0 +pev_left :mode :rect)
(pl-drag pp_board 0 :mouse +pev_left 10 10 100 100)
(. (. pp_board :get_bindings) :set :mouse 0 +pev_left :mode :frect)
(pl-drag pp_board 0 :mouse +pev_left 150 150 250 250)
(. pp_board :select (map (# (elem-get %0 +cwb_id)) (cwb-items (. pp_board :get_doc))))
(. pp_board :paint :t :stroke 0xffff0000)
(. pp_board :paint (list (elem-get (first (cwb-items (. pp_board :get_doc))) +cwb_id)) :fill 0x600000ff)
(assert-eq "a pen outlines the rect, a brush fills it, and fills the filled rect"
	(str (list 0xffff0000 0x600000ff 0 0xffff0000))
	(str (map (# (cwb-get %0 %1)) (cat (list (first (cwb-items (. pp_board :get_doc)))) (list (first (cwb-items (. pp_board :get_doc))))
			(list (second (cwb-items (. pp_board :get_doc)))) (list (second (cwb-items (. pp_board :get_doc)))))
		'(:stroke :fill :stroke :fill))))
;the middle of the mouse binds both its buttons, its own stays pan
(defq pm_board (Board (cwb-doc 400 400)))
(. (. pm_board :get_bindings) :set :mouse 0 +pev_middle :mode :rect)
(assert-eq "a middle press binds the left and the right, and makes no rule of the middle"
	"(:rect :rect :pan)" (str (list (. (. pm_board :get_bindings) :value (ptr-event 0 :mouse +pev_left 0 0) :mode)
		(. (. pm_board :get_bindings) :value (ptr-event 0 :mouse +pev_right 0 0) :mode)
		(. (. pm_board :get_bindings) :value (ptr-event 0 :mouse +pev_middle 0 0) :tool))))
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
