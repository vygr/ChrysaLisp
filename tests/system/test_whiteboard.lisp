(report-header "Whiteboard app: its window, its view and what its toolbars do, with no desktop")

(import "usr/env.inc")
(import "gui/lisp.inc")

;No test starts an app on a desktop. This loads the app as far as its
;window, in a task of its own, and then does what the GUI would: events of
;the mouse to its view, as the GUI makes them, and the actions its
;toolbars are connected to, from its event map. Each line is what it then
;says. The canvases are drawn on as the app draws them, less the last
;step, to a texture, which is the desktop's.
(defq wa_head (cat
	"(catch (import {apps/media/whiteboard/app.lisp}) :t)"
	" (defq select (list (mail-mbox) (mail-mbox) (mail-mbox) (mail-mbox)))"
	;with nothing round the document, a point of the view is a point of it
	" (setq *margin* 0) (board-resized)"
	" (defun wa-draw () (catch (progn (defq m (view-matrix))"
	" (. *board* :dirty? +board_dirty_doc) (. *board* :dirty? +board_dirty_append) (. *board* :take_appended)"
	" (draw-paper) (defq n (. *board* :draw *committed* m)) (. *board* :dirty? +board_dirty_overlay)"
	" (. *overlay* :fill 0) (. *board* :draw_overlay *overlay* m)"
	" (each (# (if (Instrument? %0) (. %0 :draw *overlay* m))) (. (. *board* :get_stage) :get_actors)) n) :threw))"
	;the app's task lets the others of its node run as it goes. What it does here is a
	;second or two with nothing waited for, on a small machine, and a test beside it
	;that times a thing was then timed wrong
	" (defun wa-mouse (kind rx ry buttons) (task-slice)"
	" (defq e (setf-> (str-alloc +ev_msg_mouse_size) (+ev_msg_type +ev_type_mouse)"
	" (+ev_msg_mouse_rx rx) (+ev_msg_mouse_ry ry) (+ev_msg_mouse_x rx) (+ev_msg_mouse_y ry) (+ev_msg_mouse_buttons buttons)))"
	" (case kind (:down (. *board_view* :mouse_down e)) (:move (. *board_view* :mouse_move e)) (:up (. *board_view* :mouse_up e))))"
	" (defun wa-drag (button x y x1 y1) (wa-mouse :down x y button) (wa-mouse :move x1 y1 (case button (3 4) (:t button))) (wa-mouse :up x1 y1 0))"
	" (defun wa-do (id) (task-slice) (catch (progn ((. *event_map* :find id)) :ok) :threw))"
	" (defun wa-mode (mode) (. *mode_toolbar* :set_selected (find mode *modes*)) (wa-do +event_mode))"
	" (defun wa-ids () (map (# (elem-get %0 +cwb_id)) (cwb-items (. *board* :get_doc))))"))

(defun wa-run (body)
	;what the app says when that is done to it, each thing printed on a line of its own
	;its canvas has pixels the nodes can reach, under a name that lasts
	;till the canvas is let go of. The end of this task does not do that,
	;its window still holds it, so it is taken out and let go here
	(split (test-output (cat wa_head " " body
		" (when *committed* (. *committed* :sub) (setq *committed* :nil))")) (ascii-char 10)))

;what the board is, kept and put back: lines, an instrument turned and set
;to draw slices, the toolbars, the zoom. As the app does it when it is
;closed and opened, through the text of a file
(defq wa_cfg (wa-run (cat
	"(wa-drag 1 100 100 220 120) (wa-mode :rect) (wa-drag 1 300 100 420 200)"
	"(. *ink_toolbar* :set_selected 2) (wa-do +event_ink) (. *radius_toolbar* :set_selected 2) (wa-do +event_radius)"
	"(wa-do +event_snap) (wa-do +event_keep_shape) (wa-do +event_zoom_in) (. *text_field* :set_text {Hello})"
	"(wa-do +event_full_protractor) (defq tool (last (. (. *board* :get_stage) :get_actors)))"
	"(def tool :angle 0.5 :mode :pie :origin (list 300.0 200.0)) (. tool :set_extent 260.0)"
	"(defq was (list (wa-ids) (get :mode *board*) (get :color *board*) (get :width *board*) (get :snap *board*) (get :keep_shape *board*) *zoom* (canvas-size)))"
	"(defq ss (string-stream (cat {}))) (tree-save ss (config-state)) (defq text (str ss))"
	;another board altogether
	"(. *board* :set_doc (cwb-doc 200 100)) (. (. *board* :get_stage) :sub tool) (wa-mode :pen)"
	"(. *ink_toolbar* :set_selected 0) (wa-do +event_ink) (wa-do +event_snap) (wa-do +event_keep_shape) (wa-do +event_zoom_out) (. *text_field* :set_text {})"
	"(defq place (config-apply (tree-load (string-stream text))))"
	"(defq now (list (wa-ids) (get :mode *board*) (get :color *board*) (get :width *board*) (get :snap *board*) (get :keep_shape *board*) *zoom* (canvas-size)))"
	"(print (list (str was) (eql (str was) (str now))))"
	"(print (list (. *mode_toolbar* :get_selected) (. *ink_toolbar* :get_selected) (. *radius_toolbar* :get_selected) (. *text_field* :get_text) (get :text *board*)))"
	"(defq tool (last (. (. *board* :get_stage) :get_actors)))"
	"(print (list (if (Circle? tool) :t) (get :mode tool) (map (const n2i) (get :origin tool)) (n2i (. tool :extent)) (< (abs (- (get :angle tool) 0.5)) 0.001)))"
	"(print (list (length place) (slice place 2 4) (. *window* :pref_size)))"
	"(print (list (config-apply (Emap)) (config-apply (scatter (Emap) :version 99)) (config-apply :nil) (wa-ids)))")))
(assert-true (cat "a board kept and put back is the board it was: " (first wa_cfg)) (ends-with " :t)" (first wa_cfg)))
(assert-eq "its toolbars and its words as they were" "(5 2 2 \qHello\q \qHello\q)" (second wa_cfg))
(assert-eq "its instrument where it was, turned as it was, as big, drawing what it drew" "(:t :pie (300 200) 260 :t)" (third wa_cfg))
(assert-true (cat "and where its window was is given back, with where its view was: " (elem-get wa_cfg 3)) (starts-with "(6 " (elem-get wa_cfg 3)))
(assert-eq "what is not a board kept, or one of another version, changes nothing" "(:nil :nil :nil (1 2))" (elem-get wa_cfg 4))

(defq wa_out (wa-run (cat
	"(print (list (View? *window*) (Board? *board*) (Board-view? *board_view*)))"
	"(print (. *window* :pref_size))"
	"(print (list (canvas-size) (wa-draw)))"
	;the pen, the left button
	"(wa-drag 1 100 100 220 120) (print (wa-ids))"
	;a box, by the mode bar
	"(print (wa-mode :rect)) (wa-drag 1 300 100 420 200) (print (cwb-get (last (cwb-items (. *board* :get_doc))) :d))"
	;red, by the ink bar, and a filled ellipse
	"(. *ink_toolbar* :set_selected 2) (print (list (wa-do +event_ink) (= (get :color *board*) +argb_red)))"
	"(wa-mode :fellipse) (wa-drag 1 500 300 620 380) (print (list (wa-ids) (cwb-get (last (cwb-items (. *board* :get_doc))) :fill)))"
	;words
	"(. *text_field* :set_text {Hello}) (def *board* :text (. *text_field* :get_text))"
	"(wa-mode :text) (wa-mouse :down 120 300 1) (wa-mouse :up 120 300 0)"
	"(print (list (wa-ids) (cwb-get (last (cwb-items (. *board* :get_doc))) :text)))"
	;the eraser
	"(def *board* :rub_mode :whole) (wa-mode :eraser) (wa-drag 1 100 100 105 101) (print (wa-ids))"
	"(print (list (wa-do +event_undo) (wa-ids))) (def *board* :rub_mode :part)"
	;the right button moves what it is on, whatever the mode
	"(wa-mode :pen) (wa-drag 3 300 150 340 190) (print (list (wa-ids) (. *board* :get_selected) (map (const n2i) (cwb-bounds (. *board* :selected_items)))))"
	;the middle button draws nothing
	"(wa-drag 2 50 50 90 90) (print (wa-ids))"
	"(print (list (wa-draw)))"
	;laid out, the view over the board is not one that covers what is under it
	"(bind (quote (w h)) (. *window* :pref_size)) (. *window* :change 0 0 w h)"
	"(print (list (bits? (getf *board_view* +view_flags 0) +view_flag_opaque) (bits? (getf *backdrop* +view_flags 0) +view_flag_opaque)"
	" (. *board_view* :get_size) (. *committed* :get_size)))"
	;what is over the document is in front of it, as the view is in front of the paper
	"(defq kids (. *backdrop* :children) top (. *board_stack* :children))"
	"(print (list (< (find *overlay* kids) (find *committed* kids)) (< (find *board_view* top) (find *backdrop* top))))"
	;the left button draws, the right goes down too and the mouse moves with both: that is the middle
	"(wa-mode :pen) (wa-mouse :down 600 500 1) (wa-mouse :move 640 520 1) (wa-mouse :move 640 520 3) (wa-mouse :move 660 540 5)"
	"(print (list (get :held *board_view*) (wa-ids)))"
	"(wa-mouse :move 700 600 4) (wa-mouse :up 700 600 0) (print (list (get :held *board_view*) (wa-ids)))")))
(assert-eq "the app's file loads, and it has a window, a board and a view of it" "(4 4 5)" (elem-get wa_out 0))
(assert-true "the window has a size it wants, no wider than a small screen"
	(progn (defq wa_size (first (read (string-stream (elem-get wa_out 1))))) (and (> (first wa_size) 400) (< (first wa_size) 1000))))
(assert-eq "the board is 1024 by 768 and draws, with nothing on it" "((1024 768) 0)" (elem-get wa_out 2))
(assert-eq "the left button draws with the pen" "(1)" (elem-get wa_out 3))
(assert-eq "set to boxes by the mode bar" ":ok" (elem-get wa_out 4))
(assert-eq "it draws a box" "M 300 100 L 420 100 420 200 300 200 Z" (elem-get wa_out 5))
(assert-eq "the ink bar sets the colour" "(:ok :t)" (elem-get wa_out 6))
(assert-eq "a filled ellipse is filled with it" (str (list '(1 2 3) +argb_red)) (elem-get wa_out 7))
(assert-eq "the words in the text field are put down where the pen goes down" "((1 2 3 4) \qHello\q)" (elem-get wa_out 8))
(assert-eq "the eraser, set to take lines whole, takes out the line it is dragged over" "(2 3 4)" (elem-get wa_out 9))
(assert-eq "and undo puts it back" "(:ok (1 2 3 4))" (elem-get wa_out 10))
(assert-eq "the right button moves what it is on, whatever the mode, by how far it is dragged, and puts it at the back"
	"((2 1 3 4) (2) (338 138 461 241))" (elem-get wa_out 11))
(assert-eq "the middle button is the view's, to move it about, and draws nothing" "(2 1 3 4)" (elem-get wa_out 12))
(assert-eq "and it draws, the three that are the document's, the one that is selected is in flight" "(3)" (elem-get wa_out 13))
(assert-eq "laid out in its window, the view over the board lets what is under it show, the paper does not, and both are the size of the board"
	"(:nil :t (1024 768) (1024 768))" (elem-get wa_out 14))
(assert-eq "what is drawn over the document is in front of it, the way round that the view is in front of the paper" "(:t :t)" (elem-get wa_out 15))
(assert-eq "the left and the right held together are the middle, and the line the left had begun is not kept" "(2 (2 1 3 4))" (elem-get wa_out 16))
(assert-eq "one let go, it is the middle still, and with both up nothing was drawn" "(0 (2 1 3 4))" (elem-get wa_out 17))

(defq wa_out (wa-run (cat
	"(wa-drag 1 100 100 220 120) (wa-mode :rect) (wa-drag 1 300 100 420 200) (wa-drag 1 500 100 600 180)"
	;instruments, each a little off the last
	"(print (list (wa-do +event_ruler) (wa-do +event_protractor) (wa-do +event_set_square)"
	" (length (filter (const Instrument?) (. (. *board* :get_stage) :get_actors)))))"
	"(print (map (# (map (const n2i) (get :origin %0))) (filter (const Instrument?) (. (. *board* :get_stage) :get_actors))))"
	;what a protractor draws is set on it, by a tap on its part that says
	"(defq pro (some (# (if (Protractor? %0) %0)) (. (. *board* :get_stage) :get_actors))) (. (. *board* :get_stage) :add pro)"
	"(bind (quote (mx my)) (map (const n2i) (. pro :to_board 0 -21))) (wa-drag 1 mx my mx my) (print (list :ok (get :mode pro)))"
	;snapping
	"(print (list (wa-do +event_snap) (get :snap *board*) (wa-do +event_snap) (get :snap *board*)))"
	"(print (list (wa-do +event_snap_angle) (> (get :snap_angle *board*) 0.2)))"
	;zoom, in and out and back, and the canvases are the size of it
	"(print (list (wa-do +event_zoom_in) *zoom* (canvas-size) (. *committed* :pref_size) (get :zoom *board*)))"
	"(wa-mouse :down 150 150 1) (wa-mouse :up 150 150 0)"
	"(print (list (wa-do +event_zoom_out) (wa-do +event_zoom_out) *zoom* (canvas-size)))"
	"(print (list (wa-do +event_zoom_in) *zoom* (canvas-size) (wa-draw)))"
	;the size of the board, by the size field
	"(. *size_field* :set_text {800x500}) (print (list (wa-do +event_size) (canvas-size) (. (. *board* :get_doc) :find :width)))"
	"(. *size_field* :set_text {nonsense}) (print (list (wa-do +event_size) (canvas-size)))"
	;what is done to what is selected
	"(. *board* :select_all) (print (map (const wa-do) (list +event_align_left +event_group)))"
	"(print (list (length (wa-ids)) (wa-do +event_ungroup) (length (wa-ids)) (wa-do +event_duplicate) (length (wa-ids))"
	" (wa-do +event_to_back) (wa-do +event_to_front) (wa-do +event_delete) (length (wa-ids))))"
	;a file
	"(board-save {tests/scratch/test_whiteboard.xyz}) (print (list *file* (> (length (load {tests/scratch/test_whiteboard.cwb})) 100)))"
	"(print (list (wa-do +event_new) (wa-ids) (canvas-size)))"
	"(. *size_field* :set_text {300x200}) (wa-do +event_new) (prin (canvas-size))"
	;new is a step like another, pressed by mistake it is undone
	"(wa-do +event_undo) (prin (list (canvas-size) (length (wa-ids)))) (wa-do +event_undo) (print (list (canvas-size) (length (wa-ids))))"
	"(board-load {tests/scratch/test_whiteboard.cwb}) (print (list (length (wa-ids)) (canvas-size) (. *size_field* :get_text) (wa-draw)))"
	"(pii-remove {tests/scratch/test_whiteboard.cwb})")))
(assert-eq "the ruler, the protractor and the set square are put on the board" "(:ok :ok :ok 3)" (elem-get wa_out 0))
(assert-eq "in the middle of it, each a little off the last" "((512 384) (542 414) (572 444))" (elem-get wa_out 1))
(assert-eq "a tap on the protractor's own part for it says what it draws" "(:ok :pie)" (elem-get wa_out 2))
(assert-eq "snap is on, to the grid of the document, and off" "(:ok 32.00000 :ok 0.00000)" (elem-get wa_out 3))
(assert-eq "angles snap" "(:ok :t)" (elem-get wa_out 4))
(assert-eq "zoomed in, the board is that much bigger on the screen, and the canvases are" "(:ok 1.50000 (1536 1152) (1536 1152) 1.50000)" (elem-get wa_out 5))
(assert-eq "out twice is three quarters" "(:ok :ok 0.75000 (768 576))" (elem-get wa_out 6))
(assert-eq "and in again is where it was, to the pixel" "(:ok 1.00000 (1024 768) 3)" (elem-get wa_out 7))
(assert-eq "the size field sets the size of the board" "(:ok (800 500) 800)" (elem-get wa_out 8))
(assert-eq "and what is not a size leaves it" "(:ok (800 500))" (elem-get wa_out 9))
(assert-eq "what is selected is lined up and grouped" "(:ok :ok)" (elem-get wa_out 10))
(assert-eq "ungrouped, copied, sent back, brought forward and deleted" "(1 :ok 3 :ok 6 :ok :ok :ok 3)" (elem-get wa_out 11))
(assert-eq "saved, it is a .cwb whatever it was called" "(\qtests/scratch/test_whiteboard.cwb\q :t)" (elem-get wa_out 12))
(assert-eq "saved, the paper is the size of what is on it, and new is an empty board of the size the field then says" "(:ok () (156 136))" (elem-get wa_out 13))
(assert-eq "of another size if the field says so. Undone, the board is the size it was, and again, what was on it is back"
	"(300 200)((156 136) 0)((156 136) 3)" (elem-get wa_out 14))
(assert-eq "loaded, the board is the file's: its items, the size of what is on it, and the field says so" "(3 (156 136) \q156x136\q 3)" (elem-get wa_out 15))

;pens and fingers, as the GUI tells of them, a pointer event to the window for the view
(defq wa_out (wa-run (cat
	;the kinds of pointer, as the host has them: 1 a pen, 2 its eraser, 3 a finger
	"(defun wa-ptr (id kind buttons rx ry) (. *window* :event (setf-> (str-alloc +ev_msg_pointer_size)"
	" (+ev_msg_type +ev_type_pointer) (+ev_msg_target_id (. *board_view* :get_id))"
	" (+ev_msg_pointer_id id) (+ev_msg_pointer_kind kind) (+ev_msg_pointer_buttons buttons) (+ev_msg_pointer_pressure 65535)"
	" (+ev_msg_pointer_x rx) (+ev_msg_pointer_y ry) (+ev_msg_pointer_rx rx) (+ev_msg_pointer_ry ry))))"
	;a pen draws a box, the board is set to boxes
	"(wa-mode :rect) (wa-ptr 0x20001 1 1 100 100) (wa-ptr 0x20001 1 1 200 180) (wa-ptr 0x20001 1 0 200 180)"
	"(print (list (wa-ids) (cwb-get (first (cwb-items (. *board* :get_doc))) :d)))"
	;two fingers on it make it twice the size
	"(wa-ptr 0x10001 3 1 110 110) (wa-ptr 0x10002 3 1 190 170)"
	"(wa-ptr 0x10001 3 1 70 80) (wa-ptr 0x10002 3 1 230 200)"
	"(wa-ptr 0x10001 3 0 70 80) (wa-ptr 0x10002 3 0 230 200)"
	"(print (map (const n2i) (cwb-bounds (. *board* :selected_items))))"
	;the other end of the pen rubs it out
	"(wa-ptr 0x20001 2 1 48 60) (wa-ptr 0x20001 2 1 49 62) (wa-ptr 0x20001 2 0 49 62)"
	"(print (wa-ids))"
	;zoomed, a point of the view is a point of the document over the zoom
	"(wa-do +event_zoom_in) (wa-ptr 0x20001 1 1 150 150) (wa-ptr 0x20001 1 1 300 300) (wa-ptr 0x20001 1 0 300 300)"
	"(print (cwb-get (last (cwb-items (. *board* :get_doc))) :d))")))
(assert-eq "a pen, told of by the GUI, draws" "((1) \qM 100 100 L 200 100 200 180 100 180 Z\q)" (elem-get wa_out 0))
(assert-eq "two fingers on what it drew make it twice the size" "(47 57 252 222)" (elem-get wa_out 1))
(assert-eq "and the other end of the pen rubs it out" "()" (elem-get wa_out 2))
(assert-eq "zoomed to one and a half, a pen draws where it is in the document" "M 100 100 L 200 100 200 200 100 200 Z" (elem-get wa_out 3))


;a thing that is selected is let go of when another tool is picked, so a colour picked next is not given to it
(defq wa_out (wa-run (cat
	"(wa-mode :frect) (wa-drag 1 100 100 200 180) (defq it (last (cwb-items (. *board* :get_doc))))"
	"(wa-mode :select) (wa-mouse :down 150 140 1) (wa-mouse :up 150 140 0) (print (. *board* :get_selected))"
	"(. *ink_toolbar* :set_selected 2) (wa-do +event_ink) (print (= (cwb-get it :stroke) +argb_red))"
	"(wa-mode :pen) (print (. *board* :get_selected))"
	"(. *ink_toolbar* :set_selected 3) (wa-do +event_ink) (print (= (cwb-get it :stroke) +argb_red))"
	"(wa-mode :select) (wa-mouse :down 150 140 1) (wa-mouse :up 150 140 0) (def *board* :mode :pen)"
	"(wa-drag 1 400 400 450 420) (print (. *board* :get_selected))")))
(assert-eq "a thing is selected" "(1)" (elem-get wa_out 0))
(assert-eq "a colour picked while it is gives it that colour" ":t" (elem-get wa_out 1))
(assert-eq "another tool picked, it is let go of" "()" (elem-get wa_out 2))
(assert-eq "and a colour picked then is for what is drawn next, it keeps its own" ":t" (elem-get wa_out 3))
(assert-eq "what is selected when a line is begun, however it came to be, is let go of" "()" (elem-get wa_out 4))

;a corner that is dragged keeps the shape of the thing, or with the lock off is free
(defq wa_out (wa-run (cat
	"(wa-mode :frect) (wa-drag 1 100 100 200 150) (wa-mode :select) (wa-mouse :down 150 125 1) (wa-mouse :up 150 125 0)"
	"(defun corner () (slice (elem-get (. (get :handles *board*) :spots) 7) 1 3))"
	"(defun size () (bind (quote (x y x1 y1)) (cwb-bounds (. *board* :selected_items))) (list (n2i (- x1 x)) (n2i (- y1 y))))"
	"(print (list (get :keep_shape *board*) (size)))"
	"(bind (quote (cx cy)) (map (const n2i) (corner))) (wa-drag 1 cx cy (+ cx 100) (+ cy 10)) (print (size))"
	"(print (list (wa-do +event_keep_shape) (get :keep_shape *board*)))"
	"(bind (quote (cx cy)) (map (const n2i) (corner))) (wa-drag 1 cx cy (+ cx 50) (- cy 40)) (print (size))"
	"(print (list (wa-do +event_keep_shape) (get :keep_shape *board*)))")))
(assert-eq "a board starts with the shape kept, a box of 100 by 50" "(:t (100 50))" (elem-get wa_out 0))
(assert-eq "its corner dragged 100 across and 10 down, it is twice as wide and twice as tall, the shape it was" "(200 100)" (elem-get wa_out 1))
(assert-eq "the lock on the toolbar puts that off" "(:ok :nil)" (elem-get wa_out 2))
(assert-eq "and the corner goes where it is put: 50 wider and 40 less tall" "(250 59)" (elem-get wa_out 3))
(assert-eq "and on again" "(:ok :t)" (elem-get wa_out 4))

;round the document there is a margin that is not it, where an instrument that
;lies half off the paper is seen. What is over the document covers that too
(defq wa_out (wa-run (cat
	"(setq *margin* 240) (board-resized) (bind (quote (w h)) (. *window* :pref_size)) (. *window* :change 0 0 w h)"
	"(print (list (. *board_stack* :get_size) (. *committed* :get_bounds) (. *overlay* :get_bounds)))"
	"(wa-mode :rect) (wa-drag 1 340 340 440 400) (print (cwb-get (last (cwb-items (. *board* :get_doc))) :d))"
	"(print (map (const n2i) (cwb-mat-point (over-matrix) 0 0)))"
	"(. (. *board* :get_stage) :add (defq ruler (Ruler *board* -60 100)))"
	;a line being drawn, from off the document's left to on it, with the ruler there
	"(wa-mode :line) (wa-mouse :down 60 700 1) (wa-mouse :move 500 700 1)"
	"(draw-over)"
	"(defq s (memory-stream)) (pixmap-write (getf *overlay* +canvas_pixmap 0) s 32) (stream-seek s 0 0) (defq d (read-blk s 100000000))"
	"(defun px (x y) (get-uint d (+ (- (length d) (* 1504 1248 4)) (* 4 (+ (* y 1504) x)))))"
	"(print (list (/= 0 (px 100 340)) (px 100 100)))"
	;it is in flight, on the canvas of what is, which is the size of the document and no more
	"(draw-flight) (defq s (memory-stream)) (pixmap-write (getf *flight* +canvas_pixmap 0) s 32) (stream-seek s 0 0) (defq f (read-blk s 100000000))"
	"(print (list (px 150 700) (px 400 700) (. *flight_clip* :get_bounds) (/= 0 (get-uint f (+ (- (length f) (* 1024 768 4)) (* 4 (+ (* 460 1024) 100)))))))"
	"(wa-mouse :up 500 700 0)"
	;from the back: the paper, the document, what is in flight, what is over it all. Taken by the right button, what is in flight is behind the document
	"(defun order () (map (# (cond ((eql %0 *paper*) :paper) ((eql %0 *committed*) :doc) ((eql %0 *flight_clip*) :flight) ((eql %0 *overlay*) :over))) (reverse (. *backdrop* :children))))"
	"(print (order))"
	"(wa-mouse :down 440 370 3) (wa-mouse :move 450 380 4) (draw-flight) (print (list (order) (get :float_back *board*) *flight_used*))"
	"(wa-mouse :up 450 380 0) (draw-flight) (print (list (order) *flight_used* (. *board* :get_selected)))"
	;still selected, it is sized by a handle with its flight still behind the document
	"(wa-mode :select) (bind (quote (kind hx hy)) (slice (elem-get (. (get :handles *board*) :spots) 4) 0 3))"
	"(wa-mouse :down (+ 240 (n2i hx)) (+ 240 (n2i hy)) 1) (wa-mouse :move (+ 270 (n2i hx)) (+ 240 (n2i hy)) 1) (draw-flight)"
	"(print (list (order) (get :float_back *board*))) (wa-mouse :up (+ 270 (n2i hx)) (+ 240 (n2i hy)) 0)"
	"(wa-mouse :down 700 700 1) (wa-mouse :up 700 700 0) (draw-flight) (print (list (. *board* :get_selected) *flight_used* (. *board* :dirty? +board_dirty_doc)))"
	"(. *board* :select (list 1)) (wa-mouse :down 400 400 1) (wa-mouse :move 410 410 1) (draw-flight) (print (order)) (wa-mouse :up 410 410 0)"
	;a thing that a hand only moves is not drawn again as it goes: its canvas is moved, in a view that stays where the paper is
	"(wa-mouse :down 410 410 1) (draw-flight) (wa-mouse :move 440 425 1) (defq sw (draw-flight))"
	"(prin (list sw (slice (. *flight* :get_bounds) 0 2) (. *flight_clip* :get_bounds)))"
	"(wa-mouse :move 450 400 1) (draw-flight) (prin (slice (. *flight* :get_bounds) 0 2))"
	"(defq was *flight*) (wa-mouse :up 450 400 0) (. *board* :touch +board_dirty_overlay)"
	"(print (list (draw-flight) (slice (. *flight* :get_bounds) 0 2) (eql was *flight_spare*) (length (. *flight_clip* :children)) (eql (first (. *flight_clip* :children)) *flight*)))"
	;a thing dragged part off the left of the paper and let go is drawn all on its canvas, moved over, the canvas as far the other way: taken up again and dragged back it is all there
	"(bind (quote (bx by bx1 by1)) (map (const n2i) (cwb-bounds (. *board* :selected_items))))"
	"(defq gx (+ 240 bx (/ (- bx1 bx) 2)) gy (+ 240 by (/ (- by1 by) 2)) off (+ bx 40))"
	;it is drawn again where it is, as it is when it is let go of or turned, and then moved on
	"(wa-mouse :down gx gy 1) (draw-flight) (wa-mouse :move (- gx off) gy 1) (setq *flight_at* :nil) (draw-flight)"
	"(bind (quote (fx fy)) (slice (. *flight* :get_bounds) 0 2))"
	"(prin (list (first (map (const n2i) (cwb-bounds (. *board* :selected_items)))) fx fy (slice *flight_at* 3 5)))"
	"(wa-mouse :move gx gy 1) (prin (list (draw-flight) (slice (. *flight* :get_bounds) 0 2)))"
	;and off the bottom right
	"(wa-mouse :move (+ 240 1024 -20) (+ 240 768 -10) 1) (setq *flight_at* :nil) (draw-flight) (bind (quote (fx fy)) (slice (. *flight* :get_bounds) 0 2))"
	"(print (list (> fx 0) (> fy 0) (- 0 fx (elem-get *flight_at* 3)) (- 0 fy (elem-get *flight_at* 4)))) (wa-mouse :move gx gy 1) (wa-mouse :up gx gy 0)")))
(assert-eq "the board with what is round it is 240 more each side, the document's canvas is in by that, and what is over it covers it all"
	"((1504 1248) (240 240 1024 768) (0 0 1504 1248))" (elem-get wa_out 0))
(assert-eq "a point of the view is a point of the document, less the margin" "M 100 100 L 200 100 200 160 100 160 Z" (elem-get wa_out 1))
(assert-eq "what is over the document is drawn in by the margin" "(240 240)" (elem-get wa_out 2))
(assert-eq "a ruler that lies off the left of the document is drawn there, in the margin, and not where it is not" "(:t 0)" (elem-get wa_out 3))
(assert-eq "a line that is being drawn is in flight: not on what is over the document, on a canvas the size of the document, where it is"
	"(0 0 (240 240 1024 768) :t)" (elem-get wa_out 4))
(assert-eq "from the back: the paper, the document, what is in flight, and what is over it all" "(:paper :doc :flight :over)" (elem-get wa_out 5))
(assert-eq "a thing taken by the right button is in flight behind the document" "((:paper :flight :doc :over) :t :t)" (elem-get wa_out 6))
(assert-eq "let go, it is selected still, and in flight still, behind the document" "((:paper :flight :doc :over) :t (1))" (elem-get wa_out 7))
(assert-eq "sized by a handle, it is still behind the document while it is" "((:paper :flight :doc :over) :t)" (elem-get wa_out 8))
(assert-eq "a click on nothing lets go of it: nothing is in flight, and the document is to be drawn again, once, with it in" "(() :nil :t)" (elem-get wa_out 9))
(assert-eq "taken by the left button it is in flight in front of the document" "(:paper :doc :flight :over)" (elem-get wa_out 10))
(assert-eq "moved by a hand, its canvas is not drawn again but moved by as much, in a view that stays on the paper, and moved on; let go, it is drawn where it is on the other canvas, which takes the place of the one that was moved"
	"(:nil (30 15) (240 240 1024 768))(40 -10)(:nil (0 0) :t 1 :t)" (elem-get wa_out 11))
(assert-eq "dragged 40 off the left of the paper and drawn there, it is drawn all on its canvas, moved over by 44, the canvas put as far to the left; dragged back, the canvas is moved and not drawn on. Off the bottom right, the canvas is put right and down"
	(cat "(-40 -44 0 (44 0))(:nil (" (str (- (+ 158 40) 44)) " 0))(:t :t 0 0)") (elem-get wa_out 12))

;the palette on the board: the right button down and up on nothing opens it,
;a tap on a wedge of it does what the toolbar would, and the toolbar shows it
(defq wa_out (wa-run (cat
	"(. *board* :tick 1000000)"
	"(wa-drag 3 500 400 500 400) (defq pal (first (palettes *board*)))"
	"(print (list (length (palettes *board*)) (map (const n2i) (get :origin pal))))"
	"(. *board* :tick 2000000) (print (list (wa-draw)))"
	"(defun wa-pick (what val) (bind '(x y) (map (const n2i) (. pal :where what val))) (wa-drag 1 x y x y))"
	"(wa-pick :color +argb_red) (wa-pick :width 12.0) (wa-pick :action :snap) (wa-pick :tool :ellipse)"
	"(print (list (get :mode *board*) (= (get :color *board*) +argb_red) (n2i (get :width *board*)) (n2i (get :snap *board*)) (length (palettes *board*))))"
	"(wa-draw) (sync-ui)"
	"(print (list (elem-get *modes* (. *mode_toolbar* :get_selected)) (. *ink_toolbar* :get_selected) (. *radius_toolbar* :get_selected) *snap*))"
	"(wa-drag 1 100 100 200 160) (print (cwb-get (last (cwb-items (. *board* :get_doc))) :kind))"
	"(. *board* :tick 3000000) (print (length (filter (const Palette?) (. (. *board* :get_stage) :get_actors))))")))
(assert-eq "the right button, down and up on nothing, opens a palette there" "(1 (500 400))" (elem-get wa_out 0))
(assert-eq "the board draws with it open" "(0)" (elem-get wa_out 1))
(assert-eq "taps on it set the colour, the width, snap and the tool, and the tool puts it away" "(:ellipse :t 12 32 0)" (elem-get wa_out 2))
(assert-eq "and the toolbars show what it set" "(:ellipse 2 2 :t)" (elem-get wa_out 3))
(assert-eq "what is then drawn is that" ":ellipse" (elem-get wa_out 4))
(assert-eq "shut, it is off the stage" "0" (elem-get wa_out 5))

;a document of very many shapes is drawn by the nodes. The app's own redraw is
;called, as its timer calls it, and what comes to the stripes' mailboxes is
;given to them as its loop gives it. With no desktop the last step of a
;draw, to a texture, throws, and is the last step, so it is caught
(defq wa_out (wa-run (cat
	"(defun wa-bytes (canvas) (defq stream (memory-stream)) (pixmap-write (getf canvas +canvas_pixmap 0) stream 32)"
	" (stream-seek stream 0 0) (read-blk stream 100000000))"
	"(defun wa-redraw () (catch (redraw) :t))"
	"(defq wa_timer (mail-mbox) wa_select (cat (. *stripes* :mboxes) (list wa_timer)))"
	"(defun wa-pump (done) (defq said :nil) (mail-timeout wa_timer (task-timeout 20) 0)"
	" (until (or said (done)) (defq msg (mail-read (elem-get wa_select (defq idx (mail-select wa_select)))))"
	" (if (= idx 3) (setq said :timeout) (progn (setq said (. *stripes* :handle idx msg)) (catch (frame-done said) :t))))"
	" (mail-timeout wa_timer 0 0) said)"
	"(defun wa-same () (defq canvas (Canvas 1024 768 1)) (. canvas :set_canvas_flags +canvas_flag_antialias)"
	" (. canvas :fill 0) (. *board* :draw canvas :nil) (eql (wa-bytes canvas) (wa-bytes *committed*)))"
	;no more than three children, other tests are running
	"(def *stripes* :herd 3) (defq doc (. *board* :get_doc))"
	"(each (lambda (i) (defq x (% (* i 37) 980) y (% (* i 53) 730))"
	" (cwb-add doc (cwb-shape (cwb-d-rect x y (+ x 40) (+ y 30) 6) :fill (+ 0xff000000 (% (* i 2654435761) 0xffffff)) :stroke 0xff000000 :width 2)))"
	" (range 0 300))"
	"(. *board* :changed :all) (. *board* :touch +board_dirty_doc)"
	;a small document, this task draws it and no node is asked. What is a
	;while is set, a slow machine is a while over 300 shapes
	"(setq *farm_ms* 100000) (wa-redraw) (print (list (/= (canvas-key *committed*) 0) (farm?) (get :jobs *stripes*) (wa-same)))"
	;with any time at all a while, and any document a big one: this task
	;draws it this once, and the nodes are started
	"(setq *farm_ms* -1 *farm_shapes* 0) (. *board* :touch +board_dirty_doc) (wa-redraw)"
	"(print (list *framing* (if (get :jobs *stripes*) :started :not) (wa-same)))"
	"(print (list (wa-pump (lambda () (. *stripes* :ready?))) (. *stripes* :cheap?)))"
	;a change, and the nodes draw it
	"(. *board* :select (list 5 6)) (. *board* :transform (cwb-mat-move 100 80)) (. *board* :select (list))"
	"(. *committed* :fill 0) (wa-redraw) (print (list *framing* (. *stripes* :busy?)))"
	;changed again while they do, it is drawn again when they have
	"(. *board* :add (cwb-shape (cwb-d-ellipse 500 400 90 60) :fill 0xff00ff00))"
	"(wa-redraw) (print (list *framing* *again*))"
	"(print (list (wa-pump (lambda () :nil)) *framing* *again* (. *board* :dirty? +board_dirty_doc)))"
	;300 shapes is a small document, as the app has it, and the next is this task's
	"(setq *farm_ms* 40 *farm_shapes* 2000 *local_ms* 1000)"
	"(. *board* :touch +board_dirty_doc) (wa-redraw) (print (list *framing* (wa-pump (lambda () :nil)) (wa-same)))"
	"(print (list *local_ms* (farm?)))"
	;a frame that never comes back: the nodes are let go and this task draws it
	"(setq *farm_ms* -1 *farm_shapes* 0) (. *board* :touch +board_dirty_doc) (wa-redraw)"
	"(. *board* :touch +board_dirty_doc) (wa-redraw)"
	"(print (list *framing* (progn (frame-watch) *framing*)))"
	"(def *stripes* :stamp (- (pii-time) 4000000)) (. *committed* :fill 0)"
	"(print (list (catch (frame-watch) :t) *framing* *farm_fails* (if (get :jobs *stripes*) :up :gone) (wa-same)))"
	"(. *stripes* :close)")))
(defq wa_shared (starts-with "(:t" (elem-get wa_out 0)))
(cond
	((not wa_shared) (test-skip "the app draws a big document by the nodes" "this host has no shared memory for pixels"))
	(:t
		(assert-eq "a small document is drawn by the app's task, and no node is started" "(:t :nil :nil :t)" (elem-get wa_out 0))
		(assert-eq "one that took a while is drawn by it once more, and the nodes are started" "(:nil :started :t)" (elem-get wa_out 1))
		(assert-eq "they come up in step with it" "(:warm :t)" (elem-get wa_out 2))
		(assert-eq "the next change is drawn by them, the app does not wait" "(:t :t)" (elem-get wa_out 3))
		(assert-eq "a change while they draw is kept for when they have" "(:t :t)" (elem-get wa_out 4))
		(assert-eq "they have, and it is to be drawn again" "(:done :nil :nil :t)" (elem-get wa_out 5))
		(assert-eq "drawn again by them it is, to the pixel, what the app's task draws" "(:t :done :t)" (elem-get wa_out 6))
		(assert-eq "and being a small document after all, the next is the app's task's" "(0 :nil)" (elem-get wa_out 7))
		(assert-eq "a frame that is out is left be while it is new" "(:t :t)" (elem-get wa_out 8))
		(assert-eq "one that has been out three seconds is given up: the nodes are let go, it is counted, and the app's task has drawn it"
			"(:t :nil 1 :gone :t)" (elem-get wa_out 9))))

;a thing taken up, or put down, changes only its part of the document's picture, and the board says which
(defq wa_dmg (wa-run (cat
	"(wa-mode :frect) (wa-drag 1 100 100 300 300) (. *ink_toolbar* :set_selected 2) (wa-do +event_ink) (wa-drag 1 200 200 400 400)"
	"(wa-mode :pen) (wa-drag 1 150 150 380 390)"
	"(. *board* :select (list)) (. *board* :take_damage)"
	"(. *board* :select (list 2)) (defq d1 (. *board* :take_damage)) (print (list (length d1) (map (const n2i) (first d1))))"
	"(. *board* :select (list)) (defq d2 (. *board* :take_damage)) (print (list (length d2) (map (const n2i) (first d2))))"
	;a thing drawn is put on top and is no part to draw again, a few taken up are parts, and a change to them is all of it
	"(wa-mode :frect) (wa-drag 1 500 500 520 520) (prin (. *board* :take_damage))"
	"(. *board* :select (list 1 2 3)) (prin (length (. *board* :take_damage))) (. *board* :style :stroke 0xff00ff00) (print (. *board* :take_damage))")))
(assert-eq "one thing taken up is one part of the picture, the box round it" "(1 (200 200 400 400))" (first wa_dmg))
(assert-eq "put down, the same part again" "(1 (200 200 400 400))" (second wa_dmg))
(assert-eq "a thing drawn is no part, three taken up are three parts, and a change to them is all of it" "()3:all" (third wa_dmg))

;only a part of the document's picture drawn again is that part of all of it drawn again, to the pixel
(defq wa_part (wa-run (cat
	"(defun pic () (task-slice) (defq s (memory-stream)) (pixmap-write (getf *committed* +canvas_pixmap 0) s 32) (stream-seek s 0 0) (read-blk s 100000000))"
	;boxes, a line and an ellipse that lie over one another
	"(wa-mode :frect) (wa-drag 1 100 100 300 300) (. *ink_toolbar* :set_selected 2) (wa-do +event_ink) (wa-drag 1 200 200 400 400)"
	"(wa-mode :pen) (. *ink_toolbar* :set_selected 4) (wa-do +event_ink) (wa-drag 1 150 150 380 390) (wa-mode :ellipse) (wa-drag 1 250 120 420 330)"
	"(. *board* :select (list)) (. *board* :dirty? +board_dirty_doc) (. *board* :take_damage) (draw-local (view-matrix)) (defq p0 (pic))"
	;the red box, in the middle of them, is taken up
	"(. *board* :select (list 2)) (defq d1 (. *board* :take_damage) parts (draw-damage d1 (view-matrix)) p1 (pic))"
	"(draw-local (view-matrix)) (defq p2 (pic))"
	"(print (list *damage_parts* parts (> (length p0) 3000000) (eql p1 p2) (eql p0 p1) (. *committed* :get_clip)))"
	;and put down again
	"(. *board* :select (list)) (draw-damage (. *board* :take_damage) (view-matrix)) (print (eql (pic) p0))"
	;one at the corner of the paper, part of it off it
	"(. *board* :select (list 1)) (. *board* :transform (cwb-mat-move -150.0 -150.0)) (. *board* :select (list)) (. *board* :take_damage) (draw-local (view-matrix)) (defq p3 (pic))"
	"(. *board* :select (list 1)) (draw-damage (. *board* :take_damage) (view-matrix)) (defq p4 (pic)) (draw-local (view-matrix)) (print (list (eql p4 (pic)) (eql p4 p3)))")))
(assert-eq "one thing taken up: that part of the picture is made clear and drawn, and the picture is then what all of it drawn again is, and not what it was. The canvas is left whole to draw on"
	"(:t ((197 197 404 404)) :t :t :nil (0 0 1024 768))" (first wa_part))
(assert-eq "put down, the picture is what it was" ":t" (second wa_part))
(assert-eq "and so with one that is partly off the corner of the paper" "(:t :nil)" (third wa_part))

;the box and handles of what is selected go with it: taken to the back they are at the back with it, on its canvas, and not in front
(defq wa_back (wa-run (cat
	"(defun wa-bits (c) (task-slice) (defq s (memory-stream)) (pixmap-write (getf c +canvas_pixmap 0) s 32) (stream-seek s 0 0) (read-blk s 100000000))"
	"(. *overlay* :fill 0) (defq blank (wa-bits *overlay*))"
	"(wa-mode :frect) (wa-drag 1 300 300 400 380) (wa-mode :select)"
	;taken by the left button, in front
	"(wa-drag 1 350 340 360 350) (draw-over) (draw-flight) (defq front (wa-bits *flight*)) (print (list (get :float_back *board*) (eql (wa-bits *overlay*) blank)))"
	;taken by the right button, to the back
	"(wa-drag 3 360 350 350 340) (draw-over) (draw-flight) (print (list (get :float_back *board*) (eql (wa-bits *overlay*) blank) (eql (wa-bits *flight*) front)))"
	;and as it is taken, the button only just down and nothing moved: they are there at once, not when it is first moved
	"(wa-drag 1 350 340 350 340) (draw-over) (draw-flight) (defq front (wa-bits *flight*))"
	"(wa-mouse :down 350 340 3) (draw-over) (draw-flight) (print (list (get :float_back *board*) (eql (wa-bits *overlay*) blank) (eql (wa-bits *flight*) front))) (wa-mouse :up 350 340 0)")))
(assert-eq "a thing taken by the left button has its box and handles in front, on what is over the board" "(:nil :nil)" (first wa_back))
(assert-eq "taken by the right button to the back, nothing of it is in front: its box and handles are on its own canvas, with it"
	"(:t :t :nil)" (second wa_back))
(assert-eq "and they are at the back as soon as the right button goes down on it, before it is moved" "(:t :t :nil)" (third wa_back))

;a line taken by an end, in select mode: it is drawn again as the end goes, it is not the picture of it that is moved
(defq wa_end (wa-run (cat
	"(defun wa-bits (c) (task-slice) (defq s (memory-stream)) (pixmap-write (getf c +canvas_pixmap 0) s 32) (stream-seek s 0 0) (read-blk s 100000000))"
	"(wa-mode :arrow2) (wa-drag 1 300 300 500 300) (wa-mode :select) (wa-drag 1 400 300 400 300) (draw-over) (draw-flight) (defq was (wa-bits *flight*))"
	"(print (list (. *board* :get_selected) (cwb-get (first (. *board* :selected_items)) :d)))"
	;its far end, taken and moved down
	"(wa-mouse :down 500 300 1) (draw-over) (draw-flight) (wa-mouse :move 500 420 1) (draw-over) (defq swapped (draw-flight))"
	"(print (list (cwb-get (first (. *board* :selected_items)) :d) (eql (wa-bits *flight*) was) (slice (. *flight* :get_bounds) 0 2)))"
	"(wa-mouse :up 500 420 0)")))
(assert-eq "an arrow with two heads, selected" "((1) \qM 300 300 L 500 300\q)" (first wa_end))
(assert-eq "its end dragged down: the line is to there, and what is seen of it is drawn again as it goes, not left as it was"
	"(\qM 300 300 L 500 420\q :nil (0 0))" (second wa_end))
