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
	;a zoom goes a little of the way each tick of the app's timer, there is none here: it is run to where it is going
	" (defun wa-zoom (id) (defq said (wa-do id)) (while (zoom-tick)) said)"
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
	"(wa-do +event_snap) (wa-do +event_keep_shape) (wa-zoom +event_zoom_in) (. *text_field* :set_text {Hello})"
	"(wa-do +event_full_protractor) (defq tool (last (. (. *board* :get_stage) :get_actors)))"
	"(def tool :angle 0.5 :mode :pie :origin (list 300.0 200.0)) (. tool :set_extent 260.0)"
	"(defq was (list (wa-ids) (get :mode *board*) (get :color *board*) (get :width *board*) (get :snap *board*) (get :keep_shape *board*) *zoom* (canvas-size)))"
	"(defq ss (string-stream (cat {}))) (tree-save ss (config-state)) (defq text (str ss))"
	;another board altogether
	"(. *board* :set_doc (cwb-doc 200 100)) (. (. *board* :get_stage) :sub tool) (wa-mode :pen)"
	"(. *ink_toolbar* :set_selected 0) (wa-do +event_ink) (wa-do +event_snap) (wa-do +event_keep_shape) (wa-zoom +event_zoom_out) (. *text_field* :set_text {})"
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
	"(wa-mouse :move 700 600 4) (wa-mouse :up 700 600 0) (print (list (get :held *board_view*) (wa-ids)))"
	;what is seen went with the mouse, 40 and 60. It is put back for what follows
	"(print (list *pan_x* *pan_y*)) (setq *pan_x* 0.0 *pan_y* 0.0) (view-changed)"
	;the eraser bites: a box, the eraser drawn through it, and undo. The mode is the board's, the app keeps none
	"(. *board* :clear) (cwb-add (. *board* :get_doc) (cwb-shape (cwb-d-rect 200 200 400 300) :fill 0xff808080 :stroke 0))"
	"(defq bite_was (wa-ids)) (def *board* :rub_mode :bite) (wa-mode :eraser) (wa-drag 1 300 150 300 350)"
	"(defq bite_is (wa-ids) bite_hit (if (cwb-hit (. *board* :get_doc) 300 250 0) :t :nil) bite_kept (. (config-state) :find :rub))"
	"(wa-do +event_undo) (config-apply (scatter (config-state) :rub :part))"
	"(print (list (length bite_was) (length bite_is) (eql (str bite_was) (str bite_is)) bite_hit bite_kept (eql (str bite_was) (str (wa-ids))) (get :rub_mode *board*)))")))
(assert-eq "the eraser bites, in the app: the box is another thing with the middle gone, no mode is kept, undo has the box back, and a mode that was kept once is not taken"
	"(1 1 :nil :nil :nil :t :bite)" (last wa_out))
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
	"(print (list (wa-zoom +event_zoom_in) *zoom* (canvas-size) (. *committed* :pref_size) (get :zoom *board*)))"
	"(wa-mouse :down 150 150 1) (wa-mouse :up 150 150 0)"
	"(print (list (wa-zoom +event_zoom_out) (wa-zoom +event_zoom_out) *zoom* (canvas-size)))"
	"(print (list (wa-zoom +event_zoom_in) *zoom* (canvas-size) (wa-draw)))"
	;the size of the board, by the size field
	"(. *size_field* :set_text {800x500}) (print (list (wa-do +event_paper_size) (canvas-size) (. (. *board* :get_doc) :find :width)))"
	"(. *size_field* :set_text {nonsense}) (print (list (wa-do +event_paper_size) (canvas-size)))"
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
(assert-eq "snap is on, to every line of the paper that shows, half the grid of the document at 1, and off" "(:ok 16.00000 :ok 0.00000)" (elem-get wa_out 3))
(assert-eq "angles snap" "(:ok :t)" (elem-get wa_out 4))
(assert-eq "zoomed in is twice, and the paper and its canvases are the size they were: it is what is seen on it that is bigger" "(:ok 2.00000 (1024 768) (1024 768) 2.00000)" (elem-get wa_out 5))
(assert-eq "out twice is a half" "(:ok :ok 0.50000 (1024 768))" (elem-get wa_out 6))
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
	;the other end of the pen takes a bite out of its side
	"(wa-ptr 0x20001 2 1 48 60) (wa-ptr 0x20001 2 1 49 62) (wa-ptr 0x20001 2 0 49 62)"
	"(print (wa-ids))"
	;zoomed about its top left, a point of the view is a point of the document over the zoom
	"(zoom-set 2.0 0 0) (wa-ptr 0x20001 1 1 200 200) (wa-ptr 0x20001 1 1 400 400) (wa-ptr 0x20001 1 0 400 400)"
	"(print (cwb-get (last (cwb-items (. *board* :get_doc))) :d))"
	;and moved, 50 across and 30 down on the screen: what was at 100 is seen 25 further across, so the same place on the screen is 25 less
	"(pan-by 50 30) (wa-ptr 0x20001 1 1 200 200) (wa-ptr 0x20001 1 1 400 400) (wa-ptr 0x20001 1 0 400 400)"
	"(print (list (cwb-get (last (cwb-items (. *board* :get_doc))) :d) *pan_x* *pan_y*))"
	;the paper's lines are where the document's 0 0 is seen, and at a zoom it rests at they are the grid, half of it at half strength, and no finer
	"(draw-paper) (print (list (get :origin_x *paper*) (get :origin_y *paper*) (map (# (list (first %0) (>> (second %0) 24))) (get :rules *paper*))))"
	;an instrument lies on the glass: what is seen is moved and zoomed under it, and it is where it was on the screen, the size it was
	"(defun wa-on () (map (# (n2i (+ %0 0.5))) (first (map (# (list (second %0) (third %0) (* 100.0 (elem-get %0 3)))) (tools-held)))))"
	"(. (. *board* :get_stage) :add (defq ruler (Ruler *board* 300 200))) (defq on0 (wa-on))"
	"(pan-by 70 -20) (defq on1 (wa-on)) (zoom-set 0.5 100 100) (defq on2 (wa-on)) (pan-glide 32 0) (while (pan-tick)) (defq on3 (wa-on))"
	"(print (list on0 on1 on2 on3 (map (const n2i) (get :origin ruler)) (get :size ruler) (n2i *pan_x*)))"
	;an instrument none of which shows, with no hand on it, is put away: it is how one is thrown away. One that shows a little, or is held, is not
	"(defun wa-tools () (length (filter (const Instrument?) (. (. *board* :get_stage) :get_actors))))"
	"(draw-over) (defq seen (list 0 0 1024 768) c0 (tools-cull seen) n0 (wa-tools))"
	"(bind (quote (bx by bw bh)) (. (elem-get (first *tools*) +tv_canvas) :get_bounds))"
	"(defq c1 (tools-cull (list (+ bx bw -4) 0 2000 2000)) c2 (tools-cull (list (+ bx bw 4) 0 2000 2000)) n2 (wa-tools))"
	"(print (list c0 n0 c1 c2 n2))"
	"(zoom-set 2.0 0 0)"
	;the bars: all of one is what is drawn and what is seen, its thumb is what is seen. Things from 75 across, seen from 300 on at twice, 512 of it
	"(defun wa-bars () (map (# (list (get :maximum %0) (get :portion %0) (get :value %0))) (list (get :hslider *image_scroll*) (get :vslider *image_scroll*))))"
	"(setq *pan_x* 300.0 *pan_y* -15.0) (view-changed) (bars-update) (print (first (wa-bars)))"
	;the bar moved back by hand to 200: what is seen is from 100 on from where there is anything, and the bars are left as they are while it goes on
	"(def (get :hslider *image_scroll*) :value 200) (bars-moved) (print (list *pan_x* (first (wa-bars)) *bars_stale*))"
	"(setq *pan_x* -25.0 *pan_y* -15.0 *bars_at* 0) (view-changed)"
	;what is seen is everybody's: while a pen has a line in hand the wheel, the zoom button, two fingers and a bar do nothing to it
	"(defun wa-seen () (list (n2i *pan_x*) (n2i *pan_y*) *zoom* (if *zoom_to* :going)))"
	"(defun wa-wheel (x y) (. *board_view* :mouse_wheel (setf-> (str-alloc +ev_msg_wheel_size) (+ev_msg_type +ev_type_wheel) (+ev_msg_wheel_x x) (+ev_msg_wheel_y y))))"
	"(wa-mode :pen) (defq s0 (wa-seen)) (wa-ptr 0x20001 1 1 300 300) (wa-ptr 0x20001 1 1 340 320)"
	"(defq b0 (. *board* :busy?)) (wa-wheel 2 3) (while (pan-tick)) (wa-zoom +event_zoom_in)"
	"(wa-ptr 0x10001 3 1 600 20) (wa-ptr 0x10002 3 1 700 20) (wa-ptr 0x10001 3 1 660 60) (wa-ptr 0x10002 3 1 860 60)"
	"(def (get :hslider *image_scroll*) :value 5) (bars-moved)"
	"(defq s1 (wa-seen))"
	;the pen lifts: the two fingers that are still down have it from where they are, and the wheel works
	"(wa-ptr 0x20001 1 0 340 320) (defq b1 (. *board* :busy?))"
	"(wa-ptr 0x10001 3 1 670 70) (wa-ptr 0x10002 3 1 870 70) (defq s2 (wa-seen))"
	"(wa-ptr 0x10001 3 0 670 70) (wa-ptr 0x10002 3 0 870 70) (wa-wheel 0 2) (while (pan-tick)) (defq s3 (wa-seen))"
	"(print (list b0 (eql (str s0) (str s1)) b1 s0 s2 s3)) (setq *pan_x* -25.0 *pan_y* -15.0 *bars_at* 0) (view-changed)"
	;a hand that holds a thing does not stop what is seen being moved, and what it holds stays in the hand: it is in flight
	"(. *board* :clear) (cwb-add (. *board* :get_doc) (cwb-shape (cwb-d-rect 100 100 140 140) :fill 0xff808080 :stroke 0)) (wa-mode :select)"
	"(defun wa-box () (bind (quote (bx by bx1 by1)) (cwb-bounds (cwb-items (. *board* :get_doc)))) (list (n2i (* (- bx *pan_x*) *zoom*)) (n2i (* (- by *pan_y*) *zoom*)) (n2i bx)))"
	"(wa-mouse :down 290 270 1) (defq h0 (wa-box) hb (. *board* :busy?)) (pan-by 60 20) (defq h1 (wa-box)) (zoom-set 1.0 290 270) (defq h2 (wa-box))"
	"(wa-mouse :up 290 270 0) (pan-by 10 0) (defq h3 (wa-box))"
	"(print (list hb h0 h1 h2 h3)) (zoom-set 2.0 0 0) (setq *pan_x* -25.0 *pan_y* -15.0) (view-changed)"
	;part way between two zooms it rests at, the finer rulings are part way up
	"(zoom-set 3.0 0 0) (draw-paper) (print (map (# (list (first %0) (>> (second %0) 24))) (get :rules *paper*)))")))
(assert-eq "a pen, told of by the GUI, draws" "((1) \qM 100 100 L 200 100 200 180 100 180 Z\q)" (elem-get wa_out 0))
(assert-eq "two fingers on what it drew make it twice the size" "(47 57 253 223)" (elem-get wa_out 1))
(assert-eq "and the other end of the pen takes a bite out of it, one bite for all the way it went, it is another shape, flat" "(2)" (elem-get wa_out 2))
(assert-eq "zoomed to twice, a pen draws where it is in the document" "M 100 100 L 200 100 200 200 100 200 Z" (elem-get wa_out 3))
(assert-eq "moved 50 and 30 on the screen at twice, it is seen from 25 and 15 back, and a pen in the same place draws that much back in the document"
	"(\qM 75 85 L 175 85 175 185 75 185 Z\q -25.00000 -15.00000)" (elem-get wa_out 4))
(assert-eq "the paper's lines are drawn from where the document's 0 0 is seen: the grid full, half of it at half, a quarter of it not at all"
	"(50.00000 30.00000 ((8.00000 0) (16.00000 127) (32.00000 255)))" (elem-get wa_out 5))
(assert-eq "an instrument lies on the glass: what is seen moved, zoomed and glided under it, it is where it was on the screen and the size it was there, and so twice the size in the document at half"
	"((138 46 100) (138 46 100) (138 46 100) (138 46 100) (1026 705) 2.00000 -274)" (elem-get wa_out 6))
(assert-eq "an instrument that shows is left; with four pixels of it showing it is left; with none of it showing it is put away"
	"(0 1 0 1 0)" (elem-get wa_out 7))
(assert-eq "a bar is all there is to see that way, from the first thing drawn, at 47, to the end of the view, at twice: 506 more than shows, the 1024 that shows its thumb, at the end"
	"(506 1024 506)" (elem-get wa_out 8))
(assert-eq "moved back by hand to 200, what is seen is from 100 on from the first thing, and the bar is not set again under the hand"
	"(147.00000 (506 1024 200) :t)" (elem-get wa_out 9))
(assert-eq "half way to the next zoom the lines are half as far apart again, the next finer three quarters up and the finest a quarter"
	"((12.00000 63) (24.00000 191) (48.00000 255))" (elem-get wa_out 12))
(assert-eq "a hand that holds a thing does not stop what is seen being moved; moved, the thing is where it was on the screen, in the hand, and another place in the document; zoomed about the hand, the place of it the hand has is still under the hand; let go, it goes with what is seen"
	"(:nil (250 230 100) (250 230 70) (270 250 70) (280 250 70))" (elem-get wa_out 11))
(assert-eq "what is seen is everybody's: while a pen has a line in hand the wheel, the zoom button, two fingers and a bar do nothing to it; when it lifts the two fingers move it from where they are, with no jump, and the wheel moves it"
	"(:t :t :nil (-25 -15 2.00000 :nil) (-30 -20 2.00000 :nil) (-30 -36 2.00000 :nil))" (elem-get wa_out 10))


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
	"(print (map (const n2i) (Mat3x2-point (over-matrix) 0 0)))"
	"(. (. *board* :get_stage) :add (defq ruler (Ruler *board* -60 100)))"
	;a line being drawn, from off the document's left to on it, with the ruler there
	"(wa-mode :line) (wa-mouse :down 60 700 1) (wa-mouse :move 500 700 1)"
	"(draw-over)"
	"(defq s (memory-stream)) (pixmap-write (getf *overlay* +canvas_pixmap 0) s 32) (stream-seek s 0 0) (defq d (read-blk s 100000000))"
	"(defun px (x y) (get-uint d (+ (- (length d) (* 1504 1248 4)) (* 4 (+ (* y 1504) x)))))"
	;the ruler is on a canvas of its own, as big as it is, put where it lies
	"(defq tc (elem-get (first *tools*) +tv_canvas) ts (memory-stream)) (pixmap-write (getf tc +canvas_pixmap 0) ts 32) (stream-seek ts 0 0) (defq td (read-blk ts 100000000))"
	"(bind (quote (tx ty tw th)) (. tc :get_bounds))"
	"(defun tpx (x y) (get-uint td (+ (- (length td) (* tw th 4)) (* 4 (+ (* (- y ty) tw) (- x tx))))))"
	"(print (list (/= 0 (tpx 100 340)) (px 100 100) (px 100 340) (< tw 600) (< th 200)))"
	;it is in flight, on the canvas of what is, which is the size of the document and no more
	"(draw-flight) (defq s (memory-stream)) (pixmap-write (getf *flight* +canvas_pixmap 0) s 32) (stream-seek s 0 0) (defq f (read-blk s 100000000))"
	"(print (list (px 150 700) (px 400 700) (. *flight_clip* :get_bounds) (/= 0 (get-uint f (+ (- (length f) (* 1024 768 4)) (* 4 (+ (* 460 1024) 100)))))))"
	"(wa-mouse :up 500 700 0)"
	;from the back: the paper, the document, what is in flight, what is over it all. Taken by the right button, what is in flight is behind the document
	"(defun order () (map (# (cond ((eql %0 *paper*) :paper) ((eql %0 *committed*) :doc) ((eql %0 *flight_clip*) :flight) ((eql %0 *overlay*) :over) ((eql %0 *top*) :top) (:t :tool))) (reverse (. *backdrop* :children))))"
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
(assert-eq "a ruler that lies off the left of the document is drawn there, in the margin, on a canvas of its own no bigger than it, and not on what is over the document" "(:t 0 0 :t :t)" (elem-get wa_out 3))
(assert-eq "a line that is being drawn is in flight: not on what is over the document, on a canvas the size of the document, where it is"
	"(0 0 (240 240 1024 768) :t)" (elem-get wa_out 4))
(assert-eq "from the back: the paper, the document, what is in flight, what is over it, the ruler, and what is over that" "(:paper :doc :flight :over :tool :top)" (elem-get wa_out 5))
(assert-eq "a thing taken by the right button is in flight behind the document" "((:paper :flight :doc :over :tool :top) :t :t)" (elem-get wa_out 6))
(assert-eq "let go, it is selected still, and in flight still, behind the document" "((:paper :flight :doc :over :tool :top) :t (1))" (elem-get wa_out 7))
(assert-eq "sized by a handle, it is still behind the document while it is" "((:paper :flight :doc :over :tool :top) :t)" (elem-get wa_out 8))
(assert-eq "a click on nothing lets go of it: nothing is in flight, and the document is to be drawn again, once, with it in" "(() :nil :t)" (elem-get wa_out 9))
(assert-eq "taken by the left button it is in flight in front of the document" "(:paper :doc :flight :over :tool :top)" (elem-get wa_out 10))
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
	"(. *board* :select (list 5 6)) (. *board* :transform (Mat3x2-translate 100 80)) (. *board* :select (list))"
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
	"(. *board* :select (list 1)) (. *board* :transform (Mat3x2-translate -150.0 -150.0)) (. *board* :select (list)) (. *board* :take_damage) (draw-local (view-matrix)) (defq p3 (pic))"
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
	;let go of first: a tap on what is selected already asks for the palette, and that is not what is being looked at here
	"(. *board* :select (list)) (wa-drag 1 350 340 350 340) (draw-over) (draw-flight) (defq front (wa-bits *flight*))"
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

;two fingers on nothing have hold of the view: moved together it goes with them, moved apart it zooms
(defq wa_out (wa-run (cat
	"(defun wa-ptr (id kind buttons rx ry) (. *window* :event (setf-> (str-alloc +ev_msg_pointer_size)"
	" (+ev_msg_type +ev_type_pointer) (+ev_msg_target_id (. *board_view* :get_id))"
	" (+ev_msg_pointer_id id) (+ev_msg_pointer_kind kind) (+ev_msg_pointer_buttons buttons) (+ev_msg_pointer_pressure 65535)"
	" (+ev_msg_pointer_x rx) (+ev_msg_pointer_y ry) (+ev_msg_pointer_rx rx) (+ev_msg_pointer_ry ry))))"
	"(setq *margin* 240 *zoom* 1.0) (. *board* :new 1024 768) (board-resized) (bind (quote (w h)) (. *window* :pref_size)) (. *window* :change 0 0 w h)"
	"(defun wa-view () (list (n2i *pan_x*) (n2i *pan_y*) *zoom* (if *zoom_to* (first *zoom_to*))))"
	"(defq was (wa-view) ids (wa-ids))"
	;two fingers down on empty paper, and both moved 60 right and 40 down
	"(wa-mode :select) (wa-ptr 0x10001 3 1 600 500) (wa-ptr 0x10002 3 1 700 500)"
	;a little at a time, first the one and then the other, as hands do
	"(each (lambda (i) (wa-ptr 0x10001 3 1 (+ 600 (* i 6)) (+ 500 (* i 4))) (wa-ptr 0x10002 3 1 (+ 700 (* i 6)) (+ 500 (* i 4)))) (range 1 11))"
	"(defq moved (wa-view))"
	;then twice as far apart
	"(each (lambda (i) (wa-ptr 0x10001 3 1 (- 660 (* i 5)) 540) (wa-ptr 0x10002 3 1 (+ 760 (* i 5)) 540)) (range 1 11))"
	"(defq zoomed (wa-view))"
	"(wa-ptr 0x10001 3 0 610 540) (wa-ptr 0x10002 3 0 810 540)"
	;let go, it is on its way to the zoom it rests at, and gets there
	"(defq going (wa-view)) (while (zoom-tick)) (defq rested (wa-view))"
	"(print (list was moved (third zoomed) (elem-get going 3) (third rested) (eql (str ids) (str (wa-ids))) (. *board* :get_selected) (length (palettes *board*)) (. (get :bands *board*) :size)))")))
(assert-eq "what is seen goes the way the two fingers went; twice as far apart it is zoomed, not quite twice as the first twelfth is let go by; let go it is on its way to twice and gets there; and nothing of the board was done: no box, no palette, nothing selected"
	"((0 0 1.00000 :nil) (-60 -40 1.00000 :nil) 1.85186 2.00000 2.00000 :t () 0 0)" (last wa_out))

;max is the board and nothing else, all of the screen: no title, no toolbars, no bars, the paper the size of the screen. The palette has one more thing to do then, the window back
(defq wa_out (wa-run (cat
	"(setq *margin* 240) (. *board* :new 400 300) (board-resized) (bind (quote (w h)) (. *window* :pref_size)) (. *window* :change 30 40 w h)"
	"(defun wa-look () (list (. *window* :get_bounds) (length (. (get :child *window*) :children)) (canvas-size) (if (get :bare *image_scroll*) :bare) (if (get :on_window *board*) :t)"
	" (length (. (Palette *board* 100 100) :get_wedges))))"
	"(defq w0 (wa-look)) (full-screen 1280 800) (defq w1 (wa-look))"
	;where the frame the board lies in is on the screen: the window's place and the frame's place in it
	"(bind (quote (wx wy ww wh)) (. *window* :get_bounds)) (bind (quote (fx fy fw fh)) (. *window* :get_relative *image_scroll*))"
	"(defq on (list (+ wx fx) (+ wy fy) fw fh) off (list (< wx 0) (< wy 0) (> ww 1280) (> wh 800)))"
	;the palette's wedge asks, and it is done on the next tick
	"((get :on_window *board*)) (defq asked *full_go*) (full-window) (defq w2 (wa-look))"
	"(print (list (eql (str w0) (str w2)) asked (rest w0))) (print (list on off (rest w1)))")))
(assert-eq "a window: a title, four rows and the board in it, a paper of 400 by 300, bars, and 37 things on the palette. Made all of the screen and back it is that again, and the palette's wedge asked for it"
	"(:t :t (6 (400 300) :nil :nil 37))" (first wa_out))
(assert-eq "all of the screen: the frame the board lies in is the screen, and the rest of the window is off its edges, nothing taken out of it; the paper is the size of the screen, no bars, and one more thing on the palette, the window back"
	"((0 0 1280 800) (:t :t :t :t) (6 (1280 800) :bare :t 38))" (second wa_out))

;the board turned a quarter, about the middle of the paper, which is then the other way up
(defq wa_out (wa-run (cat
	"(defun wa-ptr (id kind buttons rx ry) (. *window* :event (setf-> (str-alloc +ev_msg_pointer_size)"
	" (+ev_msg_type +ev_type_pointer) (+ev_msg_target_id (. *board_view* :get_id))"
	" (+ev_msg_pointer_id id) (+ev_msg_pointer_kind kind) (+ev_msg_pointer_buttons buttons) (+ev_msg_pointer_pressure 65535)"
	" (+ev_msg_pointer_x rx) (+ev_msg_pointer_y ry) (+ev_msg_pointer_rx rx) (+ev_msg_pointer_ry ry))))"
	"(setq *margin* 0) (. *board* :new 400 200) (board-resized)"
	"(defun wa-d () (cwb-get (last (cwb-items (. *board* :get_doc))) :d))"
	"(defun wa-mid () (map (const n2i) (view-middle)))"
	"(. (. *board* :get_stage) :add (defq ruler (Ruler *board* 300 100)))"
	;where the ruler is from the middle of the paper, in pixels, how big, and its angle there
	"(defun wa-on () (map (# (n2i (+ %0 0.5))) (first (map (# (list (second %0) (third %0) (* 100.0 (elem-get %0 3)) (* 100.0 (elem-get %0 4)))) (tools-held)))))"
	"(defq m0 (wa-mid) s0 (canvas-size) on0 (wa-on)) (view-turn 1) (draw-paper)"
	"(print (list s0 (canvas-size) *turn* m0 (wa-mid) (get :turned *paper*)))"
	;a pen across the top of the paper as it is seen now, 200 wide: in the document that is down its left side, from the bottom up
	"(wa-mode :line) (wa-ptr 0x20001 1 1 20 10) (wa-ptr 0x20001 1 1 180 10) (wa-ptr 0x20001 1 0 180 10) (print (wa-d))"
	;the instrument is where it was on the screen and at the angle it was there, so in the document it is turned the other way
	"(print (list on0 (wa-on) (n2i (* 100.0 (get :angle ruler)))))"
	;moved 30 across the screen: that is 30 up the document, turned as it is
	"(defq p0 (list *pan_x* *pan_y*)) (pan-by 30 0) (print (list (n2i (- *pan_x* (first p0))) (n2i (- *pan_y* (second p0)))))"
	;and three more quarters is where it began
	"(pan-by -30 0) (view-turn 1) (view-turn 1) (view-turn 1) (print (list (canvas-size) *turn* (wa-mid) (wa-on) (map (const n2i) (get :origin ruler)) (n2i (* 100.0 (get :angle ruler)))))"
	;two fingers on nothing, the one turned about the other past half way to a quarter: it is turned a quarter, once
	"(. (. *board* :get_stage) :sub ruler) (wa-mode :select) (wa-ptr 0x10001 3 1 50 180) (wa-ptr 0x10002 3 1 150 180)"
	"(wa-ptr 0x10002 3 1 140 220) (defq t1 *turn*) (wa-ptr 0x10002 3 1 100 265) (defq t2 *turn*) (wa-ptr 0x10002 3 1 50 280) (defq t3 *turn*)"
	"(wa-ptr 0x10001 3 0 50 180) (wa-ptr 0x10002 3 0 50 280) (print (list t1 t2 t3 (canvas-size)))"
	;kept with the document, and put back
	"(view-to-doc) (defq kept (. (. *board* :get_doc) :find :view)) (setq *turn* 0) (view-from-doc) (print (list (length kept) *turn*))")))
(assert-eq "turned a quarter the paper is the other way up, and what was in the middle of it is in the middle of it; its lines that went across go up and down"
	"((400 200) (200 400) 1 (200 100) (200 100) :t)" (elem-get wa_out 0))
(assert-eq "a pen across the top of the paper as it is seen is, in the document, a line up its left side" "M 10 180 L 10 20" (elem-get wa_out 1))
(assert-eq "an instrument is where it was on the screen, from the middle of the paper, and at the angle it was there: in the document it is a quarter the other way"
	"((100 0 100 0) (100 0 100 0) -158)" (elem-get wa_out 2))
(assert-eq "moved 30 across the screen, what is seen is 30 on up the document" "(0 30)" (elem-get wa_out 3))
(assert-eq "three more quarters and it is as it began, the paper, what is in the middle of it, and the instrument where it was in the document at the angle it was"
	"((400 200) 0 (200 100) (100 0 100 0) (300 100) 0)" (elem-get wa_out 4))
(assert-eq "two fingers on nothing, the one turned about the other: nothing at a quarter of the way, a quarter turn once it is past half way, and no more at all the way"
	"(0 1 1 (200 400))" (elem-get wa_out 5))
(assert-eq "how it is turned is kept with the document, the fourth of what is kept, and put back" "(4 1)" (elem-get wa_out 6))
;min puts the middle of the document back in the middle of the paper, at the zoom there is
(defq wa_home (wa-run (cat
	"(pan-by 70 -40) (zoom-set 2.0 0 0) (defq away (map (const n2i) (view-middle)))"
	"(view-home) (bind '(cw ch) (canvas-size))"
	"(print (list (eql (str away) (str (list (/ cw 2) (/ ch 2)))) (eql (str (map (const n2i) (view-middle))) (str (list (/ cw 2) (/ ch 2)))) *zoom*))"
	;and when the board is turned a quarter too
	"(view-turn 1) (pan-by -90 25) (view-home) (bind '(cw ch) (canvas-size))"
	"(print (list (map (const n2i) (view-middle)) (list (/ ch 2) (/ cw 2)) *turn*))")))
(assert-eq "moved away and zoomed, then home: the middle of the document is in the middle of the paper, the zoom as it was"
	"(:nil :t 2.00000)" (first wa_home))
(defq wa_turned (first (read (string-stream (second wa_home)) (ascii-code " "))))
(assert-eq "and the same with the board turned a quarter" (str (first wa_turned)) (str (second wa_turned)))
(assert-eq "still turned" "1" (str (third wa_turned)))
