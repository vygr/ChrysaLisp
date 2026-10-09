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
	" (board-resized)"
	" (defun wa-draw () (catch (progn (defq m (view-matrix))"
	" (. *board* :dirty? +board_dirty_doc) (. *board* :dirty? +board_dirty_append) (. *board* :take_appended)"
	" (draw-paper) (defq n (. *board* :draw *committed* m)) (. *board* :dirty? +board_dirty_overlay)"
	" (. *overlay* :fill 0) (. *board* :draw_overlay *overlay* m)"
	" (each (# (if (Instrument? %0) (. %0 :draw *overlay* m))) (. (. *board* :get_stage) :get_actors)) n) :threw))"
	" (defun wa-mouse (kind rx ry buttons)"
	" (defq e (setf-> (str-alloc +ev_msg_mouse_size) (+ev_msg_type +ev_type_mouse)"
	" (+ev_msg_mouse_rx rx) (+ev_msg_mouse_ry ry) (+ev_msg_mouse_x rx) (+ev_msg_mouse_y ry) (+ev_msg_mouse_buttons buttons)))"
	" (case kind (:down (. *board_view* :mouse_down e)) (:move (. *board_view* :mouse_move e)) (:up (. *board_view* :mouse_up e))))"
	" (defun wa-drag (button x y x1 y1) (wa-mouse :down x y button) (wa-mouse :move x1 y1 (case button (3 4) (:t button))) (wa-mouse :up x1 y1 0))"
	" (defun wa-do (id) (catch (progn ((. *event_map* :find id)) :ok) :threw))"
	" (defun wa-mode (mode) (. *mode_toolbar* :set_selected (find mode *modes*)) (wa-do +event_mode))"
	" (defun wa-ids () (map (# (elem-get %0 +cwb_id)) (cwb-items (. *board* :get_doc))))"))

(defun wa-run (body)
	;what the app says when that is done to it, each thing printed on a line of its own
	(split (test-output (cat wa_head " " body)) (ascii-char 10)))

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
	"(wa-mode :eraser) (wa-drag 1 100 100 105 101) (print (wa-ids))"
	"(print (list (wa-do +event_undo) (wa-ids)))"
	;the right button moves what it is on, whatever the mode
	"(wa-mode :pen) (wa-drag 3 300 150 340 190) (print (list (wa-ids) (. *board* :get_selected) (map (const n2i) (cwb-bounds (. *board* :selected_items)))))"
	;the middle button draws nothing
	"(wa-drag 2 50 50 90 90) (print (wa-ids))"
	"(print (list (wa-draw)))")))
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
(assert-eq "the eraser takes out the line it is dragged over" "(2 3 4)" (elem-get wa_out 9))
(assert-eq "and undo puts it back" "(:ok (1 2 3 4))" (elem-get wa_out 10))
(assert-eq "the right button moves what it is on, whatever the mode, by how far it is dragged"
	"((1 2 3 4) (2) (338 138 461 241))" (elem-get wa_out 11))
(assert-eq "the middle button is the view's, to move it about, and draws nothing" "(1 2 3 4)" (elem-get wa_out 12))
(assert-eq "and it all draws" "(4)" (elem-get wa_out 13))

(defq wa_out (wa-run (cat
	"(wa-drag 1 100 100 220 120) (wa-mode :rect) (wa-drag 1 300 100 420 200) (wa-drag 1 500 100 600 180)"
	;instruments, each a little off the last
	"(print (list (wa-do +event_ruler) (wa-do +event_protractor) (wa-do +event_set_square)"
	" (length (filter (const Instrument?) (. (. *board* :get_stage) :get_actors)))))"
	"(print (map (# (map (const n2i) (get :origin %0))) (filter (const Instrument?) (. (. *board* :get_stage) :get_actors))))"
	"(. *arc_toolbar* :set_selected 1) (print (list (wa-do +event_arc) (get :mode (some (# (if (Protractor? %0) %0)) (. (. *board* :get_stage) :get_actors)))))"
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
	"(. *size_field* :set_text {300x200}) (wa-do +event_new) (print (canvas-size))"
	"(board-load {tests/scratch/test_whiteboard.cwb}) (print (list (length (wa-ids)) (canvas-size) (. *size_field* :get_text) (wa-draw)))"
	"(pii-remove {tests/scratch/test_whiteboard.cwb})")))
(assert-eq "the ruler, the protractor and the set square are put on the board" "(:ok :ok :ok 3)" (elem-get wa_out 0))
(assert-eq "in the middle of it, each a little off the last" "((512 384) (542 414) (572 444))" (elem-get wa_out 1))
(assert-eq "the arc bar says what a protractor draws" "(:ok :pie)" (elem-get wa_out 2))
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
(assert-eq "new is an empty board of the size in the field" "(:ok () (800 500))" (elem-get wa_out 13))
(assert-eq "of another size if the field says so" "(300 200)" (elem-get wa_out 14))
(assert-eq "loaded, the board is the file's: its items, its size, and the field says so" "(3 (800 500) \q800x500\q 3)" (elem-get wa_out 15))

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
	" (cwb-paper canvas 1024 768 0 *style* 32) (. *board* :draw canvas :nil) (eql (wa-bytes canvas) (wa-bytes *committed*)))"
	"(defq doc (. *board* :get_doc))"
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
		(assert-eq "and being a small document after all, the next is the app's task's" "(0 :nil)" (elem-get wa_out 7))))
