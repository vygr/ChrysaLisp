;debug options
(case :nil
(0 (import "lib/debug/frames.inc"))
(1 (import "lib/debug/profile.inc"))
(2 (import "lib/debug/debug.inc")))

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./widgets.inc")
(import "lib/cwb/stripes.inc")
(import "lib/cwb/palette.inc")

;The Whiteboard. What is on the board is a document of shapes,
;lib/cwb/doc.inc, a .cwb file. All that changes it is the board,
;lib/cwb/board.inc, which is given pointers and has methods. This file is
;the window round it: toolbars that call those methods, a view that makes
;the mouse a pointer, ./view.inc, and two canvases the board draws on, the
;document on one and what is over it, what is being drawn or moved, the
;handles, a ruler, on the other.
;
;A hand that taps on nothing opens a palette on the board, where it tapped,
;lib/cwb/palette.inc. It can do what the toolbars do, and what it changes
;of the board the toolbars are made to show, (sync-ui).
;
;A document that takes this task a while to draw, some thousands of shapes,
;is drawn by the nodes of the machine in stripes, lib/cwb/stripes.inc,
;straight onto the pixels of the first canvas, which are in shared memory
;where the host has it. This task does not wait while they do.

(enums +select 0
	(enum main picker timer tip))

;the zooms there are, stepped through, so that in and then out is where it was
(defq +zooms ''(0.125 0.25 0.5 0.75 1.0 1.5 2.0 3.0 4.0 6.0 8.0))

(defq *zoom* 1.0 *style* :grid *snap* :nil *snap_angle* :nil
	*file* :nil *picker_mbox* :nil *picker_mode* :nil *running* :t
	*committed* :nil *overlay* :nil *flight* :nil *paper* :nil *paper_dirty* :t *flight_used* :nil
	*flight_back* :nil *canvas_size* :nil
	;the canvas of what is in flight is in a view the size of the paper,
	;*flight_clip*, which is what is stacked with the others. While a hand
	;only moves what is in flight the canvas is not drawn again, it is
	;moved in that view, and the view cuts off what goes past the paper.
	;*flight_at* is what was drawn on it and where each thing then was
	*flight_clip* :nil *flight_at* :nil
	;what was in flight and has been let go, while the nodes have yet to
	;draw the document with it in: *landing* is waiting for a frame to be
	;begun, *landed* is in the frame they are drawing. Both are still drawn
	;with what is in flight, or a thing let go would be nowhere till the
	;frame came, a blink. *flown* is what was in flight when last looked
	*flown* (list) *landing* (list) *landed* (list)
	rate (/ 1000000 60)
	;the board has this much round it, on the screen, each side, that is
	;not the document: an instrument lies half off the paper as a ruler
	;does on a desk, and is seen there. What is over the document, the
	;second canvas, covers it too
	*margin* 240
	;a document that takes this task longer than that to draw is drawn in
	;stripes, and one of fewer shapes than that is tried by this task again
	*farm_ms* 40 *farm_shapes* 2000
	*stripes* (Stripes *board*) *local_ms* 0 *framing* :nil *again* :nil
	*farm_fails* 0 *kept* 0)

(defun toolbar-states (toolbar states)
	;a button that is on is the brighter
	(defq radio_col (canvas-brighter (get :color toolbar)))
	(each (# (undef (. %0 :dirty) :color)
			(if %1 (def %0 :color radio_col)))
		(. toolbar :children) states))

;its colours are those of the ink bar that are solid, and four more
(palette-enable *board* (cat (slice *palette* 0 8) '(0xff7f7f7f 0xfff08c00 0xff7048e8 0xff8d5524)))

(defun sync-ui ()
	;the toolbars show what the board has, which a palette on it may have changed
	(defq at (find (get :mode *board*) *modes*))
	(if (and at (not (eql at (. *mode_toolbar* :get_selected)))) (. *mode_toolbar* :set_selected at))
	(setq at (find (get :color *board*) *palette*))
	(if (and at (not (eql at (. *ink_toolbar* :get_selected)))) (. *ink_toolbar* :set_selected at))
	(setq at (some (# (if (= %0 (get :width *board*)) (!))) *widths*))
	(if (and at (not (eql at (. *radius_toolbar* :get_selected)))) (. *radius_toolbar* :set_selected at))
	(defq on (/= (get :snap *board*) 0.0))
	(unless (eql on *snap*)
		(setq *snap* on)
		(toolbar-states *snap_toolbar* (list *snap* *snap_angle* (get :keep_shape *board*)))))

(defun canvas-size ()
	; (canvas-size) -> (width height)
	;the size of the board on the screen, the document's by the zoom
	(defq doc (. *board* :get_doc))
	(list (max 1 (n2i (* (n2f (. doc :find :width)) *zoom*)))
		(max 1 (n2i (* (n2f (. doc :find :height)) *zoom*)))))

(defun field-size ()
	; (field-size) -> (width height)
	;what the size field says, width x height, or the size the board is
	(defq doc (. *board* :get_doc)
		nums (filter (# (> %0 0)) (map (# (ifn (str-to-num %0) 0))
			(split (. *size_field* :get_text) (const (char-class " xX,*"))))))
	(if (= (length nums) 2)
		(list (min 16384 (n2i (first nums))) (min 16384 (n2i (second nums))))
		(list (. doc :find :width) (. doc :find :height))))

(defun view-matrix ()
	; (view-matrix) -> :nil | matrix
	;from the document to the canvas
	(if (= *zoom* 1.0) :nil (cwb-mat-scale *zoom*)))

(defun over-matrix ()
	; (over-matrix) -> :nil | matrix
	;from the document to the canvas of what is over it, which has the
	;margin round it
	(if (= *margin* 0) (view-matrix)
		(cwb-mat-mul (cwb-mat-move *margin* *margin*) (view-matrix))))

(defun view-middle ()
	; (view-middle) -> (x y)
	;the point of the document that is in the middle of what shows
	(bind '(sw sh) (. *image_scroll* :get_size))
	(bind '(cw ch) (canvas-size))
	;a scroll that has not been laid out yet shows all of it
	(if (<= sw 0) (setq sw cw))
	(if (<= sh 0) (setq sh ch))
	(defq hv (ifn (get :value (get :hslider *image_scroll*)) 0)
		vv (ifn (get :value (get :vslider *image_scroll*)) 0))
	(list (/ (n2f (- (+ hv (/ (min sw cw) 2)) *margin*)) *zoom*) (/ (n2f (- (+ vv (/ (min sh ch) 2)) *margin*)) *zoom*)))

(defun board-resized ()
	;the board is another size on the screen, the document's or the zoom
	;has changed. New canvases of that size take the place of those there
	;were, and everything is drawn again. From the back:
	;	*paper*		the paper, drawn when it changes and not else
	;	*committed*	the document, one picture, clear where nothing is.
	;				It is not drawn again while a thing is moved or drawn
	;	*flight*	what is in flight: a thing as it is moved, a line as
	;				it is drawn. It is in front of the document's, or
	;				put behind it for what was taken to the back
	;	*overlay*	the handles and the instruments, which are drawn as
	;				they are and are in no other picture, with the
	;				margin round it
	(bind '(w h) (setq *canvas_size* (canvas-size)))
	;the pixels of the one the nodes can reach are let go of by name, they
	;last till then, whatever becomes of this task
	(when *committed* (. *committed* :sub) (. *committed* :free))
	(each (# (if %0 (. %0 :sub))) (list *overlay* *flight_clip* *paper*))
	;the pixels of the document's are shared, if the host has that, for the nodes
	(setq *committed* (ifn (canvas-shared w h 1) (Canvas w h 1))
		*overlay* (Canvas (+ w *margin* *margin*) (+ h *margin* *margin*) 1)
		*flight* (Canvas w h 1) *paper* (Canvas w h 1) *paper_dirty* :t *flight_used* :nil *flight_back* :nil
		*flight_clip* (View) *flight_at* :nil)
	;a view with no colour of its own has its parent's, and covers what is under it
	(def *flight_clip* :color 0)
	(. *flight_clip* :add_child *flight*)
	(. *flight* :set_bounds 0 0 w h)
	(each (# (. %0 :set_canvas_flags +canvas_flag_antialias) (def %0 :color 0))
		(list *committed* *overlay* *flight* *paper*))
	(. *flight* :fill 0)
	;a child that is added goes behind those there are, so the front one first
	(.-> *backdrop* (:add_child *overlay*) (:add_child *flight_clip*) (:add_child *committed*) (:add_child *paper*))
	;the document's, and the two behind it, are in from the corner by the margin
	(each (# (. %0 :set_bounds *margin* *margin* w h)) (list *committed* *flight_clip* *paper*))
	(def *board* :zoom *zoom*)
	(def *board_view* :zoom *zoom* :margin *margin*)
	(defq doc (. *board* :get_doc))
	(. *size_field* :set_text (cat (str (. doc :find :width)) "x" (str (. doc :find :height))))
	(. *board_stack* :change 0 0 (+ w *margin* *margin*) (+ h *margin* *margin*))
	(each (# (. %0 :set_bounds *margin* *margin* w h)) (list *committed* *flight_clip* *paper*))
	(.-> *image_scroll* :layout :dirty_all)
	(. *board* :touch (+ +board_dirty_doc +board_dirty_overlay)))

(defun step-zoom (step)
	;to the next zoom up or down, what is in the middle of the view stays there
	(defq at (ifn (find *zoom* +zooms) (find 1.0 +zooms))
		zoom (elem-get +zooms (max 0 (min (dec (length +zooms)) (+ at step)))))
	(unless (= zoom *zoom*)
		(bind '(mx my) (view-middle))
		(setq *zoom* zoom)
		(board-resized)
		(bind '(sw sh) (. *image_scroll* :get_size))
		(def (get :hslider *image_scroll*) :value (max 0 (- (+ (n2i (* mx *zoom*)) *margin*) (/ sw 2))))
		(def (get :vslider *image_scroll*) :value (max 0 (- (+ (n2i (* my *zoom*)) *margin*) (/ sh 2))))
		(.-> *image_scroll* :layout :dirty_all)))

(defun draw-paper ()
	;what is behind the document to work on, not part of it, lib/cwb/paper.inc
	(defq doc (. *board* :get_doc))
	(bind '(w h) (canvas-size))
	(cwb-paper *paper* w h (. doc :find :background) *style*
		(max 2 (n2i (* (n2f (. doc :find :grid)) *zoom*)))))

(defun draw-local (m)
	;all of the document, by this task, and how long it took. It is not
	;yet shown
	(defq start (pii-time))
	(. *board* :take_appended)
	;clear where nothing is, the paper is behind it
	(. *committed* :fill 0)
	(. *board* :draw *committed* m)
	(setq *local_ms* (/ (- (pii-time) start) 1000)))

(defun draw-over ()
	;what is over the document, on its canvas, which has the margin round
	;it. The handles, and the box that is dragged out, are only seen
	;where the document is. An instrument or a palette is not of the
	;document, and is seen in the margin too. What is in flight is not
	;here, it has a canvas of its own, (draw-flight)
	(bind '(w h) (canvas-size))
	(. *overlay* :fill 0)
	(. *overlay* :set_clip *margin* *margin* (+ *margin* w) (+ *margin* h))
	(. *board* :draw_overlay *overlay* (over-matrix) :t)
	(. *overlay* :set_clip 0 0 (+ w *margin* *margin*) (+ h *margin* *margin*))
	(. *board* :draw_actors *overlay* (over-matrix)))

(defun flight-shift ()
	; (flight-shift) -> :nil | (x y)
	;has what is in flight only been moved, all of it by the same, since
	;it was drawn on its canvas: then how far, in pixels of the canvas. A
	;thousand things dragged are then drawn once, as a hand takes them, and
	;not again each time they move. Only while a hand has hold of them, when
	;nothing else of them is changing, and nothing is being drawn
	(when (and *flight_at* (= (. (get :temp *board*) :size) 0)
			(or (. (get :surface *board*) :holding) (/= (. (get :handles *board*) :held) 0)))
		(bind '(zoom ids ms) *flight_at*)
		(defq now (. *board* :selected_items))
		(when (and (= zoom *zoom*) (nempty? now) (= (length now) (length ids)))
			(defq ident (const (fixeds 1.0 0.0 0.0 0.0 1.0 0.0))
				m0 (ifn (elem-get (first now) +cwb_m) ident) o0 (ifn (first ms) ident)
				dx (- (elem-get m0 2) (elem-get o0 2)) dy (- (elem-get m0 5) (elem-get o0 5)))
			(if (every (lambda (item id m)
					(defq n (ifn (elem-get item +cwb_m) ident) o (ifn m ident))
					(and (= (elem-get item +cwb_id) id)
						(= (elem-get n 0) (elem-get o 0)) (= (elem-get n 1) (elem-get o 1))
						(= (elem-get n 3) (elem-get o 3)) (= (elem-get n 4) (elem-get o 4))
						(= (- (elem-get n 2) (elem-get o 2)) dx) (= (- (elem-get n 5) (elem-get o 5)) dy)))
					now ids ms)
				(list (n2i (floor (+ (* dx *zoom*) 0.5))) (n2i (floor (+ (* dy *zoom*) 0.5))))))))

(defun draw-flight ()
	; (draw-flight) -> :t | :nil
	;what is in flight, on its canvas, which is put in front of the
	;document's or behind it, as the board says. Is the canvas not what it
	;was: it has something on it now, or had and has not
	(defq back (get :float_back *board*) flying (. *board* :in_flight?)
		down (cat *landing* *landed*))
	(when (and flying (not (eql back *flight_back*)))
		;the three that are the size of the document are taken out and put
		;back in the order they are now in. One added goes behind those there
		(setq *flight_back* back)
		(each (# (. %0 :sub)) (list *flight_clip* *committed* *paper*))
		(each (# (. *backdrop* :add_child %0)) (if back (list *committed* *flight_clip* *paper*) (list *flight_clip* *committed* *paper*)))
		(bind '(w h) (canvas-size))
		(each (# (. %0 :set_bounds *margin* *margin* w h)) (list *committed* *flight_clip* *paper*))
		(. *backdrop* :dirty_all))
	(bind '(w h) (canvas-size))
	(defq shift (if (and flying (empty? down)) (catch (flight-shift) :t)))
	(cond
		((list? shift)
			;only moved: the canvas is put where they now are, as it is
			(unless (eql (str shift) (str (slice (. *flight* :get_bounds) 0 2)))
				(. *flight* :set_bounds (first shift) (second shift) w h)
				(. *flight_clip* :dirty_all))
			:nil)
		((or flying *flight_used* (nempty? down))
		(unless (eql (str (. *flight* :get_bounds)) (str (list 0 0 w h)))
			(. *flight* :set_bounds 0 0 w h)
			(. *flight_clip* :dirty_all))
		(. *flight* :fill 0)
		(when (nempty? down)
			(setq down (cwb-id-set down))
			(each (lambda ((name flags items))
				(unless (bits? flags 1) (cwb-draw-items *flight* (cwb-pick items down) (view-matrix))))
				(cwb-layers (. *board* :get_doc))))
		(if flying (. *board* :draw_flight *flight* (view-matrix)))
		;what is on it, and where each thing was when it was put there
		(defq now (if flying (. *board* :selected_items) (list)))
		(setq *flight_used* (or flying (nempty? down))
			*flight_at* (if (and (nempty? now) (empty? down))
				(list *zoom* (map (# (elem-get %0 +cwb_id)) now) (map (# (elem-get %0 +cwb_m)) now))))
		:t)))

(defun farm? ()
	;is the document one for the nodes to draw: it took this task a while
	;last time, the pixels are where they can reach, and it has not kept
	;going wrong
	(and (> *local_ms* *farm_ms*) (< *farm_fails* 3) (/= (canvas-key *committed*) 0)))

(defun redraw ()
	;draw what has changed. All of the document; or only what was put on
	;top of it, on what is there; and what is over it
	;a paper that is another size, made so or put back by an undo, has
	;new canvases first
	(unless (eql (str (canvas-size)) (str *canvas_size*)) (board-resized))
	(defq m (view-matrix) doc_dirty (. *board* :dirty? +board_dirty_doc)
		append_dirty (. *board* :dirty? +board_dirty_append) show :nil show_over :nil show_under :nil)
	;the paper, when it is another paper
	(defq show_paper *paper_dirty*)
	(when *paper_dirty*
		(setq *paper_dirty* :nil)
		(draw-paper))
	;what keeps the nodes' copies in step is told of every change
	(if (or doc_dirty append_dirty) (. *stripes* :note))
	;what has been let go of since this last looked is landing
	(when doc_dirty
		(defq now (get :floating *board*) held (cwb-id-set now))
		(setq *landing* (cat *landing* (filter (# (not (cwb-id? held %0))) *flown*)) *flown* (cat now)))
	(cond
		((and *framing* (or doc_dirty append_dirty))
			;the nodes are drawing it as it was, it is drawn again when they have
			(setq *again* :t))
		((and doc_dirty (farm?) (. *stripes* :cheap?)
				;they draw the shapes and no paper, on pixels made clear for them.
				;What shows is as it was till they have done
				(progn (. *committed* :fill 0) (. *stripes* :frame *committed* *zoom* :nil)))
			;the nodes draw it, (frame-done) when they have. What was
			;landing is in this frame
			(. *board* :take_appended)
			(setq *framing* :t *landed* *landing* *landing* (list)))
		(doc_dirty
			(draw-local m)
			(setq show :t *landing* (list) *landed* (list))
			;the nodes are started, or brought in step, for the next time
			(if (farm?) (.-> *stripes* :start :warm)))
		(append_dirty
			(cwb-draw-items *committed* (. *board* :take_appended) m)
			(setq show :t)))
	(when (. *board* :dirty? +board_dirty_overlay)
		(draw-over)
		(if (draw-flight) (setq show_under :t))
		(sync-ui)
		(setq show_over :t))
	;shown last, when all else is done
	(if show (. *committed* :swap +swap_write))
	(if show_paper (. *paper* :swap +swap_write))
	(if show_under (. *flight* :swap +swap_write))
	(if show_over (. *overlay* :swap +swap_write)))

(defun frame-done (said)
	;what the stripes said of what came to them: :done, the nodes have
	;drawn a frame, :failed, one of them could not, or :warm or :nil
	(case said
		(:done
			(setq *framing* :nil *farm_fails* 0)
			;a document that is now small is this task's again
			(if (< (get :drawn *stripes*) *farm_shapes*) (setq *local_ms* 0))
			(when *again*
				(setq *again* :nil)
				(. *board* :touch +board_dirty_doc))
			;what landed in that frame is in the document's picture now,
			;and is no longer drawn with what is in flight
			(when (nempty? *landed*)
				(setq *landed* (list))
				(. *board* :touch +board_dirty_overlay))
			(. *committed* :swap +swap_write))
		(:failed
			;this task draws it, and the nodes are left alone if they keep at it
			(setq *framing* :nil *again* :nil *farm_fails* (inc *farm_fails*))
			(if (>= *farm_fails* 3) (. *stripes* :close))
			(setq *landing* (list) *landed* (list))
			(. *board* :touch +board_dirty_overlay)
			(draw-local (view-matrix))
			(. *committed* :swap +swap_write))))

(defun frame-watch ()
	;a frame the nodes have had for three seconds is not coming. They are
	;let go, this task draws it, and they are started again when next wanted
	(when (and *framing* (> (- (pii-time) (get :stamp *stripes*)) 3000000))
		(. *stripes* :close)
		(frame-done :failed)))

(defun board-save (file)
	(setq *file* (cat (slice file 0 (if (defq i (rfind "." file)) (dec i) -1)) ".cwb"))
	(. (. *board* :get_doc) :insert :style *style*)
	;the paper is made the size of what is on it, so the file, shown as a
	;picture, is all that was drawn and no more
	(. *board* :fit)
	(cwb-save (. *board* :get_doc) (file-stream *file* +file_open_write))
	(board-resized))

(defun board-load (file)
	(when (and (ends-with ".cwb" file) (defq doc (cwb-load (file-stream file))))
		(setq *file* file)
		(. *board* :set_doc doc)
		;the paper is the size of what is on it, whatever size the file said
		(. *board* :fit)
		(. *board* :forget)
		;it is worked on as it was last
		(when (defq at (find (. doc :find :style) *styles*))
			(setq *style* (elem-get *styles* at))
			(. *style_toolbar* :set_selected at))
		(board-resized)))

;import actions and bindings
(import "./actions.inc")
(import "./config.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	;the mailboxes of the stripes are waited on after this task's own
	(defq select (cat (task-mboxes +select_size) (. *stripes* :mboxes)) *id* :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
	;a corner keeps the shape of what it sizes, till the lock is put off
	(toolbar-states *snap_toolbar* (list *snap* *snap_angle* (get :keep_shape *board*)))
	(board-resized)
	;as it was when it was last closed, if it has been: where it was and
	;how big, as far as that is on the screen there now is
	(defq config (catch (config-load) (progn (prin _) (print) :t))
		place (if (list? config) (apply view-fit (slice config 0 4)) (apply view-locate (. *window* :pref_size))))
	(bind '(x y w h) place)
	(gui-add-front-rpc (. *window* :change x y w h))
	;it opens where the view was, or on the corner of the document, a
	;little of what is round it showing
	(def (get :hslider *image_scroll*) :value (max 0 (if (list? config) (elem-get config 4) (- *margin* 24))))
	(def (get :vslider *image_scroll*) :value (max 0 (if (list? config) (elem-get config 5) (- *margin* 24))))
	(.-> *image_scroll* :layout :dirty_all)

	;main event loop
	(mail-timeout (elem-get select +select_timer) rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((>= idx +select_size)
				;from the nodes that draw in stripes
				(frame-done (. *stripes* :handle (- idx +select_size) *msg*)))
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem-get select +select_timer) rate 0)
				;the words that are put down are those in the text field now
				(def *board* :text (. *text_field* :get_text))
				;what is on the board that moves by itself is told the time
				(. *board* :tick (pii-time))
				(redraw)
				;once a second the nodes that draw are looked to
				(when (> (setq *kept* (inc *kept*)) 60)
					(setq *kept* 0)
					(frame-watch)
					(. *stripes* :keep)))
			((= idx +select_picker)
				;save/load picker response
				(setq *msg* (trim *msg*))
				(mail-send *picker_mbox* "")
				(setq *picker_mbox* :nil)
				(cond
					;closed picker
					((eql *msg* ""))
					(*picker_mode* (board-save *msg*))
					(:t (board-load *msg*))))
			;must be gui event to main mailbox
			((. *window* :dispatch *msg*))
			(:t ;gui event
				(. *window* :event *msg*))))
	;close window
	(catch (config-save) (progn (prin _) (print) :t))
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(. *stripes* :close)
	(gui-sub-rpc *window*)
	(if *committed* (. *committed* :free))
	(profile-report "Whiteboard App"))
