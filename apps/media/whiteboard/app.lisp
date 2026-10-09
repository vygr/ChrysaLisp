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

(defq *zoom* 1.0 *style* :grid *snap* :nil *snap_angle* :nil *arc* :line
	*file* :nil *picker_mbox* :nil *picker_mode* :nil *running* :t
	*committed* :nil *overlay* :nil rate (/ 1000000 60)
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
		(toolbar-states *snap_toolbar* (list *snap* *snap_angle*))))

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
	(list (/ (n2f (+ hv (/ (min sw cw) 2))) *zoom*) (/ (n2f (+ vv (/ (min sh ch) 2))) *zoom*)))

(defun board-resized ()
	;the board is another size on the screen, the document's or the zoom
	;has changed. Two new canvases of that size take the place of the two
	;there were, and everything is drawn again
	(bind '(w h) (canvas-size))
	(if *committed* (. *committed* :sub))
	(if *overlay* (. *overlay* :sub))
	;the pixels of the first are shared, if the host has that, for the nodes
	(setq *committed* (ifn (canvas-shared w h 1) (Canvas w h 1)) *overlay* (Canvas w h 1))
	(. *committed* :set_canvas_flags +canvas_flag_antialias)
	(. *overlay* :set_canvas_flags +canvas_flag_antialias)
	(def *committed* :color 0)
	(def *overlay* :color 0)
	(.-> *backdrop* (:add_child *committed*) (:add_child *overlay*))
	(def *board* :zoom *zoom*)
	(def *board_view* :zoom *zoom*)
	(defq doc (. *board* :get_doc))
	(. *size_field* :set_text (cat (str (. doc :find :width)) "x" (str (. doc :find :height))))
	(. *board_stack* :change 0 0 w h)
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
		(def (get :hslider *image_scroll*) :value (max 0 (- (n2i (* mx *zoom*)) (/ sw 2))))
		(def (get :vslider *image_scroll*) :value (max 0 (- (n2i (* my *zoom*)) (/ sh 2))))
		(.-> *image_scroll* :layout :dirty_all)))

(defun draw-paper ()
	;what is behind the document to work on, not part of it, lib/cwb/paper.inc
	(defq doc (. *board* :get_doc))
	(bind '(w h) (canvas-size))
	(cwb-paper *committed* w h (. doc :find :background) *style*
		(max 2 (n2i (* (n2f (. doc :find :grid)) *zoom*)))))

(defun draw-local (m)
	;all of the document, by this task, and how long it took. It is not
	;yet shown
	(defq start (pii-time))
	(. *board* :take_appended)
	(draw-paper)
	(. *board* :draw *committed* m)
	(setq *local_ms* (/ (- (pii-time) start) 1000)))

(defun farm? ()
	;is the document one for the nodes to draw: it took this task a while
	;last time, the pixels are where they can reach, and it has not kept
	;going wrong
	(and (> *local_ms* *farm_ms*) (< *farm_fails* 3) (/= (canvas-key *committed*) 0)))

(defun redraw ()
	;draw what has changed. All of the document; or only what was put on
	;top of it, on what is there; and what is over it
	(defq m (view-matrix) doc_dirty (. *board* :dirty? +board_dirty_doc)
		append_dirty (. *board* :dirty? +board_dirty_append) show :nil show_over :nil)
	;what keeps the nodes' copies in step is told of every change
	(if (or doc_dirty append_dirty) (. *stripes* :note))
	(cond
		((and *framing* (or doc_dirty append_dirty))
			;the nodes are drawing it as it was, it is drawn again when they have
			(setq *again* :t))
		((and doc_dirty (farm?) (. *stripes* :cheap?) (. *stripes* :frame *committed* *zoom* *style*))
			;the nodes draw it, (frame-done) when they have
			(. *board* :take_appended)
			(setq *framing* :t))
		(doc_dirty
			(draw-local m)
			(setq show :t)
			;the nodes are started, or brought in step, for the next time
			(if (farm?) (.-> *stripes* :start :warm)))
		(append_dirty
			(cwb-draw-items *committed* (. *board* :take_appended) m)
			(setq show :t)))
	(when (. *board* :dirty? +board_dirty_overlay)
		(. *overlay* :fill 0)
		(. *board* :draw_overlay *overlay* m)
		;what is on the board and not of the document, a ruler, a palette
		(. *board* :draw_actors *overlay* m)
		(sync-ui)
		(setq show_over :t))
	;shown last, when all else is done
	(if show (. *committed* :swap +swap_write))
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
			(. *committed* :swap +swap_write))
		(:failed
			;this task draws it, and the nodes are left alone if they keep at it
			(setq *framing* :nil *again* :nil *farm_fails* (inc *farm_fails*))
			(if (>= *farm_fails* 3) (. *stripes* :close))
			(draw-local (view-matrix))
			(. *committed* :swap +swap_write))))

(defun board-save (file)
	(setq *file* (cat (slice file 0 (if (defq i (rfind "." file)) (dec i) -1)) ".cwb"))
	(. (. *board* :get_doc) :insert :style *style*)
	(cwb-save (. *board* :get_doc) (file-stream *file* +file_open_write)))

(defun board-load (file)
	(when (and (ends-with ".cwb" file) (defq doc (cwb-load (file-stream file))))
		(setq *file* file)
		(. *board* :set_doc doc)
		;it is worked on as it was last
		(when (defq at (find (. doc :find :style) *styles*))
			(setq *style* (elem-get *styles* at))
			(. *style_toolbar* :set_selected at))
		(board-resized)))

;import actions and bindings
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action) (progn (prin _) (print) :t)))

(defun main ()
	;the mailboxes of the stripes are waited on after this task's own
	(defq select (cat (task-mboxes +select_size) (. *stripes* :mboxes)) *id* :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(board-resized)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

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
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(. *stripes* :close)
	(gui-sub-rpc *window*)
	(profile-report "Whiteboard App"))
