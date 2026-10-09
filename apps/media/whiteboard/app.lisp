;debug options
(case :nil
(0 (import "lib/debug/frames.inc"))
(1 (import "lib/debug/profile.inc"))
(2 (import "lib/debug/debug.inc")))

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "./widgets.inc")

;The Whiteboard. What is on the board is a document of shapes,
;lib/cwb/doc.inc, a .cwb file. All that changes it is the board,
;lib/cwb/board.inc, which is given pointers and has methods. This file is
;the window round it: toolbars that call those methods, a view that makes
;the mouse a pointer, ./view.inc, and two canvases the board draws on, the
;document on one and what is over it, what is being drawn or moved, the
;handles, a ruler, on the other.

(enums +select 0
	(enum main picker timer tip))

;the zooms there are, stepped through, so that in and then out is where it was
(defq +zooms ''(0.125 0.25 0.5 0.75 1.0 1.5 2.0 3.0 4.0 6.0 8.0))

(defq *zoom* 1.0 *style* :grid *snap* :nil *snap_angle* :nil *arc* :line
	*file* :nil *picker_mbox* :nil *picker_mode* :nil *running* :t
	*committed* :nil *overlay* :nil rate (/ 1000000 60))

(defun toolbar-states (toolbar states)
	;a button that is on is the brighter
	(defq radio_col (canvas-brighter (get :color toolbar)))
	(each (# (undef (. %0 :dirty) :color)
			(if %1 (def %0 :color radio_col)))
		(. toolbar :children) states))

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
	(setq *committed* (Canvas w h 1) *overlay* (Canvas w h 1))
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
	;what is behind the document to work on, not part of it: its
	;background, or paper if it has none, and lines by the style. The
	;lines are where the grid is, every :grid of the document from its
	;top left, so what snaps to the grid lands on a line
	(defq doc (. *board* :get_doc) back (. doc :find :background)
		gap (max 2 (n2i (* (n2f (. doc :find :grid)) *zoom*))))
	(bind '(w h) (canvas-size))
	(. *committed* :fill (if (= back 0) +paper_col back))
	(. *committed* :set_color +paper_ink)
	(case *style*
		(:grid
			(each (# (. *committed* :fbox %0 0 1 h)) (range gap w gap))
			(each (# (. *committed* :fbox 0 %0 w 1)) (range gap h gap)))
		(:lines
			(each (# (. *committed* :fbox 0 %0 w 1)) (range gap h gap)))
		(:axis
			;the two lines through the middle, on the grid, and marks along them
			(defq cx (* (/ (/ w 2) gap) gap) cy (* (/ (/ h 2) gap) gap))
			(.-> *committed* (:fbox cx 0 1 h) (:fbox 0 cy w 1))
			(each (# (. *committed* :fbox %0 (- cy 3) 1 7)) (range gap w gap))
			(each (# (. *committed* :fbox (- cx 3) %0 7 1)) (range gap h gap)))))

(defun redraw ()
	;draw what has changed. All of the document; or only what was put on
	;top of it, on what is there; and what is over it
	(defq m (view-matrix))
	(cond
		((. *board* :dirty? +board_dirty_doc)
			(. *board* :dirty? +board_dirty_append)
			(. *board* :take_appended)
			(draw-paper)
			(. *board* :draw *committed* m)
			(. *committed* :swap +swap_write))
		((. *board* :dirty? +board_dirty_append)
			(cwb-draw-items *committed* (. *board* :take_appended) m)
			(. *committed* :swap +swap_write)))
	(when (. *board* :dirty? +board_dirty_overlay)
		(. *overlay* :fill 0)
		(. *board* :draw_overlay *overlay* m)
		(each (# (if (Instrument? %0) (. %0 :draw *overlay* m))) (. (. *board* :get_stage) :get_actors))
		(. *overlay* :swap +swap_write)))

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
	(defq select (task-mboxes +select_size) *id* :t)
	(def *window* :tip_mbox (elem-get select +select_tip))
	(board-resized)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	;main event loop
	(mail-timeout (elem-get select +select_timer) rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem-get select +select_timer) rate 0)
				;the words that are put down are those in the text field now
				(def *board* :text (. *text_field* :get_text))
				(redraw))
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
	(gui-sub-rpc *window*)
	(profile-report "Whiteboard App"))
