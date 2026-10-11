;;;;;;;;;;;;;;;;;;;;;;;;
; user interface actions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun action-close ()
	(setq *running* :nil))

(defun window-round (room)
	;the window is made to show the paper with that much room round it,
	;or as much of that as the screen has. The paper is in the middle of
	;what it shows, it always is
	(bind '(cw ch) (canvas-size))
	(def *image_scroll* :min_width (+ cw room room) :min_height (+ ch room room))
	(bind '(x y w h) (apply view-fit (cat (. *window* :get_pos) (. *window* :pref_size))))
	(. *window* :change_dirty x y w h)
	(def *image_scroll* :min_width +min_width :min_height +min_height)
	(.-> *image_scroll* :layout :dirty_all))

(defun full-screen (&optional sw sh)
	;the board is all there is on the screen, the paper the size of the
	;screen. Chris: "the canvas is made to be the only thing visibale on
	;the screen !" The window is not taken apart for it. It is made big
	;enough that the frame the board lies in is the size of the screen,
	;and put where that frame is on the screen and all else of it is off
	;the edges: its title, its toolbars, its edge. "Why are we removeing
	;the Window widget at all ? It can be just places outside the screen
	;edge and larger than the screen ?" What is done there is done with
	;the palette, and it has one more thing to do while this is so: have
	;the window back
	(unless (and sw sh) (bind '(& & sw sh) (gui-info)))
	(unless *full*
		(defq doc (. *board* :get_doc))
		(setq *full* (list (. *window* :get_bounds) (. doc :find :width) (. doc :find :height)))
		;no bars, and it is to be the size of the screen
		(def *image_scroll* :bare :t :min_width sw :min_height sh)
		(.-> doc (:insert :width sw) (:insert :height sh))
		(def *board* :on_window (# (setq *full_go* :t)))
		;laid out at the size that makes it so, to find where the frame
		;is in it, and put so the frame is at the corner of the screen
		(bind '(w h) (. *window* :pref_size))
		(. *window* :change 0 0 w h)
		(bind '(fx fy &ignore) (. *window* :get_relative *image_scroll*))
		(. *window* :change_dirty (neg fx) (neg fy) w h)
		(def *image_scroll* :min_width +min_width :min_height +min_height)
		(board-resized)
		(view-changed)))

(defun full-window ()
	;it is a window again, where it was, the size it was, and the paper
	;the size it was
	(when *full*
		(bind '(bounds pw ph) *full*)
		(setq *full* :nil)
		(def *image_scroll* :bare :nil)
		(.-> (. *board* :get_doc) (:insert :width pw) (:insert :height ph))
		(undef *board* :on_window)
		(bind '(x y w h) bounds)
		(. *window* :change_dirty x y w h)
		(. *window* :dirty_all)
		(board-resized)
		(view-changed)))

(defun action-minimise ()
	;as tight round the paper as it can be: the paper and no more. Chris:
	;"I think min should shrink evrything and position so that the window
	;is tight as possible around the paper"
	(full-window)
	(window-round 0))

(defun action-maximise ()
	;the board is all there is on the screen, or, if it is, a window again
	(if *full* (full-window) (full-screen)))

(defun action-undo () (. *board* :undo))
(defun action-redo () (. *board* :redo))
(defun action-clear () (. *board* :clear))
(defun action-crop () (. *board* :crop))

(defun action-new ()
	;an empty board, of the size the size field says. It can be undone,
	;and the board that was is back, at the size it was
	(bind '(w h) (field-size))
	(. *board* :new w h)
	(setq *file* :nil))

(defun action-size ()
	;the board is the size the size field says, what is on it stays
	(bind '(w h) (field-size))
	(. *board* :resize w h))

(defun action-zoom-in () (step-zoom 1))
(defun action-zoom-out () (step-zoom -1))

(defun action-ink ()
	(def *board* :color (elem-get *palette* (. *ink_toolbar* :get_selected)))
	;what is selected takes it
	(if (nempty? (. *board* :get_selected))
		(. *board* :style :stroke (get :color *board*))))

(defun action-radius ()
	(def *board* :width (elem-get *widths* (. *radius_toolbar* :get_selected)))
	(if (nempty? (. *board* :get_selected))
		(. *board* :style :width (get :width *board*))))

(defun action-mode ()
	(def *board* :mode (elem-get *modes* (. *mode_toolbar* :get_selected)))
	;out of select mode what was selected is let go of. It has no handles
	;to show that it is, and the colours and widths would change it
	(unless (eql (get :mode *board*) :select) (. *board* :select (list)))
	(. *board* :touch +board_dirty_overlay))

(defun action-style ()
	(setq *style* (elem-get *styles* (. *style_toolbar* :get_selected)))
	(. (. *board* :get_doc) :insert :style *style*)
	;the paper is another paper, the document on it is as it was
	(setq *paper_dirty* :t)
	(. *board* :touch +board_dirty_overlay))

(defun action-snap ()
	(setq *snap* (not *snap*))
	(def *board* :snap (if *snap* (snap-gap) 0.0))
	(toolbar-states *snap_toolbar* (list *snap* *snap_angle* (get :keep_shape *board*))))

(defun action-snap-angle ()
	(setq *snap_angle* (not *snap_angle*))
	(def *board* :snap_angle (if *snap_angle* (const (/ +fp_pi 12.0)) 0.0))
	(toolbar-states *snap_toolbar* (list *snap* *snap_angle* (get :keep_shape *board*))))

(defun action-keep-shape ()
	;a corner that is dragged keeps the shape of what it sizes, locked, or
	;goes where it is put, free, and the thing is made wider or taller
	(def *board* :keep_shape (not (get :keep_shape *board*)))
	(toolbar-states *snap_toolbar* (list *snap* *snap_angle* (get :keep_shape *board*))))

(defun action-text ()
	;the words of the text field are put down where the pen next goes down
	(. *mode_toolbar* :set_selected (find :text *modes*))
	(action-mode))

(defun add-instrument (instrument)
	;in the middle of what shows of the board, each a little off the last
	;so that two are not one on the other
	(defq others (length (filter (const Instrument?) (. (. *board* :get_stage) :get_actors))))
	(bind '(x y) (get :origin instrument))
	(def instrument :origin (list (+ x (* 30.0 (n2f others))) (+ y (* 30.0 (n2f others)))))
	(. (. *board* :get_stage) :add instrument)
	(. *board* :touch +board_dirty_overlay))

(defun action-ruler () (add-instrument (apply (const Ruler) (cat (list *board*) (view-middle)))))
(defun action-protractor () (add-instrument (apply (const Protractor) (cat (list *board*) (view-middle)))))
(defun action-full-protractor () (add-instrument (apply (const Circle) (cat (list *board*) (view-middle)))))
(defun action-set-square () (add-instrument (apply (const Setsquare) (cat (list *board*) (view-middle)))))
(defun action-set-square45 () (add-instrument (apply (const Setsquare45) (cat (list *board*) (view-middle)))))

(defun action-group () (. *board* :group))
(defun action-ungroup () (. *board* :ungroup))
(defun action-delete () (. *board* :delete))
(defun action-duplicate () (. *board* :duplicate))
(defun action-to-front () (. *board* :order :t))
(defun action-to-back () (. *board* :order :nil))
(defun action-align-left () (. *board* :align :left))
(defun action-align-hcenter () (. *board* :align :hcenter))
(defun action-align-right () (. *board* :align :right))
(defun action-align-top () (. *board* :align :top))
(defun action-align-vcenter () (. *board* :align :vcenter))
(defun action-align-bottom () (. *board* :align :bottom))
(defun action-select-all () (. *board* :select_all))
(defun action-select-none () (. *board* :select (list)))

(defun action-escape ()
	;out of what one is in: a board that is all of the screen is a window
	;again, and one that is a window has nothing selected. Chris: "Maybe
	;esc key as well comes out of full screen mode"
	(if *full* (full-window) (action-select-none)))

(defun action-save ()
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(mail-send (setq *picker_mode* :t *picker_mbox*
		(open-child "apps/system/files/child.lisp" +kn_call_pin))
		(cat (elem-get select +select_picker) "Save Whiteboard..." "|" ".")))

(defun action-load ()
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(mail-send (setq *picker_mode* :nil *picker_mbox*
		(open-child "apps/system/files/child.lisp" +kn_call_pin))
		(cat (elem-get select +select_picker) "Load Whiteboard..." "|" "." "|" ".cwb")))
