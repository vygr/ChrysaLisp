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

(defun menus-renew ()
	;the floating menus make their wedges again, to have, or not, the one
	;that has the window back, as the board is all of the screen or not
	(each (# (def %0 :wedges :nil)) (palettes *board*))
	(. *board* :touch +board_dirty_overlay))

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
		;the paper is the size of the screen as it is seen, turned or not
		(if (= (logand *turn* 1) 1)
			(.-> doc (:insert :width sh) (:insert :height sw))
			(.-> doc (:insert :width sw) (:insert :height sh)))
		(setq *view_size* :nil)
		(def *board* :on_window (# (setq *full_go* :t)))
		;there are no toolbars to make a menu from: one is made, in the
		;middle, if none is open, and those open have the window wedge
		(menus-renew)
		;laid out at the size that makes it so, to find where the frame
		;is in it, and put so the frame is at the corner of the screen
		(bind '(w h) (. *window* :pref_size))
		(. *window* :change 0 0 w h)
		(bind '(fx fy &ignore) (. *window* :get_relative *image_scroll*))
		(. *window* :change_dirty (neg fx) (neg fy) w h)
		(def *image_scroll* :min_width +min_width :min_height +min_height)
		(board-resized)
		(view-changed)
		(if (empty? (palettes *board*)) (apply (const palette-float) (cat (list *board*) (view-middle))))))

(defun full-window ()
	;it is a window again, where it was, the size it was, and the paper
	;the size it was
	(when *full*
		(bind '(bounds pw ph) *full*)
		(setq *full* :nil)
		(def *image_scroll* :bare :nil)
		(.-> (. *board* :get_doc) (:insert :width pw) (:insert :height ph))
		;the paper is the frame's again, (view-follow), from the document's
		(setq *view_size* :nil)
		(undef *board* :on_window)
		(menus-renew)
		(bind '(x y w h) bounds)
		(. *window* :change_dirty x y w h)
		(. *window* :dirty_all)
		(board-resized)
		(view-changed)))

(defun action-minimise ()
	;as tight round the paper as it can be: the paper and no more. Chris:
	;"I think min should shrink evrything and position so that the window
	;is tight as possible around the paper"
	;and what is on it back in the middle of it: "min size should
	;recenter the canvas in the viewport"
	;the paper is the document at the zoom there is, as much of it as the
	;screen has room for, Chris: "I zoomed, and then hit min, thinking I
	;would be seeing the whole canvas but just with everything 2x larger"
	(full-window)
	(bind '(dw dh) (doc-size))
	(setq *view_size* (list (max 1 (n2i (* (n2f dw) *zoom*))) (max 1 (n2i (* (n2f dh) *zoom*)))))
	(view-home)
	(window-round 0))

(defun action-maximise ()
	;the board is all there is on the screen, or, if it is, a window again
	(if *full* (full-window) (full-screen)))

;only the teacher undoes and redoes, Chris: "only teacher can undo/redo
;the doc". A press says who, a key, or a script, is the teacher's
(defun msg-press ()
	; (msg-press) -> :nil | event
	;what pressed the button the action being done is of, as a pointer
	;event, :nil for a key or a script
	(when (and (str? (defq msg (get '*msg*))) (>= (length msg) +ev_msg_action_size)
			(defq view (. *window* :find_id (getf msg +ev_msg_action_source_id)))
			(defq by (get :pressed_by view)))
		(undef view :pressed_by)
		(bind '(id kind buttons) by)
		(ptr-event id kind buttons 0 0)))

(defun msg-mine ()
	; (msg-mine) -> :nil | ids
	;what the user of the press holds, :nil, all that is selected, for a key
	(if (defq event (msg-press)) (. *board* :selected_by (. *board* :user_of event))))

(defun teacher-pressed? ()
	(cond
		;a board that joined another's is no teacher of it
		((eql *share* :joined) :nil)
		((defq event (msg-press)) (= (. *board* :user_of event) +board_teacher))
		(:t)))
(defun action-undo () (if (teacher-pressed?) (. *board* :undo)))
(defun action-redo () (if (teacher-pressed?) (. *board* :redo)))
(defun action-clear () (. *board* :clear))
(defun action-crop () (. *board* :crop))

(defun action-new ()
	;an empty board, of the size the size field says. It can be undone,
	;and the board that was is back, at the size it was
	(bind '(w h) (field-size))
	(. *board* :new w h)
	;a new document has nothing on it, no instruments, no menus: the board
	;keeps the document it had, emptied, and what it said of them goes too
	(.-> (. *board* :get_doc) (:insert :tools (list)) (:insert :menus (list)))
	(stage-from-doc)
	(setq *file* :nil))

(defun action-size ()
	;the board is the size the size field says, what is on it stays
	(bind '(w h) (field-size))
	(. *board* :resize w h))

(defun action-zoom-in () (step-zoom 1))
(defun action-zoom-out () (step-zoom -1))

;The toolbars are binders, not state. Chris: "the device ID, its buttons,
;and its contact points, and what those combinations are bound to drive
;the app, not some state held in the singular toolbar. The toolbar should
;really be a place for a device/button combo to have its bound action
;changed." What a press picks is the board's to do, (. board :pick), by
;what pressed, kept on the button by the window, and the bar shows no pick
;of its own after. A pick by no press, a key or a script, is the board's own
(defun take-press (bar &optional at)
	; (take-press bar [at]) -> :nil | event
	;what pressed the button a radio bar has picked, and it shows no pick,
	;or button at of a bar of buttons, as a pointer event
	(defq button (elem-get (. bar :children) (ifn at (. bar :get_selected))))
	(unless at (. bar :set_selected -1))
	(when (defq by (get :pressed_by button))
		(undef button :pressed_by)
		(bind '(id kind buttons) by)
		(ptr-event id kind buttons 0 0)))

(defun pens-shown ()
	;the buttons and the rings show the pens as they are, :on_pens
	(each (const pen-show) (. *ink_toolbar* :children) *palette*)
	(each (# (def %0 :wedges :nil)) (palettes *board*))
	(. *board* :touch +board_dirty_overlay))

(defun pens-refill (inks)
	;the pens have these inks, each device holds the pen it did
	(each (# (elem-set *palette* (!) %0)) inks)
	(pens-shown))

(defun pen-refill (at color)
	;one pen has another ink
	(. *board* :fill_pen at color))

;the swatches, menus on the board, lib/cwb/swatches.inc, as many as are
;wanted, Chris: "each pupile can have one, there is no limit"
(defun swatches-close ()
	(each (# (. %0 :close)) (swatches-on *board*)))

(defun swatches-open (x y)
	;about a point
	(. (. *board* :get_stage) :add (Swatches *board* x y))
	(. *board* :touch +board_dirty_overlay))

(defun action-swatches ()
	;from the toolbar, in the middle of what is seen, as a floating menu is
	(apply (const swatches-open) (view-middle)))

(defun action-ink ()
	;a pen, (. board :pick). One pressed by what holds it already opens the
	;swatches, to fill it from, Chris: "We can click twice or click the
	;pallet icon to bring up the selector"
	(defq at (. *ink_toolbar* :get_selected) event (take-press *ink_toolbar*))
	(if (and event (eql (. *board* :pen_of (ptr-main event)) at))
		(action-swatches)
		(. *board* :pick event :pen at)))

(defun action-radius ()
	(defq at (. *radius_toolbar* :get_selected))
	(. *board* :pick (take-press *radius_toolbar*) :width (elem-get *widths* at)))

(defun action-mode ()
	(defq at (. *mode_toolbar* :get_selected))
	(. *board* :pick (take-press *mode_toolbar*) :mode (elem-get *modes* at)))

(defun action-style ()
	(setq *style* (elem-get *styles* (. *style_toolbar* :get_selected)))
	(. (. *board* :get_doc) :insert :style *style*)
	;the paper is another paper, the document on it is as it was
	(setq *paper_dirty* :t)
	(. *board* :touch +board_dirty_overlay))

;Snap, snap of angles and keep shape are each device's, a press turns over
;what the presser has. No press, a key or a script, turns over the board's,
;which a device bound to none of its own has
(defun action-snap () (. *board* :pick (take-press *snap_toolbar* 0) :switch :snap))
(defun action-snap-angle () (. *board* :pick (take-press *snap_toolbar* 1) :switch :snap_angle))
(defun action-keep-shape () (. *board* :pick (take-press *snap_toolbar* 2) :switch :keep_shape))

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
	;with the snap on, the 0 of its marks is on the lines of the paper from the first
	(. instrument :snap_zero)
	(. (. *board* :get_stage) :add instrument)
	(. *board* :touch +board_dirty_overlay))

(defun action-ruler () (add-instrument (apply (const Ruler) (cat (list *board*) (view-middle)))))
(defun action-protractor () (add-instrument (apply (const Protractor) (cat (list *board*) (view-middle)))))
(defun action-full-protractor () (add-instrument (apply (const Circle) (cat (list *board*) (view-middle)))))
(defun action-set-square () (add-instrument (apply (const Setsquare) (cat (list *board*) (view-middle)))))
(defun action-set-square45 () (add-instrument (apply (const Setsquare45) (cat (list *board*) (view-middle)))))
(defun action-menu ()
	;a floating menu in the middle of what is seen, lib/cwb/palette.inc
	(bind '(x y) (view-middle))
	(palette-float *board* x y))

;what is done to a selection is done to what the presser's user holds,
;Chris: "2 users can select seperate item", a key to all that is selected
(defun action-group () (. *board* :group (msg-mine)))
(defun action-ungroup () (. *board* :ungroup (msg-mine)))
(defun action-delete () (. *board* :delete (msg-mine)))
(defun action-duplicate () (. *board* :duplicate :nil :nil (msg-mine)))
(defun action-to-front () (. *board* :order :t (msg-mine)))
(defun action-to-back () (. *board* :order :nil (msg-mine)))
(defun action-align-left () (. *board* :align :left (msg-mine)))
(defun action-align-hcenter () (. *board* :align :hcenter (msg-mine)))
(defun action-align-right () (. *board* :align :right (msg-mine)))
(defun action-align-top () (. *board* :align :top (msg-mine)))
(defun action-align-vcenter () (. *board* :align :vcenter (msg-mine)))
(defun action-align-bottom () (. *board* :align :bottom (msg-mine)))
(defun action-select-all () (. *board* :select_all))
(defun action-select-none () (. *board* :select (list)))
(defun action-turn () (view-turn 1))

(defun action-escape ()
	;out of what one is in: a board that is all of the screen is a window
	;again, and one that is a window has nothing selected. Chris: "Maybe
	;esc key as well comes out of full screen mode"
	(if *full* (full-window) (action-select-none)))

(defun action-save ()
	;a board that joined another's may not keep a copy of it unless that one
	;said so, Chris: "They could take a photo, but they can't save the doc"
	(unless (and (eql *share* :joined) (not *share_may_save*))
		(if *picker_mbox* (mail-send *picker_mbox* ""))
		(mail-send (setq *picker_mode* :t *picker_mbox*
			(open-child "apps/system/files/child.lisp" +kn_call_pin))
			(cat (elem-get select +select_picker) "Save Whiteboard..." "|" "."))))

(defun action-load ()
	(if *picker_mbox* (mail-send *picker_mbox* ""))
	(mail-send (setq *picker_mode* :nil *picker_mbox*
		(open-child "apps/system/files/child.lisp" +kn_call_pin))
		(cat (elem-get select +select_picker) "Load Whiteboard..." "|" "." "|" ".cwb")))
