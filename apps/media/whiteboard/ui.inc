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
(defun teacher-pressed? ()
	(cond
		;a board that joined another's is no teacher of it
		((eql *share* :joined) :nil)
		((and (str? (defq msg (get '*msg*))) (>= (length msg) +ev_msg_action_size)
				(defq view (. *window* :find_id (getf msg +ev_msg_action_source_id)))
				(defq by (get :pressed_by view)))
			(undef view :pressed_by)
			(bind '(id kind buttons) by)
			(= (. *board* :user_of (ptr-event id kind buttons 0 0)) +board_teacher))
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
;changed." A pick binds what pressed it, kept on the button by the window,
;and the bar shows no pick of its own after. What a pick is said by no
;press, a script, is the board's own, for any device bound to nothing
(defun bind-button (button key value)
	; (bind-button button key value) -> :nil | user
	;what pressed the button is bound to the value, if a device did
	(when (defq by (get :pressed_by button))
		(undef button :pressed_by)
		(bind '(id kind buttons) by)
		;all the contacts of touch are the one device, with no buttons
		(. (. *board* :get_bindings) :set kind (ptr-device id) buttons key value)
		;drawn again, and the dots of the bars with it, (sync-ui)
		(. *board* :touch +board_dirty_overlay)
		(. *board* :user_of (ptr-event id kind buttons 0 0))))

(defun bind-pressed (bar key value)
	; (bind-pressed bar key value) -> :nil | user
	(defq button (elem-get (. bar :children) (. bar :get_selected)))
	(. bar :set_selected -1)
	(bind-button button key value))

(defun toggle-pressed (index key)
	; (toggle-pressed index key) -> :nil | user
	;a button of the snap bar binds what pressed it to the other of what it
	;has. Chris: "per device, same as ink and width"
	(defq button (elem-get (. *snap_toolbar* :children) index))
	(when (defq by (get :pressed_by button))
		(bind '(id kind buttons) by)
		;both buttons, the middle, both to the other of the main's
		(bind '(snap angle keep) (. *board* :pointer_settings (ptr-main (ptr-event id kind buttons 0 0))))
		(bind-button button key (not (case key (:snap (/= snap 0.0)) (:snap_angle (/= angle 0.0)) (:t keep))))))

;the pens, each an ink. A device's main button's pen outlines and strokes,
;its second's fills, Chris: "we use the left button bound pen to outline
;and the right button bound pen to fill"
(defun press-event (button)
	; (press-event button) -> :nil | event
	;the device that pressed the button, as a pointer event
	(if (defq by (get :pressed_by button))
		(progn (bind '(id kind buttons) by) (ptr-event id kind buttons 0 0))))

(defun pen-held (event)
	; (pen-held event) -> num
	;the number of the pen the slot of the press holds, by its main role,
	;(ptr-main), or the board's, pressed by no device
	(ifn (if event (. *board* :pen_of (ptr-main event))) (get :pen *board*)))

(defun mine-selected (event)
	; (mine-selected event) -> ids
	;what the user of the device has picked up, or, pressed by no device,
	;all that is
	(if event (. *board* :selected_by (. *board* :user_of event)) (. *board* :get_selected)))

(defun ink-bind (button at)
	;with things picked up, their outline or their fill by the button,
	;(ptr-part), takes the pen's ink and no pen is changed; with none, what
	;pressed the button holds the pen. Chris: "If you have no selected items
	;than I think you are setting the ink of the pen, but if you have
	;selected items you changing there colours", "2 users can select
	;seperate item, AND there devices left/right color THEIR slected items"
	(defq event (press-event button))
	(cond
		((nempty? (defq mine (mine-selected event)))
			(undef button :pressed_by)
			(. *board* :paint mine (if event (ptr-part event) :stroke) (elem-get *palette* at)))
		((bind-button button :pen at))
		((def *board* :pen at))))

(defun pens-shown ()
	;the buttons and the rings show the pens as they are
	(each (const pen-show) (. *ink_toolbar* :children) *palette*)
	(each (# (def %0 :wedges :nil)) (palettes *board*))
	(. *board* :touch +board_dirty_overlay))

(defun pens-refill (inks)
	;the pens have these inks, each device holds the pen it did
	(each (# (elem-set *palette* (!) %0)) inks)
	(pens-shown))

(defun pen-refill (at color)
	;one pen has another ink
	(elem-set *palette* at color)
	(pens-shown))

;the swatches, menus on the board, lib/cwb/swatches.inc, as many as are
;wanted, Chris: "each pupile can have one, there is no limit"
(defun swatch-pick (on event what val)
	;a tap on an ink fills the pen the button that tapped holds with it,
	;Chris: "that value bcome the ink in the current pen/marker". A colour
	;keeps how see through the pen was, a marker stays a marker, and how
	;see through keeps its colour. With things picked up by that user,
	;they take it, by the button, and no pen changes
	(defq held (ptr-main event) at (. *board* :pen_of held) was (. *board* :ink_of held)
		alpha (if (board-none? was) 0xff000000 (logand was 0xff000000))
		color (case what
			(:color (logior (logand val 0xffffff) alpha))
			(:alpha (logior (logand was 0xffffff) (<< val 24)))
			(:t 0))
		;wholly see through is none, whatever its colour
		color (if (board-none? color) 0 color))
	(cond
		;things picked up take it, the pens are as they were: a colour at
		;how see through each was, how see through in the colour each was
		((nempty? (defq mine (mine-selected event)))
			;the lambdas find the value by name as they are called, it has
			;one that nothing between could have
			(defq swatch_val val)
			(. *board* :paint mine (ptr-part event) 0 (case what
				(:color (lambda (old) (logior (logand swatch_val 0xffffff) (if (board-none? old) 0xff000000 (logand old 0xff000000)))))
				(:alpha (lambda (old) (if (board-none? (defq c (logior (logand old 0xffffff) (<< swatch_val 24)))) 0 c)))
				(:t (lambda (old) 0)))))
		((num? at) (pen-refill at color))
		;a slot that holds an ink of its own, as a script sets one
		((bind '(id kind buttons &ignore) event)
			(. (. *board* :get_bindings) :set kind (ptr-device id) buttons :color color)))
	(. *board* :touch +board_dirty_overlay))

(defun swatches-close ()
	(each (# (. %0 :close)) (swatches-on *board*)))

(defun swatches-open (x y)
	;about a point
	(. (. *board* :get_stage) :add (Swatches *board* x y (const swatch-pick)))
	(. *board* :touch +board_dirty_overlay))

(defun action-swatches ()
	;from the toolbar, in the middle of what is seen, as a floating menu is
	(apply (const swatches-open) (view-middle)))

(defun action-ink ()
	;the presser holds the pen, or what it has picked up takes its ink. A
	;pen pressed by what holds it already opens the swatches, to fill it
	;from, Chris: "We can click twice or click the pallet icon to bring up
	;the selector"
	(defq at (. *ink_toolbar* :get_selected) button (elem-get (. *ink_toolbar* :children) at))
	(. *ink_toolbar* :set_selected -1)
	(cond
		((and (defq event (press-event button)) (= (pen-held event) at))
			(undef button :pressed_by)
			(action-swatches))
		((ink-bind button at))))

(defun action-radius ()
	;with things picked up, their width, and the pens are as they were;
	;with none, what pressed holds the width
	(defq at (. *radius_toolbar* :get_selected) width (elem-get *widths* at)
		button (elem-get (. *radius_toolbar* :children) at) event (press-event button))
	(. *radius_toolbar* :set_selected -1)
	(cond
		((nempty? (defq mine (mine-selected event)))
			(undef button :pressed_by)
			(. *board* :style_ids mine :width width))
		((bind-button button :width width))
		((def *board* :width width))))

(defun action-mode ()
	(defq mode (elem-get *modes* (. *mode_toolbar* :get_selected)))
	(unless (defq user (bind-pressed *mode_toolbar* :mode mode))
		(def *board* :mode mode))
	;out of select mode, what was selected is let go of. It has no handles
	;to show that it is, and the colours and widths would change it, Chris:
	;"I'm not in select mode on left/right button and still see selected
	;shapes". Each user's own, (. board :settle_select)
	(. *board* :settle_select user)
	(. *board* :touch +board_dirty_overlay))

(defun action-style ()
	(setq *style* (elem-get *styles* (. *style_toolbar* :get_selected)))
	(. (. *board* :get_doc) :insert :style *style*)
	;the paper is another paper, the document on it is as it was
	(setq *paper_dirty* :t)
	(. *board* :touch +board_dirty_overlay))

;Snap, snap of angles and keep shape are each device's, a press turns over
;what the presser has. No press, a key or a script, turns over the board's,
;which a device bound to none of its own has
(defun action-snap ()
	(unless (toggle-pressed 0 :snap)
		(setq *snap* (not *snap*))
		(def *board* :snap (if *snap* (snap-gap) 0.0))))

(defun action-snap-angle ()
	(unless (toggle-pressed 1 :snap_angle)
		(setq *snap_angle* (not *snap_angle*))
		(def *board* :snap_angle (if *snap_angle* (const (/ +fp_pi 12.0)) 0.0))))

(defun action-keep-shape ()
	;a corner that is dragged keeps the shape of what it sizes, locked, or
	;goes where it is put, free, and the thing is made wider or taller
	(unless (toggle-pressed 2 :keep_shape)
		(def *board* :keep_shape (not (get :keep_shape *board*)))))

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
