;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the view of a board, where the pointers of the GUI become its pointers
;
; It lies over the canvases the board is drawn on, and takes every event
; of the mouse there. Each is made a pointer event, lib/cwb/pointer.inc, in
; the space of the document, and given to the board. The mouse is pointer
; 0, of kind :mouse, with the buttons it has held.
;
; The things of a document are anywhere, and this is a view of some of
; them: :pan is the point of the document at its top left, and :zoom how
; big it is seen. A point of the view is taken to the document by those.
;
; The middle button is not the board's. It moves what is seen, by how
; far the mouse has gone, as far as is asked, there is no edge. So does
; the wheel, or a swipe on a mouse that has a top to swipe. The left and
; the right held together are the middle, for a mouse that has none:
; what the first of them had begun is let go and not kept.
;
; A pen or a finger comes as a pointer event of the GUI, :pointer, with an
; id and a kind of its own, and is given to the board as that. Nothing
; below this knows which it was.
;
; Two fingers down on nothing are not the board's either. They have hold
; of what is seen: moved together they move it, as the middle button
; does, and moved apart or together they zoom it, what is between them
; staying between them. When they let go the zoom comes to rest. The
; board is told to give up what each was doing, a box dragged out, a tap.
;
; What is seen is everybody's, and many work at a board at once. So none
; of this moves it while anybody is drawing a line, (. board :busy?): it
; is done when they have done. A hand that holds a thing does not stop
; it. What it holds is in flight, and stays in the hand: when what is
; seen has moved, every pointer that is down is told to the board again
; where it is, which is now another place of the document, (:resend).
;
; The app is what has the pan and the zoom, and makes them so. It is
; called: :on_pan by how many pixels, :on_wheel by how many more for a
; turn of the wheel, :on_zoom to what zoom and about what point of the
; paper, :on_rest when a zoom is let go of, about what point.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/view/lisp.inc")
(import "gui/backdrop/lisp.inc")
(import "gui/slider/lisp.inc")
(import "lib/cwb/board.inc")


(defclass Board-frame () (Backdrop)
	; (Board-frame) -> board_frame
	;what the paper and the desk round it lie in, in the window, with a
	;bar along the bottom and one up the side. The paper is in the middle
	;of what is left, whatever size that is: with room to spare its own
	;colour shows round it, with too little it is cut off the same each
	;side.
	;
	;The bars do not move the paper. They move what is seen on it, as the
	;wheel does. All of a bar is all there is to see that way, what is
	;drawn and where the view is now, and its thumb is the view: how much
	;of it shows, and where in it. The app sets them, and is called when
	;one is moved, :on_bar. Chris, of bars that moved the paper: "they just
	;don't marry up with what you do in the app."
	;:bare, and it has no bars: a board that is all there is on the screen
	(def this :child :nil :min_width 0 :min_height 0 :on_bar :nil :bare :nil
		:vslider (Slider) :hslider (Slider))
	(each (# (. %0 :connect (. this :get_id)) (. this :add_front %0))
		(list (get :vslider this) (get :hslider this)))

	(defmethod :add_child (child)
		; (. board_frame :add_child child) -> board_frame
		(. child :set_flags +view_flag_subtree +view_flag_subtree)
		(if (defq old_child (get :child this)) (. old_child :sub))
		(. this :add_back (set this :child child)))

	(defmethod :action (data)
		; (. board_frame :action data) -> board_frame
		;a bar has been moved
		(if (defq on_bar (get :on_bar this)) (on_bar))
		this)

	(defmethod :constraint ()
		; (. board_frame :constraint) -> (width height)
		(defq bare (get :bare this))
		(list (+ (ifn (def? :min_width this) 0) (if bare 0 (first (. (get :vslider this) :get_constraint))))
			(+ (ifn (def? :min_height this) 0) (if bare 0 (second (. (get :hslider this) :get_constraint))))))

	(defmethod :layout ()
		; (. board_frame :layout) -> board_frame
		(bind '(w h) (. this :get_size))
		(defq vslider (get :vslider this) hslider (get :hslider this) bare (get :bare this)
			sw (if bare 0 (first (. vslider :get_constraint))) sh (if bare 0 (second (. hslider :get_constraint))))
		(. vslider :set_bounds (- w sw) 0 sw (- h sh))
		(. hslider :set_bounds 0 (- h sh) (- w sw) sh)
		(when (defq child (get :child this))
			(bind '(cw ch) (. child :get_size))
			(. child :set_bounds (/ (- (- w sw) cw) 2) (/ (- (- h sh) ch) 2) cw ch))
		this)
	)

(defclass Board-view (board) (View)
	; (Board-view board) -> board_view
	;it draws nothing, what is under it shows. A view with a colour that is
	;solid is taken, when it is laid out, to cover what is under it, and
	;one with no colour of its own has its parent's. So it has one, of
	;nothing
	(def (. this :set_flags +view_flag_solid +view_flag_solid)
		:board board :zoom 1.0 :margin 0 :held 0 :scroll :nil :drag :nil :color 0
		;the matrix from a point of this to a point of the document, :nil
		;for they are the same. The fingers that are down, each where it is in the view. The two that
		;have hold of what is seen, and how they and it were when they
		;took it. And what the app is called by, see above
		:inv :nil :fingers (Fmap 11) :grip :nil :on_pan :nil :on_wheel :nil :on_zoom :nil :on_rest :nil :on_turn :nil
		;every pointer that is down, the mouse too: its kind, what it
		;holds, where it is in the view and how hard it is pressed
		:down (Fmap 11))

	(defmethod :to_doc (rx ry)
		; (. board_view :to_doc rx ry) -> (x y)
		;a point of the view as a point of the document. The paper is in
		;from the corner of the view by :margin
		;by the matrix the app has for it, the other way to the one it
		;draws by: moved, zoomed and turned
		(Mat3x2-point (get :inv this) rx ry))

	(defmethod :mouse_pointer (event buttons)
		; (. board_view :mouse_pointer event buttons) -> board_view
		;an event of the mouse, with these buttons held, to the board
		(defq rx (getf event +ev_msg_mouse_rx) ry (getf event +ev_msg_mouse_ry))
		(if (/= buttons 0) (. (get :down this) :insert 0 (list :mouse buttons rx ry :nil)) (. (get :down this) :erase 0))
		(bind '(x y) (. this :to_doc rx ry))
		(. (get :board this) :pointers (list (ptr-event 0 :mouse buttons x y)))
		this)

	(defmethod :resend ()
		; (. board_view :resend) -> board_view
		;what is seen has moved. Each pointer that is down is where it was
		;in the view, and that is another place of the document now: the
		;board is told so, as if each had moved there. So a thing that is
		;held stays in the hand that holds it, and a box that is being
		;dragged out still ends at the hand
		(defq events (list))
		(. (get :down this) :each (lambda (id (kind buttons rx ry pressure))
			(bind '(x y) (. this :to_doc rx ry))
			(push events (if pressure (ptr-event id kind buttons x y pressure) (ptr-event id kind buttons x y)))))
		(if (nempty? events) (. (get :board this) :pointers events))
		this)

	(defmethod :mouse_down (event)
		; (. board_view :mouse_down event) -> board_view
		;a button that goes down is told as its number, 1 2 or 3, the
		;buttons held are told as bits, 1 2 and 4
		(defq button (getf event +ev_msg_mouse_buttons)
			held (case button (2 +pev_middle) (3 +pev_right) (:t +pev_left)))
		(def this :held held)
		(cond
			((= held +pev_middle) (. this :pan_from event))
			(:t (. this :mouse_pointer event held)))
		this)

	(defmethod :pan_from (event)
		; (. board_view :pan_from event) -> board_view
		;what is seen is moved from here, where the mouse is on the screen
		(def this :drag (list (getf event +ev_msg_mouse_x) (getf event +ev_msg_mouse_y)))
		this)

	(defmethod :mouse_move (event)
		; (. board_view :mouse_move event) -> board_view
		(defq held (get :held this))
		;as the mouse moves the buttons held are told as bits, the left 1
		;and the right 4. Both, and it is the middle from now till all are
		;up. What the first had begun, a line say, ends here, and if that
		;made a step it is taken back
		(when (and (= (getf event +ev_msg_mouse_buttons) 5) (/= held +pev_middle))
			(when (/= held 0)
				(defq board (get :board this) steps (length (get :undo_stack board)))
				(. this :mouse_pointer event 0)
				(if (> (length (get :undo_stack board)) steps) (. board :undo)))
			(def this :held (setq held +pev_middle))
			(. this :pan_from event))
		(cond
			((= held +pev_middle)
				;by how far the mouse has gone since it was last told
				(when (and (defq on_pan (get :on_pan this)) (defq drag (get :drag this)))
					(bind '(sx sy) drag)
					(defq x (getf event +ev_msg_mouse_x) y (getf event +ev_msg_mouse_y))
					(def this :drag (list x y))
					(unless (or (and (= x sx) (= y sy)) (. (get :board this) :busy?)) (on_pan (- x sx) (- y sy)))))
			((/= held 0) (. this :mouse_pointer event held))
			;with nothing held it is near, not down: the board shows what
			;would catch it
			((get :hover_edges (get :board this)) (. this :mouse_pointer event 0)))
		this)

	(defmethod :mouse_up (event)
		; (. board_view :mouse_up event) -> board_view
		(defq held (get :held this))
		(def this :held 0 :drag :nil)
		(if (and (/= held 0) (/= held +pev_middle)) (. this :mouse_pointer event 0))
		this)

	(defmethod :mouse_wheel (event)
		; (. board_view :mouse_wheel event) -> board_view
		;a turn of the wheel, or a swipe on a mouse that has a top to
		;swipe, moves what is seen, the way a scroll goes: 16 pixels a turn
		(if (and (defq on_wheel (get :on_wheel this)) (not (. (get :board this) :busy?)))
			(on_wheel (* -16 (getf event +ev_msg_wheel_x)) (* 16 (getf event +ev_msg_wheel_y))))
		this)

	(defmethod :pointer (event)
		; (. board_view :pointer event) -> board_view
		;a pen or a finger, as the GUI tells of one, to the board as it is:
		;its own id, its kind, what it has held and how hard it is pressed
		(defq id (getf event +ev_msg_pointer_id) buttons (getf event +ev_msg_pointer_buttons)
			kind (elem-get '(:mouse :pen :eraser :touch) (min 3 (getf event +ev_msg_pointer_kind)))
			rx (getf event +ev_msg_pointer_rx) ry (getf event +ev_msg_pointer_ry)
			pressure (/ (n2f (getf event +ev_msg_pointer_pressure)) 65535.0))
		(if (/= buttons 0) (. (get :down this) :insert id (list kind buttons rx ry pressure)) (. (get :down this) :erase id))
		(bind '(x y) (. this :to_doc rx ry))
		(. (get :board this) :pointers (list (ptr-event id kind buttons x y pressure)))
		(if (= (min 3 (getf event +ev_msg_pointer_kind)) 3) (. this :finger event))
		this)

	(defmethod :finger (event)
		; (. board_view :finger event) -> board_view
		;a finger, after the board has had it. Two that are on nothing
		;take hold of what is seen, and have it till one of them comes up
		(defq fingers (get :fingers this) id (getf event +ev_msg_pointer_id) board (get :board this)
			grip (get :grip this) margin (get :margin this))
		(if (/= (getf event +ev_msg_pointer_buttons) 0)
			(. fingers :insert id (list (getf event +ev_msg_pointer_rx) (getf event +ev_msg_pointer_ry)))
			(. fingers :erase id))
		;one of the two has come up: what is seen is let go of, and if it
		;was zoomed it comes to rest, about where the two last were
		(when (and grip (not (and (. fingers :find (first grip)) (. fingers :find (second grip)))))
			(bind '(ida idb mx my apart zoom zoomed &ignore) grip)
			(if (and zoomed (defq on_rest (get :on_rest this))) (on_rest (- mx margin) (- my margin)))
			(def this :grip (setq grip :nil)))
		(unless grip
			;the first two, in the order they came, that are on nothing
			(defq free (list))
			(. fingers :each (lambda (fid at) (if (. board :pointer_free? fid) (push free fid))))
			(when (>= (length free) 2)
				(sort free (const -))
				(defq ida (first free) idb (second free))
				(. board :pointer_idle ida)
				(. board :pointer_idle idb)
				(bind '(ax ay) (. fingers :find ida))
				(bind '(bx by) (. fingers :find idb))
				;who they are, the middle of them, how far apart they
				;are, the zoom there is, and that it has not been zoomed
				;and the angle the one is at from the other
				(def this :grip (setq grip (list ida idb (/ (+ ax bx) 2) (/ (+ ay by) 2)
					(max 1.0 (sqrt (n2f (+ (* (- bx ax) (- bx ax)) (* (- by ay) (- by ay))))))
					(get :zoom this) :nil (path-angle (n2f (- bx ax)) (n2f (- by ay))))))))
		(when grip
			(bind '(ida idb mx my apart zoom zoomed angle) grip)
			(bind '(ax ay) (. fingers :find ida))
			(bind '(bx by) (. fingers :find idb))
			(defq nx (/ (+ ax bx) 2) ny (/ (+ ay by) 2)
				by_far (/ (sqrt (n2f (+ (* (- bx ax) (- bx ax)) (* (- by ay) (- by ay))))) apart)
				;somebody is drawing: these two do nothing to what is seen
				;till they have done, and then go on from where they are,
				;not from where they were
				on_pan (if (. board :busy?) :nil (get :on_pan this))
				on_zoom (if on_pan (get :on_zoom this)))
			(unless on_pan (elem-set grip 4 (max 1.0 (* by_far apart))) (elem-set grip 5 (get :zoom this)))
			;moved together: what is seen goes with them, by how far the
			;middle of them has gone since it was last told
			(elem-set grip 2 nx)
			(elem-set grip 3 ny)
			(if (and on_pan (not (and (= nx mx) (= ny my))))
				(on_pan (- nx mx) (- ny my)))
			;apart or together: it is zoomed by how far apart they are
			;against how far they were, about the middle of them. Not till
			;that is a twelfth more or less: two fingers that only move a
			;thing do not keep their distance. From there on it is by how
			;far past that they have gone, so it does not jump as it starts
			(when on_zoom
				(cond
					((> by_far 1.08) (setq by_far (/ by_far 1.08)))
					((< by_far 0.92) (setq by_far (/ by_far 0.92)))
					(:t (setq by_far 1.0)))
				(when (or zoomed (/= by_far 1.0))
					(elem-set grip 6 :t)
					(on_zoom (* zoom by_far) (- nx margin) (- ny margin))))
			;turned, the one about the other: past half way to a quarter
			;the board is turned a quarter, a jump, and there it is till
			;they are half way to the next. It has no other angle to be at
			(when (and on_pan (defq on_turn (get :on_turn this)))
				(defq turned (- (path-angle (n2f (- bx ax)) (n2f (- by ay))) angle))
				(while (> turned +fp_pi) (setq turned (- turned +fp_2pi)))
				(while (<= turned (neg +fp_pi)) (setq turned (+ turned +fp_2pi)))
				(when (> (abs turned) (const (/ +fp_pi 4.0)))
					(elem-set grip 7 (+ angle (if (> turned 0.0) +fp_hpi (neg +fp_hpi))))
					(on_turn (if (> turned 0.0) 1 -1)))))
		this)

	(defmethod :mouse_hover (event)
		; (. board_view :mouse_hover event) -> board_view
		;over the board with no button held
		(. this :mouse_pointer event 0))
	)
