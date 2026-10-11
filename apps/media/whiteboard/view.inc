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
; The app is what has the pan and the zoom, and makes them so. It is
; called: :on_pan by how many pixels, :on_wheel by how many more for a
; turn of the wheel, :on_zoom to what zoom and about what point of the
; paper, :on_rest when a zoom is let go of, about what point.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/view/lisp.inc")
(import "lib/cwb/board.inc")


(defclass Board-view (board) (View)
	; (Board-view board) -> board_view
	;it draws nothing, what is under it shows. A view with a colour that is
	;solid is taken, when it is laid out, to cover what is under it, and
	;one with no colour of its own has its parent's. So it has one, of
	;nothing
	(def (. this :set_flags +view_flag_solid +view_flag_solid)
		:board board :zoom 1.0 :margin 0 :held 0 :scroll :nil :drag :nil :color 0
		;the point of the document at the top left of the paper. The
		;fingers that are down, each where it is in the view. The two that
		;have hold of what is seen, and how they and it were when they
		;took it. And what the app is called by, see above
		:pan (list 0.0 0.0) :fingers (Fmap 11) :grip :nil :on_pan :nil :on_wheel :nil :on_zoom :nil :on_rest :nil)

	(defmethod :to_doc (rx ry)
		; (. board_view :to_doc rx ry) -> (x y)
		;a point of the view as a point of the document. The paper is in
		;from the corner of the view by :margin
		(defq zoom (get :zoom this) margin (get :margin this))
		(bind '(px py) (get :pan this))
		(list (+ px (/ (n2f (- rx margin)) zoom)) (+ py (/ (n2f (- ry margin)) zoom))))

	(defmethod :mouse_pointer (event buttons)
		; (. board_view :mouse_pointer event buttons) -> board_view
		;an event of the mouse, with these buttons held, to the board
		(bind '(x y) (. this :to_doc (getf event +ev_msg_mouse_rx) (getf event +ev_msg_mouse_ry)))
		(. (get :board this) :pointers (list (ptr-event 0 :mouse buttons x y)))
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
					(unless (and (= x sx) (= y sy)) (on_pan (- x sx) (- y sy)))))
			((/= held 0) (. this :mouse_pointer event held)))
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
		(if (defq on_wheel (get :on_wheel this))
			(on_wheel (* -16 (getf event +ev_msg_wheel_x)) (* 16 (getf event +ev_msg_wheel_y))))
		this)

	(defmethod :pointer (event)
		; (. board_view :pointer event) -> board_view
		;a pen or a finger, as the GUI tells of one, to the board as it is:
		;its own id, its kind, what it has held and how hard it is pressed
		(bind '(x y) (. this :to_doc (getf event +ev_msg_pointer_rx) (getf event +ev_msg_pointer_ry)))
		(. (get :board this) :pointers (list (ptr-event
			(getf event +ev_msg_pointer_id)
			(elem-get '(:mouse :pen :eraser :touch) (min 3 (getf event +ev_msg_pointer_kind)))
			(getf event +ev_msg_pointer_buttons) x y
			(/ (n2f (getf event +ev_msg_pointer_pressure)) 65535.0))))
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
			(bind '(ida idb mx my apart zoom zoomed) grip)
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
				(def this :grip (setq grip (list ida idb (/ (+ ax bx) 2) (/ (+ ay by) 2)
					(max 1.0 (sqrt (n2f (+ (* (- bx ax) (- bx ax)) (* (- by ay) (- by ay))))))
					(get :zoom this) :nil)))))
		(when grip
			(bind '(ida idb mx my apart zoom zoomed) grip)
			(bind '(ax ay) (. fingers :find ida))
			(bind '(bx by) (. fingers :find idb))
			(defq nx (/ (+ ax bx) 2) ny (/ (+ ay by) 2)
				by_far (/ (sqrt (n2f (+ (* (- bx ax) (- bx ax)) (* (- by ay) (- by ay))))) apart))
			;moved together: what is seen goes with them, by how far the
			;middle of them has gone since it was last told
			(elem-set grip 2 nx)
			(elem-set grip 3 ny)
			(if (and (defq on_pan (get :on_pan this)) (not (and (= nx mx) (= ny my))))
				(on_pan (- nx mx) (- ny my)))
			;apart or together: it is zoomed by how far apart they are
			;against how far they were, about the middle of them. Not till
			;that is a twelfth more or less: two fingers that only move a
			;thing do not keep their distance. From there on it is by how
			;far past that they have gone, so it does not jump as it starts
			(when (defq on_zoom (get :on_zoom this))
				(cond
					((> by_far 1.08) (setq by_far (/ by_far 1.08)))
					((< by_far 0.92) (setq by_far (/ by_far 0.92)))
					(:t (setq by_far 1.0)))
				(when (or zoomed (/= by_far 1.0))
					(elem-set grip 6 :t)
					(on_zoom (* zoom by_far) (- nx margin) (- ny margin)))))
		this)

	(defmethod :mouse_hover (event)
		; (. board_view :mouse_hover event) -> board_view
		;over the board with no button held
		(. this :mouse_pointer event 0))
	)
