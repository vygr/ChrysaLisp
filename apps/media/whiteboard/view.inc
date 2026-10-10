;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the view of a board, where the pointers of the GUI become its pointers
;
; It lies over the canvases the board is drawn on, and takes every event
; of the mouse there. Each is made a pointer event, lib/cwb/pointer.inc, in
; the space of the document, a point of the view over the zoom, and given
; to the board. The mouse is pointer 0, of kind :mouse, with the buttons
; it has held.
;
; The middle button is not the board's. It moves the view about in its
; scroll, by how far the mouse has gone on the screen, not in the view,
; which moves under it. The left and the right held together are the
; middle, for a mouse that has none: what the first of them had begun is
; let go and not kept.
;
; A pen or a finger comes as a pointer event of the GUI, :pointer, with an
; id and a kind of its own, and is given to the board as that. Nothing
; below this knows which it was.
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
		:board board :zoom 1.0 :margin 0 :held 0 :scroll :nil :drag :nil :color 0)

	(defmethod :mouse_pointer (event buttons)
		; (. board_view :mouse_pointer event buttons) -> board_view
		;an event of the mouse, with these buttons held, to the board
		;the document is in from the corner of the view by :margin
		(defq zoom (get :zoom this) margin (get :margin this))
		(. (get :board this) :pointers (list (ptr-event 0 :mouse buttons
			(/ (n2f (- (getf event +ev_msg_mouse_rx) margin)) zoom) (/ (n2f (- (getf event +ev_msg_mouse_ry) margin)) zoom))))
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
		;the view is moved about from here: where the mouse is on the
		;screen, and where the view is in its scroll
		(when (defq scroll (get :scroll this))
			(def this :drag (list (getf event +ev_msg_mouse_x) (getf event +ev_msg_mouse_y)
				(ifn (get :value (get :hslider scroll)) 0) (ifn (get :value (get :vslider scroll)) 0))))
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
				(when (and (defq scroll (get :scroll this)) (defq drag (get :drag this)))
					(bind '(sx sy hv vv) drag)
					(def (get :hslider scroll) :value (max 0 (- hv (- (getf event +ev_msg_mouse_x) sx))))
					(def (get :vslider scroll) :value (max 0 (- vv (- (getf event +ev_msg_mouse_y) sy))))
					(.-> scroll :layout :dirty_all)))
			((/= held 0) (. this :mouse_pointer event held)))
		this)

	(defmethod :mouse_up (event)
		; (. board_view :mouse_up event) -> board_view
		(defq held (get :held this))
		(def this :held 0 :drag :nil)
		(if (and (/= held 0) (/= held +pev_middle)) (. this :mouse_pointer event 0))
		this)

	(defmethod :pointer (event)
		; (. board_view :pointer event) -> board_view
		;a pen or a finger, as the GUI tells of one, to the board as it is:
		;its own id, its kind, what it has held and how hard it is pressed
		(defq zoom (get :zoom this) margin (get :margin this) buttons (getf event +ev_msg_pointer_buttons))
		(. (get :board this) :pointers (list (ptr-event
			(getf event +ev_msg_pointer_id)
			(elem-get '(:mouse :pen :eraser :touch) (min 3 (getf event +ev_msg_pointer_kind)))
			buttons
			(/ (n2f (- (getf event +ev_msg_pointer_rx) margin)) zoom) (/ (n2f (- (getf event +ev_msg_pointer_ry) margin)) zoom)
			(/ (n2f (getf event +ev_msg_pointer_pressure)) 65535.0))))
		this)

	(defmethod :mouse_hover (event)
		; (. board_view :mouse_hover event) -> board_view
		;over the board with no button held
		(. this :mouse_pointer event 0))
	)
