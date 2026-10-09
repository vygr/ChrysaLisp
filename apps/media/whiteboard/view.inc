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
; which moves under it.
;
; When the host has pens and fingers to tell of, they come in here too,
; each with an id and a kind of its own, and nothing below this changes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/view/lisp.inc")
(import "lib/cwb/board.inc")

(defclass Board-view (board) (View)
	; (Board-view board) -> board_view
	(def (. this :set_flags +view_flag_solid +view_flag_solid)
		:board board :zoom 1.0 :held 0 :scroll :nil :drag :nil)

	(defmethod :pointer (event buttons)
		; (. board_view :pointer event buttons) -> board_view
		;an event of the mouse, with these buttons held, to the board
		(defq zoom (get :zoom this))
		(. (get :board this) :pointers (list (ptr-event 0 :mouse buttons
			(/ (n2f (getf event +ev_msg_mouse_rx)) zoom) (/ (n2f (getf event +ev_msg_mouse_ry)) zoom))))
		this)

	(defmethod :mouse_down (event)
		; (. board_view :mouse_down event) -> board_view
		;a button that goes down is told as its number, 1 2 or 3, the
		;buttons held are told as bits, 1 2 and 4
		(defq button (getf event +ev_msg_mouse_buttons)
			held (case button (2 +pev_middle) (3 +pev_right) (:t +pev_left)))
		(def this :held held)
		(cond
			((= held +pev_middle)
				;where the mouse is on the screen, and where the view is in its scroll
				(when (defq scroll (get :scroll this))
					(def this :drag (list (getf event +ev_msg_mouse_x) (getf event +ev_msg_mouse_y)
						(ifn (get :value (get :hslider scroll)) 0) (ifn (get :value (get :vslider scroll)) 0)))))
			(:t (. this :pointer event held)))
		this)

	(defmethod :mouse_move (event)
		; (. board_view :mouse_move event) -> board_view
		(defq held (get :held this))
		(cond
			((= held +pev_middle)
				(when (and (defq scroll (get :scroll this)) (defq drag (get :drag this)))
					(bind '(sx sy hv vv) drag)
					(def (get :hslider scroll) :value (max 0 (- hv (- (getf event +ev_msg_mouse_x) sx))))
					(def (get :vslider scroll) :value (max 0 (- vv (- (getf event +ev_msg_mouse_y) sy))))
					(.-> scroll :layout :dirty_all)))
			((/= held 0) (. this :pointer event held)))
		this)

	(defmethod :mouse_up (event)
		; (. board_view :mouse_up event) -> board_view
		(defq held (get :held this))
		(def this :held 0 :drag :nil)
		(if (and (/= held 0) (/= held +pev_middle)) (. this :pointer event 0))
		this)

	(defmethod :mouse_hover (event)
		; (. board_view :mouse_hover event) -> board_view
		;over the board with no button held
		(. this :pointer event 0))
	)
