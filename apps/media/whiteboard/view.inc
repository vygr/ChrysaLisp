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
;
; Two fingers down on nothing are not the board's either. They have hold
; of the view: moved together they move it about in its scroll, as the
; middle button does, and moved apart or together they zoom it, a step of
; the zooms at a time, what is between them staying between them. The
; board is told to give up what each was doing, a box dragged out, a tap.
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
		;the fingers that are down, each where it is on the screen and
		;in the view; the two that have hold of the view, and how it and
		;they were when they took it; the zooms there are, and what to
		;call to have another, (zoom x y), the point of what the scroll
		;shows that is to stay where it is
		:fingers (Fmap 11) :grip :nil :zooms :nil :pinch :nil)

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
		(if (= (min 3 (getf event +ev_msg_pointer_kind)) 3) (. this :finger event))
		this)

	(defmethod :finger (event)
		; (. board_view :finger event) -> board_view
		;a finger, after the board has had it. Two that are on nothing
		;take hold of the view, and have it till one of them comes up
		(defq fingers (get :fingers this) id (getf event +ev_msg_pointer_id) board (get :board this)
			scroll (get :scroll this) grip (get :grip this))
		(if (/= (getf event +ev_msg_pointer_buttons) 0)
			(. fingers :insert id (list (getf event +ev_msg_pointer_x) (getf event +ev_msg_pointer_y)
				(getf event +ev_msg_pointer_rx) (getf event +ev_msg_pointer_ry)))
			(. fingers :erase id))
		(when scroll
			(defq hslider (get :hslider scroll) vslider (get :vslider scroll))
			;one of the two has come up, the view is let go of
			(if (and grip (not (and (. fingers :find (first grip)) (. fingers :find (second grip)))))
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
					(bind '(ax ay &ignore) (. fingers :find ida))
					(bind '(bx by &ignore) (. fingers :find idb))
					;who they are, the middle of them on the screen, how far
					;apart, where the view was in its scroll, and its zoom
					(def this :grip (setq grip (list ida idb (/ (+ ax bx) 2) (/ (+ ay by) 2)
						(max 1.0 (sqrt (n2f (+ (* (- bx ax) (- bx ax)) (* (- by ay) (- by ay))))))
						(ifn (get :value hslider) 0) (ifn (get :value vslider) 0) (get :zoom this))))))
			(when grip
				(bind '(ida idb mx my apart hv vv zoom) grip)
				(bind '(ax ay arx ary) (. fingers :find ida))
				(bind '(bx by brx bry) (. fingers :find idb))
				(defq nx (/ (+ ax bx) 2) ny (/ (+ ay by) 2)
					now (sqrt (n2f (+ (* (- bx ax) (- bx ax)) (* (- by ay) (- by ay))))))
				;moved together: the view goes with them, by how far they
				;have gone on the screen, the view moves under them
				(def hslider :value (max 0 (- hv (- nx mx))))
				(def vslider :value (max 0 (- vv (- ny my))))
				(.-> scroll :layout :dirty_all)
				;apart or together: the zoom that is nearest what they ask
				;for. If it is another, the app makes it so, with what is
				;between them where it was, and the view is moved from there
				(when (and (defq zooms (get :zooms this)) (defq pinch (get :pinch this)))
					(defq want (* zoom (/ now apart)) best (get :zoom this))
					(each (# (if (< (abs (- (/ want %0) 1.0)) (abs (- (/ want best) 1.0))) (setq best %0))) zooms)
					;not for a hair past half way, or it would flicker there,
					;and not till they are a fifth nearer or further than they
					;were: two fingers that move a view do not keep their distance
					(when (and (/= best (get :zoom this))
							(or (/= (get :zoom this) zoom) (> (abs (- (/ now apart) 1.0)) 0.2))
							(< (abs (- (/ want best) 1.0)) (* 0.8 (abs (- (/ want (get :zoom this)) 1.0)))))
						(pinch best (- (/ (+ arx brx) 2) (ifn (get :value hslider) 0)) (- (/ (+ ary bry) 2) (ifn (get :value vslider) 0)))
						;how far apart they were, and the zoom then, are kept: the
						;zoom is by how far apart they are now against that
						(elem-set grip 2 nx)
						(elem-set grip 3 ny)
						(elem-set grip 5 (ifn (get :value hslider) 0))
						(elem-set grip 6 (ifn (get :value vslider) 0))))))
		this)

	(defmethod :mouse_hover (event)
		; (. board_view :mouse_hover event) -> board_view
		;over the board with no button held
		(. this :mouse_pointer event 0))
	)
