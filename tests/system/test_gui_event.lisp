(report-header "Host GUI driver event")

(import "sys/pii/lisp.inc")

;the event record must match src/host/gui_event.h, field for field
(assert-list-eq "event types" '(0 1 2 3 4 5 6 7 8 9)
	(list +gui_ev_none +gui_ev_quit +gui_ev_shown +gui_ev_resized +gui_ev_key_down
		+gui_ev_key_up +gui_ev_mouse_motion +gui_ev_mouse_down +gui_ev_mouse_up +gui_ev_mouse_wheel))
(assert-list-eq "event fields" '(0 4 8 12 16 20 24)
	(list +gui_event_type +gui_event_x +gui_event_y +gui_event_buttons
		+gui_event_count +gui_event_scode +gui_event_direction))
;a pen or a finger is three more kinds of event and three more fields, after
;what was there, where 4 bytes of the record were not used
(assert-list-eq "the events of a pen or a finger come after the mouse's" '(10 11 12)
	(list +gui_ev_pointer_down +gui_ev_pointer_motion +gui_ev_pointer_up))
(assert-list-eq "and its fields after the rest" '(28 32 36)
	(list +gui_event_id +gui_event_kind +gui_event_pressure))
(assert-list-eq "the kinds of pointer" '(0 1 2 3) (list +gui_kind_mouse +gui_kind_pen +gui_kind_eraser +gui_kind_touch))
(assert-eq "event size" 40 +gui_event_size)

;a mouse down as a driver gives it
(defq msg (setf-> (str-alloc 32)
	(+gui_event_type +gui_ev_mouse_down)
	(+gui_event_x -5) (+gui_event_y 20)
	(+gui_event_buttons 3) (+gui_event_count 2)))
(assert-list-eq "event read back" (list +gui_ev_mouse_down -5 20 3 2)
	(getf-> msg +gui_event_type +gui_event_x +gui_event_y +gui_event_buttons +gui_event_count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a pen or a finger, as a window hands it to a view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "usr/env.inc")
(import "gui/lisp.inc")

;a view that takes pointers, and one that only knows a mouse
(defclass Ge-pointer () (View)
	(def this :got (list))
	(defmethod :pointer (event)
		(push (get :got this) (list (getf event +ev_msg_pointer_id) (getf event +ev_msg_pointer_kind)
			(getf event +ev_msg_pointer_buttons) (getf event +ev_msg_pointer_rx) (getf event +ev_msg_pointer_ry)))
		this))
(defclass Ge-mouse () (View)
	(def this :got (list))
	(defmethod :mouse_down (event) (push (get :got this) (list :down (getf event +ev_msg_mouse_buttons) (getf event +ev_msg_mouse_rx))) this)
	(defmethod :mouse_move (event) (push (get :got this) (list :move (getf event +ev_msg_mouse_buttons) (getf event +ev_msg_mouse_rx))) this)
	(defmethod :mouse_up (event) (push (get :got this) (list :up (getf event +ev_msg_mouse_buttons) (getf event +ev_msg_mouse_rx))) this))
(ui-window ge_window ()
	(ui-element ge_pointer (Ge-pointer))
	(ui-element ge_mouse (Ge-mouse)))
(defun ge-send (view id kind buttons rx ry)
	;a pointer event as the GUI service sends one, to the window that has the view
	(. ge_window :event (setf-> (str-alloc +ev_msg_pointer_size)
		(+ev_msg_type +ev_type_pointer) (+ev_msg_target_id (. view :get_id))
		(+ev_msg_pointer_id id) (+ev_msg_pointer_kind kind) (+ev_msg_pointer_buttons buttons)
		(+ev_msg_pointer_pressure 65535) (+ev_msg_pointer_x rx) (+ev_msg_pointer_y ry)
		(+ev_msg_pointer_rx rx) (+ev_msg_pointer_ry ry))))

(ge-send ge_pointer 0x10001 +gui_kind_touch 1 10 20)
(ge-send ge_pointer 0x20001 +gui_kind_pen 1 30 40)
(ge-send ge_pointer 0x10001 +gui_kind_touch 0 12 22)
(assert-list-eq "a view that takes pointers is given each, with its id, its kind and where"
	(list (list 0x10001 +gui_kind_touch 1 10 20) (list 0x20001 +gui_kind_pen 1 30 40) (list 0x10001 +gui_kind_touch 0 12 22))
	(get :got ge_pointer))

;a view that only knows a mouse: the first pointer that is down is its mouse
(ge-send ge_mouse 0x10001 +gui_kind_touch 1 10 20)
(ge-send ge_mouse 0x10002 +gui_kind_touch 1 90 90)
(ge-send ge_mouse 0x10001 +gui_kind_touch 1 15 20)
(ge-send ge_mouse 0x10002 +gui_kind_touch 0 90 90)
(ge-send ge_mouse 0x10001 +gui_kind_touch 0 15 20)
(assert-list-eq "for a view that knows only a mouse, the first finger down is a mouse: down, a move, up"
	'((:down 1 10) (:move 1 15) (:up 0 15)) (get :got ge_mouse))
(clear (get :got ge_mouse))
(ge-send ge_mouse 0x20001 +gui_kind_pen 0 50 50)
(assert-list-eq "a pen that is only near it does nothing" '() (get :got ge_mouse))
(ge-send ge_mouse 0x20001 +gui_kind_pen 4 50 50)
(ge-send ge_mouse 0x20001 +gui_kind_pen 0 50 50)
(assert-list-eq "a pen with the button of its barrel held is the right button" '((:down 4 50) (:up 0 50)) (get :got ge_mouse))

