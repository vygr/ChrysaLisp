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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a pen or a finger, as the GUI service takes it from the host's driver
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;The actions of the GUI service, as it has them, service/gui/app_impl.lisp:
;they are read in as a module and done later by its loop, with what the
;loop has, the screen and the event. No test did one, and the first finger
;on a desktop stopped the GUI: what the action kept, the view each pointer
;went down on, was not to be found from the loop
(defq *env_user* "Guest")
(import "usr/Guest/env.inc")
(import *env_keyboard_map*)
(import "service/gui/actions.inc")
(defq ga_mbox (mail-mbox) *screen* (View) ga_view (View) *mouse_x* 0 *mouse_y* 0 *mouse_id* 0 *mods* 0 *focus* :nil)
(. *screen* :set_bounds 0 0 400 300)
(. ga_view :set_bounds 100 50 200 100)
(. *screen* :add_child ga_view)
(. ga_view :set_owner ga_mbox)
(defun ga-pointer (type id x y buttons)
	;an event of the driver done as the GUI's loop does it, and what was sent to the owner of the view
	(defq msg (setf-> (str-alloc +gui_event_size) (+gui_event_type type) (+gui_event_x x) (+gui_event_y y)
		(+gui_event_id id) (+gui_event_kind +gui_kind_touch) (+gui_event_buttons buttons) (+gui_event_pressure 65535)))
	(defq threw (catch (progn ((. *event_map* :find type)) :nil) (str _)))
	(cond
		(threw)
		((mail-poll (list ga_mbox))
			(defq ev (mail-read ga_mbox))
			(list (getf ev +ev_msg_type) (getf ev +ev_msg_pointer_id) (getf ev +ev_msg_pointer_buttons)
				(getf ev +ev_msg_pointer_rx) (getf ev +ev_msg_pointer_ry)))
		(:t :none)))
;where it is in the view is from where the view was last drawn, which here it has not been
(assert-list-eq "a finger down on a view is told to its owner, as a pointer"
	(list +ev_type_pointer 0x10007 1 150 80) (ga-pointer +gui_ev_pointer_down 0x10007 150 80 1))
(assert-list-eq "moved off the view it is still that view's, it went down there"
	(list +ev_type_pointer 0x10007 1 10 10) (ga-pointer +gui_ev_pointer_motion 0x10007 10 10 1))
(assert-list-eq "and when it comes up" (list +ev_type_pointer 0x10007 0 10 10) (ga-pointer +gui_ev_pointer_up 0x10007 10 10 0))
(assert-eq "after that, off the view, it is not" :none (ga-pointer +gui_ev_pointer_motion 0x10007 10 10 0))
(assert-list-eq "a second finger is its own" (list +ev_type_pointer 0x10008 1 100 50) (ga-pointer +gui_ev_pointer_down 0x10008 100 50 1))
(ga-pointer +gui_ev_pointer_up 0x10008 100 50 0)

;a turn of the wheel belongs to the view it began over: turns that come one
;soon after another go where the first went, wherever the mouse has got to
(defun mouse-type (view rx ry))
(defq *mouse_buttons* 0 *env_wheel_x* 1 *env_wheel_y* 1 ga_other (View) ga_mbox2 (mail-mbox))
(. ga_other :set_bounds 100 180 200 100)
(. *screen* :add_child ga_other)
(. ga_other :set_owner ga_mbox2)
(defun ga-wheel (x y)
	;the mouse at a point and a turn of the wheel there: which of the two views was told of it
	(setq *mouse_x* x *mouse_y* y)
	(defq msg (setf-> (str-alloc +gui_event_size) (+gui_event_type +gui_ev_mouse_wheel) (+gui_event_x 0) (+gui_event_y 1)) out (list))
	((. *event_map* :find +gui_ev_mouse_wheel))
	(each (lambda (mbox name)
		(while (mail-poll (list mbox))
			(if (= (getf (mail-read mbox) +ev_msg_type) +ev_type_wheel) (push out name))))
		(list ga_mbox ga_mbox2) '(:first :second))
	out)
(assert-list-eq "a turn of the wheel over a view is told to it" '(:first) (ga-wheel 150 80))
(assert-list-eq "the mouse moves over another view and a turn comes at once: it is the first view's still, the wheel ran on"
	'(:first) (ga-wheel 150 230))
(assert-list-eq "and the next, while they keep coming" '(:first) (ga-wheel 150 240))
(task-sleep 500000)
(assert-list-eq "after a pause a turn is a new one, for the view the mouse is over" '(:second) (ga-wheel 150 240))
(assert-list-eq "and stays that one's when the mouse goes back and they keep coming" '(:second) (ga-wheel 150 80))
(task-sleep 500000)
(def (penv) '*env_wheel_y* -1)
(setq *env_wheel_y* -1)
(setq *mouse_x* 150 *mouse_y* 80)
(defq msg (setf-> (str-alloc +gui_event_size) (+gui_event_type +gui_ev_mouse_wheel) (+gui_event_x 0) (+gui_event_y 1)))
((. *event_map* :find +gui_ev_mouse_wheel))
(defq ga_turn 0)
(while (mail-poll (list ga_mbox))
	(if (= (getf (defq ga_ev (mail-read ga_mbox)) +ev_msg_type) +ev_type_wheel) (setq ga_turn (getf ga_ev +ev_msg_wheel_y))))
(assert-eq "with the wheel set the other way round, usr/Guest/env.inc, a turn up is a turn down" -1 ga_turn)
