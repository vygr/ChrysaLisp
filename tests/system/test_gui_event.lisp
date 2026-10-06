(report-header "Host GUI driver event")

(import "sys/pii/lisp.inc")

;the event record must match src/host/gui_event.h, field for field
(assert-list-eq "event types" '(0 1 2 3 4 5 6 7 8 9)
	(list +gui_ev_none +gui_ev_quit +gui_ev_shown +gui_ev_resized +gui_ev_key_down
		+gui_ev_key_up +gui_ev_mouse_motion +gui_ev_mouse_down +gui_ev_mouse_up +gui_ev_mouse_wheel))
(assert-list-eq "event fields" '(0 4 8 12 16 20 24)
	(list +gui_event_type +gui_event_x +gui_event_y +gui_event_buttons
		+gui_event_count +gui_event_scode +gui_event_direction))
(assert-eq "event size" 28 +gui_event_size)

;a mouse down as a driver gives it
(defq msg (setf-> (str-alloc 32)
	(+gui_event_type +gui_ev_mouse_down)
	(+gui_event_x -5) (+gui_event_y 20)
	(+gui_event_buttons 3) (+gui_event_count 2)))
(assert-list-eq "event read back" (list +gui_ev_mouse_down -5 20 3 2)
	(getf-> msg +gui_event_type +gui_event_x +gui_event_y +gui_event_buttons +gui_event_count))
