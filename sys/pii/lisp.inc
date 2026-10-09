;;;;;;;;;;;;;;;;;;;;;;;
; host GUI driver event
;;;;;;;;;;;;;;;;;;;;;;;

;what a host GUI driver gives the GUI service for (gui-event), the same
;from every driver, see src/host/gui_event.h

(enums +gui_ev 0
	(enum none quit shown resized key_down key_up
		mouse_motion mouse_down mouse_up mouse_wheel
		pointer_down pointer_motion pointer_up))

;the kind of a pointer
(enums +gui_kind 0
	(enum mouse pen eraser touch))

(structure +gui_event 0
	(uint type)
	;mouse position, wheel steps, or the new size
	(int x y)
	;the buttons held in a motion, the button in a down or up
	(uint buttons)
	;clicks, in a down or up
	(uint count)
	;key, a USB HID usage number, see lib/consts/scodes.inc
	(uint scode)
	;wheel, 0 normal, 1 flipped
	(uint direction)
	;a pointer event, of a pen or a finger: which one, what it is, and
	;how hard it is pressed, 0 to 65535. Only set in one of those
	(uint id kind pressure))
