;;;;;;;;;;;;;;;;;;;;;;;
; host GUI driver event
;;;;;;;;;;;;;;;;;;;;;;;

;what a host GUI driver gives the GUI service for (gui-event), the same
;from every driver, see src/host/gui_event.h

(enums +gui_ev 0
	(enum none quit shown resized key_down key_up
		mouse_motion mouse_down mouse_up mouse_wheel))

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
	(uint direction))
