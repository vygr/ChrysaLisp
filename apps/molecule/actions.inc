;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
event_map (xmap-kv
	+event_xrot action-xrot
	+event_yrot action-yrot
	+event_zrot action-zrot
	+event_prev action-prev
	+event_next action-next
	+event_auto action-auto
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise)

key_map (xmap-kv)

key_map_shift (xmap-kv)

key_map_control (xmap-kv
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export (penv)
	event_map key_map key_map_shift key_map_control)
(env-pop)
