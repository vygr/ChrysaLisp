;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./utils.inc")
(import "./ui.inc")

(defq
event_map (xmap-kv
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_reset action-reset)

key_map (xmap-kv)

key_map_shift (xmap-kv)

key_map_control (xmap-kv
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export (penv)
	event_map key_map key_map_shift key_map_control)
(env-pop)
