;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (scatter (Fmap)
	+event_close action-close
	+event_hvalue action-hvalue
	+event_clear action-clear
	+event_clear_all action-clear-all)

*key_map* (scatter (Fmap)
	(ascii-code "x") action-clear)

*key_map_shift* (scatter (Fmap)
	(ascii-code "X") action-clear-all)

*key_map_control* (scatter (Fmap)))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)
