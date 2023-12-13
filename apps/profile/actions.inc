;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (Fmap-kv
	+event_close action-close
	+event_hvalue action-hvalue
	+event_clear action-clear
	+event_clear_all action-clear-all)

*key_map* (Fmap-kv
	(ascii-code "x") action-clear)

*key_map_shift* (Fmap-kv
	(ascii-code "X") action-clear-all)

*key_map_control* (Fmap-kv)
)

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)
