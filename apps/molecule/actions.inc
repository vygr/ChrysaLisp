;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (Fmap-kv
	+event_xrot action-xrot
	+event_yrot action-yrot
	+event_zrot action-zrot
	+event_prev action-prev
	+event_next action-next
	+event_auto action-auto
	+event_style action-style
	+event_close action-close
	+event_layout action-layout
	+event_min action-minimise
	+event_max action-maximise)

*key_map* (Fmap-kv
	(ascii-code "a") action-auto
	(ascii-code "n") action-next
	(ascii-code "p") action-prev)

*key_map_shift* (Fmap-kv)

*key_map_control* (Fmap-kv
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)
