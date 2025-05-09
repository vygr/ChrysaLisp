;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (scatter (Fmap)
	+event_xrot action-xrot
	+event_yrot action-yrot
	+event_zrot action-zrot
	+event_auto action-auto
	+event_mode action-mode
	+event_style action-style
	+event_close action-close
	+event_layout action-layout
	+event_min action-minimise
	+event_max action-maximise)

*key_map* (scatter (Fmap)
	(ascii-code "a") action-auto
	(ascii-code "m") action-mode)

*key_map_shift* (scatter (Fmap))

*key_map_control* (scatter (Fmap)
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)
