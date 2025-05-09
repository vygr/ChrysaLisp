;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (scatter (Fmap)
	+event_ink action-ink
	+event_stroke action-stroke
	+event_close action-close
	+event_style action-style
	+event_radius action-radius
	+event_pen action-mode
	+event_clear action-clear
	+event_undo action-undo
	+event_redo action-redo
	+event_save action-save
	+event_load action-load
	+event_min action-minimise
	+event_max action-maximise)

*key_map* (scatter (Fmap))

*key_map_shift* (scatter (Fmap))

*key_map_control* (scatter (Fmap)
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)
