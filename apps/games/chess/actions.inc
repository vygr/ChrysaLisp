;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (scatter (Fmap)
	+event_close action-close
	+event_reset action-reset
	+event_undo action-undo
	+event_redo action-redo)

*key_map* (scatter (Fmap)
	(ascii-code "r") action-reset
	(ascii-code "u") action-undo)

*key_map_shift* (scatter (Fmap)
	(ascii-code "U") action-redo)

*key_map_control* (scatter (Fmap)
	(ascii-code "z") action-undo
	(ascii-code "Z") action-redo))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)