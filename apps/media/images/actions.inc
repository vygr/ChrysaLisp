;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (scatter (Fmap)
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_file_leaf_action action-file-leaf-action)

*key_map* (scatter (Fmap))

*key_map_shift* (scatter (Fmap))

*key_map_control* (scatter (Fmap)
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)