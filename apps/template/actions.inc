;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./utils.inc")
(import "./undo.inc")
(import "./clipboard.inc")
(import "./ui.inc")

(defq
*event_map* (scatter (Fmap)
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_undo action-undo
	+event_redo action-redo
	+event_rewind action-rewind
	+event_cut action-cut
	+event_copy action-copy
	+event_paste action-paste
	+event_button_1 action-button-1
	+event_button_2 action-button-2
	+event_button_3 action-button-3
	+event_button_4 action-button-4)

*key_map* (scatter (Fmap)
	(ascii-code "1") action-button-1
	(ascii-code "2") action-button-2
	(ascii-code "3") action-button-3
	(ascii-code "4") action-button-4)

*key_map_shift* (scatter (Fmap))

*key_map_control* (scatter (Fmap)
	(ascii-code "z") action-undo
	(ascii-code "Z") action-redo
	(ascii-code "x") action-cut
	(ascii-code "c") action-copy
	(ascii-code "v") action-paste
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)
