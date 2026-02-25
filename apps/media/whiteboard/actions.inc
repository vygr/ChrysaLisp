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
	+event_move action-mode
	+event_clear action-clear
	+event_undo action-undo
	+event_redo action-redo
	+event_save action-save
	+event_load action-load
	+event_min action-minimise
	+event_max action-maximise
	+event_group action-group
	+event_ungroup action-ungroup
	+event_delete action-delete
	+event_align_left action-align-left
	+event_align_vcenter action-align-vcenter
	+event_align_right action-align-right
	+event_align_top action-align-top
	+event_align_hcenter action-align-hcenter
	+event_align_bottom action-align-bottom)

*key_map* (scatter (Fmap))

*key_map_shift* (scatter (Fmap))

*key_map_control* (scatter (Fmap)
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control* action-style
		action-group action-ungroup action-delete action-align-left action-align-vcenter
		action-align-right action-align-top action-align-hcenter action-align-bottom))
(env-pop)