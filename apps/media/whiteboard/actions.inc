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
	+event_zoom_in action-zoom-in
	+event_zoom_out action-zoom-out
	+event_undo action-undo
	+event_redo action-redo
	+event_new action-new
	+event_load action-load
	+event_save action-save
	+event_clear action-clear
	+event_style action-style
	+event_snap action-snap
	+event_snap_angle action-snap-angle
	+event_radius action-radius
	+event_ink action-ink
	+event_mode action-mode
	+event_ruler action-ruler
	+event_protractor action-protractor
	+event_set_square action-set-square
	+event_arc action-arc
	+event_group action-group
	+event_ungroup action-ungroup
	+event_delete action-delete
	+event_duplicate action-duplicate
	+event_to_front action-to-front
	+event_to_back action-to-back
	+event_align_left action-align-left
	+event_align_hcenter action-align-hcenter
	+event_align_right action-align-right
	+event_align_top action-align-top
	+event_align_vcenter action-align-vcenter
	+event_align_bottom action-align-bottom
	+event_size action-size
	+event_text action-text)

*key_map* (scatter (Fmap)
	+char_backspace action-delete
	+char_delete action-delete
	+char_esc action-select-none)

*key_map_shift* (scatter (Fmap))

*key_map_control* (scatter (Fmap)
	(ascii-code "z") action-undo
	(ascii-code "Z") action-redo
	(ascii-code "a") action-select-all
	(ascii-code "d") action-duplicate
	(ascii-code "g") action-group
	(ascii-code "G") action-ungroup
	(ascii-code "s") action-save
	(ascii-code "o") action-load
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control* action-style action-mode))
(env-pop)
