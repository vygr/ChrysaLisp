;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./ui.inc")

(defq
*event_map* (Fmap-kv
	+event_black action-ink
	+event_white action-ink
	+event_red action-ink
	+event_green action-ink
	+event_blue action-ink
	+event_cyan action-ink
	+event_yellow action-ink
	+event_magenta action-ink
	+event_tblack action-ink
	+event_twhite action-ink
	+event_tred action-ink
	+event_tgreen action-ink
	+event_tblue action-ink
	+event_tcyan action-ink
	+event_tyellow action-ink
	+event_tmagenta action-ink
	+event_close action-close
	+event_grid action-style
	+event_plain action-style
	+event_axis action-style
	+event_lines action-style
	+event_radius1 action-radius
	+event_radius2 action-radius
	+event_radius3 action-radius
	+event_pen action-mode
	+event_line action-mode
	+event_arrow1 action-mode
	+event_arrow2 action-mode
	+event_box action-mode
	+event_circle action-mode
	+event_fbox action-mode
	+event_fcircle action-mode
	+event_clear action-clear
	+event_undo action-undo
	+event_redo action-redo
	+event_save action-save
	+event_load action-load
	+event_min action-minimise
	+event_max action-maximise)

*key_map* (Fmap-kv)

*key_map_shift* (Fmap-kv)

*key_map_control* (Fmap-kv
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*))
(env-pop)
