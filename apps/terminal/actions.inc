;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "lib/files/urls.inc")
(import "./utils.inc")
(import "./cursor.inc")
(import "./edit.inc")
(import "././edit/select.inc")
(import "./clipboard.inc")
(import "./state.inc")
(import "./ui.inc")

(defq
*event_map* (scatter (Fmap)
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_copy action-copy
	+event_paste action-paste
	+event_paragraph action-select-paragraph
	+event_layout action-layout
	+event_xscroll action-xscroll
	+event_yscroll action-yscroll)

*key_map* (scatter (Fmap)
	0x40000050 action-left
	0x4000004f action-right
	0x40000052 action-up
	0x40000051 action-down
	0x4000004A action-home
	0x4000004D action-end
	+char_lf action-break
	+char_cr action-break
	+char_backspace action-backspace
	+char_delete action-delete
	+char_tab action-tab
	+char_esc action-escape)

*key_map_shift* (scatter (Fmap)
	0x40000050 action-left-select
	0x4000004f action-right-select
	0x4000004A action-home-select
	0x4000004D action-end-select)

*key_map_control* (scatter (Fmap)
	(ascii-code "w") action-select-word
	(ascii-code "W") action-copy-word
	(ascii-code "l") action-select-line
	(ascii-code "L") action-copy-line
	(ascii-code "p") action-select-paragraph
	(ascii-code "P") action-copy-paragraph
	(ascii-code "c") action-copy
	(ascii-code "v") action-paste
	(ascii-code "{") action-scale-down
	(ascii-code "}") action-scale-up
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export-symbols
	'(*event_map* *key_map* *key_map_shift* *key_map_control*
	action-insert state-load state-save))
(export-classes
	'(Viewer-edit Terminal-edit))
(env-pop)
