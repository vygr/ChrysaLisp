;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./utils.inc")
(import "./cursor.inc")
(import "./edit.inc")
(import "./select.inc")
(import "./clipboard.inc")
(import "././edit/ui.inc")

(defq
event_map (fmap-kv
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_copy action-copy
	+event_paste action-paste
	+event_paragraph action-select-paragraph
	+event_layout action-layout
	+event_xscroll action-xscroll
	+event_yscroll action-yscroll)

key_map (fmap-kv
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

key_map_shift (fmap-kv)

key_map_control (fmap-kv
	(ascii-code "w") action-select-word
	(ascii-code "W") action-copy-word
	(ascii-code "l") action-select-line
	(ascii-code "L") action-copy-line
	(ascii-code "p") action-select-paragraph
	(ascii-code "P") action-copy-paragraph
	(ascii-code "c") action-copy
	(ascii-code "v") action-paste
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise))

;module
(export (penv)
	event_map key_map key_map_shift key_map_control
	action-select-word action-select-line action-select-paragraph
	action-insert)
(env-pop)
