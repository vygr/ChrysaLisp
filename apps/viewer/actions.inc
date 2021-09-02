;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "././edit/utils.inc")
(import "././edit/cursor.inc")
(import "././edit/block.inc")
(import "././edit/select.inc")
(import "././edit/clipboard.inc")
(import "./ui.inc")

(defq
event_map (fmap-kv
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_copy action-copy
	+event_paragraph action-select-paragraph
	+event_block action-select-block
	+event_bracket_left action-left-bracket
	+event_bracket_right action-right-bracket
	+event_layout action-layout
	+event_xscroll action-xscroll
	+event_yscroll action-yscroll
	+event_tree_action action-tree-action
	+event_file_leaf_action action-file-leaf-action)

key_map (fmap-kv
	0x40000050 action-left
	0x4000004f action-right
	0x40000052 action-up
	0x40000051 action-down
	0x4000004A action-home
	0x4000004D action-end)

key_map_shift (fmap-kv
	0x40000050 action-left-select
	0x4000004f action-right-select
	0x40000052 action-up-select
	0x40000051 action-down-select
	0x4000004A action-home-select
	0x4000004D action-end-select)

key_map_control (fmap-kv
	(ascii-code "a") action-select-all
	(ascii-code "b") action-select-block
	(ascii-code "B") action-copy-block
	(ascii-code "w") action-select-word
	(ascii-code "W") action-copy-word
	(ascii-code "l") action-select-line
	(ascii-code "L") action-copy-line
	(ascii-code "p") action-select-paragraph
	(ascii-code "P") action-copy-paragraph
	(ascii-code "c") action-copy
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise
	(ascii-code "9") action-left-bracket
	(ascii-code "0") action-right-bracket))

;module
(export (penv)
	event_map key_map key_map_shift key_map_control action-maximise
	action-select-word action-select-line action-select-paragraph)
(env-pop)
