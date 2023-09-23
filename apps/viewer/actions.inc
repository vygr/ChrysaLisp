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
(import "././edit/search.inc")
(import "./ui.inc")

(defq
*event_map* (Fmap-kv
	+event_find_down action-find-down
	+event_find_up action-find-up
	+event_whole_words action-whole-words
	+event_regexp action-regexp
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
	+event_file_tree_expand action-file-tree-expand
	+event_file_tree_collapse action-file-tree-collapse
	+event_file_leaf_action action-file-leaf-action)

*key_map* (Fmap-kv
	0x40000050 action-left
	0x4000004f action-right
	0x40000052 action-up
	0x40000051 action-down
	0x4000004A action-home
	0x4000004D action-end)

*key_map_shift* (Fmap-kv
	0x40000050 action-left-select
	0x4000004f action-right-select
	0x40000052 action-up-select
	0x40000051 action-down-select
	0x4000004A action-home-select
	0x4000004D action-end-select)

*key_map_control* (Fmap-kv
	(ascii-code "f") action-set-find-text
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
	(ascii-code "]") action-scale-up
	(ascii-code "[") action-scale-down
	(ascii-code "9") action-left-bracket
	(ascii-code "0") action-right-bracket)

*find_actions* (list
	action-find-down action-find-up))

;module
(export-symbols
	*find_actions*
	*event_map* *key_map* *key_map_shift* *key_map_control*
	action-minimise)
(export-classes
	Viewer-edit)
(env-pop)
