;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./utils.inc")
(import "./undo.inc")
(import "./cursor.inc")
(import "./block.inc")
(import "./edit.inc")
(import "./file.inc")
(import "./select.inc")
(import "./clipboard.inc")
(import "./search.inc")
(import "./macros.inc")
(import "./ui.inc")

(defq
event_map (fmap-kv
	+event_macro_record action-macro-record
	+event_macro_playback action-macro-playback
	+event_macro_to_eof action-macro-to-eof
	+event_comment action-comment-block
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_close_buffer action-close-buffer
	+event_save action-save
	+event_save_all action-save-all
	+event_new action-new
	+event_next action-next-buffer
	+event_prev action-prev-buffer
	+event_scratch action-scratch-buffer
	+event_find_down action-find-down
	+event_find_up action-find-up
	+event_whole_words action-whole-words
	+event_replace action-replace
	+event_replace_all action-replace-all
	+event_undo action-undo
	+event_redo action-redo
	+event_rewind action-rewind
	+event_cut action-cut
	+event_copy action-copy
	+event_paste action-paste
	+event_tab_left action-left-tab
	+event_tab_right action-right-tab
	+event_reflow action-reflow
	+event_paragraph action-select-paragraph
	+event_block action-select-block
	+event_bracket_left action-left-bracket
	+event_bracket_right action-right-bracket
	+event_toupper action-to-upper
	+event_tolower action-to-lower
	+event_ordered action-ordered
	+event_unique action-ordered-unique
	+event_layout action-layout
	+event_xscroll action-xscroll
	+event_yscroll action-yscroll
	+event_file_leaf_action action-file-leaf-action
	+event_tree_action action-tree-action
	+event_open_leaf_action action-open-leaf-action)

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
	+char_tab action-tab)

key_map_shift (fmap-kv
	0x40000050 action-left-select
	0x4000004f action-right-select
	0x40000052 action-up-select
	0x40000051 action-down-select
	0x4000004A action-home-select
	0x4000004D action-end-select
	+char_tab action-left-tab)

key_map_control (fmap-kv
	(ascii-code "M") action-macro-record
	(ascii-code "m") action-macro-playback
	(ascii-code "/") action-comment-block
	(ascii-code "a") action-select-all
	(ascii-code "b") action-select-block
	(ascii-code "B") action-cut-block
	(ascii-code "w") action-select-word
	(ascii-code "W") action-cut-word
	(ascii-code "l") action-select-line
	(ascii-code "L") action-cut-line
	(ascii-code "p") action-select-paragraph
	(ascii-code "P") action-cut-paragraph
	(ascii-code "f") action-set-find-text
	(ascii-code "r") action-replace
	(ascii-code "R") action-replace-all
	(ascii-code "z") action-undo
	(ascii-code "Z") action-redo
	(ascii-code "s") action-save
	(ascii-code "S") action-save-all
	(ascii-code "t") action-trim
	(ascii-code "x") action-cut
	(ascii-code "c") action-copy
	(ascii-code "v") action-paste
	(ascii-code "=") action-maximise
	(ascii-code "-") action-minimise
	(ascii-code "n") action-next-buffer
	(ascii-code "N") action-prev-buffer
	(ascii-code "q") action-reflow
	(ascii-code "9") action-left-bracket
	(ascii-code "0") action-right-bracket
	(ascii-code "u") action-to-upper
	(ascii-code "U") action-to-lower
	(ascii-code "o") action-ordered
	(ascii-code "O") action-ordered-unique)

recorded_actions_list (list
	action-backspace action-break action-comment-block action-copy action-cut
	action-cut-block action-cut-line action-cut-paragraph action-cut-word
	action-copy-block action-copy-line action-copy-paragraph action-copy-word
	action-delete action-down action-down-select action-end action-end-select
	action-find-down action-find-up action-home action-home-select action-insert
	action-left action-left-bracket action-left-select action-left-tab
	action-ordered action-ordered-unique action-paste action-redo action-reflow
	action-replace action-replace-all action-rewind action-right
	action-right-bracket action-right-select action-right-tab action-select-all
	action-select-block action-select-line action-select-paragraph
	action-select-word action-set-find-text action-tab action-to-lower
	action-to-upper action-trim action-undo action-up action-up-select
	action-whole-words))

;module
(export-symbols
	event_map key_map key_map_shift key_map_control recorded_actions_list
	action-insert action-save-all action-maximise
	select-word action-select-word action-insert)
(export-classes
	Editor-edit)
(env-pop)
