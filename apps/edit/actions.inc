;module
(env-push)

;;;;;;;;;
; actions
;;;;;;;;;

(import "./utils.inc")
(import "./cursor.inc")
(import "./block.inc")
(import "./edit.inc")
(import "./file.inc")
(import "./select.inc")
(import "./clipboard.inc")
(import "./search.inc")
(import "./undo.inc")
(import "./macros.inc")
(import "./ui.inc")
(import "./state.inc")

(defq
*event_map* (scatter (Fmap)
	+event_open_tree_expand action-open-tree-expand
	+event_open_tree_collapse action-open-tree-collapse
	+event_file_tree_expand action-file-tree-expand
	+event_file_tree_collapse action-file-tree-collapse
	+event_close_all action-close-all
	+event_close_buffer action-close-buffer
	+event_macro_record action-macro-record
	+event_macro_playback action-macro-playback
	+event_macro_to_eof action-macro-to-eof
	+event_macro_global action-macro-global
	+event_comment action-comment
	+event_close action-close
	+event_min action-minimise
	+event_max action-maximise
	+event_save action-save
	+event_save_all action-save-all
	+event_load_selected action-load-selected
	+event_new action-new
	+event_next action-next-buffer
	+event_prev action-prev-buffer
	+event_scratch action-scratch-buffer
	+event_find_down action-find-down
	+event_find_up action-find-up
	+event_global action-find-global
	+event_whole_words action-whole-words
	+event_regexp action-regexp
	+event_region action-region
	+event_replace action-replace
	+event_replace_all action-replace-all
	+event_replace_global action-replace-global
	+event_collect action-collect
	+event_collect_global action-collect-global
	+event_undo action-undo
	+event_redo action-redo
	+event_rewind action-rewind
	+event_gundo action-undo-global
	+event_gredo action-redo-global
	+event_cut action-cut
	+event_copy action-copy
	+event_paste action-paste
	+event_tab_left action-left-tab
	+event_tab_right action-right-tab
	+event_reflow action-reflow
	+event_split action-split
	+event_paragraph action-select-paragraph
	+event_block action-select-block
	+event_bracket_left action-left-bracket
	+event_bracket_right action-right-bracket
	+event_toupper action-to-upper
	+event_tolower action-to-lower
	+event_sort action-sort
	+event_unique action-unique
	+event_invert action-invert
	+event_layout action-layout
	+event_xscroll action-xscroll
	+event_yscroll action-yscroll
	+event_file_leaf_action action-file-leaf-action
	+event_file_folder_action action-file-folder-action
	+event_tree_action action-tree-action
	+event_open_leaf_action action-open-leaf-action)

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
	+char_tab action-tab)

*key_map_shift* (scatter (Fmap)
	0x40000050 action-left-select
	0x4000004f action-right-select
	0x40000052 action-up-select
	0x40000051 action-down-select
	0x4000004A action-home-select
	0x4000004D action-end-select
	+char_tab action-left-tab)

*key_map_control* (scatter (Fmap)
	(ascii-code "0") action-macro-playback-0
	(ascii-code "1") action-macro-playback-1
	(ascii-code "2") action-macro-playback-2
	(ascii-code "3") action-macro-playback-3
	(ascii-code "4") action-macro-playback-4
	(ascii-code "5") action-macro-playback-5
	(ascii-code "6") action-macro-playback-6
	(ascii-code "7") action-macro-playback-7
	(ascii-code "8") action-macro-playback-8
	(ascii-code "9") action-macro-playback-9
	(ascii-code "!") action-macro-save-1
	(ascii-code "@") action-macro-save-2
	(ascii-code "#") action-macro-save-3
	(ascii-code "$") action-macro-save-4
	(ascii-code "%") action-macro-save-5
	(ascii-code "^") action-macro-save-6
	(ascii-code "&") action-macro-save-7
	(ascii-code "*") action-macro-save-8
	(ascii-code "(") action-macro-save-9
	(ascii-code "M") action-macro-record
	(ascii-code "m") action-macro-playback
	(ascii-code "/") action-comment
	(ascii-code "a") action-select-all
	(ascii-code "A") action-region
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
	(ascii-code "y") action-collect
	(ascii-code "Y") action-collect-global
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
	(ascii-code "Q") action-split
	(ascii-code "u") action-to-upper
	(ascii-code "U") action-to-lower
	(ascii-code "i") action-invert
	(ascii-code "o") action-sort
	(ascii-code "O") action-unique
	(ascii-code "{") action-scale-down
	(ascii-code "}") action-scale-up
	(ascii-code "[") action-left-bracket
	(ascii-code "]") action-right-bracket
	(ascii-code "d") action-push
	(ascii-code "D") action-pop
	(ascii-code "e") action-load-depends
	(ascii-code "E") action-load-all-depends
	(ascii-code "j") action-find-function
	(ascii-code "J") action-pop
	0x4000004a action-top
	0x4000004d action-bottom)

*recorded_actions* (list
	action-backspace action-break action-comment action-copy action-cut
	action-copy-block action-copy-line action-copy-paragraph action-copy-word
	action-cut-block action-cut-line action-cut-paragraph action-cut-word
	action-delete action-down action-down-select action-end action-end-select
	action-find-down action-find-up action-home action-home-select action-insert
	action-invert action-left action-left-bracket action-left-select
	action-left-tab action-macro-playback-1 action-macro-playback-2
	action-macro-playback-3 action-macro-playback-4 action-macro-playback-5
	action-macro-playback-6 action-macro-playback-7 action-macro-playback-8
	action-macro-playback-9 action-paste action-redo action-reflow action-region
	action-replace action-replace-all action-rewind action-right
	action-right-bracket action-right-select action-right-tab action-select-all
	action-select-block action-select-line action-select-paragraph
	action-select-word action-sort action-tab action-to-lower action-to-upper
	action-trim action-undo action-unique action-up action-up-select action-split
	action-top action-bottom action-push action-pop)

*replace_actions* (list
	action-replace action-replace-all action-replace-global)

*find_actions* (cat *replace_actions* (list
	action-find-down action-find-up action-find-global
	action-collect action-collect-global)))

;module
(export-symbols
	'(*recorded_actions* *find_actions* *replace_actions*
	*event_map* *key_map* *key_map_shift* *key_map_control*
	action-insert action-save-all action-maximise
	select-word action-select-word action-insert
	macro-record state-load state-save find-count))
(export-classes
	'(Editor-edit))
(env-pop)
