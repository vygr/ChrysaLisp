;module
(env-push)

(defun action-left ()
	(. text_buf :left)
	(clear-selection) (refresh))

(defun action-right ()
	(. text_buf :right)
	(clear-selection) (refresh))

(defun action-up ()
	(. text_buf :up)
	(clear-selection) (refresh))

(defun action-down ()
	(. text_buf :down)
	(clear-selection) (refresh))

(defun action-break ()
	(. text_buf :cut anchor_x anchor_y)
	(. text_buf :break)
	(clear-selection) (refresh))

(defun action-backspace ()
	(when (eql (. text_buf :cut anchor_x anchor_y) "")
		(. text_buf :backspace))
	(clear-selection) (refresh))

(defun action-delete ()
	(when (eql (. text_buf :cut anchor_x anchor_y) "")
		(. text_buf :delete))
	(clear-selection) (refresh))

(defun action-tab ()
	(. text_buf :cut anchor_x anchor_y)
	(bind '(x y) (. text_buf :get_cursor))
	(defq tab_width (get :tab_width text_buf))
	(. text_buf :insert (pad "" (- tab_width (% x tab_width)) "        "))
	(clear-selection) (refresh))

(defun action-undo ()
	(. text_buf :undo)
	(clear-selection) (refresh))

(defun action-redo ()
	(. text_buf :redo)
	(clear-selection) (refresh))

(defun action-trim ()
	(. text_buf :trim)
	(clear-selection) (refresh))

(defun action-save ()
	(action-trim)
	(. text_buf :file_save current_file))

(defun action-cut ()
	(clipboard-put (. text_buf :cut anchor_x anchor_y))
	(clear-selection) (refresh))

(defun action-copy ()
	(clipboard-put (. text_buf :copy anchor_x anchor_y))
	(clear-selection) (refresh))

(defun action-paste ()
	(unless (eql (defq data (clipboard-get)) "")
		(. text_buf :cut anchor_x anchor_y)
		(. text_buf :paste data)
		(clear-selection) (refresh)))

(defun action-select-all ()
	(bind '(_ y) (. text_buf :get_size))
	(setq anchor_x 0 anchor_y 0)
	(. text_buf :set_cursor 0 y)
	(create-selection) (refresh))

(defun action-minimise ()
	(vdu-resize vdu_min_width vdu_min_height))

(defun action-maximise ()
	(vdu-resize vdu_max_width vdu_max_height))

(defun action-close ()
	(setq running nil))

(defun action-layout ()
	;user window resize
	(apply window-resize (. vdu :max_size)))

(defun action-xscroll ()
	;user xscroll bar
	(bind '(scroll_x scroll_y x y) (. scroll_map :find current_file))
	(bind '(x y) (. text_buf :get_cursor))
	(defq scroll_x (get :value xslider))
	(. scroll_map :insert current_file (list scroll_x scroll_y x y))
	(load-display scroll_x scroll_y))

(defun action-yscroll ()
	;user yscroll bar
	(bind '(scroll_x scroll_y x y) (. scroll_map :find current_file))
	(bind '(x y) (. text_buf :get_cursor))
	(defq scroll_y (get :value yslider))
	(. scroll_map :insert current_file (list scroll_x scroll_y x y))
	(load-display scroll_x scroll_y))

(defun action-file-leaf-action ()
	;load up the file selected
	(if selected_node (undef (. selected_node :dirty) :color))
	(setq selected_node (. mywindow :find_id (getf msg +ev_msg_action_source_id)))
	(def (. selected_node :dirty) :color +argb_grey12)
	(defq file (. file_tree :get_route selected_node))
	(. open_tree :add_route file)
	(bind '(w h) (. open_tree :pref_size))
	(. open_tree :change 0 0 w h)
	(.-> open_tree_scroll :layout :dirty_all)
	(populate-vdu file))

(defun action-file-folder-action ()
	;highlight the folder selected
	(if selected_node (undef (. selected_node :dirty) :color))
	(setq selected_node (. mywindow :find_id (getf msg +ev_msg_action_source_id)))
	(def (. selected_node :dirty) :color +argb_grey12))

(defun action-tree-action ()
	;any tree mutate action
	(defq scroll (penv (. mywindow :find_id (getf msg +ev_msg_action_source_id))))
	(.-> scroll :layout :dirty_all))

(defun action-open-leaf-action ()
	;switch to the file selected
	)

(defq event_map (xmap-kv
		+event_close action-close
		+event_min action-minimise
		+event_max action-maximise
		+event_save action-save
		+event_undo action-undo
		+event_redo action-redo
		+event_cut action-cut
		+event_copy action-copy
		+event_paste action-paste
		+event_paste action-paste
		+event_layout action-layout
		+event_xscroll action-xscroll
		+event_yscroll action-yscroll
		+event_file_leaf_action action-file-leaf-action
		+event_file_folder_action action-file-folder-action
		+event_tree_action action-tree-action
		+event_open_leaf_action action-open-leaf-action)

	key_map (xmap-kv
		0x40000050 action-left
		0x4000004f action-right
		0x40000052 action-up
		0x40000051 action-down
		+char_lf action-break
		+char_cr action-break
		+char_backspace action-backspace
		+char_delete action-delete
		+char_tab action-tab)

	key_map_control (xmap-kv
		(ascii-code "a") action-select-all
		(ascii-code "z") action-undo
		(ascii-code "Z") action-redo
		(ascii-code "s") action-save
		(ascii-code "t") action-trim
		(ascii-code "x") action-cut
		(ascii-code "c") action-copy
		(ascii-code "v") action-paste
		(ascii-code "-") action-minimise
		(ascii-code "=") action-maximise))

;module
(export (penv)
	event_map key_map key_map_control)
(env-pop)
