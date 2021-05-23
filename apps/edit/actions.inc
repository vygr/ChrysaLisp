;module
(env-push)

(defun action-left ()
	(. current_buffer :left)
	(clear-selection) (refresh))

(defun action-right ()
	(. current_buffer :right)
	(clear-selection) (refresh))

(defun action-up ()
	(. current_buffer :up)
	(clear-selection) (refresh))

(defun action-down ()
	(. current_buffer :down)
	(clear-selection) (refresh))

(defun action-left-select ()
	(unless shift_select
		(setq anchor_x cursor_x anchor_y cursor_y shift_select t))
	(. current_buffer :left)
	(create-selection) (refresh))

(defun action-right-select ()
	(unless shift_select
		(setq anchor_x cursor_x anchor_y cursor_y shift_select t))
	(. current_buffer :right)
	(create-selection) (refresh))

(defun action-up-select ()
	(unless shift_select
		(setq anchor_x cursor_x anchor_y cursor_y shift_select t))
	(. current_buffer :up)
	(create-selection) (refresh))

(defun action-down-select ()
	(unless shift_select
		(setq anchor_x cursor_x anchor_y cursor_y shift_select t))
	(. current_buffer :down)
	(create-selection) (refresh))

(defun action-left-tab ()
	(. current_buffer :tab_left anchor_x anchor_y)
	(create-selection) (refresh))

(defun action-right-tab ()
	(. current_buffer :tab_right anchor_x anchor_y)
	(create-selection) (refresh))

(defun action-break ()
	(. current_buffer :cut anchor_x anchor_y)
	(. current_buffer :break)
	(clear-selection) (refresh))

(defun action-backspace ()
	(when (eql (. current_buffer :cut anchor_x anchor_y) "")
		(. current_buffer :backspace))
	(clear-selection) (refresh))

(defun action-delete ()
	(when (eql (. current_buffer :cut anchor_x anchor_y) "")
		(. current_buffer :delete))
	(clear-selection) (refresh))

(defun action-tab ()
	(. current_buffer :cut anchor_x anchor_y)
	(bind '(x y) (. current_buffer :get_cursor))
	(defq tab_width (get :tab_width current_buffer))
	(. current_buffer :insert (pad "" (- tab_width (% x tab_width)) "        "))
	(clear-selection) (refresh))

(defun action-undo ()
	(. current_buffer :undo)
	(clear-selection) (refresh))

(defun action-redo ()
	(. current_buffer :redo)
	(clear-selection) (refresh))

(defun action-trim ()
	(. current_buffer :trim)
	(clear-selection) (refresh))

(defun action-save ()
	(when current_file
		(action-trim)
		(. current_buffer :file_save current_file)))

(defun action-cut ()
	(clipboard-put (. current_buffer :cut anchor_x anchor_y))
	(clear-selection) (refresh))

(defun action-copy ()
	(clipboard-put (. current_buffer :copy anchor_x anchor_y))
	(clear-selection) (refresh))

(defun action-paste ()
	(unless (eql (defq data (clipboard-get)) "")
		(. current_buffer :cut anchor_x anchor_y)
		(. current_buffer :paste data)
		(clear-selection) (refresh)))

(defun action-select-all ()
	(bind '(x y) (. current_buffer :get_size))
	(setq anchor_x 0 anchor_y 0 cursor_x 0 cursor_y y)
	(. current_buffer :set_cursor cursor_x cursor_y)
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
	(setq scroll_x (get :value xslider))
	(load-display))

(defun action-yscroll ()
	;user yscroll bar
	(setq scroll_y (get :value yslider))
	(load-display))

(defun action-tree-action ()
	;any tree mutate action
	(defq scroll (penv (. mywindow :find_id (getf msg +ev_msg_action_source_id))))
	(.-> scroll :layout :dirty_all))

(defun action-file-leaf-action ()
	;load up the file selected
	(if selected_file_node (undef (. selected_file_node :dirty) :color))
	(if selected_open_node (undef (. selected_open_node :dirty) :color))
	(setq selected_file_node (. mywindow :find_id (getf msg +ev_msg_action_source_id)))
	(def (. selected_file_node :dirty) :color +argb_grey12)
	(defq file (. file_tree :get_route selected_file_node))
	(. open_tree :add_route file)
	(setq selected_open_node (. open_tree :find_node file))
	(def (. selected_open_node :dirty) :color +argb_grey12)
	(bind '(w h) (. open_tree :pref_size))
	(. open_tree :change 0 0 w h)
	(.-> open_tree_scroll :layout :dirty_all)
	(populate-vdu file))

(defun action-open-leaf-action ()
	;switch to the file selected
	(if selected_file_node (undef (. selected_file_node :dirty) :color))
	(if selected_open_node (undef (. selected_open_node :dirty) :color))
	(setq selected_open_node (. mywindow :find_id (getf msg +ev_msg_action_source_id)))
	(def (. selected_open_node :dirty) :color +argb_grey12)
	(defq file (. open_tree :get_route selected_open_node))
	(setq selected_file_node (. file_tree :find_node file))
	(def (. selected_file_node :dirty) :color +argb_grey12)
	(populate-vdu file))

(defun action-new ()
	(when (and
			(not (eql (defq file (get :clear_text name_text)) ""))
			(= (age file) 0))
		;file does not exist
		(save "" file)
		(def name_text :cursor 0 :clear_text "")
		(.-> name_text :layout :dirty_all)
		(if selected_file_node (undef (. selected_file_node :dirty) :color))
		(if selected_open_node (undef (. selected_open_node :dirty) :color))
		(. open_tree :add_route file)
		(. file_tree :add_route file)
		(setq selected_open_node (. open_tree :find_node file))
		(setq selected_file_node (. file_tree :find_node file))
		(def (. selected_open_node :dirty) :color +argb_grey12)
		(def (. selected_file_node :dirty) :color +argb_grey12)
		(bind '(w h) (. open_tree :pref_size))
		(. open_tree :change 0 0 w h)
		(.-> open_tree_scroll :layout :dirty_all)
		(bind '(w h) (. file_tree :pref_size))
		(. file_tree :change 0 0 w h)
		(.-> file_tree_scroll :layout :dirty_all)
		(populate-vdu file)))

(defq event_map (xmap-kv
		+event_close action-close
		+event_min action-minimise
		+event_max action-maximise
		+event_save action-save
		+event_new action-new
		+event_undo action-undo
		+event_redo action-redo
		+event_cut action-cut
		+event_copy action-copy
		+event_paste action-paste
		+event_tab_left action-left-tab
		+event_tab_right action-right-tab
		+event_layout action-layout
		+event_xscroll action-xscroll
		+event_yscroll action-yscroll
		+event_file_leaf_action action-file-leaf-action
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

	key_map_shift (xmap-kv
		0x40000050 action-left-select
		0x4000004f action-right-select
		0x40000052 action-up-select
		0x40000051 action-down-select)

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
		(ascii-code "=") action-maximise
		0x40000050 action-left-tab
		0x4000004f action-right-tab))

;module
(export (penv)
	event_map key_map key_map_shift key_map_control)
(env-pop)
