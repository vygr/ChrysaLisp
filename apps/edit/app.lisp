(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "apps/clipboard/app.inc")
(import "lib/consts/chars.inc")
(import "lib/text/buffer.inc")

(enums +event 0
	(enum close max min)
	(enum layout xscroll yscroll)
	(enum tree_action folder_action leaf_action)
	(enum save undo redo cut copy paste))

(defq vdu_min_width 16 vdu_min_height 16 vdu_max_width 120 vdu_max_height 50
	vdu_width 80 vdu_height 40 tabs 4 anchor_x 0 anchor_y 0
	text_buf (Buffer) scroll_map (xmap 31) underlay (list)
	current_file nil selected_node nil id t mouse_state :u
	+selected (apply array (map (lambda (_) 0x80000000) (range 0 1024)))
	+not_selected (apply array (map (lambda (_) 0) (range 0 1024))))

(ui-window mywindow (:color +argb_grey2)
	(ui-title-bar mytitle "" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-tool-bar _ ()
		(ui-buttons (0xea07 0xe9fe 0xe99d 0xea08 0xe9ca 0xe9c9) +event_save))
	(ui-flow _ (:flow_flags +flow_right_fill :font *env_terminal_font*)
		(ui-scroll tree_scroll +scroll_flag_vertical nil
			(. (ui-tree tree +event_tree_action (:min_width 0 :color +argb_white))
				:connect +event_tree_action))
		(ui-flow _ (:flow_flags +flow_left_fill)
			(. (ui-slider yslider) :connect +event_yscroll)
			(ui-flow _ (:flow_flags +flow_up_fill)
				(. (ui-slider xslider) :connect +event_xscroll)
				(ui-flow _ (:flow_flags +flow_stack_fill)
					(ui-vdu vdu (:min_width vdu_width :min_height vdu_height
						:vdu_width vdu_width :vdu_height vdu_height
						:ink_color +argb_white))
					(ui-vdu vdu_underlay (:vdu_width vdu_width :vdu_height vdu_height
						:min_width 0 :min_height 0
						:ink_color +argb_grey6)))))))

(defun all-src-files (root)
	;all source files from root downwards, none recursive
	(defq stack (list root) files (list))
	(while (setq root (pop stack))
		(unless (starts-with "./obj" root)
			(each! 0 -1 (lambda (file type)
				(cond
					((eql type "8")
						;file
						(if (or	;src file ?
								(ends-with ".vp" file)
								(ends-with ".inc" file)
								(ends-with ".lisp" file))
							(push files (cat (slice 2 -1 root) "/" file))))
					(t	;dir
						(unless (starts-with "." file)
							(push stack (cat root "/" file))))))
				(unzip (split (pii-dirlist root) ",") (list (list) (list))))))
	files)

(defun create-underlay ()
	;create the underlay
	(bind '(x y) (. text_buf :get_cursor))
	(if (<= y anchor_y)
		(defq x1 anchor_x y1 anchor_y)
		(defq x1 x y1 y x anchor_x y anchor_y))
	(and (= y y1) (> x x1)
		(defq tx x x x1 x1 tx))
	(setq underlay (cap y1 (list)))
	(defq uy 0 buffer (get :buffer text_buf))
	(while (< uy y)
		(push underlay "")
		(setq uy (inc uy)))
	(cond
		((= y y1)
			(push underlay (cat (slice 0 x +not_selected) (slice x x1 +selected))))
		(t	(push underlay (cat (slice 0 x +not_selected) (slice x (length (elem y buffer)) +selected)))
			(while (< (setq y (inc y)) y1)
				(push underlay (slice 0 (length (elem y buffer)) +selected)))
			(push underlay (slice 0 x1 +selected)))))

(defun clear-underlay ()
	;clear the underlay
	(bind '(x y) (. text_buf :get_cursor))
	(setq anchor_x x anchor_y y)
	(clear underlay))

(defun load-display (scroll_x scroll_y)
	;load the vdu widgets with the text and selection underlay
	(. text_buf :vdu_load vdu scroll_x scroll_y)
	(. vdu_underlay :load underlay scroll_x scroll_y -1 -1))

(defun set-sliders (file)
	;set slider values for this file
	(bind '(scroll_x scroll_y) (. scroll_map :find file))
	(bind '(text_width text_height) (. text_buf :get_size))
	(defq scroll_maxx (max 0 (- text_width vdu_width -1))
		scroll_maxy (max 0 (- text_height vdu_height -1))
		scroll_x (min scroll_x scroll_maxx)
		scroll_y (min scroll_y scroll_maxy))
	(def (. xslider :dirty) :maximum scroll_maxx :portion vdu_width :value scroll_x)
	(def (. yslider :dirty) :maximum scroll_maxy :portion vdu_height :value scroll_y)
	(. scroll_map :insert file (list scroll_x scroll_y))
	(list scroll_x scroll_y))

(defun populate-vdu (file)
	;load up the vdu widget from this file
	(. text_buf :file_load (setq current_file file))
	(clear-underlay)
	(bind '(scroll_x scroll_y) (set-sliders file))
	(. text_buf :set_cursor 0 0)
	(load-display scroll_x scroll_y)
	(def mytitle :text (cat "Edit -> " file))
	(.-> mytitle :layout :dirty))

(defun all-dirs (files)
	;return all the dir routes
	(reduce (lambda (dirs file)
		(defq dir (find-rev "/" file) dir (if dir (cat (slice 0 dir file) "/.")))
		(if (and dir (not (find dir dirs)))
			(push dirs dir) dirs)) files (list)))

(defun populate-tree ()
	;load up the file tree and the first file
	(defq all_src_files (sort cmp (all-src-files ".")))
	(each (# (. tree :add_route %0)) (all-dirs all_src_files))
	(each (# (. tree :add_route %0)) all_src_files)
	(each (# (. scroll_map :insert %0 '(0 0))) all_src_files)
	(populate-vdu (elem 0 all_src_files)))

(defun window-resize (w h)
	;layout the window and size the vdu to fit
	(setq vdu_width w vdu_height h)
	(set vdu :vdu_width w :vdu_height h :min_width w :min_height h)
	(set vdu_underlay :vdu_width w :vdu_height h)
	(bind '(x y) (. vdu :get_pos))
	(bind '(w h) (. vdu :pref_size))
	(bind '(scroll_x scroll_y) (set-sliders current_file))
	(set vdu :min_width vdu_min_width :min_height vdu_min_height)
	(set vdu_underlay :min_width vdu_min_width :min_height vdu_min_height)
	(. vdu :change x y w h)
	(. vdu_underlay :change x y w h)
	(load-display scroll_x scroll_y))

(defun vdu-resize (w h)
	;size the vdu and layout the window to fit
	(setq vdu_width w vdu_height h)
	(set vdu :vdu_width w :vdu_height h :min_width w :min_height h)
	(set vdu_underlay :vdu_width w :vdu_height h)
	(bind '(x y w h) (apply view-fit
		(cat (. mywindow :get_pos) (. mywindow :pref_size))))
	(set vdu :min_width vdu_min_width :min_height vdu_min_height)
	(set vdu_underlay :min_width vdu_min_width :min_height vdu_min_height)
	(. mywindow :change_dirty x y w h)
	(bind '(scroll_x scroll_y) (set-sliders current_file))
	(load-display scroll_x scroll_y))

(defun refresh ()
	;refresh display and ensure cursor is visible
	(bind '(cx cy) (. text_buf :get_cursor))
	(bind '(scroll_x scroll_y) (. scroll_map :find current_file))
	(bind '(w h) (. vdu :vdu_size))
	(if (< cx scroll_x) (setq scroll_x cx))
	(if (< cy scroll_y) (setq scroll_y cy))
	(if (>= cx (+ scroll_x w)) (setq scroll_x (- cx w -1)))
	(if (>= cy (+ scroll_y h)) (setq scroll_y (- cy h -1)))
	(. scroll_map :insert current_file (list scroll_x scroll_y))
	(bind '(scroll_x scroll_y) (set-sliders current_file))
	(load-display scroll_x scroll_y))

;import key binding after any editor functions are defind !
(import "apps/edit/bindings.inc")

(defun main ()
	(populate-tree)
	(bind '(w h) (. tree :pref_size))
	(. tree :change 0 0 (def tree_scroll :min_width w) h)
	(bind '(x y w h) (apply view-locate (.-> mywindow (:connect +event_layout) :pref_size)))
	(gui-add (. mywindow :change x y w h))
	(load-display 0 0)
	(while id (cond
		((= (setq id (getf (defq msg (mail-read (task-mailbox))) +ev_msg_target_id)) +event_close)
			(setq id nil))
		((= id +event_layout)
			;user window resize
			(apply window-resize (. vdu :max_size)))
		((= id +event_min)
			;min button
			(vdu-resize vdu_min_width vdu_min_height))
		((= id +event_max)
			;max button
			(vdu-resize vdu_max_width vdu_max_height))
		((= id +event_save)
			;save
			(. text_buf :file_save current_file))
		((= id +event_undo)
			;undo
			(. text_buf :undo)
			(refresh))
		((= id +event_redo)
			;redo
			(. text_buf :redo)
			(refresh))
		((= id +event_cut)
			;cut selection to clipboard
			(clipboard-put (. text_buf :cut))
			(refresh))
		((= id +event_copy)
			;copy selection to clipboard
			(clipboard-put (. text_buf :copy)))
		((= id +event_paste)
			;paste from clipboard if present
			(unless (eql (defq data (clipboard-get)) "")
				(. text_buf :paste data)
				(refresh)))
		((= id +event_xscroll)
			;user xscroll bar
			(bind '(scroll_x scroll_y) (. scroll_map :find current_file))
			(defq scroll_x (get :value xslider))
			(. scroll_map :insert current_file (list scroll_x scroll_y))
			(load-display scroll_x scroll_y))
		((= id +event_yscroll)
			;user yscroll bar
			(bind '(scroll_x scroll_y) (. scroll_map :find current_file))
			(defq scroll_y (get :value yslider))
			(. scroll_map :insert current_file (list scroll_x scroll_y))
			(load-display scroll_x scroll_y))
		((= id +event_tree_action)
			;tree view action
			(bind '(w h) (. tree :pref_size))
			(defq w (get :min_width tree_scroll))
			(. tree :change 0 0 w h)
			(.-> tree_scroll :layout :dirty_all))
		((= id +event_leaf_action)
			;load up the file selected
			(if selected_node (undef (. selected_node :dirty) :color))
			(setq selected_node (. mywindow :find_id (getf msg +ev_msg_action_source_id)))
			(def (. selected_node :dirty) :color +argb_grey12)
			(populate-vdu (. tree :get_route selected_node)))
		((= id +event_folder_action)
			;highlight the folder selected
			(if selected_node (undef (. selected_node :dirty) :color))
			(setq selected_node (. mywindow :find_id (getf msg +ev_msg_action_source_id)))
			(def (. selected_node :dirty) :color +argb_grey12))
		((and (= id (. vdu :get_id)) (= (getf msg +ev_msg_type) +ev_type_mouse))
			;mouse event on display
			(bind '(cw ch) (. vdu :char_size))
			(bind '(sx sy) (. scroll_map :find current_file))
			(defq x (getf msg +ev_msg_mouse_rx) y (getf msg +ev_msg_mouse_ry))
			(setq x (if (>= x 0) x (- x cw)) y (if (>= y 0) y (- y ch)))
			(setq x (+ sx (/ x cw)) y (+ sy (/ y ch)))
			(cond
				((/= (getf msg +ev_msg_mouse_buttons) 0)
					;mouse button is down
					(case mouse_state
						(:d	;was down last time
							(bind '(x y) (. text_buf :constrain x y))
							(. text_buf :set_cursor x y)
							(create-underlay))
						(:u	;was up last time
							(bind '(x y) (. text_buf :constrain x y))
							(. text_buf :set_cursor x y)
							(setq anchor_x x anchor_y y mouse_state :d)
							(create-underlay))))
				(t	;mouse button is up
					(case mouse_state
						(:d	;was down last time
							(setq mouse_state :u))
						(:u	;was up last time
							))))
			(refresh))
		((and (= (getf msg +ev_msg_type) +ev_type_key)
				(> (getf msg +ev_msg_key_keycode) 0))
			;key event
			(defq key (getf msg +ev_msg_key_key) mod (getf msg +ev_msg_key_mod))
			(cond
				((/= 0 (logand mod (const (+ +ev_key_mod_control +ev_key_mod_command))))
					;call bound control/command key action
					(when (defq action (. key_map_control :find key))
						(action)))
				((defq action (. key_map :find key))
					;call bound key action
					(action))
				((<= +char_space key +char_tilda)
					;insert the char
					(. text_buf :insert (char key))))
			(clear-underlay)
			(refresh))
		(t	;gui event
			(. mywindow :event msg))))
	(. mywindow :hide))
