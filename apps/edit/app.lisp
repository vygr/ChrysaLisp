(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")
(import "apps/clipboard/app.inc")
(import "lib/consts/chars.inc")
(import "lib/text/buffer.inc")

(enums +event 0
	(enum close max min)
	(enum layout xscroll yscroll)
	(enum tree_action)
	(enum file_folder_action file_leaf_action)
	(enum open_folder_action open_leaf_action)
	(enum save undo redo cut copy paste))

(defq vdu_min_width 16 vdu_min_height 16 vdu_max_width 120 vdu_max_height 48
	vdu_width 80 vdu_height 40 tabs 4 anchor_x 0 anchor_y 0
	text_buf (Buffer) scroll_map (xmap 31) underlay (list)
	current_file nil selected_node nil mouse_state :u
	+selected (apply array (map (lambda (_) 0x80000000) (str-alloc 1024)))
	+not_selected (apply array (map (lambda (_) 0) (str-alloc 1024))))

(ui-window mywindow (:color +argb_grey2)
	(ui-title-bar mytitle "" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-tool-bar _ ()
		(ui-buttons (0xea07 0xe9fe 0xe99d 0xea08 0xe9ca 0xe9c9) +event_save))
	(ui-flow _ (:flow_flags +flow_right_fill :font *env_terminal_font*)
		(ui-flow _ (:flow_flags +flow_stack_fill)
			(ui-grid tree_grid (:grid_width 1 :grid_height 2 :color +argb_grey14)
				(ui-flow _ (:flow_flags +flow_down_fill)
					(ui-label _ (:text "Open" :border 1))
					(ui-scroll open_tree_scroll +scroll_flag_vertical nil
						(. (ui-tree open_tree +event_open_folder_action (:min_width 0 :color +argb_white))
							:connect +event_tree_action)))
				(ui-flow _ (:flow_flags +flow_down_fill)
					(ui-label _ (:text "Project" :border 1))
					(ui-scroll file_tree_scroll +scroll_flag_vertical nil
						(. (ui-tree file_tree +event_file_folder_action (:min_width 0 :color +argb_white))
							:connect +event_tree_action))))
			(ui-backdrop _ (:color +argb_white)))
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

(defun create-selection ()
	;create the underlay
	(bind '(x y) (. text_buf :get_cursor))
	(defq x1 anchor_x y1 anchor_y)
	(if (> y y1)
		(defq x (logxor x x1) x1 (logxor x x1) x (logxor x x1)
			y (logxor y y1) y1 (logxor y y1) y (logxor y y1)))
	(and (= y y1) (> x x1)
		(defq x (logxor x x1) x1 (logxor x x1) x (logxor x x1)))
	(setq underlay (cap y1 (clear underlay)))
	(defq uy -1 buffer (get :buffer text_buf))
	(while (< (setq uy (inc uy)) y) (push underlay ""))
	(cond
		((= y y1)
			(push underlay (cat (slice 0 x +not_selected) (slice x x1 +selected))))
		(t	(push underlay (cat (slice 0 x +not_selected) (slice x (inc (length (elem y buffer))) +selected)))
			(while (< (setq y (inc y)) y1)
				(push underlay (slice 0 (inc (length (elem y buffer))) +selected)))
			(push underlay (slice 0 x1 +selected)))))

(defun clear-selection ()
	;clear the underlay
	(bind '(x y) (. text_buf :get_cursor))
	(setq anchor_x x anchor_y y)
	(clear underlay))

(defun load-display (scroll_x scroll_y)
	;load the vdu widgets with the text and selection underlay
	(. text_buf :vdu_load vdu scroll_x scroll_y)
	(. vdu_underlay :load underlay scroll_x scroll_y -1 -1))

(defun refresh ()
	;refresh display and ensure cursor is visible
	(bind '(scroll_x scroll_y x y) (. scroll_map :find current_file))
	(bind '(x y) (. text_buf :get_cursor))
	(bind '(x y) (. text_buf :constrain x y))
	(. text_buf :set_cursor x y)
	(bind '(w h) (. vdu :vdu_size))
	(if (< x scroll_x) (setq scroll_x x))
	(if (< y scroll_y) (setq scroll_y y))
	(if (>= x (+ scroll_x w)) (setq scroll_x (- x w -1)))
	(if (>= y (+ scroll_y h)) (setq scroll_y (- y h -1)))
	(. scroll_map :insert current_file (list scroll_x scroll_y x y))
	(bind '(scroll_x scroll_y) (set-sliders current_file))
	(load-display scroll_x scroll_y))

(defun set-sliders (file)
	;set slider values for this file
	(bind '(scroll_x scroll_y x y) (. scroll_map :find current_file))
	(bind '(w h) (. text_buf :get_size))
	(bind '(x y) (. text_buf :get_cursor))
	(defq scroll_maxx (max 0 (- w vdu_width -1))
		scroll_maxy (max 0 (- h vdu_height -1))
		scroll_x (min scroll_x scroll_maxx)
		scroll_y (min scroll_y scroll_maxy))
	(def (. xslider :dirty) :maximum scroll_maxx :portion vdu_width :value scroll_x)
	(def (. yslider :dirty) :maximum scroll_maxy :portion vdu_height :value scroll_y)
	(. scroll_map :insert file (list scroll_x scroll_y x y))
	(list scroll_x scroll_y))

(defun populate-vdu (file)
	;load up the vdu widget from this file
	(. text_buf :file_load (setq current_file file))
	(bind '(_ _ x y) (. scroll_map :find current_file))
	(. text_buf :set_cursor x y)
	(clear-selection) (refresh)
	(def mytitle :text (cat "Edit -> " file))
	(.-> mytitle :layout :dirty))

(defun all-dirs (files)
	;return all the dir routes
	(reduce (lambda (dirs file)
		(defq dir (find-rev "/" file) dir (if dir (cat (slice 0 dir file) "/.")))
		(if (and dir (not (find dir dirs)))
			(push dirs dir) dirs)) files (list)))

(defun populate-file-tree ()
	;load up the file tree
	(defq all_src_files (sort cmp (all-src-files ".")))
	(each (# (. file_tree :add_route %0)) (all-dirs all_src_files))
	(each (# (. file_tree :add_route %0)) all_src_files)
	(each (# (. scroll_map :insert %0 '(0 0 0 0))) all_src_files)
	(populate-vdu (elem 0 all_src_files))
	(. open_tree :add_route current_file))

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

;import editor actions and bindings
(import "apps/edit/actions.inc")

(defun main ()
	(populate-file-tree)
	(bind '(ow oh) (. open_tree :pref_size))
	(bind '(fw fh) (. file_tree :pref_size))
	(def open_tree :min_width fw)
	(def file_tree :min_width fw)
	(def open_tree_scroll :min_width fw :min_height oh)
	(def file_tree_scroll :min_width fw)
	(. open_tree :change 0 0 fw oh)
	(. file_tree :change 0 0 fw fh)
	(bind '(x y w h) (apply view-locate (.-> mywindow (:connect +event_layout) :pref_size)))
	(gui-add (. mywindow :change x y w h))
	(load-display 0 0)
	(defq running t)
	(while running
		(cond
			((defq id (getf (defq msg (mail-read (task-mailbox))) +ev_msg_target_id)
					action (. event_map :find id))
				;call bound event action
				(action))
			((and (= id (. vdu :get_id)) (= (getf msg +ev_msg_type) +ev_type_mouse))
				;mouse event on display
				(bind '(cw ch) (. vdu :char_size))
				(bind '(sx sy x y) (. scroll_map :find current_file))
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
								(create-selection))
							(:u	;was up last time
								(bind '(x y) (. text_buf :constrain x y))
								(. text_buf :set_cursor x y)
								(setq anchor_x x anchor_y y mouse_state :d)
								(create-selection))))
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
						(. text_buf :cut anchor_x anchor_y)
						(. text_buf :insert (char key))
						(clear-selection) (refresh))))
			(t	;gui event
				(. mywindow :event msg))))
	(. mywindow :hide))
