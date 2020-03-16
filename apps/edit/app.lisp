;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/edit/input.inc)
(import 'apps/login/pupa.inc)

(structure 'event 0
	(byte 'win_close 'win_min 'win_max 'win_layout 
		'win_scroll 'new 'save 'open 'close 'prev 'next
		'tab_sel))

;text buffer tuple
(structure 'text 0
	(byte 'index 'path 'title 'buffer 'position))
 
(defq id t vdu_min_width 40 vdu_min_height 24 vdu_width 60 vdu_height 40 text_store (list) tmp_num 0
	current_text (list) empty_buffer '("") home_dir (cat "apps/login/" *env_user* "/"))

(ui-tree window (create-window (+ window_flag_close window_flag_min window_flag_max)) ('color argb_grey2)
	(ui-element _ (create-flow) ('flow_flags (logior flow_flag_down flow_flag_fillw flow_flag_lasth))
		(ui-element toolbar (create-flow) ( 'color toolbar_col 'flow_flags (logior flow_flag_right flow_flag_fillh flow_flag_lastw))
			(ui-element _ (create-grid) ('grid_width 7 'grid_height  1 'font (create-font "fonts/Entypo.ctf" 32))
				(each (lambda (c e)
						(component-connect (ui-element _ (create-button) ('text (num-to-utf8 c))) e))
					'(0xe9e9 0xea07 0xe9f0 0xe96f 0xe93c 0xe93d)
					(list event_open event_save event_new event_close event_prev event_next))
				(ui-element buf_disp (create-label) ('text "0/0" 'color toolbar_col 'font (create-font "fonts/Hack-Regular.ctf" 16))))
			(ui-element textfield (create-textfield) ('font (create-font "fonts/Hack-Regular.ctf" 16) 'text "" 'color argb_grey13)))
		(ui-element _ (create-flow) ('flow_flags (logior flow_flag_left flow_flag_fillh flow_flag_lastw))
			(component-connect (ui-element slider (create-slider) ('color slider_col)) event_win_scroll)
			(ui-element vdu (create-vdu) ('vdu_width vdu_width 'vdu_height vdu_height 'min_width vdu_width 'min_height vdu_height
				'color argb_black 'ink_color argb_white 'font (create-font "fonts/Hack-Regular.ctf" 14))))))

(defun-bind window-resize (w h)
	(bind '(_ path title buffer position) current_text)
	(bind '(ox oy cx cy sx) position)
	(setq vdu_width w vdu_height h)
	(set vdu 'vdu_width w 'vdu_height h 'min_width w 'min_height h)
	(bind '(x y _ _) (view-get-bounds window))
	(bind '(w h) (view-pref-size window))
	(set vdu 'min_width vdu_min_width 'min_height vdu_min_height)
	(view-change-dirty window x y w h)
	(vdu-load vdu buffer ox oy cx cy))

(defun-bind window-layout (w h)
	(bind '(index path title buffer position) current_text)
	(bind '(ox oy cx cy sx) position)
	;for display purposes, index starts at 1.
	(set buf_disp 'text (cat (str (inc index)) "/" (str (length text_store))))
	(setq vdu_width w vdu_height h)
	(set vdu 'vdu_width w 'vdu_height h 'min_width w 'min_height h)
	(bind '(x y _ _) (view-get-bounds vdu))
	(bind '(w h) (view-pref-size vdu))
	(set vdu 'min_width vdu_min_width 'min_height vdu_min_height)
	(view-layout textfield)
	(view-change vdu x y w h)
	(window-set-title window title)
	;set slider and textfield values
	(def slider 'maximum (max 0 (- (length buffer) vdu_height)) 'portion vdu_height 'value oy)
	(view-dirty-all window)
	(vdu-load vdu buffer ox oy cx cy))

;cursor_xy = (+ (mouse_xy / char_wh) offset_xy)
(defun-bind mouse-cursor (mouse_xy)
	(defq buffer (tuple-get text_buffer current_text))
	(bind '(ox oy cx cy sx) (tuple-get text_position current_text))
	(defq cursor_xy (list cx cy) char_wh (vdu-char-size vdu) offset_xy (list ox oy))
	(setq cursor_xy (map + (map / mouse_xy char_wh) offset_xy)
		cx (elem 0 cursor_xy) cy (elem 1 cursor_xy))
	(if (>= cy (length buffer)) 
		(setq cy (min cy (dec (length buffer))) cx (length (elem cy buffer))))
	(if (> cx (length (elem cy buffer)))
		(setq cx (max (set-sticky) (length (elem cy buffer)))) (setq sx cx))
	(tuple-set text_buffer current_text buffer)
	(tuple-set text_position current_text (list ox oy cx cy sx)))

(defun-bind open-buffer (path)
	(defq i 0 index (length text_store) pos (list 0 0 0 0 0))
	(cond
		((eql path "") 
			(defq title (if (eql path "") (cat "Untitled-" (str (setq tmp_num (inc tmp_num)))) path)
				buffer (list (join " " (ascii-char 10))))
			(push text_store (list index path title buffer pos)))
		((some (lambda (_) (eql path (tuple-get text_path _))) text_store)
			(while (< i (length text_store)) 
				(if (eql path (tuple-get text_path (elem i text_store)))
					(setq index i)) (setq i (inc i))))
		((not (file-stream path))
			(save (join " " (ascii-char 10)) path)
			(open-buffer path))
		(t
			(unless (find "/" path)
				(defq path (cat home_dir path)))
			(defq title path buffer (list))
			(each-line (lambda (_) (push buffer _)) (file-stream path))
			(push text_store (list index path title buffer pos))))
	(elem index text_store))

(defun-bind save-buffer (path)
	(unless (eql path "")
		(unless (find "/" path) (setq path (cat home_dir path)))
		(defq save_buffer (join (tuple-get text_buffer current_text) (const (ascii-char 10))))
		(save save_buffer path)
		(tuple-set text_title current_text path)
		(tuple-set text_path current_text path)))

(defun-bind close-buffer (index)
	(defq i 0)
	(setq text_store (erase text_store index (inc index)))
	(each (lambda (_) (tuple-set text_index _ i) (setq i (inc i))) text_store)
	(prev-buffer index))

(defun-bind prev-buffer (index)
	(unless (= index 0)
		(setq index (dec index)))
	(elem index text_store))

(defun-bind next-buffer (index)
	(unless (= index (dec (length text_store)))
		(setq index (inc index)))
	(elem index text_store))

(defun-bind vdu-input (c)
	(bind '(index path title buffer position) current_text)
	(bind '(ox oy cx cy sx) position)
	(cond
		((or (= c 10) (= c 13))		(return) (setq cx 0))
		((= c 8)					(backspace) (setq sx cx))
		((or (= c 9) (<= 32 c 127))	(printable c) (setq sx cx))
		((= c 0x40000050)			(left) (setq sx cx))
		((= c 0x4000004f)			(right) (setq sx cx))
		((= c 0x40000052)			(up))
		((= c 0x40000051)			(down)))
	; ensures behavior resembling other editor interfaces when adjusting cx
	(cursor-visible)
	(set-sticky)
	(set slider 'value oy)
	(tuple-set text_buffer current_text buffer)
	(tuple-set text_position current_text (list ox oy cx cy sx))
	(vdu-load vdu buffer ox (get slider 'value) cx cy)
	(vdu-load vdu buffer ox oy cx cy)
	(view-dirty slider))

;open the window
(gui-add (apply view-change (cat (list window 48 16)
	(view-pref-size (window-set-title
		(component-connect (window-connect-close (window-connect-min
			(window-connect-max window event_win_max) event_win_min) event_win_close) event_win_layout) 
				"")))))

;open buffers from pupa or open new buffer
(each open-buffer (if (= (length *env_edit_auto*) 0) '("") *env_edit_auto*))
(setq current_text (elem 0 text_store))
(set textfield 'text (tuple-get text_path current_text))
(window-layout vdu_width vdu_height)

;main loop
(while id
	(cond
		((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
			(setq id nil))
		((= id event_new)
			(setq current_text (open-buffer ""))
			(set textfield 'text "")
			(window-layout vdu_width vdu_height))
		((= id event_open)
			(defq path (get textfield 'text))
			(setq current_text (open-buffer path))
			(set textfield 'text (tuple-get text_path current_text))
			(window-layout vdu_width vdu_height))
		((= id event_save)
			(defq path (get textfield 'text))
			(save-buffer path)
			(set textfield 'text (tuple-get text_path current_text))
			(window-layout vdu_width vdu_height))
		((= id event_close)
			(cond 
				((<= (length text_store) 1)
					(setq id nil))
				((> (length text_store) 1)
					(setq current_text (close-buffer (tuple-get text_index current_text)))
					(set textfield 'text (tuple-get text_path current_text))
					(window-layout vdu_width vdu_height))))
		((= id event_prev)
			(setq current_text (prev-buffer (tuple-get text_index current_text)))
			(set textfield 'text (tuple-get text_path current_text))
			(window-layout vdu_width vdu_height))
		((= id event_next)
			(setq current_text (next-buffer (tuple-get text_index current_text)))
			(set textfield 'text (tuple-get text_path current_text))
			(window-layout vdu_width vdu_height))
		((= id event_win_layout)
			;user window resize
			(apply window-layout (vdu-max-size vdu)))
		((= id event_win_min)
			;min button
			(window-resize 60 40))
		((= id event_win_max)
			;max button
			(window-resize 120 40))
		((= id event_win_scroll)
			(defq buffer (tuple-get text_buffer current_text))
			(bind '(ox oy cx cy sx) (tuple-get text_position current_text))
			;user scroll bar
			(vdu-load vdu buffer 0 (defq new_oy (get slider 'value)) cx cy)
			(setq oy new_oy)
			(tuple-set text_buffer current_text buffer)
			(tuple-set text_position current_text (list ox oy cx cy sx)))
		((= id (component-get-id vdu))
			(cond 
				((and (= (get-long msg ev_msg_type) ev_type_key)
					(> (get-int msg ev_msg_key_keycode) 0)
					(vdu-input (get-int msg ev_msg_key_key))))
				((and (= (get-long msg ev_msg_type) ev_type_mouse)
					(/= (get-int msg ev_msg_mouse_buttons) 0))
					(defq rx (get-int msg ev_msg_mouse_rx) ry (get-int msg ev_msg_mouse_ry) 
						mouse_xy (list rx ry))
					(mouse-cursor mouse_xy)))
			(window-layout vdu_width vdu_height))
		(t 
			(view-event window msg))))

(view-hide window)
