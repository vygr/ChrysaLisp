;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/edit/input.inc)

(structure 'event 0
	(byte 'win_close 'win_min 'win_max 'win_layout 
		'win_scroll 'save 'new))

;1st attempt at buffer structure.
(structure 'buffer 0
	(str 'path) (list 'text) (int 'ox 'oy 'cx 'cy))
 
(defq id t vdu_min_width 40 vdu_min_height 24 vdu_width 60 vdu_height 40 sx 0 buffer_store (list) buf_num 0 tmp_num 0
	sample_path "apps/edit/app.lisp")

;vdu doesn't play well with textfields.
(ui-tree window (create-window (+ window_flag_close window_flag_min window_flag_max)) ('color argb_white)
	(ui-element _ (create-flow) ('flow_flags (logior flow_flag_down flow_flag_fillw flow_flag_lasth))	
		(ui-element _ (create-grid) ('grid_width 2 'grid_height 1)
			(component-connect (ui-element _ (create-button) ('text "New" 'color toolbar_col)) event_new)
			(component-connect (ui-element _ (create-button) ('text "Save" 'color toolbar_col)) event_save))
			; (ui-element textfield (create-textfield) ('text "" 'color argb_white))
			; (ui-element label (create-label)('text "" 'color toolbar_col)))
		(ui-element _ (create-flow) ('flow_flags (logior flow_flag_left flow_flag_fillh flow_flag_lastw))
			(component-connect (ui-element slider (create-slider) ('color slider_col)) event_win_scroll)
			(ui-element vdu (create-vdu) 
				('vdu_width vdu_width 'vdu_height vdu_height 'min_width vdu_width 'min_height vdu_height
				'ink_color argb_black 'font (create-font "fonts/Hack-Regular.ctf" 16))))))

(gui-add (apply view-change (cat (list window 48 16)
	(view-pref-size (window-set-title
		(component-connect (window-connect-close (window-connect-min
			(window-connect-max window event_win_max) event_win_min) event_win_close) event_win_layout) 
				"edit")))))

(defun-bind new-buffer ()
	(defq path (cat "apps/login/Guest/" "tmp_txt_" (str (setq tmp_num (inc tmp_num))))
		text (list (join " " (ascii-char 10))) ox 0 oy 0 cx 0 cy 0)
	(list path text ox oy cx cy))

(defun-bind open-buffer (path)
	;if optional pos, implementor must check if it's a valid position.
	;(if (not pos) (defq pos '(0 0 0 0)))
	(defq text (list) ox 0 oy 0 cx 0 cy 0)
	(each-line (lambda (_) (push text _)) (file-stream path))
	;(push text (ascii-char 10))
	; (bind '(_ _ _ _) position)
	(list path text ox oy cx cy))

;returns -1 if not found or if no buffers are in buffer_store. Otherwise, it returns index.
(defun-bind find-buffer (path)
	(defq i 0 ret -1)
	(unless (= (length buffer_store) 0)
		(while (<= i (length buffer_store))
			(if (eql path (elem 0 (elem i buffer_store)))
				(setq ret i)) (inc i))) ret)

(defun-bind save-buffer ()
	(save (join buffer_text (ascii-char 10)) buffer_path))

(defun-bind window-resize (w h)
	(setq vdu_width w vdu_height h)
	(set vdu 'vdu_width w 'vdu_height h 'min_width w 'min_height h)
	(bind '(x y _ _) (view-get-bounds window))
	(bind '(w h) (view-pref-size window))
	(set vdu 'min_width vdu_min_width 'min_height vdu_min_height)
	(view-change-dirty window x y w h)
	(vdu-load vdu buffer_text buffer_ox buffer_oy buffer_cx buffer_cy))

(defun-bind window-layout (w h)
	(setq vdu_width w vdu_height h)
	(set vdu 'vdu_width w 'vdu_height h 'min_width w 'min_height h)
	(bind '(x y _ _) (view-get-bounds vdu))
	(bind '(w h) (view-pref-size vdu))
	(set vdu 'min_width vdu_min_width 'min_height vdu_min_height)
	(view-change vdu x y w h)
	;set slider values
	(def slider 'maximum (max 0 (- (length buffer_text) vdu_height)) 'portion vdu_height 'value buffer_oy)
	(view-dirty slider)
	(vdu-load vdu buffer_text buffer_ox buffer_oy buffer_cx buffer_cy))

(defun-bind vdu-input (c)
	;'buffer_text ox oy cx cy' could also be abstracted
	;into a text buffer object and then we can have multiple text buffers and
	;all that good stuff too.
	(cond
		((or (= c 10) (= c 13))
			(return)
			(setq buffer_cx 0))
		((= c 8)
			(backspace)
			(setq sx buffer_cx))
		((or (= c 9) (<= 32 c 127))
			(printable c)
			(setq sx buffer_cx))
		((= c 0x40000050)
			(left)
			(setq sx buffer_cx))
		((= c 0x4000004f)
			(right)
			(setq sx buffer_cx))
		((= c 0x40000052)
			(up))
		((= c 0x40000051)
			(down)))
	; ensures behavior resembling other text editors when adjusting cx
	(set-sticky)
	(bind '(buffer_ox buffer_oy) (cursor-visible))
	(set slider 'value buffer_oy)
	(vdu-load vdu buffer_text 0 (get slider 'value) buffer_cx buffer_cy)
	(vdu-load vdu buffer_text buffer_ox buffer_oy buffer_cx buffer_cy)
	(view-dirty slider))

(defq current_buffer (open-buffer sample_path))
(bind '(buffer_path buffer_text buffer_ox buffer_oy buffer_cx buffer_cy) current_buffer)
(window-layout vdu_width vdu_height)

(while id
	(cond
		((= (setq id (get-long (defq msg (mail-read (task-mailbox))) 
			ev_msg_target_id)) event_win_close)
			;close button
			(setq id nil))
		((= id event_new)
			(push buffer_store current_buffer)
			(setq current_buffer (new-buffer))
			(bind '(buffer_path buffer_text buffer_ox buffer_oy buffer_cx buffer_cy) current_buffer)
			(window-layout vdu_width vdu_height))
		((= id event_save)
			(save-buffer))
		((= id event_win_layout)
			;user window resize
			(apply window-layout (vdu-max-size vdu)))
		((= id event_win_min)
			;min button
			(vdu-resize 60 40))
		((= id event_win_max)
			;max button
			(vdu-resize 120 40))
		((= id event_win_scroll)
			;user scroll bar
			(vdu-load vdu buffer_text 0 (get slider 'value) buffer_cx buffer_cy))
		(t
			(view-event window msg)
			(and (= (get-long msg ev_msg_type) ev_type_key)
				(> (get-int msg ev_msg_key_keycode) 0)
				(vdu-input (get-int msg ev_msg_key_key))))))

(view-hide window)