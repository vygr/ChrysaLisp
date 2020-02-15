;TODO:
;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/edit/input.inc)
<<<<<<< HEAD
(import 'apps/login/pupa.inc)

(structure 'event 0
	(byte 'win_close 'win_min 'win_max 'win_layout 
		'win_scroll 'new 'save 'open))

;TODO: implement for buffer store struct
(structure 'stored 0
	(byte 'buffer_path 'buffer 'pos))
 
(defq id t vdu_min_width 40 vdu_min_height 24 vdu_width 60 vdu_height 40 sx 0 buffer_store (list) buf_num 0 tmp_num 0
	msg_path "apps/edit/message" home_dir (cat "apps/login/" *env_user* "/") buffer_path msg_path buffer (list)
	ox 0 oy 0 cx 0 cy 0 sx 0 pos (list ox oy cx cy sx))

(ui-tree window (create-window (+ window_flag_close window_flag_min window_flag_max)) ('color argb_grey2)
	(ui-element _ (create-flow) ('flow_flags (logior flow_flag_down flow_flag_fillw flow_flag_lasth))	
		(ui-element _ (create-grid) ('grid_width 3 'grid_height 1)
			(component-connect (ui-element _ (create-button) ('text "New" 'color toolbar_col)) event_new)
			(component-connect (ui-element _ (create-button) ('text "Open" 'color toolbar_col)) event_open)
			(component-connect (ui-element _ (create-button) ('text "Save" 'color toolbar_col)) event_save))
			(ui-element textfield (create-textfield) ('text "" 'color argb_white))
=======

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
>>>>>>> b0fb575f93512378096a371bd34fcc7ded8129a1
		(ui-element _ (create-flow) ('flow_flags (logior flow_flag_left flow_flag_fillh flow_flag_lastw))
			(component-connect (ui-element slider (create-slider) ('color slider_col)) event_win_scroll)
			(ui-element vdu (create-vdu) 
				('vdu_width vdu_width 'vdu_height vdu_height 'min_width vdu_width 'min_height vdu_height
<<<<<<< HEAD
				'color argb_black 'ink_color argb_white 'font (create-font "fonts/Hack-Regular.ctf" 16))))))

(defun-bind window-close ()
	(mail-send (const (char event_win_close long_size)) (task-mailbox))
	(view-hide window))

=======
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

>>>>>>> b0fb575f93512378096a371bd34fcc7ded8129a1
(defun-bind window-resize (w h)
	(setq vdu_width w vdu_height h)
	(set vdu 'vdu_width w 'vdu_height h 'min_width w 'min_height h)
	(bind '(x y _ _) (view-get-bounds window))
	(bind '(w h) (view-pref-size window))
	(set vdu 'min_width vdu_min_width 'min_height vdu_min_height)
	(view-change-dirty window x y w h)
<<<<<<< HEAD
	(vdu-load vdu buffer ox oy cx cy))
=======
	(vdu-load vdu buffer_text buffer_ox buffer_oy buffer_cx buffer_cy))
>>>>>>> b0fb575f93512378096a371bd34fcc7ded8129a1

(defun-bind window-layout (w h)
	(setq vdu_width w vdu_height h)
	(set vdu 'vdu_width w 'vdu_height h 'min_width w 'min_height h)
	(bind '(x y _ _) (view-get-bounds vdu))
	(bind '(w h) (view-pref-size vdu))
<<<<<<< HEAD
	(bind '(tx ty _ _) (view-get-bounds textfield))
	(bind '(tw th) (view-pref-size textfield))
	(set vdu 'min_width vdu_min_width 'min_height vdu_min_height)
	(view-change vdu x y w h)
	(view-change textfield tx ty w th)
	;set slider and textfield values
	(def slider 'maximum (max 0 (- (length buffer) vdu_height)) 'portion vdu_height 'value oy)
	(def textfield 'text buffer_path)
	(view-dirty slider)
	(view-dirty textfield)
	(vdu-load vdu buffer ox oy cx cy))

;cursor_xy = (+ (mouse_xy / char_wh) offset_xy)
(defun-bind mouse-cursor (mouse_xy)
	(defq cursor_xy (list cx cy) char_wh (vdu-char-size vdu) offset_xy (list ox oy))
	(setq cursor_xy (map + (map / mouse_xy char_wh) offset_xy)
		cx (elem 0 cursor_xy) cy (elem 1 cursor_xy))
	(if (>= cx (length (elem cy buffer))) (set-sticky)) (setq sx cx))

(defun-bind new-buffer ()
	(setq buffer_path (cat home_dir "tmp_txt_" (str (setq tmp_num (inc tmp_num)))) 
		buffer (list (join " " (ascii-char 10))) ox 0 oy 0 cx 0 cy 0 sx 0 pos (list ox oy cx cy sx))
	(list buffer_path buffer pos))

(defun-bind open-buffer (path)
	(setq buffer_path path buffer (list) ox 0 oy 0 cx 0 cy 0 sx 0 pos (list ox oy cx cy sx))
	(each-line (lambda (_) (push buffer _)) (file-stream buffer_path))
	(list buffer_path buffer pos))

;returns -1 if not found or if no buffers are in buffer_store. Otherwise, it returns index.
; (defun-bind find-buffer (path)
; 	(defq i 0 ret -1)
; 	(unless (= (length buffer_store) 0)
; 		(while (<= i (length buffer_store))
; 			(if (eql path (elem 'stored_buffer_path (elem i buffer_store)))
; 				(setq ret i)) (inc i))) ret)

(defun-bind save-buffer ()
	(setq buffer_path (str (get textfield 'text)))
	(save (join buffer (ascii-char 10)) buffer_path))

(defun-bind vdu-input (c)
	(cond
		((or (= c 10) (= c 13))		(return) (setq cx 0))
		((= c 8)					(backspace) (setq sx cx))
		((or (= c 9) (<= 32 c 127))	(printable c) (setq sx cx))
		((= c 0x40000050)			(left) (setq sx cx))
		((= c 0x4000004f)			(right) (setq sx cx))
		((= c 0x40000052)			(up))
		((= c 0x40000051)			(down)))
	; ensures behavior resembling other editor interfaces when adjusting cx
	(set-sticky)
	(defq new_off (cursor-visible))
	(set slider 'value oy)
	(vdu-load vdu buffer 0 (get slider 'value) cx cy)
	(vdu-load vdu buffer ox oy cx cy)
	(view-dirty slider))

(defq current_buffer (open-buffer buffer_path))
(set textfield 'text buffer_path)

(gui-add (apply view-change (cat (list window 48 16)
	(view-pref-size (window-set-title (component-connect (window-connect-close (window-connect-min
		(window-connect-max window event_win_max) event_win_min) event_win_close) event_win_layout)
			"edit")))))

=======
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
>>>>>>> b0fb575f93512378096a371bd34fcc7ded8129a1
(window-layout vdu_width vdu_height)

(while id
	(cond
<<<<<<< HEAD
		((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
			(setq id nil)
			(window-close))
		((= id event_new)
			(window-layout vdu_width vdu_height)
			(setq current_buffer (new-buffer))
			(set textfield 'text buffer_path)
			(window-layout vdu_width vdu_height))
		((= id event_open)
			(setq buffer_path (get textfield 'text)
				current_buffer (open-buffer buffer_path))
			(window-layout vdu_width vdu_height))
		((= id event_save)
			(window-layout vdu_width vdu_height)
			(set textfield 'text buffer_path)
=======
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
>>>>>>> b0fb575f93512378096a371bd34fcc7ded8129a1
			(save-buffer))
		((= id event_win_layout)
			;user window resize
			(apply window-layout (vdu-max-size vdu)))
		((= id event_win_min)
			;min button
			(window-resize 60 40))
		((= id event_win_max)
			;max button
<<<<<<< HEAD
			(window-resize 120 40))
		((= id event_win_scroll)
			;user scroll bar
			(vdu-load vdu buffer 0 (defq new_oy (get slider 'value)) cx cy)
			(setq oy new_oy))
		((= id (component-get-id vdu))
			(view-event window msg)
			(cond 
				((and (= (get-long msg ev_msg_type) ev_type_key)
					(> (get-int msg ev_msg_key_keycode) 0))
					(vdu-input (get-int msg ev_msg_key_key)))
				((and (= (get-long msg ev_msg_type) ev_type_mouse)
					(/= (get-int msg ev_msg_mouse_buttons) 0))
					;(setq cx ox cy oy)
					(defq rx (get-int msg ev_msg_mouse_rx) ry (get-int msg ev_msg_mouse_ry) 
						mouse_xy (list rx ry))
					(mouse-cursor mouse_xy)
					(window-layout vdu_width vdu_height))))
		(t 
			(view-event window msg))))	
	(window-layout vdu_width vdu_height)
=======
			(vdu-resize 120 40))
		((= id event_win_scroll)
			;user scroll bar
			(vdu-load vdu buffer_text 0 (get slider 'value) buffer_cx buffer_cy))
		((= id (component-get-id vdu))
			(view-event window msg)
			(and (= (get-long msg ev_msg_type) ev_type_key)
				(> (get-int msg ev_msg_key_keycode) 0)
				(vdu-input (get-int msg ev_msg_key_key))))
		(t
			(view-event window msg))))
>>>>>>> b0fb575f93512378096a371bd34fcc7ded8129a1

(view-hide window)