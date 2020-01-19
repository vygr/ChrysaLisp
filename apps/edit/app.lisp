;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_close 'win_min 'win_max))

(defq id t vdu_width 60 vdu_height 40 cursor_x 0 cursor_y 0 offset_x 0 offset_y 0 text_buf (list) sticky_x 0)

;this will need scrollbars etc to change the text offset in the VDU area
(ui-tree window (create-window (+ window_flag_close window_flag_min window_flag_max)) ('color 0xc0000000)
	(ui-element vdu (create-vdu) ('vdu_width vdu_width 'vdu_height vdu_height 'ink_color argb_green
		'font (create-font "fonts/Hack-Regular.ttf" 16))))

(gui-add (apply view-change (cat (list window 32 32)
	(view-pref-size (window-set-title (window-connect-close (window-connect-min
		(window-connect-max window event_win_max) event_win_min) event_win_close) "Editor - Test code for now !!!")))))

;load some temp text for now
(each-line (lambda (_)
	(push text_buf _)) (file-stream "apps/edit/app.lisp"))

(defun-bind edit-input (c)
	;insert at cursor etc. Things like char insert and so forth probably want to be
	;asbstracted into some text_buf commands that can later be scripted and this would just
	;become key bindings to call things.
	;'text_buf offset_x offset_y cursor_x cursor_y' could also be abstracted into a text buffer
	;object and then we can have multiple text buffers and all that good stuff too.
	(cond
		((or (= c 10) (= c 13))
			;return key
			(cond
				((>= cursor_y (length text_buf))
					;off end of text so just append a blank line
					(push text_buf "")
					(setq cursor_y (length text_buf)))
				(t	;break this line
					(defq line (elem cursor_y text_buf)
						line_front (slice 0 (min cursor_x (length line)) line)
						line_back (slice (min cursor_x (length line)) -1 line))
					(elem-set cursor_y text_buf line_front)
					(setq cursor_y (min (inc cursor_y) (length text_buf))
						text_buf (insert text_buf cursor_y (list line_back)))))
				(setq cursor_x 0))
		((= c 0x40000050)
			;cursor left key
			(setq cursor_x (max (dec cursor_x) 0))
      (setq sticky_x cursor_x))
		((= c 0x4000004f)
			;cursor right key
			(defq line (if (>= cursor_y (length text_buf)) "" (elem cursor_y text_buf)))
			(setq cursor_x (min (inc cursor_x) (length line)))
      (setq sticky_x cursor_x))
		((= c 0x40000052)
			;cursor up key
			(setq cursor_y (max (dec cursor_y) 0))
      (setq cursor_x (min sticky_x (length (elem cursor_y text_buf)))))
		((= c 0x40000051)
			;cursor down key
      (if (= cursor_y (dec (length text_buf)))
        (setq cursor_x (length (elem cursor_y text_buf)) sticky_x cursor_x))
      (setq cursor_y  (min (inc cursor_y) (dec (length text_buf))))
      (setq cursor_x (min sticky_x (length (elem cursor_y text_buf)))))
		((= c 8)
			;backspace key
			(cond
				((> cursor_x 0)
				  	(elem-set cursor_y text_buf (erase (elem cursor_y text_buf) (dec cursor_x) cursor_x))
					(setq cursor_x (dec cursor_x)))
        			((<= cursor_y 0)
					(setq cursor_y 0))
				((<= cursor_x 0)
					;backspace into previous line
					(defq prev_line (elem (dec cursor_y) text_buf)
						cat_line (cat prev_line (elem cursor_y text_buf)))
					(setq cursor_x (length prev_line) cursor_y (dec cursor_y))
					(elem-set cursor_y text_buf cat_line)
					(setq text_buf (erase text_buf (inc cursor_y) (+ cursor_y 2))))))
		((or (= c 9) (<= 32 c 127))
			;insert the tab/char at cursor or append to end etc
			(defq line (if (>= cursor_y (length text_buf)) "" (elem cursor_y text_buf))
				line (insert line (min cursor_x (length line)) (ascii-char c)))
			(setq cursor_x (inc cursor_x))
			(if (>= cursor_y (length text_buf))
				(push text_buf line)
				(elem-set cursor_y text_buf line))))
	;set text offsets to ensure cursor visible, maybe add some margin ?
	(cond
		((< cursor_x offset_x)
			(setq offset_x cursor_x))
		((>= cursor_x (+ offset_x vdu_width))
			(setq offset_x (- cursor_x vdu_width -1))))
	(cond
		((< cursor_y offset_y)
			(setq offset_y cursor_y))
		((>= cursor_y (+ offset_y vdu_height))
			(setq offset_y (- cursor_y vdu_height -1))))
  ; ensures behavior resembling other text editors when adjusting cursor_x
  (unless (= cursor_y (dec (length text_buf)))
    (if (and (> cursor_x sticky_x) (<= sticky_x (length (elem cursor_y text_buf))))
      (setq cursor_x sticky_x))
    (if (>= cursor_x (length (elem cursor_y text_buf)))
      (setq cursor_x (length (elem cursor_y text_buf)))))
	;load the vdu display
	(vdu-load vdu text_buf offset_x offset_y cursor_x cursor_y))

(vdu-load vdu text_buf offset_x offset_y cursor_x cursor_y)
(while id
	(cond
		((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
			;close button
			(setq id nil))
		((= id event_win_min)
			;min button
			(setq vdu_width 60 vdu_height 40)
			(set vdu 'vdu_width vdu_width 'vdu_height vdu_height)
			(bind '(x y _ _) (view-get-bounds window))
			(bind '(w h) (view-pref-size window))
			(view-change-dirty window x y w h)
			(vdu-load vdu text_buf offset_x offset_y cursor_x cursor_y))
		((= id event_win_max)
			;max button
			(setq vdu_width 120 vdu_height 40)
			(set vdu 'vdu_width vdu_width 'vdu_height vdu_height)
			(bind '(x y _ _) (view-get-bounds window))
			(bind '(w h) (view-pref-size window))
			(view-change-dirty window x y w h)
			(vdu-load vdu text_buf offset_x offset_y cursor_x cursor_y))
		(t	;it's a GUI event
			(view-event window msg)
			(and (= (get-long msg ev_msg_type) ev_type_key)
				(> (get-int msg ev_msg_key_keycode) 0)
				(edit-input (get-int msg ev_msg_key_key))))))

(view-hide window)
