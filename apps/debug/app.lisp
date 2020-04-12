;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'win_debug 'win_hvalue)
	(byte 'win_play 'win_pause 'win_step 'win_clear)
	(byte 'win_play_all 'win_pause_all 'win_step_all 'win_clear_all))

;single instance only
(unless (mail-enquire "DEBUG_SERVICE")
	(kernel-declare "DEBUG_SERVICE" (task-mailbox))

(structure 'debug_msg 0
	(long 'command 'reply_id 'tcb)
	(offset 'data))

(defq vdu_width 60 vdu_height 30 buf_keys (list) buf_list (list) buf_index nil)

(ui-window window ('color 0xc0000000)
	(ui-flow _ ('flow_flags flow_down_fill)
		(ui-title _ ('text "Debug"))
		(ui-tool-bar _ ()
			(ui-buttons (0xe95e 0xe95d 0xe95c 0xe960) (const event_win_play))
			(ui-buttons (0xe95e 0xe95d 0xe95c 0xe960) (const event_win_play_all) nil ('color (const *env_toolbar2_col*))))
		(component-connect (ui-slider hslider ('value 0)) event_win_hvalue)
		(ui-vdu vdu ('vdu_width vdu_width 'vdu_height vdu_height 'ink_color argb_yellow))))

(defun-bind vdu-print (vdu buf s)
	(each (lambda (c)
		(cond
			((eql c (ascii-char 10))
				;line feed and truncate
				(if (> (length (push buf "")) (const vdu_height))
					(setq buf (slice (const (dec (neg vdu_height))) -1 buf))))
			(t	;char
				(elem-set -2 buf (cat (elem -2 buf) c))))) s)
	(if vdu (vdu-load vdu buf 0 0 (length (elem -2 buf)) (dec (length buf)))) buf)

(defun set-slider-values ()
	(defq val (get hslider 'value) mho (max 0 (dec (length buf_list))))
	(def hslider 'maximum mho 'portion 1 'value (min val mho))
	(view-dirty hslider))

(defun play (_)
	(unless (elem 1 _)
		(step _))
	(elem-set 1 _ t))

(defun pause (_)
	(elem-set 1 _ nil))

(defun step (_)
	(when (elem 2 _)
		(mail-send "" (elem 2 _))
		(elem-set 2 _ nil)))

(defun reset (&optional _)
	(setd _ -1)
	(if (<= 0 _ (dec (length buf_list)))
		(progn
			(def hslider 'value _)
			(setq buf_index _)
			(vdu-print vdu (elem 0 (elem buf_index buf_list)) ""))
		(progn
			(clear buf_list)
			(clear buf_keys)
			(setq buf_index nil)
			(vdu-load vdu '(
				"ChrysaLisp Debug 0.3"
				"Toolbar1 buttons act on a single task."
				"Toolbar2 buttons act on all tasks."
				"Slider to switch between tasks."
				""
				"In Lisp files:"
				"add (import 'class/lisp/debug.inc)"
				"then use (defun-debug name ([arg ...]) body)"
				""
				"In VP files:"
				"use (debug-reg)") 0 0 0 1000)))
	(set-slider-values))

(defun-bind main ()
	(gui-add (apply view-change (cat (list window 640 16) (view-pref-size window))))
	(reset)
	(while t
		(cond
			;new debug msg
			((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_debug)
				(defq reply_id (get-long msg debug_msg_reply_id)
					tcb (get-long msg debug_msg_tcb)
					data (get-cstr msg debug_msg_data)
					key (sym (str (>> reply_id 32) ":" tcb))
					index (find key buf_keys))
				(unless index
					(push buf_keys key)
					(push buf_list (list (list "") nil nil))
					(reset (setq index (dec (length buf_list)))))
				(elem-set 0 (defq buf_rec (elem index buf_list))
					(vdu-print (if (= index buf_index) vdu) (elem 0 buf_rec) data))
				(if (elem 1 buf_rec)
					(mail-send "" reply_id)
					(elem-set 2 buf_rec reply_id)))
			;moved task slider
			((= id event_win_hvalue)
				(reset (get hslider 'value)))
			;pressed play button
			((= id event_win_play)
				(when buf_index
					(play (elem buf_index buf_list))))
			;pressed pause button
			((= id event_win_pause)
				(when buf_index
					(pause (elem buf_index buf_list))))
			;pressed step button
			((= id event_win_step)
				(when buf_index
					(step (elem buf_index buf_list))))
			;pressed clear button
			((= id event_win_clear)
				(when buf_index
					(step (elem buf_index buf_list))
					(setq buf_keys (cat (slice 0 buf_index buf_keys) (slice (inc buf_index) -1 buf_keys)))
					(setq buf_list (cat (slice 0 buf_index buf_list) (slice (inc buf_index) -1 buf_list)))
					(reset (min buf_index (dec (length buf_list))))))
			;pressed play all button
			((= id event_win_play_all)
				(each play buf_list))
			;pressed pause all button
			((= id event_win_pause_all)
				(each pause buf_list))
			;pressed step all button
			((= id event_win_step_all)
				(each step buf_list))
			;pressed clear all button
			((= id event_win_clear_all)
				(each step buf_list)
				(reset))
			;otherwise
			(t (view-event window msg))))
	(view-hide window))
)
