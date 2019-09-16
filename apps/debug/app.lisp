;imports
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

;single instance only
(unless (mail-enquire "DEBUG_SERVICE")
	(kernel-declare "DEBUG_SERVICE" (task-mailbox))

(structure 'debug_msg 0
	(long 'command 'reply_id 'tcb)
	(offset 'data))

(structure 'event 0
	(byte 'win_debug 'win_hvalue)
	(byte 'win_play 'win_pause 'win_step 'win_clear)
	(byte 'win_play_all 'win_pause_all 'win_step_all 'win_clear_all))

(defq vdu_width 60 vdu_height 30 vdu_index nil vdu_keys (list) vdu_list (list))

(ui-tree window (create-window window_flag_status) ('color 0xc0000000)
	(ui-element vdu_flow (create-flow) ('flow_flags (logior flow_flag_down flow_flag_fillw))
		(ui-element _ (create-flow) ('flow_flags (logior flow_flag_right flow_flag_fillh)
				'font (create-font "fonts/Entypo.otf" 32))
			(each (lambda (l)
				(button-connect-click (ui-element __ (create-button)
					('text l 'color (if (>= _ 4) toolbar2_col toolbar_col))) (+ event_win_play _)))
						'("" "" "" "" "" "" "" "")))
		(slider-connect-value (ui-element hslider (create-slider) ('value 0 'color slider_col)) event_win_hvalue)
		(ui-element vdu (create-view))))

(defun set-slider-values ()
	(defq val (get hslider 'value) mho (max 0 (dec (length vdu_list))))
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
	(if (<= 0 _ (dec (length vdu_list)))
		(progn
			(def hslider 'value _)
			(view-sub vdu)
			(view-dirty (view-layout (view-add-back vdu_flow
				(setq vdu (elem 0 (elem (setq vdu_index _) vdu_list)))))))
		(progn
			(clear vdu_list)
			(clear vdu_keys)
			(setq vdu_index nil)
			(view-sub vdu)
			(def (setq vdu (create-vdu)) 'vdu_width vdu_width 'vdu_height vdu_height 'ink_color argb_yellow
				'font (create-font "fonts/Hack-Regular.ttf" 16))
			(view-layout (view-add-back vdu_flow vdu))
			(view-dirty (vdu-print vdu (cat
				"ChrysaLisp Debug 0.3" (ascii-char 10)
				"Toolbar1 buttons act on a single task." (ascii-char 10)
				"Toolbar2 buttons act on all tasks." (ascii-char 10)
				"Slider to switch between tasks." (ascii-char 10) (ascii-char 10)
				"In Lisp files:" (ascii-char 10)
				"add (import 'class/lisp/debug.inc)" (ascii-char 10)
				"then use (defun-debug name ([arg ...]) body)" (ascii-char 10) (ascii-char 10)
				"In VP files:" (ascii-char 10)
				"use (debug-reg)" (ascii-char 10))))))
	(set-slider-values))

(reset)
(gui-add (apply view-change (cat (list window 640 16)
	(view-pref-size (window-set-title (window-set-status window "Ready") "Debug")))))

(while t
	(cond
		;new debug msg
		((= (defq id (get-long (defq msg (mail-mymail)) ev_msg_target_id)) event_win_debug)
			(defq reply_id (get-long msg debug_msg_reply_id)
				tcb (get-long msg debug_msg_tcb)
				data (sym (get-cstr msg debug_msg_data))
				key (sym (str (>> reply_id 32) ":" tcb))
				index (find key vdu_keys))
			(unless index
				(def (defq new_vdu (create-vdu)) 'vdu_width vdu_width 'vdu_height vdu_height
					'ink_color argb_yellow 'font (create-font "fonts/Hack-Regular.ttf" 16))
				(push vdu_keys key)
				(push vdu_list (list new_vdu nil nil))
				(reset (setq index (dec (length vdu_list)))))
			(vdu-print (elem 0 (defq vdu_rec (elem index vdu_list))) data)
			(if (elem 1 vdu_rec)
				(mail-send "" reply_id)
				(elem-set 2 vdu_rec reply_id)))
		;moved task slider
		((= id event_win_hvalue)
			(when vdu_index
				(reset (get hslider 'value))))
		;pressed play button
		((= id event_win_play)
			(when vdu_index
				(play (elem vdu_index vdu_list))))
		;pressed pause button
		((= id event_win_pause)
			(when vdu_index
				(pause (elem vdu_index vdu_list))))
		;pressed step button
		((= id event_win_step)
			(when vdu_index
				(step (elem vdu_index vdu_list))))
		;pressed clear button
		((= id event_win_clear)
			(when vdu_index
				(step (elem vdu_index vdu_list))
				(setq vdu_keys (cat (slice 0 vdu_index vdu_keys) (slice (inc vdu_index) -1 vdu_keys)))
				(setq vdu_list (cat (slice 0 vdu_index vdu_list) (slice (inc vdu_index) -1 vdu_list)))
				(reset (min vdu_index (dec (length vdu_list))))))
		;pressed play all button
		((= id event_win_play_all)
			(each play vdu_list))
		;pressed pause all button
		((= id event_win_pause_all)
			(each pause vdu_list))
		;pressed step all button
		((= id event_win_step_all)
			(each step vdu_list))
		;pressed clear all button
		((= id event_win_clear_all)
			(each step vdu_list)
			(reset))
		;otherwise
		(t (view-event window msg))))
)
