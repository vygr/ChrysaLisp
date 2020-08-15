;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'event 0
	(byte 'debug 'hvalue)
	(byte 'play 'pause 'step 'clear)
	(byte 'play_all 'pause_all 'step_all 'clear_all)
	(byte 'close))

(defq vdu_width 60 vdu_height 40 buf_keys (list) buf_list (list) buf_index nil id t)

;single instance only
(when (= (length (mail-enquire "DEBUG_SERVICE")) 0)
	(mail-declare "DEBUG_SERVICE" (task-mailbox))

(structure 'debug_msg 0
	(long 'command 'reply_id 'tcb)
	(offset 'data))

(ui-window window (:color 0xc0000000)
	(ui-flow _ (:flow_flags flow_down_fill)
		(ui-title-bar _ "Debug" (0xea19) (const event_close))
		(ui-tool-bar _ ()
			(ui-buttons (0xe95e 0xe95d 0xe95c 0xe960) (const event_play))
			(ui-buttons (0xe95e 0xe95d 0xe95c 0xe960) (const event_play_all) (:color (const *env_toolbar2_col*))))
		(component-connect (ui-slider hslider (:value 0)) event_hvalue)
		(ui-vdu vdu (:vdu_width vdu_width :vdu_height vdu_height :ink_color argb_yellow))))

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
	(defq val (get :value hslider) mho (max 0 (dec (length buf_list))))
	(def hslider :maximum mho :portion 1 :value (min val mho))
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
			(def hslider :value _)
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
				"then use (defun-debug name ([arg ...]) body)") 0 0 0 1000)))
	(set-slider-values))

(defun-bind main ()
	(bind '(w h) (view-pref-size window))
	(bind '(x y w h) (view-locate w h))
	(gui-add (view-change window x y w h))
	(reset)
	(while id
		(cond
			;close ?
			((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_close)
				(setq id nil))
			;new debug msg
			((= id event_debug)
				(defq reply_id (get-long msg debug_msg_reply_id)
					tcb (get-long msg debug_msg_tcb)
					data (slice debug_msg_data -1 msg)
					key (sym (str (>> reply_id 32) ":" tcb))
					index (find-rev key buf_keys))
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
			((= id event_hvalue)
				(reset (get :value hslider)))
			;pressed play button
			((= id event_play)
				(when buf_index
					(play (elem buf_index buf_list))))
			;pressed pause button
			((= id event_pause)
				(when buf_index
					(pause (elem buf_index buf_list))))
			;pressed step button
			((= id event_step)
				(when buf_index
					(step (elem buf_index buf_list))))
			;pressed clear button
			((= id event_clear)
				(when buf_index
					(step (elem buf_index buf_list))
					(setq buf_keys (cat (slice 0 buf_index buf_keys) (slice (inc buf_index) -1 buf_keys)))
					(setq buf_list (cat (slice 0 buf_index buf_list) (slice (inc buf_index) -1 buf_list)))
					(reset (min buf_index (dec (length buf_list))))))
			;pressed play all button
			((= id event_play_all)
				(each play buf_list))
			;pressed pause all button
			((= id event_pause_all)
				(each pause buf_list))
			;pressed step all button
			((= id event_step_all)
				(each step buf_list))
			;pressed clear all button
			((= id event_clear_all)
				(each step buf_list)
				(reset))
			;otherwise
			(t (view-event window msg))))
	(mail-forget "DEBUG_SERVICE" (task-mailbox))
	(view-hide window))
)
