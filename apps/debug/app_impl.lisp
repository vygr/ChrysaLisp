(import "././login/env.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close)
	(enum hvalue)
	(enum play forward pause step clear)
	(enum play_all forward_all pause_all step_all clear_all))

(structure +debug 0
	(netid reply origin)
	(int type)
	(offset data))

(enums +debug_rec 0
	(enum buf state reply_id))

(enums +select 0
	(enum main service tip exit))

(defq +width 60 +height 40 +rate_exit 1000000)

(ui-window *window* (:color +argb_grey1)
	(ui-flow _ (:flow_flags +flow_down_fill)
		(ui-title-bar _ "Debug" (0xea19) +event_close)
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-tool-bar *main_toolbar* ()
				(ui-buttons (0xe95e 0xe95a 0xe95d 0xe95c 0xe960) +event_play))
			(ui-tool-bar *main_toolbar2* (:color (const *env_toolbar2_col*))
				(ui-buttons (0xe95e 0xe95a 0xe95d 0xe95c 0xe960) +event_play_all))
			(ui-backdrop _ (:color (const *env_toolbar_col*))))
		(. (ui-slider *hslider* (:value 0)) :connect +event_hvalue)
		(ui-vdu *vdu* (:vdu_width +width :vdu_height +height :ink_color +argb_yellow))))

(defun vdu-print (vdu buf s)
	(defq ch (const (dec +height)))
	(. buf :paste s)
	(bind '(w h) (. buf :get_size))
	(when (> h ch)
		(.-> buf (:set_cursor 0 0) (:cut 0 (- h ch)))
		(. buf :set_cursor 0 ch))
	(. buf :clear_undo)
	(if vdu (. buf :vdu_load vdu 0 0)))

(defun set-slider-values ()
	(defq val (get :value *hslider*) mho (max 0 (dec (length buf_list))))
	(def *hslider* :maximum mho :portion 1 :value (min val mho))
	(. *hslider* :dirty))

(defun step (_)
	(when (elem-get +debug_rec_reply_id _)
		(mail-send (elem-get +debug_rec_reply_id _) "")
		(elem-set +debug_rec_reply_id _ :paused)))

(defun play (_)
	(if (eql (elem-get +debug_rec_state _) :paused) (step _))
	(elem-set +debug_rec_state _ :play))

(defun forward (_)
	(if (eql (elem-get +debug_rec_state _) :paused) (step _))
	(elem-set +debug_rec_state _ :forward))

(defun pause (_)
	(elem-set +debug_rec_state _ :paused))

(defun reset (&optional _)
	(setd _ -1)
	(if (<= 0 _ (dec (length buf_list)))
		(progn
			(def *hslider* :value _)
			(setq buf_index _)
			(vdu-print *vdu* (elem-get +debug_rec_buf (elem-get buf_index buf_list)) ""))
		(progn
			(clear buf_list)
			(clear buf_keys)
			(setq buf_index :nil)
			(. *vdu* :load '(
				{ChrysaLisp Debug 0.5}
				{Toolbar1 buttons act on a single task.}
				{Toolbar2 buttons act on all tasks.}
				{Slider to switch between tasks.}
				{}
				{In Lisp files:}
				{}
				{add (import "lib/debug/debug.inc")}
				{to debug all functions/methods.}
				{and use:}
				{}
				{(debug-brk xxx form)}
				{}
				{as a conditional breakpoint.}) 0 0 0 1000)))
	(set-slider-values))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get +select_tip select))
	(ui-tool-tips *main_toolbar*
		'("play" "forward" "pause" "step" "clear"))
	(ui-tool-tips *main_toolbar2*
		'("play all" "forward all" "pause all" "step all" "clear all")))

(defun main ()
	(defq select (alloc-select +select_size) syntax (Syntax)
		buf_keys (list) buf_list (list) buf_index :nil id :t
		entry (mail-declare (elem-get +select_service select) "*Debug" "Debug Service 0.4"))
	(tooltips)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(reset)
	(while id
		(defq idx (mail-select select) *msg* (mail-read (elem-get idx select)))
		(cond
			;new debug *msg*
			((= idx +select_service)
				(defq reply_id (getf *msg* +debug_reply)
					key (sym (getf *msg* +debug_origin))
					type (getf *msg* +debug_type)
					data (slice +debug_data -1 *msg*)
					index (find-rev key buf_keys))
				(unless index
					(push buf_keys key)
					(push buf_list (list (Buffer :t syntax) :paused :nil))
					(reset (setq index (dec (length buf_list)))))
				(defq buf_rec (elem-get index buf_list)
					buf (elem-get +debug_rec_buf buf_rec)
					state (elem-get +debug_rec_state buf_rec))
				(when (> type 0)
					(vdu-print (if (= index buf_index) *vdu*) buf data)
					(pause (elem-get index buf_list)))
				(unless (eql state :forward)
					(vdu-print (if (= index buf_index) *vdu*) buf data))
				(if (or (eql (defq state (elem-get +debug_rec_state buf_rec)) :play)
						(eql state :forward))
					(mail-send reply_id "")
					(elem-set +debug_rec_reply_id buf_rec reply_id)))
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_exit)
				;exit mail
				(setq id :nil))
			;close ?
			((= (setq id (getf *msg* +ev_msg_target_id)) +event_close)
				;drop the service entry now !
				(mail-forget entry)
				;few seconds delay till exit !
				;just let any stray debug sends arrive.
				(mail-timeout (elem-get +select_exit select) +rate_exit 0))
			;moved task slider
			((= id +event_hvalue)
				(reset (get :value *hslider*)))
			;pressed play button
			((= id +event_play)
				(when buf_index
					(play (elem-get buf_index buf_list))))
			;pressed forward button
			((= id +event_forward)
				(when buf_index
					(forward (elem-get buf_index buf_list))))
			;pressed pause button
			((= id +event_pause)
				(when buf_index
					(pause (elem-get buf_index buf_list))))
			;pressed step button
			((= id +event_step)
				(when buf_index
					(pause (elem-get buf_index buf_list))
					(step (elem-get buf_index buf_list))))
			;pressed clear button
			((= id +event_clear)
				(when buf_index
					(step (elem-get buf_index buf_list))
					(setq buf_keys (cat (slice 0 buf_index buf_keys) (slice (inc buf_index) -1 buf_keys)))
					(setq buf_list (cat (slice 0 buf_index buf_list) (slice (inc buf_index) -1 buf_list)))
					(reset (min buf_index (dec (length buf_list))))))
			;pressed play all button
			((= id +event_play_all)
				(each play buf_list))
			;pressed foward all button
			((= id +event_forward_all)
				(each forward buf_list))
			;pressed pause all button
			((= id +event_pause_all)
				(each pause buf_list))
			;pressed step all button
			((= id +event_step_all)
				(each pause buf_list)
				(each step buf_list))
			;pressed clear all button
			((= id +event_clear_all)
				(each step buf_list)
				(reset))
			;otherwise
			(:t (. *window* :event *msg*))))
	(free-select select)
	(gui-sub *window*)
	;restart any paused debug sends
	(each play buf_list))
