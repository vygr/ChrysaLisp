(import "././login/env.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

(enums +event 0
	(enum close)
	(enum hvalue)
	(enum clear)
	(enum clear_all))

(structure +profile_msg 0
	(netid tcb)
	(offset data))

(enums +profile_rec 0
	(enum buf))

(enums +select 0
	(enum main service tip))

(defq +width 60 +height 48)

(ui-window *window* (:color +argb_grey1)
	(ui-flow _ (:flow_flags +flow_down_fill)
		(ui-title-bar _ "Profile" (0xea19) +event_close)
		(ui-flow _ (:flow_flags +flow_right_fill)
			(ui-tool-bar *main_toolbar* ()
				(ui-buttons (0xe960) +event_clear))
			(ui-tool-bar *main_toolbar2* (:color (const *env_toolbar2_col*))
				(ui-buttons (0xe960) +event_clear_all))
			(ui-backdrop _ (:color (const *env_toolbar_col*))))
		(. (ui-slider *hslider* (:value 0)) :connect +event_hvalue)
		(ui-vdu *vdu* (:vdu_width +width :vdu_height +height :ink_color +argb_yellow))))

(defun vdu-print (vdu buf s)
	(defq ch (const (dec +height)) cl 0
		cl (some (# (if (eql %0 (ascii-char 10)) (if (= (setq cl (inc cl)) ch) _))) s))
	(. buf :paste (slice 0 (if cl (inc cl) -1) s))
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

(defun reset (&optional _)
	(setd _ -1)
	(if (<= 0 _ (dec (length buf_list)))
		(progn
			(def *hslider* :value _)
			(setq selected_index _)
			(vdu-print *vdu* (elem-get +profile_rec_buf (elem-get selected_index buf_list)) ""))
		(progn
			(clear buf_list)
			(clear buf_keys)
			(setq selected_index :nil)
			(. *vdu* :load '(
				{ChrysaLisp Profile 0.2}
				{Toolbar1 buttons act on a single task.}
				{Toolbar2 buttons act on all tasks.}
				{Slider to switch between tasks.}
				{}
				{In Lisp files:}
				{}
				{add (import "lib/debug/profile.inc")}
				{to profile all functions/methods.}
				{}
				{Use:}
				{}
				{(profile-report name [reset])}
				{}
				{to send the report.}) 0 0 0 1000)))
	(set-slider-values))

(defun tooltips ()
	(def *window* :tip_mbox (elem-get +select_tip select))
	(ui-tool-tips *main_toolbar*
		'("clear"))
	(ui-tool-tips *main_toolbar2*
		'("clear all")))

(defun main ()
	(defq select (alloc-select +select_size) syntax (Syntax)
		buf_keys (list) buf_list (list) selected_index :nil id :t
		entry (mail-declare (elem-get +select_service select) "*Profile" "Profile Service 0.1"))
	(tooltips)
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(reset)
	(while id
		(defq idx (mail-select select) *msg* (mail-read (elem-get idx select)))
		(cond
			;new profile msg
			((= idx +select_service)
				(defq tcb (getf *msg* +profile_msg_tcb)
					data (slice +profile_msg_data -1 *msg*)
					key (sym (str tcb))
					index (find-rev key buf_keys))
				(unless index
					(push buf_keys key)
					(push buf_list (list (Buffer :t syntax)))
					(reset (setq index (dec (length buf_list)))))
				(defq buf_rec (elem-get index buf_list)
					buf (elem-get +profile_rec_buf buf_rec))
				(vdu-print (if (= index selected_index) *vdu*) buf data))
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			;close ?
			((= (setq id (getf *msg* +ev_msg_target_id)) +event_close)
				(setq id :nil))
			;moved task slider
			((= id +event_hvalue)
				(reset (get :value *hslider*)))
			;pressed clear button
			((= id +event_clear)
				(when selected_index
					(setq buf_keys (cat (slice 0 selected_index buf_keys) (slice (inc selected_index) -1 buf_keys)))
					(setq buf_list (cat (slice 0 selected_index buf_list) (slice (inc selected_index) -1 buf_list)))
					(reset (min selected_index (dec (length buf_list))))))
			;pressed clear all button
			((= id +event_clear_all)
				(reset))
			;otherwise
			(:t (. *window* :event *msg*))))
	(mail-forget entry)
	(free-select select)
	(gui-sub *window*))
