(import "././login/env.inc")
(import "gui/lisp.inc")

;our UI widgets and events
(import "./widgets.inc")

(structure +profile_msg 0
	(netid tcb)
	(offset data))

(enums +profile_rec 0
	(enum buf))

(enums +select 0
	(enum main service tip))

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

;import actions, bindings and app ui classes
(import "./actions.inc")

(defun dispatch-action (&rest action)
	(catch (eval action)
		(progn (print _)(print)
			(setq *refresh_mode* (list 0)) :t)))

(defun main ()
	(defq select (alloc-select +select_size) syntax (Syntax)
		buf_keys (list) buf_list (list) selected_index :nil *running* :t
		entry (mail-declare (elem-get +select_service select) "*Profile" "Profile Service 0.1"))
	(def *window* :tip_mbox (elem-get +select_tip select))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front (. *window* :change x y w h))
	(reset)
	(while *running*
		(defq *msg* (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			((= idx +select_tip)
				;tip time mail
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
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
			;must be GUI event
			((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
				;call bound event action
				(dispatch-action action))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key_down)
					(> (getf *msg* +ev_msg_key_scode) 0))
				;key event
				(defq key (getf *msg* +ev_msg_key_key)
					mod (getf *msg* +ev_msg_key_mod))
				(cond
					((/= 0 (logand mod (const
							(+ +ev_key_mod_control +ev_key_mod_alt +ev_key_mod_meta))))
						;call bound control/command key action
						(when (defq action (. *key_map_control* :find key))
							(dispatch-action action)))
					((/= 0 (logand mod +ev_key_mod_shift))
						;call bound shift key action, else insert
						(cond
							((defq action (. *key_map_shift* :find key))
								(dispatch-action action))
							((<= +char_space key +char_tilde)
								;insert char etc ...
								(char key))))
					((defq action (. *key_map* :find key))
						;call bound key action
						(dispatch-action action))
					((<= +char_space key +char_tilde)
						;insert char etc ...
						(char key))))
			;otherwise
			(:t (. *window* :event *msg*))))
	(mail-forget entry)
	(free-select select)
	(gui-sub *window*))
