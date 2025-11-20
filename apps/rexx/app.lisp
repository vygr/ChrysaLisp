;debug options
(case 2
(0 (import "lib/debug/frames.inc"))
(1 (import "lib/debug/profile.inc"))
(2 (import "lib/debug/debug.inc")))

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "./app.inc")
(import "./interpreter.inc")

;our UI widgets
(import "./widgets.inc")

;action handlers
(import "./actions.inc")

(defun main ()
	(gui-init)
	(defq select (task-mboxes +select_size) *running* :t
		*window* (create-main-window))
	(. *window* :tip_mbox (elem-get select +select_tip))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))

	;start SYSTEM port service as background task
	(open-task "apps/rexx/system_port.lisp" 0 +kn_call_open)

	;create REXX interpreter instance
	(def *window* :interpreter (RexxInterpreter))

	;event loop
	(mail-timeout (elem-get select +select_timer) +rate 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_tip)
				;tip event
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			((= idx +select_timer)
				;timer event
				(mail-timeout (elem-get select +select_timer) +rate 0))
			((defq id (getf *msg* +ev_msg_target_id) action (. *event_map* :find id))
				;call bound event action
				(dispatch-action action))
			((and (not (Textfield? (. *window* :find_id id)))
					(= (getf *msg* +ev_msg_type) +ev_type_key_down)
					(> (getf *msg* +ev_msg_key_scode) 0))
				;key event
				(handle-key-event *msg*))
			(:t ;gui event
				(. *window* :event *msg*))))

	(gui-sub-rpc *window*)
	(profile-report "REXX"))
