;import settings
(run 'apps/sys.lisp)
(run 'apps/ui.lisp)

(defq app_list '(
	"apps/netmon/app"
	"apps/terminal/app"
	"apps/canvas/app.lisp"
	"apps/raymarch/app.lisp"
	"apps/calculator/app.lisp"
	"tests/farm.lisp"
	"tests/pipe.lisp"
	"tests/global.lisp"
	"tests/migrate.lisp")
	window (slot create_window nil 0)
	flow (slot create_flow nil))

(slot set_title window "Launcher")
(def-props flow 'flow_flags (bit-or flow_flag_down flow_flag_fillw) 'color 0xffffff00)
(each (lambda (_)
	(defq button (slot create_button nil))
	(def-props button 'text _)
	(slot connect_click button 0)
	(slot add_child flow button)) app_list)
(slot add_child window flow)
(bind '(w h) (slot pref_size window))
(slot change window 32 32 (add w 32) h)
(slot gui_add window)

(while t
	(cond
		((ge (read-long ev_msg_target_id (defq msg (slot mail_mymail nil))) 0)
			(slot open_child nil
				(get-prop (slot find_id window (read-long ev_msg_action_source_id msg)) 'text)
				kn_call_open))
		(t (slot event window msg))))
