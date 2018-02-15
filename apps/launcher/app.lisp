;import settings
(run 'apps/sys.lisp)
(run 'apps/ui.lisp)

(defq app_list '(
	"apps/terminal/app"
	"apps/netmon/app.lisp"
	"apps/canvas/app.lisp"
	"apps/raymarch/app.lisp"
	"apps/calculator/app.lisp"
	"tests/farm.lisp"
	"tests/pipe.lisp"
	"tests/global.lisp"
	"tests/migrate.lisp"))

(ui-tree window (create-window 0) nil
	(ui-element flow (create-flow) ('flow_flags (bit-or flow_flag_down flow_flag_fillw) 'color 0xffffff00)
		(each (lambda (_)
			(ui-element button (create-button) ('text _))
			(slot connect_click button 0)) app_list)))

(slot set_title window "Launcher")
(bind '(w h) (slot pref_size window))
(slot change window 32 32 (add w 32) h)
(slot gui_add window)

(while t
	(cond
		((ge (read-long ev_msg_target_id (defq msg (mail-mymail))) 0)
			(open-child (get (slot find_id window (read-long ev_msg_action_source_id msg)) 'text)
				kn_call_open))
		(t (slot event window msg))))
