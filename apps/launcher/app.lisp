;import ui settings
(run 'apps/ui.lisp)

(defq app_list '(
	"apps/netmon/app"
	"apps/terminal/app"
	"apps/canvas/app"
	"apps/raymarch/app"
	"apps/calculator/app.lisp"
	"tests/farm"
	"tests/pipe"
	"tests/global"
	"tests/migrate")
	window (call slot_create_window nil 0)
	flow (call slot_create_flow nil))

(call slot_set_title window "Launcher")
(def-props flow 'flow_flags (bit-or flow_flag_down flow_flag_fillw) 'color 0xffffff00)
(each (lambda (_)
	(defq button (call slot_create_button nil))
	(def-props button 'text _)
	(call slot_connect_click button 0)
	(call slot_add_child flow button)) app_list)
(call slot_add_child window flow)
(bind '(w h) (call slot_pref_size window))
(call slot_change window 32 32 (add w 32) h)
(call slot_gui_add window)

(while t
	(cond
		((ge (read-long ev_msg_target_id (defq msg (mail-mymail))) 0)
			(open-child (get-prop (call slot_find_id window (read-long ev_msg_action_source_id msg)) 'text)
				kn_call_open))
		(t (call slot_event window msg))))
