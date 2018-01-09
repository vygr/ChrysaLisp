;import ui settings
(run 'apps/ui.lisp)

(defq app_list '(
	"apps/netmon/app"
	"apps/terminal/app"
	"apps/canvas/app"
	"apps/raymarch/app"
	"apps/calculator/app"
	"tests/farm"
	"tests/pipe"
	"tests/global"
	"tests/migrate")
	window (ui-window 0)
	flow (ui-flow))

(call slot_set_title window "Launcher")
(eval (list defq 'flow_flags (bit-or flow_flag_down flow_flag_fillw) 'color 0xffffff00) flow)
(each (lambda (_)
	(defq button (ui-button))
	(eval (list defq 'text _) button)
	(call slot_connect_click button 0)
	(call slot_add_child flow button)) app_list)
(call slot_add_child window flow)
(bind '(w h) (call slot_pref_size window))
(call slot_change window 32 32 (add w 32) h)
(call slot_gui_add window)

(while t
	(cond
		((ge (read-long ev_msg_target_id (defq msg (mail-mymail))) 0)
			(open-child (eval 'text (call slot_find_id window (read-long ev_msg_action_source_id msg))) kn_call_open))
		(t (call slot_event window msg))))
