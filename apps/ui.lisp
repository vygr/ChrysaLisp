;import ui settings
(bind '(
	flow_flag_left flow_flag_right flow_flag_up flow_flag_down
	flow_flag_fillw flow_flag_fillh flow_flag_lastw flow_flag_lasth
	flow_flag_align_hcenter flow_flag_align_hleft flow_flag_align_hright
	flow_flag_align_vcenter flow_flag_align_vtop flow_flag_align_vbottom
	ev_msg_target_id ev_msg_action_source_id
	kn_call_open
	slot_add_child slot_change
	slot_pref_size slot_connect_click
	slot_set_title slot_set_status)
	(within-compile-env (lambda ()
		(import 'gui/gui.inc)
		(import 'sys/kernel/kernel.inc)
		(import 'class/flow/flow.inc)
		(import 'class/button/button.inc)
		(import 'class/window/window.inc)
		(list flow_flag_left flow_flag_right flow_flag_up flow_flag_down
			flow_flag_fillw flow_flag_fillh flow_flag_lastw flow_flag_lasth
			flow_flag_align_hcenter flow_flag_align_hleft flow_flag_align_hright
			flow_flag_align_vcenter flow_flag_align_vtop flow_flag_align_vbottom
			ev_msg_target_id ev_msg_action_source_id
			kn_call_open
			(method-slot 'view 'add_child) (method-slot 'view 'change)
			(method-slot 'view 'pref_size) (method-slot 'button 'connect_click)
			(method-slot 'window 'set_title) (method-slot 'window 'set_status)))))
