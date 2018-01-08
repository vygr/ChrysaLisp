;import ui settings
(bind '(
	flow_flag_left flow_flag_right flow_flag_up flow_flag_down
	flow_flag_fillw flow_flag_fillh flow_flag_lastw flow_flag_lasth
	flow_flag_align_hcenter flow_flag_align_hleft flow_flag_align_hright
	flow_flag_align_vcenter flow_flag_align_vtop flow_flag_align_vbottom
	ev_msg_target_id ev_msg_action_source_id
	kn_call_open)
	(within-compile-env (lambda ()
		(import 'class/flow/flow.inc)
		(import 'gui/gui.inc)
		(import 'sys/kernel/kernel.inc)
		(list flow_flag_left flow_flag_right flow_flag_up flow_flag_down
			flow_flag_fillw flow_flag_fillh flow_flag_lastw flow_flag_lasth
			flow_flag_align_hcenter flow_flag_align_hleft flow_flag_align_hright
			flow_flag_align_vcenter flow_flag_align_vtop flow_flag_align_vbottom
			ev_msg_target_id ev_msg_action_source_id
			kn_call_open))))

(defun read-byte (o s)
	(code (elem o s)))
(defun read-short (o s)
	(add (read-byte o s) (bit-shl (read-byte (inc o) s) 8)))
(defun read-int (o s)
	(add (read-short o s) (bit-shl (read-short (add o 2) s) 16)))
(defun read-long (o s)
	(add (read-int o s) (bit-shl (read-int (add o 4) s) 32)))

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

(ui-set-title window "Launcher")
(ui-add-child window flow)
(eval `(,defq flow_flags ,(bit-or flow_flag_down flow_flag_fillw) color 0xffffff00) flow)
(each (lambda (_)
	(defq button (ui-button))
	(eval `(,defq text ,_) button)
	(ui-connect-click button 0)
	(ui-add-child flow button)) app_list)
(bind '(w h) (ui-pref-size window))
(ui-change window 32 32 (add w 32) h)
(ui-add-to-screen window)

(while t
	(defq msg (mail-mymail) id (read-long ev_msg_target_id msg))
	(cond
		((ge id 0)
			(if (defq button (ui-find-id window (read-long ev_msg_action_source_id msg)))
				(if (defq app (eval 'text button))
					(open-child app kn_call_open))))
		(t (ui-event window msg))))
