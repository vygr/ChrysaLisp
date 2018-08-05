;import ui settings
(bind '(flow_flag_left flow_flag_right flow_flag_up flow_flag_down flow_flag_fillw
	flow_flag_fillh flow_flag_lastw flow_flag_lasth flow_flag_align_hcenter
	flow_flag_align_hleft flow_flag_align_hright flow_flag_align_vcenter
	flow_flag_align_vtop flow_flag_align_vbottom window_flag_close window_flag_min
	window_flag_max ev_msg_target_id ev_msg_action_source_id)
	(within-compile-env (lambda ()
		(import 'gui/gui.inc)
		(import 'class/flow/flow.inc)
		(import 'class/window/window.inc)
		(list flow_flag_left flow_flag_right flow_flag_up flow_flag_down flow_flag_fillw
			flow_flag_fillh flow_flag_lastw flow_flag_lasth flow_flag_align_hcenter
			flow_flag_align_hleft flow_flag_align_hright flow_flag_align_vcenter
			flow_flag_align_vtop flow_flag_align_vbottom window_flag_close
			window_flag_min window_flag_max ev_msg_target_id ev_msg_action_source_id))))

;some helpful macros
(defmacro get (_ b) `(eval ,b ,_))

(defmacro ui-tree (n c &optional p &rest x)
	(if p
		`(progn
			(defq _ui (list (defq ,n ,c)))
			(def ,n ~p)
			~x (setq _ui nil))
		`(progn
			(defq _ui (list (defq ,n ,c)))
			~x (setq _ui nil))))

(defmacro ui-element (n c &optional p &rest x)
	(if p
		`(progn
			(view-add-child (elem -3 (push _ui (defq ,n ,c))) ,n)
			(def ,n ~p)
			~x (pop _ui))
		`(progn
			(view-add-child (elem -3 (push _ui (defq ,n ,c))) ,n)
			~x (pop _ui))))

;lisp bindings
(ffi gui-add "gui/lisp_add" 0)
