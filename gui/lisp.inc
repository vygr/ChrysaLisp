;import ui settings
(bind '(load_flag_shared flow_flag_left flow_flag_right flow_flag_up flow_flag_down
	flow_flag_fillw flow_flag_fillh flow_flag_lastw flow_flag_lasth
	flow_flag_align_hcenter flow_flag_align_hleft flow_flag_align_hright
	flow_flag_align_vcenter flow_flag_align_vtop flow_flag_align_vbottom
	window_flag_close window_flag_min window_flag_max window_flag_status
	ev_msg_target_id ev_msg_action_source_id argb_black argb_white argb_red
	argb_green argb_blue argb_yellow argb_cyan argb_magenta)
	(within-compile-env (lambda ()
		(import 'gui/gui/class.inc)
		(import 'gui/ctx/class.inc)
		(import 'gui/flow/class.inc)
		(import 'gui/window/class.inc)
		(import 'gui/canvas/class.inc)
		(list load_flag_shared flow_flag_left flow_flag_right flow_flag_up flow_flag_down
			flow_flag_fillw flow_flag_fillh flow_flag_lastw flow_flag_lasth
			flow_flag_align_hcenter flow_flag_align_hleft flow_flag_align_hright
			flow_flag_align_vcenter flow_flag_align_vtop flow_flag_align_vbottom
			window_flag_close window_flag_min window_flag_max  window_flag_status
			ev_msg_target_id ev_msg_action_source_id argb_black argb_white argb_red
			argb_green argb_blue argb_yellow argb_cyan argb_magenta))))

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
(ffi button-connect-click "gui/button/lisp_connect_click" 0)
(ffi canvas-blend-fbox "gui/canvas/lisp_blend_fbox" 0)
(ffi canvas-blend-fpoly "gui/canvas/lisp_blend_fpoly" 0)
(ffi canvas-blend-plot "gui/canvas/lisp_blend_plot" 0)
(ffi canvas-fill "gui/canvas/lisp_fill" 0)
(ffi canvas-load "gui/canvas/lisp_load" 0)
(ffi canvas-set-fbox "gui/canvas/lisp_set_fbox" 0)
(ffi canvas-set-fpoly "gui/canvas/lisp_set_fpoly" 0)
(ffi canvas-set-plot "gui/canvas/lisp_set_plot" 0)
(ffi canvas-swap "gui/canvas/lisp_swap" 0)
(ffi create-button "gui/button/lisp_create" 0)
(ffi create-canvas "gui/canvas/lisp_create" 0)
(ffi create-flow "gui/flow/lisp_create" 0)
(ffi create-font "gui/font/lisp_create" 0)
(ffi create-grid "gui/grid/lisp_create" 0)
(ffi create-label "gui/label/lisp_create" 0)
(ffi create-progress "gui/progress/lisp_create" 0)
(ffi create-slider "gui/slider/lisp_create" 0)
(ffi create-vdu "gui/vdu/lisp_create" 0)
(ffi create-view "gui/view/lisp_create" 0)
(ffi create-window "gui/window/lisp_create" 0)
(ffi gui-add "gui/gui/lisp_add" 0)
(ffi points-gen-arc "gui/points/lisp_gen_arc" 0)
(ffi points-gen-cubic "gui/points/lisp_gen_cubic" 0)
(ffi points-gen-quadratic "gui/points/lisp_gen_quadratic" 0)
(ffi points-simplify "gui/points/lisp_simplify" 0)
(ffi points-stroke-polygons "gui/points/lisp_stroke_polygons" 0)
(ffi points-stroke-polylines "gui/points/lisp_stroke_polylines" 0)
(ffi points-transform "gui/points/lisp_transform" 0)
(ffi slider-connect-value "gui/slider/lisp_connect_value" 0)
(ffi vdu-print "gui/vdu/lisp_print" 0)
(ffi view-add-back "gui/view/lisp_add_back" 0)
(ffi view-add-child "gui/view/lisp_add_child" 0)
(ffi view-add-dirty "gui/view/lisp_add_dirty" 0)
(ffi view-add-front "gui/view/lisp_add_front" 0)
(ffi view-add-opaque "gui/view/lisp_add_opaque" 0)
(ffi view-change "gui/view/lisp_change" 0)
(ffi view-dirty "gui/view/lisp_dirty" 0)
(ffi view-dirty-all "gui/view/lisp_dirty_all" 0)
(ffi view-event "gui/view/lisp_event" 0)
(ffi view-find-id "gui/view/lisp_find_id" 0)
(ffi view-get-bounds "gui/view/lisp_get_bounds" 0)
(ffi view-layout "gui/view/lisp_layout" 0)
(ffi view-opaque "gui/view/lisp_opaque" 0)
(ffi view-pref-size "gui/view/lisp_pref_size" 0)
(ffi view-set-bounds "gui/view/lisp_set_bounds" 0)
(ffi view-sub "gui/view/lisp_sub" 0)
(ffi view-sub-opaque "gui/view/lisp_sub_opaque" 0)
(ffi window-connect-close "gui/window/lisp_connect_close" 0)
(ffi window-connect-layout "gui/window/lisp_connect_layout" 0)
(ffi window-connect-max "gui/window/lisp_connect_max" 0)
(ffi window-connect-min "gui/window/lisp_connect_min" 0)
(ffi window-set-status "gui/window/lisp_set_status" 0)
(ffi window-set-title "gui/window/lisp_set_title" 0)
