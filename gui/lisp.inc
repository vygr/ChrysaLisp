;import ui settings
(bind '(load_flag_shared flow_flag_left flow_flag_right flow_flag_up flow_flag_down
	flow_flag_fillw flow_flag_fillh flow_flag_lastw flow_flag_lasth
	flow_flag_align_hcenter flow_flag_align_hleft flow_flag_align_hright
	flow_flag_align_vcenter flow_flag_align_vtop flow_flag_align_vbottom
	window_flag_close window_flag_min window_flag_max ev_msg_target_id
	ev_msg_action_source_id)
	(within-compile-env (lambda ()
		(import 'gui/class.inc)
		(import 'class/flow/class.inc)
		(import 'class/window/class.inc)
		(import 'class/canvas/class.inc)
		(list load_flag_shared flow_flag_left flow_flag_right flow_flag_up flow_flag_down
			flow_flag_fillw flow_flag_fillh flow_flag_lastw flow_flag_lasth
			flow_flag_align_hcenter flow_flag_align_hleft flow_flag_align_hright
			flow_flag_align_vcenter flow_flag_align_vtop flow_flag_align_vbottom
			window_flag_close window_flag_min window_flag_max ev_msg_target_id
			ev_msg_action_source_id))))

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
(ffi button-connect-click "class/button/lisp_connect_click" 0)
(ffi slider-connect-value "class/slider/lisp_connect_value" 0)
(ffi canvas-blend-fbox "class/canvas/lisp_blend_fbox" 0)
(ffi canvas-blend-fpoly "class/canvas/lisp_blend_fpoly" 0)
(ffi canvas-blend-plot "class/canvas/lisp_blend_plot" 0)
(ffi canvas-fill "class/canvas/lisp_fill" 0)
(ffi canvas-load "class/canvas/lisp_load" 0)
(ffi canvas-set-fbox "class/canvas/lisp_set_fbox" 0)
(ffi canvas-set-fpoly "class/canvas/lisp_set_fpoly" 0)
(ffi canvas-set-plot "class/canvas/lisp_set_plot" 0)
(ffi canvas-swap "class/canvas/lisp_swap" 0)
(ffi create-view "class/view/lisp_create" 0)
(ffi create-button "class/button/lisp_create" 0)
(ffi create-slider "class/slider/lisp_create" 0)
(ffi create-canvas "class/canvas/lisp_create" 0)
(ffi create-flow "class/flow/lisp_create" 0)
(ffi create-font "class/font/lisp_create" 0)
(ffi create-grid "class/grid/lisp_create" 0)
(ffi create-label "class/label/lisp_create" 0)
(ffi create-progress "class/progress/lisp_create" 0)
(ffi create-window "class/window/lisp_create" 0)
(ffi gui-add "gui/lisp_add" 0)
(ffi points-gen-arc "class/points/lisp_gen_arc" 0)
(ffi points-gen-cubic "class/points/lisp_gen_cubic" 0)
(ffi points-gen-quadratic "class/points/lisp_gen_quadratic" 0)
(ffi points-simplify "class/points/lisp_simplify" 0)
(ffi points-stroke-polygons "class/points/lisp_stroke_polygons" 0)
(ffi points-stroke-polylines "class/points/lisp_stroke_polylines" 0)
(ffi points-transform "class/points/lisp_transform" 0)
(ffi view-add-back "class/view/lisp_add_back" 0)
(ffi view-add-child "class/view/lisp_add_child" 0)
(ffi view-add-dirty "class/view/lisp_add_dirty" 0)
(ffi view-add-front "class/view/lisp_add_front" 0)
(ffi view-add-opaque "class/view/lisp_add_opaque" 0)
(ffi view-change "class/view/lisp_change" 0)
(ffi view-dirty "class/view/lisp_dirty" 0)
(ffi view-dirty-all "class/view/lisp_dirty_all" 0)
(ffi view-event "class/view/lisp_event" 0)
(ffi view-find-id "class/view/lisp_find_id" 0)
(ffi view-get-bounds "class/view/lisp_get_bounds" 0)
(ffi view-layout "class/view/lisp_layout" 0)
(ffi view-opaque "class/view/lisp_opaque" 0)
(ffi view-pref-size "class/view/lisp_pref_size" 0)
(ffi view-set-bounds "class/view/lisp_set_bounds" 0)
(ffi view-sub "class/view/lisp_sub" 0)
(ffi view-sub-opaque "class/view/lisp_sub_opaque" 0)
(ffi window-connect-layout "class/window/lisp_connect_layout" 0)
(ffi window-connect-close "class/window/lisp_connect_close" 0)
(ffi window-connect-max "class/window/lisp_connect_max" 0)
(ffi window-connect-min "class/window/lisp_connect_min" 0)
(ffi window-set-status "class/window/lisp_set_status" 0)
(ffi window-set-title "class/window/lisp_set_title" 0)
