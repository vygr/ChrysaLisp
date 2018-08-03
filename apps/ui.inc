;import ui settings
(bind '(flow_flag_left flow_flag_right flow_flag_up flow_flag_down flow_flag_fillw
	flow_flag_fillh flow_flag_lastw flow_flag_lasth flow_flag_align_hcenter
	flow_flag_align_hleft flow_flag_align_hright flow_flag_align_vcenter
	flow_flag_align_vtop flow_flag_align_vbottom window_flag_close window_flag_min
	window_flag_max ev_msg_target_id ev_msg_action_source_id slot_find_id
	slot_gui_add slot_event slot_add_child slot_change slot_pref_size
	slot_get_bounds slot_connect_click slot_set_title slot_set_status
	slot_connect_close slot_connect_min slot_connect_max slot_set_fbox
	slot_set_fpoly slot_blend_fpoly slot_set_plot slot_blend_plot slot_fill
	slot_swap slot_transform slot_simplify slot_gen_quadratic slot_gen_cubic
	slot_gen_arc slot_stroke_polylines slot_stroke_polygons slot_layout slot_dirty
	slot_dirty_all)
	(within-compile-env (lambda ()
		(import 'gui/gui.inc)
		(import 'class/flow/flow.inc)
		(import 'class/button/button.inc)
		(import 'class/window/window.inc)
		(import 'class/canvas/canvas.inc)
		(import 'class/points/points.inc)
		(list flow_flag_left flow_flag_right flow_flag_up flow_flag_down flow_flag_fillw
			flow_flag_fillh flow_flag_lastw flow_flag_lasth flow_flag_align_hcenter
			flow_flag_align_hleft flow_flag_align_hright flow_flag_align_vcenter
			flow_flag_align_vtop flow_flag_align_vbottom window_flag_close
			window_flag_min window_flag_max ev_msg_target_id ev_msg_action_source_id
			(method-slot 'view 'find_id) (method-slot 'gui_gui 'add)
			(method-slot 'view 'event) (method-slot 'view 'add_child)
			(method-slot 'view 'change) (method-slot 'view 'pref_size)
			(method-slot 'view 'get_bounds) (method-slot 'button 'connect_click)
			(method-slot 'window 'set_title) (method-slot 'window 'set_status)
			(method-slot 'window 'connect_close) (method-slot 'window 'connect_min)
			(method-slot 'window 'connect_max) (method-slot 'canvas 'set_fbox)
			(method-slot 'canvas 'set_fpoly) (method-slot 'canvas 'blend_fpoly)
			(method-slot 'canvas 'set_plot) (method-slot 'canvas 'blend_plot)
			(method-slot 'canvas 'fill) (method-slot 'canvas 'swap)
			(method-slot 'points 'transform) (method-slot 'points 'simplify)
			(method-slot 'points 'gen_quadratic) (method-slot 'points 'gen_cubic)
			(method-slot 'points 'gen_arc) (method-slot 'points 'stroke_polylines)
			(method-slot 'points 'stroke_polygons) (method-slot 'view 'layout)
			(method-slot 'view 'dirty) (method-slot 'view 'dirty_all)))))

;some helpful macros
(defmacro get (_ b) `(eval ,b ,_))
(defmacro slot (_ &rest b) `(call ,(sym-cat 'slot_ _) ~b))

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
			(slot add_child (elem -3 (push _ui (defq ,n ,c))) ,n)
			(def ,n ~p)
			~x (pop _ui))
		`(progn
			(slot add_child (elem -3 (push _ui (defq ,n ,c))) ,n)
			~x (pop _ui))))

;system ui bindings
(ffi create-label "class/label/lisp_create" 0)
(ffi create-button "class/button/lisp_create" 0)
(ffi create-flow "class/flow/lisp_create" 0)
(ffi create-grid "class/grid/lisp_create" 0)
(ffi create-canvas "class/canvas/lisp_create" 0)
(ffi canvas-load "class/canvas/lisp_load" 0)
(ffi create-progress "class/progress/lisp_create" 0)
(ffi create-font "class/font/lisp_create" 0)
(ffi create-window "class/window/lisp_create" 0)
