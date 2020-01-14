;import gui settings
(bind (defq _syms_ '( load_flag_shared load_flag_film load_flag_noswap
	flow_flag_left flow_flag_right flow_flag_up flow_flag_down flow_flag_fillw
	flow_flag_fillh flow_flag_lastw flow_flag_lasth flow_flag_align_hcenter
	flow_flag_align_hleft flow_flag_align_hright flow_flag_align_vcenter
	flow_flag_align_vtop flow_flag_align_vbottom window_flag_close window_flag_min
	window_flag_max window_flag_status scroll_flag_vertical scroll_flag_horizontal
	view_flag_solid view_flag_opaque view_flag_dirty_all ev_msg_target_id
	ev_msg_action_source_id ev_msg_type ev_type_mouse ev_msg_mouse_buttons
	ev_type_key ev_type_gui ev_msg_key_keycode ev_msg_key_key argb_black argb_white
	argb_red argb_green argb_blue argb_yellow argb_cyan argb_magenta argb_grey1
	argb_grey2 argb_grey3 argb_grey4 argb_grey5 argb_grey6 argb_grey7 argb_grey8
	argb_grey9 argb_grey10 argb_grey11 argb_grey12 argb_grey13 argb_grey14
	argb_grey15 window_col title_col toolbar_col toolbar2_col slider_col status_col
	join_miter join_bevel join_round cap_butt cap_square cap_tri cap_arrow
	cap_round window_close window_min window_max window_title window_status
	canvas_color canvas_flags view_flags view_flag_at_back view_flag_dirty_all
	view_x view_y view_w view_h vdu_clear_screen vdu_clear_line))
	(within-compile-env (lambda ()
		(include 'gui/gui/class.inc)
		(include 'gui/ctx/class.inc)
		(include 'gui/flow/class.inc)
		(include 'gui/vdu/class.inc)
		(include 'gui/window/class.inc)
		(include 'gui/scroll/class.inc)
		(include 'gui/canvas/class.inc)
		(include 'gui/points/class.inc)
		(map eval _syms_))))

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

(defmacro view-dirty-all (_)
	`(view-set-flags ,_ view_flag_dirty_all view_flag_dirty_all))

;lisp bindings
(ffi component-connect "gui/component/lisp_connect" 0)
(ffi create-backdrop "gui/backdrop/lisp_create" 0)
(ffi create-button "gui/button/lisp_create" 0)
(ffi create-flow "gui/flow/lisp_create" 0)
(ffi create-font "gui/font/lisp_create" 0)
(ffi create-grid "gui/grid/lisp_create" 0)
(ffi create-label "gui/label/lisp_create" 0)
(ffi create-progress "gui/progress/lisp_create" 0)
(ffi create-scroll "gui/scroll/lisp_create" 0)
(ffi create-slider "gui/slider/lisp_create" 0)
(ffi create-vdu "gui/vdu/lisp_create" 0)
(ffi create-view "gui/view/lisp_create" 0)
(ffi gui-add "gui/gui/lisp_add" 0)
(ffi gui-add-back "gui/gui/lisp_add_back" 0)
(ffi vdu-print "gui/vdu/lisp_print" 0)
(ffi vdu-load "gui/vdu/lisp_load" 0)
(import 'gui/view/lisp.inc)
(import 'gui/points/lisp.inc)
(import 'gui/canvas/lisp.inc)
(import 'gui/window/lisp.inc)
