%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

	fn_function class/view/deinit
		;inputs
		;r0 = view object
		;trashes
		;all but r0, r4

		;save object
		vp_push r0

		;sub view from any parent
		static_call view, sub

		;deref any child views
		vp_cpy [r4], r0
		loop_list_forward r0 + view_list, r0, r1
			vp_sub view_node, r0
			vp_push r1
			static_call view, sub
			static_call view, deref
			vp_pop r1
		loop_end

		;free view object data
		vp_cpy [r4], r0
		vp_lea [r0 + view_dirty_region], r1
		static_bind gui_gui, statics, r0
		vp_lea [r0 + gui_statics_rect_heap], r0
		static_call gui_region, free
		vp_cpy [r4], r1
		vp_lea [r1 + view_opaque_region], r1
		static_call gui_region, free

		;deinit parent
		vp_pop r0
		super_call view, deinit
		vp_ret

	fn_function_end
