%include "func.inc"
%include "list.inc"
%include "heap.inc"
%include "gui.inc"

	fn_function "gui/view_free"
		;inputs
		;r0 = view object
		;trashes
		;r0-r2

		;save view object
		vp_push r0

		;sub view from any parent
		fn_call gui/view_sub

		;free any child views
		loop_list_forwards r0 + GUI_VIEW_LIST, r1, r0
			vp_push r1
			fn_call sys/gui_view_free
			vp_pop r1
		loop_end

		;free view object
		vp_pop r0
		fn_bind sys/gui_statics, r1
		vp_lea [r1 + GUI_STATICS_VIEW_HEAP], r1
		hp_freecell r1, r0, r2

		vp_ret

	fn_function_end
