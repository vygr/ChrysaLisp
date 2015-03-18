%include "func.inc"
%include "list.inc"
%include "heap.inc"
%include "gui.inc"

	fn_function "sys/gui_free_view"
		;inputs
		;r0 = view object
		;trashes
		;r0-r2

		;save view object
		vp_push r0

		;sub view from any parent
		fn_call gui/gui_sub_view

		;free any child views
		vp_cpy [r0 + GUI_VIEW_LIST + LH_LIST_HEAD], r1
		loopstart
			vp_cpy r1, r0
			ln_get_succ r1, r1
			breakif r1, ==, 0
			vp_push r1
			fn_call sys/gui_free_view
			vp_pop r1
		loopend

		;free view object
		vp_pop r0
		fn_bind sys/gui_statics, r1
		vp_lea [r1 + GUI_STATICS_VIEW_HEAP], r1
		hp_freecell r1, r0, r2

		vp_ret

	fn_function_end
