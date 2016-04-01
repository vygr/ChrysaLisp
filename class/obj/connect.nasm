%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_obj.inc'

	fn_function class/obj/connect
		;inputs
		;r0 = object
		;r1 = signal list
		;r2 = target object
		;r3 = target address
		;trashes
		;all but r0, r4

		;save inputs
		vp_cpy r0, r5
		vp_cpy r1, r6
		vp_cpy r2, r7
		vp_cpy r3, r8

		;gui sigslot heap
		static_bind gui_gui, statics, r0
		vp_add gui_statics_sigslot_heap, r0

		;create sigslot record
		static_call sys_heap, alloc
		fn_assert r1, !=, 0

		;fill in target and method
		vp_cpy r7, [r1 + gui_sigslot_inst]
		vp_cpy r8, [r1 + gui_sigslot_addr]

		;add to sig and slot lists
		vp_lea [r1 + gui_sigslot_sig_node], r2
		lh_add_at_tail r6, r2, r3
		vp_lea [r1 + gui_sigslot_slot_node], r2
		vp_lea [r5 + obj_slot_list], r6
		lh_add_at_tail r6, r2, r3

		;restore inst
		vp_cpy r5, r0
		vp_ret

	fn_function_end
