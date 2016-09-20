%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_component.inc'

	def_function class/component/connect
		;inputs
		;r0 = component object
		;r1 = signal list
		;r2 = target component object
		;r3 = target address
		;trashes
		;all but r0, r4

		;save inputs
		set_src r0, r1, r2, r3
		set_dst r5, r6, r7, r8
		map_src_to_dst

		;gui sigslot heap
		s_bind gui_gui, statics, r0
		vp_add gui_statics_sigslot_heap, r0

		;create sigslot record
		s_call sys_heap, alloc, {r0}, {r1}
		assert r1, !=, 0

		;fill in target and method
		vp_cpy r7, [r1 + gui_sigslot_inst]
		vp_cpy r8, [r1 + gui_sigslot_addr]

		;add to sig and slot lists
		vp_lea [r1 + gui_sigslot_sig_node], r2
		lh_add_at_tail r6, r2, r3
		vp_lea [r1 + gui_sigslot_slot_node], r2
		vp_lea [r5 + component_slot_list], r6
		lh_add_at_tail r6, r2, r3

		;restore inst
		vp_cpy r5, r0
		vp_ret

	def_function_end
