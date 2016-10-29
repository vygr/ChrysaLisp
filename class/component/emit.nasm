%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_component.inc'

	def_func class/component/emit
		;inputs
		;r0 = component object
		;r1 = signal list
		;trashes
		;all but r0, r4
			;callback api
			;r0 = target inst
			;r1 = source inst
			;r15 = dispatch register
			;rest as passed by call to emit
			;callback should normally save register the signal sends
			;but it could filter or adjust them for fancy reasons !

		def_struct local
			ptr local_inst
			ptr local_next
		def_struct_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst [r4 + local_inst]
		map_src_to_dst

		;emit the signal
		loop_list_forward r1, r0, r1
			vp_cpy r1, [r4 + local_next]

			;call method on target component object
			vp_cpy [r0 + gui_sigslot_addr], r15
			vp_cpy [r0 + gui_sigslot_inst], r0
			vp_cpy [r4 + local_inst], r1
			vp_call r15

			vp_cpy [r4 + local_next], r1
		loop_end

		;restore inst
		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	def_func_end
