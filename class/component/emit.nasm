%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_component.inc'

	fn_function class/component/emit
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

		def_local
			def_local_long	inst
			def_local_long	next
		def_local_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, .inst

		;emit the signal
		loop_list_forward r1, r0, r1
			vp_cpy r1, .next

			;call method on target component object
			vp_cpy [r0 + gui_sigslot_addr], r15
			vp_cpy [r0 + gui_sigslot_inst], r0
			vp_cpy .inst, r1
			vp_call r15

			vp_cpy .next, r1
		loop_end

		;restore inst
		vp_cpy .inst, r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
