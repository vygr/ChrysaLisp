%include 'inc/func.inc'
%include 'class/class_label.inc'
%include 'class/class_window.inc'

	fn_function class/window/set_status
		;inputs
		;r0 = window object
		;r1 = 0, else status string object

		def_local
			def_local_long	inst
		def_local_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, .inst

		static_call label, set_text, {[r0 + window_status], r1}

		vp_cpy .inst, r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
