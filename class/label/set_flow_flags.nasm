%include 'inc/func.inc'
%include 'class/class_label.inc'
%include 'class/class_flow.inc'

	fn_function class/label/set_flow_flags
		;inputs
		;r0 = label object
		;r1 = flags

		vp_push r0

		vp_cpy [r0 + label_flow], r0
		static_call flow, set_flow_flags

		vp_pop r0
		vp_ret

	fn_function_end
