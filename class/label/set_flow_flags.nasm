%include 'inc/func.inc'
%include 'class/class_label.inc'
%include 'class/class_flow.inc'

	fn_function class/label/set_flow_flags
		;inputs
		;r0 = label object
		;r1 = flags

		vp_push r0
		s_call flow, set_flow_flags, {[r0 + label_flow], r1}
		vp_pop r0
		vp_ret

	fn_function_end
