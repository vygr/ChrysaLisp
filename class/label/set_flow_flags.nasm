%include 'inc/func.inc'
%include 'class/class_label.inc'
%include 'class/class_flow.inc'

	def_func class/label/set_flow_flags
		;inputs
		;r0 = label object
		;r1 = flags

		vp_push r0
		f_call flow, set_flow_flags, {[r0 + label_flow], r1}
		vp_pop r0
		vp_ret

	def_func_end
