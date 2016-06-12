%include 'inc/func.inc'
%include 'class/class_master.inc'

	fn_function class/master/output_ready
		;inputs
		;r0 = master object
		;outputs
		;r0 = master object
		;r1 = 0 if data not available

		vp_cpy [r0 + master_output_list + lh_list_head], r1
		vp_cpy [r1 + ln_node_succ], r1
		vp_ret

	fn_function_end
