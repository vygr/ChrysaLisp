%include 'inc/func.inc'
%include 'inc/link.inc'

	fn_function sys/link_init
		;get statics
		fn_bind sys/link_statics, r0

		vp_lea [r0 + lk_statics_links_list], r0
		lh_init r0, r1
		vp_ret

	fn_function_end
