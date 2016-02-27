%include "func.inc"
%include "link.inc"

	fn_function "sys/link_init"
		;get statics
		fn_bind sys/link_statics, r0

		vp_lea [r0 + LK_STATICS_LINKS_LIST], r0
		lh_init r0, r1
		vp_ret

	fn_function_end
