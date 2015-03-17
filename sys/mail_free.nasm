%include "func.inc"
%include "mail.inc"

	fn_function "sys/mail_free"
		;inputs
		;r1 = mail message
		;trashes
		;r0-r2

		fn_bind sys/mail_statics, r0
		vp_lea [r0 + ML_STATICS_HEAP], r0
		hp_freecell r0, r1, r2
		vp_ret

	fn_function_end
