%include 'inc/func.inc'
%include 'inc/link.inc'

	def_func sys/link_init
		;get statics
		f_bind sys_link, statics, r0

		vp_lea [r0 + lk_statics_links_list], r0
		lh_init r0, r1
		vp_ret

	def_func_end
