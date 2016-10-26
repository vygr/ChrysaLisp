%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_component.inc'

	def_func class/component/disconnect_sig
		;inputs
		;r0 = component object
		;r1 = signal list
		;trashes
		;all but r0, r4

		;save inputs
		set_src r0, r1
		set_dst r7, r6
		map_src_to_dst

		;gui sigslot heap
		f_bind gui_gui, statics, r0
		vp_add gui_statics_sigslot_heap, r0

		;disconnect signal
		loop_list_forward r1, r2, r3
			;remove from slot list
			vp_sub gui_sigslot_sig_node, r2
			vp_lea [r2 + gui_sigslot_slot_node], r1
			ln_remove_node r1, r5

			;free sigslot record
			hp_freecell r0, r2, r1
		loop_end

		;clear signal list
		lh_init r6, r1

		;restore inst
		vp_cpy r7, r0
		vp_ret

	def_func_end
