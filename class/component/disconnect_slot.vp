%include 'inc/func.ninc'
%include 'inc/gui.ninc'
%include 'class/class_component.ninc'

def_func class/component/disconnect_slot
	;inputs
	;r0 = component object
	;r1 = 0 for all, else target address
	;trashes
	;all but r0, r4

	;save inputs
	set_src r0, r1
	set_dst r7, r6
	map_src_to_dst

	;gui sigslot heap
	f_bind gui_gui, statics, r0
	vp_add gui_statics_sigslot_heap, r0

	;disconnect slots
	loop_list_forward r7 + component_slot_list, r2, r3
		vp_jmpif r6, ==, 0, freeit
		continueif r6, !=, [r2 + gui_sigslot_addr]
	freeit:
		assert r7, ==, [r2 + gui_sigslot_inst]

		;remove from slot list
		vp_cpy r2, r1
		ln_remove_node r1, r5

		;remove from signal list
		vp_sub gui_sigslot_slot_node, r2
		vp_lea [r2 + gui_sigslot_sig_node], r1
		ln_remove_node r1, r5

		;free sigslot record
		hp_freecell r0, r2, r1
	loop_end

	;restore inst
	vp_cpy r7, r0
	vp_ret

def_func_end
