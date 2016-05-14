%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

	fn_function class/view/event
		;inputs
		;r0 = view object
		;r1 = event message
		;trashes
		;all but r0, r4

		;what type of event ?
		vp_cpy [r1 + ev_data_type], r2
		switch
		case r2, ==, ev_type_mouse
			;so what state are we in ?
			vp_cpy [r1 + ev_data_buttons], r2
			vp_cpy [r0 + view_last_buttons], r3
			if r3, !=, 0
				;was down previously
				if r2, !=, 0
					;is down now, so move
					m_jmp view, mouse_move, {r0, r1}
				else
					;is not down now, so release
					vp_cpy r2, [r0 + view_last_buttons]
					m_jmp view, mouse_up, {r0, r1}
				endif
			else
				;was not down previously
				if r2, !=, 0
					;is down now, so first down
					vp_cpy r2, [r0 + view_last_buttons]
					m_jmp view, mouse_down, {r0, r1}
				else
					;is not down now, so hover
					m_jmp view, mouse_hover, {r0, r1}
				endif
			endif
			break
		case r2, ==, ev_type_key
			vp_cpy [r1 + ev_data_keycode], r2
			if r2, >=, 0
				m_jmp view, key_down, {r0, r1}
			else
				m_jmp view, key_up, {r0, r1}
			endif
		endswitch
		vp_ret

	fn_function_end
