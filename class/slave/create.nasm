%include 'inc/func.inc'
%include 'class/class_slave.inc'

	fn_function class/slave/create
		;inputs
		;r0 = input mailbox
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3, r5

		;create new slave object
		set_src r0
		set_dst r5
		map_src_to_dst

		s_call slave, new, {}, {r0}
		if r0, !=, 0
			;init the object
			slot_function class, slave
			s_call slave, init, {r0, @_function_, r5}, {r1}
			if r1, ==, 0
				;error with init
				m_call slave, delete, {r0}, {}, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
