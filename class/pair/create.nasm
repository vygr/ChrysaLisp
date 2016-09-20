%include 'inc/func.inc'
%include 'class/class_pair.inc'

def_function class/pair/create
	;inputs
	;r0 = first object
	;r1 = second object
	;outputs
	;r0 = 0 if error, else object
	;trashes
	;r1-r3, r5-r7

	;save data
	vp_cpy r0, r6
	vp_cpy r1, r7

	;create new string object
	s_call pair, new, {}, {r0}
	if r0, !=, 0
		;init the object
		slot_function class, pair
		s_call pair, init, {r0, @_function_, r6, r7}, {r1}
		if r1, ==, 0
			;error with init
			m_call pair, delete, {r0}, {}, r1
			vp_xor r0, r0
		endif
	endif
	vp_ret

def_function_end
