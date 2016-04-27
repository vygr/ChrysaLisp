%include 'inc/func.inc'
%include 'class/class_component.inc'

	fn_function class/component/init
		;inputs
		;r0 = component object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;init parent
		super_call component, init, {r0, r1}, {r1}
		if r1, !=, 0
			;init myself
			vp_lea [r0 + component_slot_list], r1
			lh_init r1, r2
		endif
		vp_ret

	fn_function_end
