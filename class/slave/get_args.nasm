%include 'inc/func.inc'
%include 'class/class_slave.inc'

	def_function class/slave/get_args
		;inputs
		;r0 = slave object
		;outputs
		;r0 = slave object
		;r1 = command args

		vp_cpy [r0 + slave_args], r1
		vp_ret

	def_function_end
