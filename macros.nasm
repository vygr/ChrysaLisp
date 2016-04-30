%include 'inc/func.inc'
%include 'inc/gui.inc'

fn_function macros

	def_local
		def_local_long	a
		def_local_long	b
		def_local_long	c
		def_local_long	d
		def_local_long	e
		def_local_long	f
	def_local_end

	assign {.a * .b + .c * .d}, {r0}

	assign {.a * (.b + .c) * .d}, {r1}

	assign {.a * .b + .c * .d, .a * (.b + .c) * .d}, {r0, r1}

	assign {-.a * -.b - .c * -.d, -.a * -(-.b / -.c) * -.d}, {r8, r9}

	assign {-11 * -.a * -11}, {r0}

	assign {.a * .b + .c * .d, .a * (.b + .c) * .d, -.a * -.b - .c * -.d, \
	 		-.a * -(-.b / -.c) * -.d}, {r8, r9, r10, r11}

	assign {@this/path, "a string", :.f * :.b}, {r0, r1, r2}

fn_function_end
