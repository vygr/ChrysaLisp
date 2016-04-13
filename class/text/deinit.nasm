%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_text.inc'
%include 'class/class_string.inc'

	fn_function class/text/deinit
		;inputs
		;r0 = text object
		;trashes
		;all but r0, r4

		;save object
		vp_push r0

		;deref any string object
		vp_cpy [r0 + text_string], r0
		static_call string, deref

		;deinit parent
		vp_pop r0
		super_jmp text, deinit

	fn_function_end
