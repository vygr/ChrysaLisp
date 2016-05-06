%ifndef CLASS_BUTTON_1234
%define CLASS_BUTTON_1234

%include 'class/class_label.inc'

	button_border_size		equ 2
	button_state_pressed	equ 1 << 0

	def_class button, label
	def_method create, class/button/create
	def_method new, class/button/new
	def_method init, class/button/init
	def_method sig_pressed, class/button/sig_pressed, static, {r0}, {r1}

	def_method pref_size, class/button/pref_size, override
	def_method draw, class/button/draw, override
	def_method layout, class/button/layout, override
	def_method mouse_down, class/button/mouse_down, override
	def_method mouse_up, class/button/mouse_up, override
	def_method mouse_move, class/button/mouse_move, override

	def_structure	button, label
		long	button_state
		struct	button_pressed_signal, lh_list
	def_structure_end

%endif
