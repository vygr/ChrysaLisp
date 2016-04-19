%ifndef CLASS_LABEL_1234
%define CLASS_LABEL_1234

%include 'class/class_view.inc'

	label_border_size	equ 0

	def_class label, view
	def_method create, class/label/create
	def_method new, class/label/new
	def_method init, class/label/init
	def_method set_font, class/label/set_font, nonvirtual, 'r0, r1'
	def_method set_text, class/label/set_text, nonvirtual, 'r0, r1'
	def_method get_text, class/label/get_text, nonvirtual, 'r0', 'r1'
	def_method set_text_color, class/label/set_text_color, nonvirtual, 'r0, r1'
	def_method set_flow_flags, class/label/set_flow_flags, nonvirtual, 'r0, r1'

	def_method pref_size, class/label/pref_size, override
	def_method draw, class/label/draw, override
	def_method layout, class/label/layout, override

	def_structure	label, view
		def_long	label_flow
		def_long	label_text
	def_structure_end

%endif
