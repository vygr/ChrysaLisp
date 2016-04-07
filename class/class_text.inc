%ifndef CLASS_TEXT_1234
%define CLASS_TEXT_1234

%include 'class/class_view.inc'

	def_class text, view
	def_method create, class/text/create
	def_method new, class/text/new
	def_method init, class/text/init
	def_method set_font, class/text/set_font
	def_method set_text, class/text/set_text
	def_method get_text, class/text/get_text
	def_method set_text_color, class/text/set_text_color

	def_method pref_size, class/text/pref_size, override
	def_method draw, class/text/draw, override

	def_structure	text, view
		def_long	text_font
		def_long	text_text
		def_long	text_text_color
	def_structure_end

%endif
