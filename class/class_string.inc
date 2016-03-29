%ifndef CLASS_STRING_1234
%define CLASS_STRING_1234

%include 'class/class_view.inc'

	def_class string, view
	def_method create, class/string/create
	def_method new, class/string/new
	def_method init, class/string/init
	def_method set_font, class/string/set_font
	def_method set_text, class/string/set_text
	def_method get_text, class/string/get_text
	def_method set_text_color, class/string/set_text_color

	def_method pref_size, class/string/pref_size, override
	def_method draw, class/string/draw, override

	def_structure	string, view
		def_long	string_font
		def_long	string_text
		def_long	string_text_color
	def_structure_end

%endif
