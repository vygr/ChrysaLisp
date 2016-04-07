%ifndef CLASS_STRING_1234
%define CLASS_STRING_1234

%include 'class/class_ref.inc'

	def_class string, ref
	def_method create, class/string/create
	def_method new, class/string/new
	def_method init, class/string/init

	def_structure	string, ref
		def_long	string_length
		def_struct	string_data, null
	def_structure_end

%endif
