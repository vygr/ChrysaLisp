%ifndef CLASS_PROGRESS_1234
%define CLASS_PROGRESS_1234

%include 'class/class_view.inc'

	progress_border_size	equ 1

	def_class progress, view
	def_method create, class/progress/create
	def_method new, class/progress/new
	def_method set_max, class/progress/set_max, static, 'r0, r1'
	def_method set_val, class/progress/set_val, static, 'r0, r1'

	def_method pref_size, class/progress/pref_size, override
	def_method draw, class/progress/draw, override

	def_structure	progress, view
		def_long	progress_max
		def_long	progress_val
	def_structure_end

%endif
