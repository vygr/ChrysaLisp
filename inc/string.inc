%ifndef STRING_1234
%define STRING_1234

%include 'inc/class.inc'

;;;;;;;;;;;;;;;;;;;;;
; string static class
;;;;;;;;;;;;;;;;;;;;;

	def_class string
	def_method length, sys/string_length
	def_method copy, sys/string_copy
	def_method compare, sys/string_compare
	def_method int, sys/string_int

%endif
