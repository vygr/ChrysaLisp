%ifndef STRING_1234
%define STRING_1234

%include 'inc/class.inc'

;;;;;;;;;;;;;;;;;;;;;
; string static class
;;;;;;;;;;;;;;;;;;;;;

	def_class sys_string
	def_method length, sys/string_length, nonvirtual, 'r0', 'r1'
	def_method copy, sys/string_copy
	def_method compare, sys/string_compare
	def_method to_long, sys/string_to_long
	def_method from_long, sys/string_from_long
	def_method hash, sys/string_hash

%endif
