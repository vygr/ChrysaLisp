%ifndef STRING_1234
%define STRING_1234

%include 'inc/class.ninc'

;;;;;;;;;;;;;;;;;;;;;
; string static class
;;;;;;;;;;;;;;;;;;;;;

def_class sys_string
def_method length, sys/string_length, static, {r0}, {r1}
def_method copy, sys/string_copy, static, {r0, r1}, {r0, r1}
def_method compare, sys/string_compare, static, {r0, r1}, {r0}
def_method to_long, sys/string_to_long, static, {r0, r1}, {r0}
def_method from_long, sys/string_from_long, static, {r0, r1, r2}
def_method hash, sys/string_hash, static, {r0}, {r1}

%endif
