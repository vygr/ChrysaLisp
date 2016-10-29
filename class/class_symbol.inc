%ifndef CLASS_SYMBOL_1234
%define CLASS_SYMBOL_1234

%include 'class/class_string.inc'

def_class symbol, string
def_method create_from_buffer, class/symbol/create_from_buffer, static, {r0, r1}, {r0}
def_method create_from_cstr, class/symbol/create_from_cstr, static, {r0}, {r0}
def_method create_from_file, class/symbol/create_from_file, static, {r0}, {r0}
def_method create_from_long, class/symbol/create_from_long, static, {r0, r1}, {r0}
def_method append, class/symbol/append, static, {r0, r1}, {r0}

def_struc symbol, string
def_struc_end

%endif
