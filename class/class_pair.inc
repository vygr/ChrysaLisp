%ifndef CLASS_PAIR_1234
%define CLASS_PAIR_1234

%include 'class/class_ref.inc'

def_class pair, ref
def_method create, class/pair/create, static, {r0, r1}, {r0}
def_method new, class/pair/new
def_method init, class/pair/init, static, {r0, r1, r2, r3}, {r1}
def_method ref_first, class/pair/ref_first, static, {r0}, {r1}
def_method ref_second, class/pair/ref_second, static, {r0}, {r1}
def_method get_first, class/pair/get_first, static, {r0}, {r1}
def_method get_second, class/pair/get_second, static, {r0}, {r1}
def_method set_second, class/pair/set_second, static, {r0, r1}

def_method deinit, class/pair/deinit, override

def_struct pair, ref
	ptr pair_first
	ptr pair_second
def_struct_end

%endif
