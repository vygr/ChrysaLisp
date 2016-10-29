%ifndef CLASS_BOXED_PTR_1234
%define CLASS_BOXED_PTR_1234

%include 'class/class_ref.inc'

def_class boxed_ptr, ref
def_method create, class/boxed_ptr/create
def_method new, class/boxed_ptr/new
def_method init, class/boxed_ptr/init
def_method get_value, class/boxed_ptr/get_value, static, {r0}, {r1}
def_method set_value, class/boxed_ptr/set_value, static, {r0, r1}
def_method get_flags, class/boxed_ptr/get_flags, static, {r0}, {r1}
def_method set_flags, class/boxed_ptr/set_flags, static, {r0, r1}

def_struc boxed_ptr, ref
	ptr boxed_ptr_value
	ulong boxed_ptr_flags
def_struc_end

%endif
