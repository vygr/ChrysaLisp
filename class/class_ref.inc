%ifndef CLASS_REF_1234
%define CLASS_REF_1234

%include 'class/class_obj.ninc'

def_class ref, obj
def_method init, class/ref/init
def_method ref, class/ref/ref, static, {r0}
def_method deref, class/ref/deref, static, {r0}
def_method ref_if, class/ref/ref_if, static, {r0}
def_method deref_if, class/ref/deref_if, static, {r0}

def_struct ref, obj
	ulong ref_count
def_struct_end

%endif
