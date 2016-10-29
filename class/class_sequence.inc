%ifndef CLASS_SEQUENCE_1234
%define CLASS_SEQUENCE_1234

%include 'class/class_ref.inc'

def_class sequence, ref
def_method get_length, class/obj/null, virtual, {r0}, {r1}
def_method ref_element, class/obj/null, virtual, {r0, r1}, {r1}
def_method slice, class/obj/null, virtual, {r0, r1, r2}, {r1}

def_struct sequence, ref
def_struct_end

%endif
