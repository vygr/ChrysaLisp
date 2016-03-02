%ifndef CLASS_REF_1234
    %define CLASS_REF_1234

%include 'class/class_obj.inc'

    def_class ref, obj
    def_method_override ref, new, class/ref/new
    def_method_override ref, init, class/ref/init

%endif
