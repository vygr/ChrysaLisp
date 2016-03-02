%ifndef CLASS_VIEW1_1234
    %define CLASS_VIEW1_1234

%include 'class/class_ref.inc'

    def_class view1, ref
    def_method_override view1, new, class/view1/new
    def_method_override view1, init, class/view1/init
    def_method_override view1, deinit, class/view1/deinit
	def_method view1, mul, class/view1/mul
	def_method view1, add, class/view1/add
    def_method view1, sub, class/view1/sub

%endif
