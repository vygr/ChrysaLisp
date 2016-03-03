%ifndef CLASS_VIEW_1234
    %define CLASS_VIEW_1234

%include 'class/class_ref.inc'

    def_class view, ref
	def_method_override create, class/view/create
	def_method_override new, class/view/new
    def_method_override init, class/view/init
    def_method_override deinit, class/view/deinit
    def_method add, class/view/add
    def_method sub, class/view/sub

	struc view, ref_size
	endstruc

%endif
