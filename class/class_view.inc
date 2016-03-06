%ifndef CLASS_VIEW_1234
    %define CLASS_VIEW_1234

%include 'class/class_ref.inc'

    def_class view, ref
	def_method create, class/view/create
	def_method new, class/view/new
    def_method init, class/view/init
	def_method add, class/view/add
	def_method sub, class/view/sub
    def_method deinit, class/view/deinit, override
	def_method draw, class/obj/null, virtual

	struc view, ref_size
	endstruc

%endif
