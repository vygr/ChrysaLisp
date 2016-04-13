%ifndef CLASS_COMPONENT_1234
%define CLASS_COMPONENT_1234

%include 'class/class_ref.inc'

	def_class component, ref
	def_method init, class/component/init
	def_method connect, class/component/connect
	def_method emit, class/component/emit
	def_method disconnect_slot, class/component/disconnect_slot
	def_method disconnect_sig, class/component/disconnect_sig

	def_method deinit, class/component/deinit, override

	def_structure	component, ref
		def_struct	component_slot_list, lh_list
	def_structure_end

%endif
