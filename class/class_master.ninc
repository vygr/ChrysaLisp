%ifndef CLASS_MASTER_1234
%define CLASS_MASTER_1234

%include 'class/class_ref.ninc'

def_class master, ref
def_method create, class/master/create, static, {}, {r0}
def_method new, class/master/new, static, {}, {r0}
def_method init, class/master/init, static, {r0, r1}, {r1}
def_method start, class/master/start, static, {r0, r1, r2}
def_method stop, class/master/stop, static, {r0}
def_method get_state, class/master/get_state, static, {r0}, {r1}
def_method get_stream, class/master/get_stream, static, {r0, r1}, {r1}
def_method select, class/master/select, static, {r0, r1}, {r1}
def_method get_input, class/master/get_input, static, {r0}, {r1}

def_method deinit, class/master/deinit, override

def_struct master, ref
	ptr master_streams
	ptr master_mailbox_array
	pptr master_select_array
	ulong master_state
def_struct_end

%endif
