%ifndef CLASS_STREAM_STR_1234
%define CLASS_STREAM_STR_1234

%include 'class/class_stream.inc'

def_class stream_str, stream
def_method create, class/stream_str/create, static, {r0}, {r0}
def_method new, class/stream_str/new, static
def_method init, class/stream_str/init, static, {r0, r1, r2}, {r1}
def_method ref_string, class/stream_str/ref_string, static, {r0}, {r1}

def_method write_next, class/stream_str/write_next, override

def_structure stream_str, stream
def_structure_end

%endif
