%ifndef CLASS_STREAM_1234
%define CLASS_STREAM_1234

%include 'class/class_ref.inc'

	def_class stream, ref
	def_method create, class/stream/create, static, {r0, r1, r2, r3}, {r0}
	def_method new, class/stream/new, static, {}, {r0}
	def_method init, class/stream/init, static, {r0, r1, r2, r3, r5, r6}, {r1}
	def_method available, class/stream/available, static, {r0}, {r1}
	def_method read_char, class/stream/read_char, static, {r0}, {r1}
	def_method read_line, class/stream/read_line, static, {r0, r1, r2}, {r1}
	def_method skip, class/stream/skip, static, {r0, r1}
	def_method skip_not, class/stream/skip_not, static, {r0, r1}
	def_method split, class/stream/split, static, {r0, r1}, {r1}

	def_structure stream, ref
		ptr stream_object
		ptr stream_buffer
		pubyte stream_bufp
		pubyte stream_bufe
	def_structure_end

%endif
