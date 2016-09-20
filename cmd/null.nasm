%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_slave.inc'

	def_function cmd/null

		ptr slave
		ulong eof

		;init app vars
		push_scope

		;initialize pipe details and command args, abort on error
		static_call slave, create, {}, {slave}
		if {slave}
			;dump stdin till EOF
			loop_start
				method_call stream, read_next, {slave->slave_stdin}, {eof}
			loop_until {eof == -1}

			;clean up
			static_call slave, deref, {slave}
		endif
		pop_scope
		return

	def_function_end
