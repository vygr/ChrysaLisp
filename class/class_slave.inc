%ifndef CLASS_SLAVE_1234
%define CLASS_SLAVE_1234

(import 'class/class_ref.inc)

(def-class 'slave 'ref)
(def-method 'create 'class/slave/create 'static '() '(r0))
(def-method 'new 'class/slave/new 'static '() '(r0))
(def-method 'init 'class/slave/init 'static '(r0 r1) '(r1))
(def-method 'get_args 'class/slave/get_args 'static '(r0) '(r1))

(def-method 'deinit 'class/slave/deinit 'override)

(def-struct 'slave 'ref)
	(ptr 'slave_stdin)
	(ptr 'slave_stdout)
	(ptr 'slave_stderr)
	(ptr 'slave_args)
(def-struct-end)

(def-struct 'slave_mail_init 'msg_header)
	(struct 'slave_mail_init_stdout_id 'id)
	(struct 'slave_mail_init_stderr_id 'id)
	(struct 'slave_mail_init_ack_id 'id)
	(offset 'slave_mail_init_args)
(def-struct-end)

%endif
