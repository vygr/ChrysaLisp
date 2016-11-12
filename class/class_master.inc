%ifndef CLASS_MASTER_1234
%define CLASS_MASTER_1234

(import 'class/class_ref.inc)

(def-class 'master 'ref)
(def-method 'create 'class/master/create 'static '() '(r0))
(def-method 'new 'class/master/new 'static '() '(r0))
(def-method 'init 'class/master/init 'static '(r0 r1) '(r1))
(def-method 'start 'class/master/start 'static '(r0 r1 r2))
(def-method 'stop 'class/master/stop 'static '(r0))
(def-method 'get_state 'class/master/get_state 'static '(r0) '(r1))
(def-method 'get_stream 'class/master/get_stream 'static '(r0 r1) '(r1))
(def-method 'select 'class/master/select 'static '(r0 r1) '(r1))
(def-method 'get_input 'class/master/get_input 'static '(r0) '(r1))

(def-method 'deinit 'class/master/deinit 'override)

(def-struct 'master 'ref)
	(ptr 'master_streams)
	(ptr 'master_mailbox_array)
	pptr master_select_array
	(ulong 'master_state)
(def-struct-end)

%endif
