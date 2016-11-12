%ifndef CLASS_STREAM_1234
%define CLASS_STREAM_1234

(import 'class/class_ref.inc)

(def-class 'stream 'ref)
(def-method 'create 'class/stream/create 'static '(r0, r1, r2, r3), '(r0))
(def-method 'new 'class/stream/new 'static '(), '(r0))
(def-method 'init 'class/stream/init 'static '(r0, r1, r2, r3, r5, r6), '(r1))
(def-method 'available 'class/stream/available 'static '(r0), '(r1))
(def-method 'read_char 'class/stream/read_char 'static '(r0), '(r1))
(def-method 'read_line 'class/stream/read_line 'static '(r0, r1, r2), '(r1))
(def-method 'read 'class/stream/read 'static '(r0, r1, r2), '(r1))
(def-method 'write_char 'class/stream/write_char 'static '(r0, r1))
(def-method 'write 'class/stream/write 'static '(r0, r1, r2))
(def-method 'write_cstr 'class/stream/write_cstr 'static '(r0, r1))
(def-method 'skip 'class/stream/skip 'static '(r0, r1))
(def-method 'skip_not 'class/stream/skip_not 'static '(r0, r1))
(def-method 'split 'class/stream/split 'static '(r0, r1), '(r1))

(def-method 'deinit 'class/stream/deinit 'override)

(def-method 'read_ready 'class/stream/read_ready 'virtual '(r0), '(r1))
(def-method 'read_next 'class/stream/read_next 'virtual '(r0), '(r1))
(def-method 'write_next 'class/stream/write_next 'virtual '(r0))
(def-method 'write_flush 'class/stream/write_flush 'virtual '(r0))

(def-struct 'stream 'ref)
	ptr stream_object
	ptr stream_buffer
	pubyte stream_bufp
	pubyte stream_bufe
(def-struct-end)

%endif
