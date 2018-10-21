(include 'class/stream/class.inc)

(def-class 'stream_str 'stream)
(dec-method 'vtable 'class/class_stream_str)
(dec-method 'create 'class/stream_str/create 'static '(r0) '(r0))
(dec-method 'new 'class/stream_str/new 'static)
(dec-method 'init 'class/stream_str/init 'static '(r0 r1 r2))
(dec-method 'ref_string 'class/stream_str/ref_string 'static '(r0) '(r0 r1))

(dec-method 'write_next 'class/stream_str/write_next 'final)
(dec-method 'write_flush 'class/stream_str/write_flush 'final)

(def-struct 'stream_str 'stream)
(def-struct-end)
