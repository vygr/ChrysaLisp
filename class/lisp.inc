;import symbols
(import 'sys/symbols.inc)

;class lisp bindings
(ffi get-field "class/obj/lisp_get_field" 0)
(ffi set-field "class/obj/lisp_set_field" 0)
(ffi stream-avail "class/stream/lisp_available" 0)
(ffi stream-flush "class/stream/lisp_write_flush" 0)
(import 'class/num/lisp.inc)
(import 'class/fixed/lisp.inc)
(import 'class/nums/lisp.inc)
(import 'class/out/lisp.inc)
(import 'class/in/lisp.inc)
(import 'class/stdio/lisp.inc)
