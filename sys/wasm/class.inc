(include "sys/class.inc")

;;;;;;;;;;;;;;;;;;;
; host WASM methods
;;;;;;;;;;;;;;;;;;;

(def-class 'host_wasm 'sys_obj)
(dec-method 'vtable :load)
(dec-method 'vtable :unload)
(dec-method 'vtable :get_export)
(dec-method 'vtable :call_i64)
(dec-method 'vtable :call_f64)
(dec-method 'vtable :get_memory)
(dec-method 'vtable :write_memory)
(dec-method 'vtable :read_memory)
(dec-method 'vtable :get_error)
(def-method-end)
