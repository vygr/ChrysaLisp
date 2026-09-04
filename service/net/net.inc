;;;;;;;;;;;;;;;;;;;;;;
; service/net/net.inc
;;;;;;;;;;;;;;;;;;;;;;
;low-level PII network bindings
(ffi "service/net/lisp_init" net-init)
; (net-init) -> num
(ffi "service/net/lisp_deinit" net-deinit)
; (net-deinit) -> num
(ffi "service/net/lisp_connect" net-connect)
; (net-connect host port) -> handle
(ffi "service/net/lisp_listen" net-listen)
; (net-listen port) -> handle
(ffi "service/net/lisp_accept" net-accept)
; (net-accept handle) -> handle
(ffi "service/net/lisp_send" net-send)
; (net-send handle str) -> bytes_sent
(ffi "service/net/lisp_recv" net-recv)
; (net-recv handle max_len) -> str | :nil
(ffi "service/net/lisp_close" net-close)
; (net-close handle) -> num
(ffi "service/net/lisp_poll" net-poll)
; (net-poll handle) -> flags