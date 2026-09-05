;;;;;;;;;;;;;;;;;;;;;
; service/net/app.lisp
;;;;;;;;;;;;;;;;;;;;;
(if (empty? (mail-enquire "*Socket,"))
	(import "./app_impl.lisp"))