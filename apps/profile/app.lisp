(import "sys/lisp.inc")

;single instance only check
(if (= (length (mail-enquire "*Profile")) 0)
	(import "./app_impl.lisp"))
