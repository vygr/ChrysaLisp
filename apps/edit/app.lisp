(import "sys/lisp.inc")

;single instance per node only
(if (= 0 (length (mail-enquire "Edit,")))
	(import "./app_impl.lisp"))
