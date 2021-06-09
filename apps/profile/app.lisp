(import "sys/lisp.inc")

;single instance only check
(if (= (length (mail-enquire "PROFILE_SERVICE")) 0)
	(import "./app_impl.lisp"))
