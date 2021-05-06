(import "sys/lisp.inc")

;single instance only
(if (= (length (mail-enquire "DEBUG_SERVICE")) 0)
	(import "apps/debug/app_impl.lisp"))
