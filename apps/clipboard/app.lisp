(import "sys/lisp.inc")

;single instance only
(if (= (length (mail-enquire "CLIPBOARD_SERVICE")) 0)
	(import "apps/clipboard/app_impl.lisp"))
