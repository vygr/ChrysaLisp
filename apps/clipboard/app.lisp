;single instance per node only
(if (= 0 (length (mail-enquire "Clipboard,")))
	(import "./app_impl.lisp"))
