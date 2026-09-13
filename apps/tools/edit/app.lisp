;single system instance only
(if (= 0 (length (mail-enquire "@Edit,")))
	(import "./app_impl.lisp"))
