;single system instance only
(if (= 0 (length (mail-enquire "@Onslaught,")))
	(import "./app_impl.lisp"))
