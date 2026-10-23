;single system instance only
(if (empty? (mail-enquire "@Source,"))
	(import "./app_impl.lisp"))
