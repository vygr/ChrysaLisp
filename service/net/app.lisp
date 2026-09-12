;single system instance only
(if (empty? (mail-enquire "@Net,"))
	(import "./app_impl.lisp"))