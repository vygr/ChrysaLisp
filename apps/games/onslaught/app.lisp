;single system instance only
(when (empty? (mail-enquire "@Onslaught,"))
	(import "./app_impl.lisp"))
