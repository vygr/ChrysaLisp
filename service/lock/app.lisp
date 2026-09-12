;single system instance only
(if (empty? (mail-enquire "@Lock,"))
	(import "./app_impl.lisp"))
