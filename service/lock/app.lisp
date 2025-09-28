;single instance per system only
(if (empty? (mail-enquire "*Lock,"))
	(import "./app_impl.lisp"))
