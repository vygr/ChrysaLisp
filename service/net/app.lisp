;single instance per system only
(if (empty? (mail-enquire "*Net,"))
	(import "./app_impl.lisp"))