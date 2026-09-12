;single system instance only
(if (empty? (mail-enquire "@Clipboard,"))
	(import "./app_impl.lisp"))
