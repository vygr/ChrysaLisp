;single instance per node only
(if (empty? (mail-enquire "Clipboard,"))
	(import "./app_impl.lisp"))
