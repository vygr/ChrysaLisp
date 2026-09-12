;single system instance only
(if (= (length (mail-enquire "@Debug")) 0)
	(import "./app_impl.lisp"))
