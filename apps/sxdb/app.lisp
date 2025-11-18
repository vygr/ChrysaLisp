;single instance per node only
(if (= 0 (length (mail-enquire "SXDb,")))
	(import "./app_impl.lisp"))
