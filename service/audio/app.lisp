;one for each node that asks, which is each desktop. There was one for the
;machine, on the node of whichever desktop came up first, and when that
;desktop quit the others had no sound
(unless (mail-here "@Audio")
	(import "./app_impl.lisp"))
