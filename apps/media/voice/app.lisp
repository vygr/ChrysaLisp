;one for each desktop, as a phone has one phone app: the call is the voice
;service's, and two of it would record and play over each other. Each
;desktop is a node, the app is on its node. One that died without going
;is still said, and is no guard, (mail-here)
(unless (mail-here "Voice")
	(import "./app_impl.lisp"))
