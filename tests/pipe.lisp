;import system settings
(run 'apps/sys.lisp)

;open pipe of children
(defq pipe (map (lambda (_) "tests/child") (range 0 128))
	ids (slot open_pipe nil pipe))

;send msgs etc
(while (defq cpu (pop ids) mbox (pop ids))
	(if (ne mbox 0) (slot mail_send nil "" mbox cpu)))
