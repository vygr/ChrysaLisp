;import ui settings
(run 'apps/ui.lisp)

;open pipe of children
(defq pipe (map (lambda (_) "tests/child") (range 0 128))
	ids (slot open_pipe nil pipe))

;send msgs etc
(while (defq cpu (pop ids) mbox (pop ids))
	(if mbox (slot mail_send nil "" mbox cpu)))
