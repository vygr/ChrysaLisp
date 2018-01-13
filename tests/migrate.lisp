;import system settings
(run 'apps/sys.lisp)

;open farm of children
(defq ids (slot open_farm nil "tests/migrate_child" 128 kn_call_child))

;send msgs etc
(while (defq cpu (pop ids) mbox (pop ids))
	(if mbox (slot mail_send nil (char 10 long_size) mbox cpu)))
