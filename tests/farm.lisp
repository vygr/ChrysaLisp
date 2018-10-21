;import system settings
(import 'sys/lisp.inc)

;open farm of children
(defq ids (open-farm "tests/child" 128 kn_call_child))

;send msgs etc
(while (defq mbox (pop ids))
	(if (ne mbox 0) (mail-send "" mbox)))
