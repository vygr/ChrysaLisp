;import system settings
(run 'apps/sys.lisp)

;open pipe of children
(defq pipe (list "tests/child"))
(while (lt (length pipe) 128) (setq pipe (cat pipe pipe)))
(defq ids (open-pipe pipe))

;send msgs etc
(while (defq cpu (pop ids) mbox (pop ids))
	(if (ne mbox 0) (mail-send "" mbox cpu)))
