;import system settings
(run 'sys/lisp.inc)

;open pipe of children
(defq pipe (list "tests/child"))
(while (lt (length pipe) 128) (setq pipe (cat pipe pipe)))
(defq ids (open-pipe pipe))

;send msgs etc
(while (defq mbox (pop ids))
	(if (ne mbox 0) (mail-send "" mbox)))
