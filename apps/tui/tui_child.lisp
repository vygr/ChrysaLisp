;read args from parent, who may be gone before it says
(defq mbox (mail-read-timeout (task-mbox) (task-timeout 10)) running mbox)

;poll pii stdin
(while running
	(while (cond
			((> (defq c (pii-read-char 0)) 0)
				(mail-send mbox (char c))
				:t)
			((= c -1)
				;stdin EOF - send Ctrl-D to trigger clean exit
				(mail-send mbox "\x04")
				(setq running :nil)
				:nil)
			(:t :nil)))
	(when running
		(task-sleep 10000)))
