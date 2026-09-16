;read args from parent
(defq mbox (mail-read (task-mbox)) running :t)

;poll pii stdin
(while running
	(while (cond
		((> (defq c (pii-read-char 0)) 0)
			(mail-send mbox (char c))
			:t)
		((= c -1)
			;stdin EOF - send Ctrl-D to trigger clean exit
			(mail-send mbox (ascii-char 4))
			(setq running :nil)
			:nil)
		(:t :nil)))
	(when running
		(task-sleep 10000)))

