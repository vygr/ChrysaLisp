;the watch of a rack session, ./session.lisp. What waits for the session,
;(rack-fresh), writes the time each second while it does. If that stops,
;the one that asked was stopped by hand or has gone, and nothing waits:
;the session is stopped here, its nodes and their notes cleared, and no
;desktop of it is left up. Sent the name of the run.
(import "./rack.inc")
(defq run (mail-read (task-mbox)) file (cat +rack_alive run))
(while (and (defq then (load file)) (< (- (pii-time) (str-as-num then)) +rack_stale))
	(task-sleep 1000000))
(rack-session-clear)
;and the files of the run, as what waited would have
(each (# (pii-remove (cat %0 run))) (list +rack_cmds +rack_out +rack_alive))
;the nodes told to go before this one does, or the mail to them goes with it
(task-sleep 500000)
(pii-exit)
