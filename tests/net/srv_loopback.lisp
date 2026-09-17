(defun run-server ()
	(print "=== SERVER STARTED (10 nodes) ===")
	(mail-send (open-child "service/net/link" +kn_call_pin) ":4567")
	(print "=== SERVER LISTENING ON :4567 ===")
	(while :t
		(task-sleep 1000000)))

(catch
	(run-server)
	(progn
		;report error
		(print "Server failed with error " _)
		;signal to abort the catch
		:t))

(stream-flush (io-stream "stdout"))
(pii-exit)
