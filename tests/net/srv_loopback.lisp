(print "=== SERVER STARTED (10 nodes) ===")
(mail-send (open-child "service/net/link" +kn_call_open) ":4567")
(print "=== SERVER LISTENING ON :4567 ===")
(while :t
	(task-sleep 1000000))
