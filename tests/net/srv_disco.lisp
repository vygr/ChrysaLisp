(import "service/net/app.inc")

(print "=== DISCO SERVER STARTED (10 nodes) ===")
(defq l_status (net-link-rpc ":4567"))
(print "=== SERVER LISTENING ON :4567 (status: " l_status ") ===")
(defq b_status (net-beacon-rpc 4567))
(print "=== SERVER BEACONING ON PORT 4567 (status: " b_status ") ===")

(while :t
	(task-sleep 1000000))
