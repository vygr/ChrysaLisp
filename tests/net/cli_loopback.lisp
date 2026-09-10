(print "=== CLIENT STARTED (10 nodes) ===")
(print "CLIENT: Initial peer nodes count = " (length (mail-nodes)))

(print "CLIENT: Connecting to 127.0.0.1:4567...")
(mail-send (open-child "service/net/link" +kn_call_open) "127.0.0.1:4567")

; Wait for remote nodes to appear (expecting at least 10 server nodes)
(defq timeout 50)
(while (and (< (length (mail-nodes)) 10) (> timeout 0))
	(task-sleep 100000)
	(setq timeout (- timeout 1)))

(print "CLIENT: Connected! Peer nodes count = " (length (mail-nodes)))
(if (>= (length (mail-nodes)) 10)
	(print "CLIENT: SUCCESS - Discovered nodes across link!")
	(print "CLIENT: FAILED - Did not discover nodes."))

; Test message dispatch across cluster
(defq reply_mbox (mail-mbox))
(defq child_code (cat
	"(mail-send (hex-decode \q" (hex-encode reply_mbox) "\q) \qPING_PONG_OK\q)"))
(open-child child_code +kn_call_child)
(defq res (mail-read reply_mbox))
(print "CLIENT: Remote task response = " res)
(if (eql res "PING_PONG_OK")
	(print "=== LOOPBACK TEST RESULT: SUCCESS ===")
	(print "=== LOOPBACK TEST RESULT: FAILED ==="))

((ffi "service/gui/lisp_deinit"))
