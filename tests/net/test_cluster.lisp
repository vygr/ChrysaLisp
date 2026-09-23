(import "lib/task/pipe.inc")

(defun run-test ()
	(print "=== ChrysaLisp Cluster Node Diagnostic Query ===")

	; 1. Start system network service if not already running
	(when (empty? (mail-enquire "@Net,"))
		(print "Starting Net service (@Net)...")
		(open-child "service/net/app.lisp" +kn_call_run)
		(task-sleep 200000))

	; 2. Wait for local nodes to finish booting and stabilize
	(print "Waiting for local nodes to stabilize...")
	(defq local_nodes (net-quiet 500000 6)
		my_sys_id (hex-encode (system-id)))
	(print "Local nodes booted   : " (length local_nodes)
		" (System: " (slice my_sys_id 0 8) " " (cpu) "/" (abi) "/" (os) ")")

	; 3. Start LAN auto-discovery listener
	(print "\nStarting LAN auto-discovery listener...")
	(pipe-run "link -a" (const prin))

	; 4. Wait for cluster peers to connect
	(print "\nWaiting for cluster peers to join...")
	(defq waited 0 max_wait 30)
	(while (and (<= (length (lisp-nodes)) (length local_nodes)) (< (setq waited (inc waited)) max_wait))
		(task-sleep 1000000)
		(prin "[" waited "s: " (length (lisp-nodes)) " nodes] ")
		(stream-flush (io-stream "stdout")))
	(print "")

	; 5. Stabilize cluster topology
	(if (> (length (lisp-nodes)) (length local_nodes))
		(print "Remote peer(s) connected! Stabilizing network topology...")
		(print "No remote peers joined within timeout; probing available nodes..."))
	(defq all_nodes (net-quiet 500000 8))
	(print "Cluster topology stabilized: " (length all_nodes) " nodes active.")

	; 6. Query cluster statistics via cluster command
	(print "\nRunning cluster statistics probe across all active nodes...")
	(pipe-run "cluster -v" (lambda (%0) (prin %0) (stream-flush (io-stream "stdout")))))

(catch
	(run-test)
	(progn
		(print "Test failed with error: " _)
		:t))

(stream-flush (io-stream "stdout"))
(task-sleep 200000)
(pii-exit)
