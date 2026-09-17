(import "lib/task/pipe.inc")

(defun run-test ()
	(print "=== ChrysaLisp Cluster Node Diagnostic Query ===")

	; 1. Wait for local nodes to finish booting
	(print "Waiting for local nodes to boot...")
	(defq local_nodes (net-quiet)
		my_sys_id (hex-encode (system-id)))
	(print "Local nodes booted   : " (length local_nodes)
		" (System: " (slice my_sys_id 0 8) " " (cpu) "/" (abi) "/" (os) ")")

	; 2. Start LAN auto-discovery listener
	(print "Starting LAN auto-discovery listener...")
	(pipe-run "link -a" prin)

	; 3. Wait for cluster peers to join
	(print "Waiting for cluster peers to join...")
	(net-quiet 500000 8 (length local_nodes))

	; 4. Query cluster statistics via cluster command
	(pipe-run "cluster -v" (lambda (%0) (prin %0) (stream-flush (io-stream "stdout")))))

(catch
	(run-test)
	(progn
		;report error
		(print "Test failed with error " _)
		;signal to abort the catch
		:t))

(stream-flush (io-stream "stdout"))
(task-sleep 200000)
(pii-exit)
