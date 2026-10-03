;Start Onslaught on the GUI node of another machine on the LAN, and let its
;bot play the game there for a while.
;
;The other machine must be running the GUI, ./run.sh, with a link listening,
;'link -l 3333 -a' from its Terminal. Then on this machine:
;
;	./run_tui.sh -f -s tests/net/remote_onslaught.lisp
;
;How it works. 'link -a' finds the other machine. A probe task on each of its
;nodes says if it can see a Gui service. One task is then pinned on that GUI
;node. It starts the audio service if needed, opens the game, and runs the
;'onslaught -b' bot command there, as the game's @Onslaught service is system
;wide, so is only seen on its own machine. The bot's progress lines are
;mailed back here. The game is left open on the other machine.
(import "lib/task/pipe.inc")
(when (empty? (mail-enquire "@Net,"))
	(open-child "service/net/app.lisp" +kn_call_run)
	(task-sleep 200000))
(defq local_nodes (net-quiet 500000 6) waited 0)
(pipe-run "link -a" (const prin))
(while (and (<= (length (lisp-nodes)) (length local_nodes)) (< (++ waited) 30))
	(task-sleep 1000000))
(defq all_nodes (net-quiet 500000 8)
	remote_nodes (filter (# (not (find %0 local_nodes))) all_nodes)
	probe_mbox (mail-mbox) reply_mbox (mail-mbox) launch_mbox (mail-mbox) gui_node :nil)
(print "local nodes " (length local_nodes) ", remote nodes " (length remote_nodes))
;ask every remote node if it can see a Gui service
(each (lambda (node)
	(open-task (str `(mail-send (hex-decode ,(hex-encode probe_mbox))
			(str (list (hex-encode (task-nodeid)) (length (mail-enquire "Gui,"))))))
		node +kn_call_pin 0 launch_mbox)) remote_nodes)
(times (length remote_nodes)
	(when (defq msg (mail-read-timeout probe_mbox 3000000))
		(bind '(node_hex gui_count) (first (read (string-stream msg))))
		(if (> gui_count 0) (setq gui_node node_hex))))
(ifn gui_node
	(print "NO GUI on the remote machine, it needs to be running ./run.sh")
	(print "remote GUI node " (slice gui_node 0 12) ", starting the game and the bot...")
	(open-task (str `(progn
			(import "lib/task/pipe.inc")
			(defq out (list))
			(if (empty? (mail-enquire "@Audio,"))
				(open-child "service/audio/app.lisp" +kn_call_pin))
			(task-sleep 500000)
			(if (empty? (mail-enquire "@Onslaught,"))
				(open-child "apps/games/onslaught/app.lisp" +kn_call_pin))
			(task-sleep 3000000)
			(pipe-run "onslaught -b 60" (lambda (%0) (push out %0)))
			(mail-send (hex-decode ,(hex-encode reply_mbox))
				(cat (cpu) " " (abi) " " (os) "\n" (join out "")))))
		(hex-decode gui_node) +kn_call_pin 0 launch_mbox)
	(if (defq reply (mail-read-timeout reply_mbox 120000000))
		(print "REMOTE BOT on " reply)
		(print "no reply from the remote node")))
(stream-flush (io-stream "stdout"))
(task-sleep 200000)
(pii-exit)
