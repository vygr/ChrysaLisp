;Start Onslaught on the GUI node of another machine on the LAN, then play it
;from this machine, with the bot running here and the game running there.
;
;The other machine must be running the GUI, ./run.sh, with a link listening,
;'link -l 3333 -a' from its Terminal. Then on this machine:
;
;	./run_tui.sh -f -s tests/net/remote_onslaught.lisp
;
;How it works. 'link -a' finds the other machines. Each node known has the
;system id of its machine, so (lisp-systems) is the machines, and (lisp-nodes
;system) the nodes of one. A probe task on each node of the other machines
;says if it can see a Gui service. One task is then pinned on that GUI node. It starts the audio service if needed, opens the game, and mails back
;the mailbox id of the game's service.
;
;That id is the trick. The game declares itself as @Onslaught, a system wide
;name, so a (mail-enquire) for it from here finds nothing. But a mailbox is
;good from anywhere, mail is routed by its id, so once we have the id we can
;talk to the game directly. 'onslaught -m id -b 60' runs the bot on this
;machine, reading the state and setting the keys of the game on the other
;one, 20 times a second, over the link.
(import "lib/task/pipe.inc")
(when (empty? (mail-enquire "@Net,"))
	(open-child "service/net/app.lisp" +kn_call_run)
	(task-sleep 200000))
(defq waited 0)
(net-quiet 500000 6)
(pipe-run "link -a" (const prin))
(while (and (< (length (lisp-systems)) 2) (< (++ waited) 30))
	(task-sleep 1000000))
(net-quiet 500000 8)
(defq systems (rest (lisp-systems))
	remote_nodes (reduce (# (cat %0 (lisp-nodes %1))) systems (list))
	probe_mbox (mail-mbox) reply_mbox (mail-mbox) launch_mbox (mail-mbox) gui_node :nil)
(print "local nodes " (length (lisp-nodes :t)))
(each (# (print "machine " (slice (hex-encode %0) 0 8) ", nodes " (length (lisp-nodes %0)))) systems)
;ask the nodes of the other machines if they can see a Gui service
(each (lambda (node)
	(open-task (str `(mail-send (hex-decode ,(hex-encode probe_mbox))
			(str (list (hex-encode (task-nodeid)) (length (mail-enquire "Gui,"))))))
		node +kn_call_pin 0 launch_mbox)) remote_nodes)
(times (length remote_nodes)
	(when (defq msg (mail-read-timeout probe_mbox 3000000))
		(bind '(node_hex gui_count) (first (read (string-stream msg))))
		(if (> gui_count 0) (setq gui_node node_hex))))
(ifn gui_node
	(print "NO GUI on another machine, one needs to be running ./run.sh")
	(print "remote GUI node " (slice gui_node 0 12) ", starting the game...")
	(open-task (str `(progn
			(if (empty? (mail-enquire "@Audio,"))
				(open-child "service/audio/app.lisp" +kn_call_pin))
			(task-sleep 500000)
			(if (empty? (mail-enquire "@Onslaught,"))
				(open-child "apps/games/onslaught/app.lisp" +kn_call_pin))
			;wait for the game to declare its service, then send its mailbox id
			(defq tries 0)
			(while (and (empty? (defq svc (mail-enquire "@Onslaught,"))) (< (++ tries) 50))
				(task-sleep 100000))
			(mail-send (hex-decode ,(hex-encode reply_mbox))
				(if (empty? svc) "" (second (split (first svc) ","))))))
		(hex-decode gui_node) +kn_call_pin 0 launch_mbox)
	(defq game_mbox (mail-read-timeout reply_mbox 15000000))
	(if (or (not game_mbox) (eql game_mbox ""))
		(print "the game did not start on the remote node")
		(progn
			(print "game service mailbox " game_mbox)
			(print "playing it from here...")
			(pipe-run (cat "onslaught -m " game_mbox " -b 60")
				(lambda (%0) (prin %0) (stream-flush (io-stream "stdout")))))))
(stream-flush (io-stream "stdout"))
(task-sleep 200000)
(pii-exit)
