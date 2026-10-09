(report-header "Rack: one command on every machine of the mesh, each in a session of its own")

;this starts sessions and a node of another system, so it is in tests/solo/,
;and runs on its own. The tests are themselves run by a rack run, on each
;machine, so this is a rack run inside a session of a rack run, which is
;why a session has files of its own, lib/rack/rack.inc

(import "lib/rack/rack.inc")

(defun rk-clear (root)
	(each (# (pii-remove (cat root "/" %0))) (sync-walk root (sync-rules ""))))

(defun rk-at (text what)
	;where in the text that is first found, :nil if it is not
	(if (nempty? (defq found (substr text what))) (first (first (first found)))))

(defun rk-rack-files ()
	;the files of rack sessions left in /tmp, there are to be none after a run
	(filter (# (or (starts-with "chrysalisp_rack_cmds_" %0) (starts-with "chrysalisp_rack_out_" %0)))
		(map (const first) (partition (split (pii-dirlist "/tmp") ",") 2))))

(cond
	((nempty? (sync-services))
		;a rack run is of every machine that takes a sync. Run from a desktop
		;that is joined to a mesh, this would send a tree to real machines
		;and run a command on them. The harness runs it in a session that is
		;joined to nothing
		(test-skip "rack" "a machine that takes a sync is in reach, this is only run where there is none"))
	(:t
		;a session, new, on this machine: each phase a session of its own,
		;one after another, and what the commands of each said
		(defq rk_left (rk-rack-files) rk_mine (lisp-nodes)
			rk_said (rack-fresh (list (list "echo one" "echo two") (list "echo three"))))
		(assert-true "a session runs its command lines, and the next phase runs after"
			(and (defq rk_1 (rk-at rk_said "one")) (defq rk_2 (rk-at rk_said "two"))
				(defq rk_3 (rk-at rk_said "three")) (< rk_1 rk_2 rk_3)))
		(assert-eq "each session says how long it took" 2 (length (substr rk_said "s]")))
		(assert-list-eq "and leaves none of its files" rk_left (rk-rack-files))
		;a node of the test before this may still be going from the list, so
		;none new, not the same list
		(assert-true "the nodes it started are not of this network" (every (# (find %0 rk_mine)) (lisp-nodes)))
		(assert-true "a command there is none of is not hung on, the session ends"
			(starts-with "[" (rack-fresh (list (list "no_such_command_xyz")))))

		;another machine. A node with a system id of its own, as a subnet has,
		;that takes a sync into a tree of its own. Both are small trees of
		;tests/scratch/, not the system's
		(defq rk_src "tests/scratch/rack_src" rk_dst "tests/scratch/rack_dst"
			rk_sid "racktestsystemid" rk_me (hex-encode (system-id)))
		(rk-clear rk_src) (rk-clear rk_dst)
		(save "one" (cat rk_src "/one.txt"))
		(save "two" (cat rk_src "/deep/two.txt"))
		(save "not sent" (cat rk_src "/built.o"))
		(save "*.o" (cat rk_src "/.gitignore"))
		(save "old" (cat rk_dst "/one.txt"))
		(save "to go" (cat rk_dst "/gone.txt"))
		(save "stays" (cat rk_dst "/stays.txt"))
		(defq rk_pids (node-net :ring 1 0 :nil :nil "rk_other" rk_sid) rk_t0 (pii-time))
		(while (and (empty? (lisp-nodes rk_sid)) (< (- (pii-time) rk_t0) (task-timeout 10)))
			(task-sleep 50000))
		(defq rk_node (first (lisp-nodes rk_sid)))
		(assert-true "a node of another system is started" rk_node)
		(when rk_node
			(open-task (str `(mail-send (open-child "service/sync/app.lisp" +kn_call_pin)
					(cat "*Sync" (ascii-char 10) ,rk_dst)))
				rk_node +kn_call_pin 0 (mail-mbox))
			(setq rk_t0 (pii-time))
			(while (and (empty? (sync-services)) (< (- (pii-time) rk_t0) (task-timeout 10)))
				(task-sleep 50000))
			(defq rk_svc (first (sync-services)))
			(assert-true "it takes a sync, and is not this machine" (and rk_svc (nql (second rk_svc) rk_me)))
			(when rk_svc
				;the run: the other is made the same, a file is removed there,
				;and the command is run on both
				;the sessions are a user's, and the command says whose. A line
				;with a / in it is one a run keeps of what a session said
				(defq rk_out (rack-run "lisp -r (print {user/} (get (quote *env_node_user*)))"
						:nil :nil (list "gone.txt") "Test" rk_src)
					rk_sync (filter (# (starts-with "SYNC " %0)) rk_out)
					rk_ran (filter (# (starts-with "RAN " %0)) rk_out))
				(assert-eq "one other machine is made the same" 1 (length rk_sync))
				(assert-true "three files sent, the tree's two and its .gitignore, none failed"
					(and (found? (first rk_sync) " 3 sent ") (found? (first rk_sync) " 0 failed ")))
				(assert-eq "what is here is there" "one" (load (cat rk_dst "/one.txt")))
				(assert-eq "in its folder" "two" (load (cat rk_dst "/deep/two.txt")))
				(assert-eq "what the rules leave out is not sent" :nil (load (cat rk_dst "/built.o")))
				(assert-true "the file that was to go is removed there" (some (# (eql %0 (cat "GONE " (elem-get rk_svc 2) " 1 of 1 removed"))) rk_out))
				(assert-eq "it is gone" :nil (load (cat rk_dst "/gone.txt")))
				(assert-eq "and no other is" "stays" (load (cat rk_dst "/stays.txt")))
				(assert-eq "the command is run on both machines" 2 (length rk_ran))
				(assert-true "each in a session that says how long it took" (every (# (found? %0 "s]")) rk_ran))
				(assert-true "and is the user's it was asked to be" (every (# (found? %0 "user/Test")) rk_ran))
				(assert-true "the last line is DONE" (starts-with "DONE " (last rk_out)))
				(assert-list-eq "and no file of a session is left" rk_left (rk-rack-files))

				;a machine can be made the same and left out of the run
				(save "three" (cat rk_src "/three.txt"))
				(setq rk_out (rack-run "echo ran here" :nil (list (elem-get rk_svc 2)) :nil :nil rk_src))
				(assert-true "a machine left out is still made the same"
					(and (found? (first rk_out) " 1 sent ") (eql (load (cat rk_dst "/three.txt")) "three")))
				(assert-true "and is said to be left out, not run"
					(every (# (found? %0 "left out, not run")) (filter (# (starts-with "RAN " %0)) rk_out)))))
		;the other machine is told to go, by node, no note of a network is
		;kept on Windows
		(if rk_node (open-task "(pii-exit)" rk_node +kn_call_pin 0 (mail-mbox)))
		(node-stop "rk_other")
		(setq rk_t0 (pii-time))
		(while (and (some (const pii-alive) rk_pids) (< (- (pii-time) rk_t0) (task-timeout 10)))
			(task-sleep 20000))
		(assert-true "the other machine is stopped" (notany (const pii-alive) rk_pids))
		(setq rk_t0 (pii-time))
		(while (and (nempty? (sync-services)) (< (- (pii-time) rk_t0) (task-timeout 10)))
			(task-sleep 50000))
		(assert-list-eq "and no machine takes a sync after" '() (sync-services))
		(rk-clear rk_src) (rk-clear rk_dst)))
