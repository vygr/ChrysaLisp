(import "lib/options/options.inc")
(import "lib/task/pipe.inc")

(defq usage `(
(("-h" "--help")
"Usage: mesh [options]

    options:
        -h --help: this help info.
        -j --join: join this machine to the others on the network.
        -t --to host: join by way of one machine, its name or address,
            for when the others are not found by themselves.
        -k --key: make a key, the file mesh_key, if there is none.

    Join the machines on a network into one, and say how it stands.

    With no options, say how it stands: if this machine has a key, if it
    has joined, and each machine there is, how many nodes it has, and
    what it is. If something looks wrong it says what to look at.

        mesh -k           ; once, on one machine, then copy mesh_key to
                          ; the same place on the others, by hand
        mesh -j           ; on each machine, each time it is started
        mesh              ; who is there ?

    docs/intro/mesh.md is the guide.")
(("-j" "--join") ,(opt-flag 'opt_j))
(("-t" "--to") ,(opt-str 'opt_t))
(("-k" "--key") ,(opt-flag 'opt_k))
))

(defun joined? ()
	;has a node of this machine a Net service that is in the mesh
	(defq here (lisp-nodes :t))
	(some (# (find (slice (hex-decode (second (split %0 ","))) +mailbox_id_size -1) here))
		(mail-enquire "*Mesh,")))

(defun what-is (system)
	;what a machine says it is, arm64/Darwin, asked of one of its nodes
	(defq mbox (mail-mbox) nodes (lisp-nodes system))
	(cond
		((empty? nodes) "?")
		(:t (open-task (str `(mail-send (hex-decode ,(hex-encode mbox)) (cat (str (cpu)) "/" (str (os)))))
				(first nodes) +kn_call_pin 0 (mail-mbox))
			(ifn (mail-read-timeout mbox (task-timeout 2)) "no answer"))))

(defun status ()
	(defq key (if (pii-fstat "mesh_key") :t) is_joined (joined?) systems (lisp-systems))
	(print (if key "Key:     mesh_key, only machines with the same file can join."
		"Key:     none. Any machine on the network can join. mesh -k makes one."))
	(print (if is_joined "Joined:  yes." "Joined:  no. mesh -j joins."))
	(print "Machines:")
	(each (lambda (system)
		(print "    " (slice (hex-encode system) 0 8) "  "
			(pad (str (length (lisp-nodes system))) 3) " nodes  " (what-is system)
			(if (eql system (cat (system-id))) "  (this machine)" "")))
		systems)
	;what to look at when it is not as hoped
	(when (and is_joined (= (length systems) 1))
		(print)
		(print "No other machine is seen. If one should be:")
		(print "    Has it joined too ? mesh -j there, and mesh to see.")
		(print (if key "    Has it the same mesh_key file, in the same place ?"
			"    Has it a mesh_key file ? A machine with a key will not join one with none."))
		(print "    Was the key put there before ChrysaLisp was started ? It is read at the start.")
		(print "    Is it the same version ? Update and make install on both.")
		(print "    Does a firewall stop it ? TCP port 3333 and UDP port 3334 must be let in.")
		(print "    Are they on one network ? If not, or if it will not find it, name one: mesh -t address")))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_j :nil opt_t :nil opt_k :nil args (options stdio usage)))
		(when opt_k
			(pipe-run "link -k" (const prin))
			(unless opt_j
				(print "Copy mesh_key to the same place on each other machine, by hand,")
				(print "then start ChrysaLisp again on each and mesh -j.")))
		(when (or opt_j opt_t)
			;listen, say so to the network, and look for the others. Or
			;listen, and ask one machine who the others are
			(pipe-run "link -l 3333 -a" (const prin))
			(pipe-run (if opt_t (cat "link -m " opt_t) "link -a") (const prin))
			;a few seconds when all are new. A machine that was here a
			;moment ago and left is not tried again at once, so longer
			(print "Joining. It can take half a minute for the others to be seen.")
			(defq t0 (pii-time))
			(while (and (= (length (lisp-systems)) 1) (< (- (pii-time) t0) (task-timeout 30)))
				(task-sleep 200000))
			;one is seen, the rest are close behind
			(task-sleep 2000000))
		(unless (and opt_k (not opt_j) (not opt_t)) (status))))
