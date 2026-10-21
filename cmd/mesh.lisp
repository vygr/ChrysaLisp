(import "lib/options/options.inc")
(import "lib/task/pipe.inc")
(import "service/net/app.inc")
(import "service/net/keys.inc")

(defq usage `(
(("-h" "--help")
"Usage: mesh [options]

    options:
        -h --help: this help info.
        -j --join: join this machine to the others on the network, now.
            It does by itself as it starts, this is to ask again.
        -t --to host: join by way of one machine, its name or address,
            for when the others are not found by themselves.
        -k --key: make a key, own, and use it, if there is none. An
            install has made one.

    The machines of a network that have the same key join into one, by
    themselves. This says how it stands.

    With no options, say how it stands: if this machine has a key, if it
    has joined, and each machine there is, how many nodes it has, and
    what it is. If something looks wrong it says what to look at.

    A machine has a key from its install, its own, and is by itself. To
    join it to the others take their key into its keyring and use it,
    key -i and key -u, the key command. It sees the key change, and
    joins. With no key in use it is off the network.

    Of the sessions of one machine, a desktop and a terminal say, the
    first to start is its member of the mesh. Another takes its place
    when it has gone.

        mesh              ; who is there ?
        mesh -t address   ; join by way of that machine

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

(defun status ()
	(defq key (if (pii-fstat "mesh_key") :t) is_joined (joined?) systems (lisp-systems))
	(print (if key (cat "Key:     " (ifn (keys-in-use) "mesh_key") ", " (keys-print (door-key)) ", only machines with the same key can join.")
		"Key:     none. This machine is off the network till it has one. Take in the key of the others, key -i and key -u, or mesh -k makes one."))
	(print (cond
		(is_joined "Joined:  yes.")
		(key "Joined:  no. Another session of this machine may be its member, the first to start is. Or it is a moment too soon.")
		(:t "Joined:  no.")))
	(print "Machines:")
	(each (lambda (system)
		(print "    " (slice (hex-encode system) 0 8) "  "
			(pad (str (length (lisp-nodes system))) 3) " nodes  " (net-what-is system)
			(if (eql system (cat (system-id))) "  (this machine)" "")))
		systems)
	;what the Net service has heard of, by their beacons. One that is
	;heard and is not a machine above has not joined, and one on another
	;build, or that speaks another version of the link, is why
	(defq heard (ifn (net-peers-rpc) (list)) known (map (const hex-encode) systems)
		odd (filter (lambda ((sys ip port linked build link))
			(or (eql build "differs") (notany (# (eql link %0)) '("same" "unknown"))
				(not (find (to-upper sys) known)))) heard))
	(when (nempty? odd)
		(print "Heard, and not right:")
		(each (lambda ((sys ip port linked build link))
			(defq other (notany (# (eql link %0)) '("same" "unknown")))
			(print "    " (slice (to-upper sys) 0 8) "  " ip
				(if (find (to-upper sys) known) "" ", has not joined")
				(cond
					((not other) "")
					((eql link "0") ", is of before the link had versions")
					((cat ", speaks version " link " of the link")))
				(if (eql build "differs") ", is on a different build" "")
				(if (or other (eql build "differs")) ". Update, and make install, on both." "")))
			odd))
	;what to look at when it is not as hoped
	(when (and is_joined (= (length systems) 1) (empty? odd))
		(print)
		(print "No other machine is seen. If one should be:")
		(print "    Has it joined too ? mesh there, to see.")
		(print "    Has it the same key ? key on each says its fingerprint.")
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
				(print "key -x own says it, to take into the keyring of each other machine, key -i, and use, key -u.")
				(print "Each sees it and joins.")))
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
