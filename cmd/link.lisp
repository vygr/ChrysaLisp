(import "lib/options/options.inc")
(import "service/net/app.inc")

(defun opt-listen (opt_var)
	(static-qq (lambda (args arg)
		(if (and (nempty? args) (some (# (find %0 "0123456789")) (first args)) (not (starts-with "-" (first args))))
			(progn (setq ,opt_var (str-as-num (first args))) (rest args))
			(progn (setq ,opt_var 3333) args)))))

(defq usage `(
(("-h" "--help")
"Usage: link [options] [host[:port] ...]

    options:
        -h --help: this help info.
        -l --listen [port]: listen for incoming TCP network link (default: 3333).
        -a --auto: auto-discovery mode (beacon when listening, discover when client).
        -m --mesh: link to the peers of whoever is linked to, with no discovery.
        -k --key: make a key for this machine, the file mesh_key, if it has none.
        -v --verbose: verbose output.

    Start TCP network link driver/s.

    Network links:
        link -l 3333          ; Listen on port 3333 for incoming network links
        link -l 3333 -a       ; Listen on port 3333 and beacon for auto-discovery
        link -a               ; Auto-discover LAN peers and connect
        link -m 192.168.1.100 ; Connect to a peer, and to each peer it knows
        link 192.168.1.100    ; Connect to peer on default port 3333
        link 127.0.0.1:3333   ; Connect to peer on specified port
        link server.local     ; Connect to peer by DNS/mDNS hostname

    Every machine that runs both link -l 3333 -a and link -a finds the
    others and has one link to each, none is on the way between two.

    A machine with a key, the file mesh_key at the root of the tree, links
    only to machines with the same key, each end proves it has it before
    the link carries anything. link -k makes one, 64 hex digits. Put the
    same file on every machine of the network, by hand, it is never sent.
    Start the Net service again after, it reads the key as it starts.

    If no host names given on command line and -l/-a/-m not passed,
    then names are read from stdin.")
(("-l" "--listen") ,(opt-listen 'opt_l))
(("-a" "--auto") ,(opt-flag 'opt_a))
(("-m" "--mesh") ,(opt-flag 'opt_m))
(("-k" "--key") ,(opt-flag 'opt_k))
(("-v" "--verbose") ,(opt-flag 'opt_v))
))

(defun start-mesh (mesh_only)
	;the service makes a mesh, or says why not
	(unless (eql (net-discover-rpc mesh_only) 0)
		(print "link: no auto discovery or mesh. The Net service could not start it, the port may be in use, or the boot image is older than the source, make all boot.")))

(defun start-link (target verbose)
	(when verbose (print "Starting network link: " target))
	(net-link-rpc target))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_l :nil opt_a :nil opt_m :nil opt_k :nil opt_v :nil args (options stdio usage)))
		(when opt_k
			(import "lib/crypto/random.inc")
			(cond
				((pii-fstat "mesh_key") (print "This machine has a key already, mesh_key. It is left as it is."))
				(:t (save (cat (to-lower (hex-encode (random-bytes 32))) (ascii-char 10)) "mesh_key")
					(print "A key for this machine is in mesh_key. Put the same file on every machine that is to link to it."))))
		(when opt_l
			(start-link (cat ":" (str opt_l)) opt_v)
			(when opt_a
				(when opt_v (print "Advertising network link on port " opt_l " via UDP broadcast"))
				(net-beacon-rpc opt_l)))
		(when opt_m
			(when opt_v (print "Making a mesh with the peers of each link"))
			(start-mesh :t))
		(cond
			((and opt_a (not opt_l))
				(when opt_v (print "Starting LAN auto-discovery listener..."))
				(start-mesh :nil))
			((<= (length args) 1)
				(unless (or opt_l opt_m opt_k)
					;from stdin
					(lines! (# (start-link %0 opt_v) :nil) (io-stream 'stdin))))
			(:t
				;from args
				(each (# (start-link %0 opt_v)) (rest args))))))


