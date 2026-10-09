(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: nodes [options]

    options:
        -h --help: this help info.
        -a --add num: start num more nodes on this machine,
            linked to this node and to each other.
        -g --gui num: start num more nodes, each a GUI desktop.
        -t --tui num: start num more nodes on the TUI host,
            which is the lighter, it has no GUI.
        -s --shape name: add a network of that shape, hung from this
            node: full, ring, star, tree, mesh or cube. It is given a
            name to stop it by.
        -n --num cnt: how many nodes the shape has, with this one, or
            how wide a mesh or a cube is. Sized to the machine if not
            given.
        -o --own: with -s, the shape is a system of its own. It has a
            system id that is not this machine's, this node is not
            one of it, and one link from here is the way in.
        -x --stop name: stop a network that was added with -s, all of
            its nodes at once, or all for every one there is.
        -i --info: this node's process id, and the processors
            and memory of its machine.

    List the nodes known to this node, and the networks added by name.

        nodes -s ring -n 8    ; a ring of 8, this node one of them
        nodes -s cube -n 2 -o ; a cube of 8, a system of its own
        nodes                 ; the nodes, and the networks
        nodes -x k3f9         ; stop that ring")
(("-a" "--add") ,(opt-num 'opt_a))
(("-g" "--gui") ,(opt-num 'opt_g))
(("-t" "--tui") ,(opt-num 'opt_t))
(("-s" "--shape") ,(opt-str 'opt_s))
(("-n" "--num") ,(opt-num 'opt_n))
(("-o" "--own") ,(opt-flag 'opt_o))
(("-x" "--stop") ,(opt-str 'opt_x))
(("-i" "--info") ,(opt-flag 'opt_i))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_a 0 opt_g 0 opt_t 0 opt_i :nil opt_s :nil opt_n 0 opt_x :nil opt_o :nil
				args (options stdio usage)))
		(cond
			(opt_i
				(print "pid: " (pii-pid))
				(print "cpus: " (pii-cpus))
				(print "memory: " (/ (pii-memory) 1048576) "MB")
				(print "nodes: " (length (lisp-nodes))))
			(opt_x
				(defq nets (node-nets) ids (if (eql opt_x "all") (map (const first) nets) (list opt_x)))
				(if (empty? ids) (print "No network was added by name."))
				(each (lambda (id)
					(print (if (defq told (node-stop id))
						(cat "stopped " id ", " (str told) " nodes")
						(cat "no network is named " id))))
					ids))
			(opt_s
				(cond
					((not (find opt_s '("full" "ring" "star" "tree" "mesh" "cube")))
						(print "A shape is full, ring, star, tree, mesh or cube, not " opt_s))
					(:t ;a name of four, as a link has two threes
						(defq chars "0123456789abcdefghijklmnopqrstuvwxyz"
							id (apply (const cat) (map (lambda (&) (elem-get chars (random 36))) (range 0 4)))
							;a system id of its own is its name and twelve more
							sid (if opt_o (apply (const cat) (cat (list id)
								(map (lambda (&) (elem-get chars (random 36))) (range 0 12)))))
							pids (node-net (sym (cat ":" opt_s)) opt_n 0 :nil :nil id sid)
							bad (length (filter (# (< %0 0)) pids)))
						(print "started " id ", a " opt_s " of "
							(if sid (cat (str (length pids)) ", a system of its own, linked to this node")
								(cat (str (inc (length pids))) " with this node"))
							(if (> bad 0) (cat ", " (str bad) " could not be started") "")))))
			((> (+ opt_a opt_g opt_t) 0)
				(each (# (print (if (< %0 0) "failed to start a node" (cat "started pid: " (str %0)))))
					(cat (node-spawn opt_a)
						(node-spawn opt_g :gui "service/gui/app.lisp")
						(node-spawn opt_t :tui))))
			(:t (each (# (print (hex-encode %0))) (lisp-nodes))
				(each (lambda ((id shape total pids & & sid))
					(print "network " id ", a " shape " of " total ", " (length pids) " nodes started"
						(if sid ", a system of its own" "")))
					(node-nets))))))
