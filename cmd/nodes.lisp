(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: nodes [options]

    options:
        -h --help: this help info.
        -a --add num: start num more nodes on this machine,
            linked to this node and to each other.
        -i --info: this node's process id, and the processors
            and memory of its machine.

    List the nodes known to this node.")
(("-a" "--add") ,(opt-num 'opt_a))
(("-i" "--info") ,(opt-flag 'opt_i))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_a 0 opt_i :nil args (options stdio usage)))
		(cond
			(opt_i
				(print "pid: " (pii-pid))
				(print "cpus: " (pii-cpus))
				(print "memory: " (/ (pii-memory) 1048576) "MB")
				(print "nodes: " (length (lisp-nodes))))
			((> opt_a 0)
				(each (# (print (if (< %0 0) "failed to start a node" (cat "started pid: " (str %0)))))
					(node-spawn opt_a)))
			(:t (each (# (print (hex-encode %0))) (mail-nodes))))))
