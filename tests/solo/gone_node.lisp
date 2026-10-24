;a helper of tests/solo/test_gone.lisp, run on a node that test starts.
;It is mailed where to say it is done, and the code of a task. It starts
;six nodes, stops them, and at once leaves 60 of that task to find a node.
;The six that have gone are the neighbours it knows with the least to do.
(defq msg (mail-read (task-mbox)) done (slice msg 0 +net_id_size) code (slice msg +net_id_size -1)
	before (lisp-nodes :t) pids (node-spawn 6))
;six seen that were not before, not the count of all: a node of another
;test that goes meanwhile is not waited for
(while (< (length (filter (# (not (find %0 before))) (lisp-nodes :t))) 6) (task-sleep 50000))
;this node is new and is still learning of the nodes there were, so the
;six are not known by being new to it. Every node is told the processes
;that are to go, as (node-stop) does it
(task-sleep 300000)
(each (# (open-task (str `(if (find (pii-pid) (quote ,pids)) (pii-exit))) %0 +kn_call_pin 0 (mail-mbox)))
	(lisp-nodes :t))
(while (some (const pii-alive) pids) (task-sleep 2000))
(times 60 (open-task code (task-nodeid) +kn_call_run 0 (mail-mbox)))
(mail-send done (str (length (filter (# (> %0 0)) pids))))
