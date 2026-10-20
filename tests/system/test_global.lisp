(report-header "System: global, a task on every node, and a node that can not start it")

(import "lib/task/global.inc")

;a node need not be one that runs what it is sent, docs/ai_digest/alien_nodes.md.
;Such a node answers an id of 0, as a node here does of a file it has not
;got, and that is what stands for it: the task asked for is not there
(defq gl_mbox (mail-mbox) gl_made (list) gl_gone (list)
	gl_task "tests/system/no_such_child_zyz.lisp")

(defun gl-create (key now)
	(push gl_made (cat key))
	(open-task gl_task key +kn_call_pin 0 gl_mbox)
	(def (defq val (env 1)) :timestamp now)
	val)

(defun gl-destroy (key val)
	(push gl_gone (cat key)))

(defun gl-size (global)
	(defq num 0)
	(. global :each (lambda (key val) (++ num)))
	num)

(defun gl-replies (global num)
	;the answers to that many asked, how many of them were of a task started
	(defq started 0)
	(times num
		(when (defq msg (mail-read-timeout gl_mbox (task-timeout 10)))
			(if (. global :reply msg) (++ started))))
	started)

(defq gl (Global gl-create gl-destroy))
(. gl :refresh)
(defq gl_nodes (length gl_made))
(assert-true "a task is asked of every node there is" (and (> gl_nodes 0) (= gl_nodes (length (lisp-nodes)))))
(assert-eq "and each has an entry" gl_nodes (gl-size gl))
(assert-eq "none of them could start it" 0 (gl-replies gl gl_nodes))
(assert-eq "the entry of each that could not is destroyed" (str (sort (map (const hex-encode) gl_made))) (str (sort (map (const hex-encode) gl_gone))))
(assert-eq "none is left" 0 (gl-size gl))
(assert-eq "each is known to have refused" gl_nodes (length (. gl :refused)))
(setq gl_made (list))
(. gl :refresh)
(assert-eq "and none of them is asked again" 0 (length gl_made))
(. gl :refresh 0)
(assert-eq "nor when the time to wait has gone" 0 (length gl_made))
(. gl :close)

;a task that can be started: its entry is given back, to keep the id in
(setq gl_made (list) gl_gone (list) gl_task "(task-sleep 1)")
(defq gl (Global gl-create gl-destroy))
(. gl :refresh)
(assert-eq "one that every node can start is started on each" gl_nodes (gl-replies gl (length gl_made)))
(assert-eq "none refused" 0 (length (. gl :refused)))
(assert-eq "and every entry is kept" gl_nodes (gl-size gl))
(. gl :close)
(assert-eq "till it is closed" gl_nodes (length gl_gone))

;a farm, children on any node, leaves a node that could not start one too,
;while that farm lasts, and asks for the child of another at once. Chris:
;"they would ignore that node from then on. It's only for this specific
;instance, so not permenant."
(import "lib/task/farm.inc")

(defq fm_mbox (mail-mbox) fm_asked (list) fm_gone 0 fm_task "tests/system/no_such_child_zyz.lisp" fm_at 0)

(defun fm-create (key val nodes)
	;each node in turn, of those the farm gives: not one it has left
	(defq node (elem-get nodes (% (++ fm_at) (length nodes))))
	(push fm_asked (cat node))
	(open-task fm_task node +kn_call_pin key fm_mbox))

(defun fm-destroy (key val)
	(++ fm_gone))

(defun fm-replies (farm want)
	;the answers, till no more come: how many were of a child started. While
	;fewer than want have come a quiet second is a busy machine, not the end
	(defq started 0 got 0)
	(while (defq msg (mail-read-timeout fm_mbox (task-timeout (if (< got want) 10 1))))
		(++ got)
		(if (. farm :reply msg) (++ started)))
	started)

(defq fm (Farm fm-create fm-destroy 1))
(assert-eq "a farm of one child asks one node for it" 1 (length fm_asked))
(assert-eq "no node can start a child that is not there" 0 (fm-replies fm (length (lisp-nodes))))
(assert-eq "every node was asked, the once, each as the one before said no"
	(str (sort (map (const hex-encode) (lisp-nodes)))) (str (sort (map (const hex-encode) fm_asked))))
(assert-eq "and each is known to have refused" (length (lisp-nodes)) (length (. fm :refused)))
(assert-eq "the farm has its one child still, asked for and not there" 1 (. fm :size))
(defq fm_were (length fm_asked))
(. fm :refresh)
(assert-eq "it is not asked for again till its time is up" fm_were (length fm_asked))
(. fm :close)

;one that can be started: its entry is given back, and no node is left
(setq fm_asked (list) fm_task "(task-sleep 1)")
(defq fm (Farm fm-create fm-destroy 3))
(assert-eq "three children that can be started are" 3 (fm-replies fm 3))
(assert-eq "none refused" 0 (length (. fm :refused)))
(. fm :close)
