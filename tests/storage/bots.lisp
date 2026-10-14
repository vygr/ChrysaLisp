;the bots band thrashed: every node busy with apps, hundreds of gophers, some that never come back, and how long the apps were kept waiting

(import "service/storage/scene.inc")
(import "service/storage/gophers.inc")

(defq +files ''("storage_service.md" "storage_design_notes.md" "lisp_traps.md" "shader_language.md"
		"alien_nodes.md" "lock_service.md")
	+out 300 +late 20)

(defun start-apps (nodes)
	;two busy apps and a probe on each node, the probes' mailbox
	(defq heard (mail-mbox) tasks (list))
	(each (lambda (node)
		(times 2 (push tasks (open-remote "tests/hog.lisp" node +kn_call_pin)))
		(push tasks (defq probe (open-remote "tests/hog.lisp" node +kn_call_pin))))
		nodes)
	(each (lambda (task) (mail-send task (if (= (% (!) 3) 2) (hex-encode heard) "busy"))) tasks)
	(list tasks heard))

(defun stop-apps (apps)
	;each is told to stop, and the probes say the longest they waited, ms
	(bind '(tasks heard) apps)
	(each (# (mail-send %0 "stop")) tasks)
	(defq worst 0)
	(times (/ (length tasks) 3)
		(if (defq said (mail-read-timeout heard 5000000)) (setq worst (max worst (str-as-num said)))))
	(/ worst 1000))

(defun send-all (sc objects jobs late)
	;all of them out at once, how many came back right, how long till the
	;last of those did, ms, and how long till those that never come back
	;were all ended
	(defq block (. (storage-info (scene-svc sc "m-a")) :find :block_size)
		want (map (lambda ((path data)) (map (lambda ((name g)) (gopher-fold g data block)) jobs)) objects)
		t0 (pii-time)
		sent (map (lambda (i)
			(defq o (% i (length objects)) j (% (/ i (length objects)) (length jobs)) from (elem-get +scene_ids (% i 5)))
			(bind '(state visit) (if (< i late) (list 0 '(lambda (s data at count) (while :t) (list s (inc at))))
				(second (elem-get jobs j))))
			(list o j (< i late) (storage-walk-send (first (elem-get objects o)) state visit :nil (scene-svc sc from))))
			(range 0 +out))
		back (map (lambda ((o j bad mbox)) (list o j bad (storage-walk-answer mbox 120))) (filter (# (not (third %0))) sent))
		good_ms (/ (- (pii-time) t0) 1000)
		back (cat back (map (lambda ((o j bad mbox)) (list o j bad (storage-walk-answer mbox 120))) (filter (# (third %0)) sent))))
	(list (length (filter (lambda ((o j bad answer)) (and (not bad) answer
			(eql (str (first answer)) (str (elem-get (elem-get want o) j))))) back))
		(length (filter (lambda ((o j bad answer)) (and bad (not answer))) back))
		good_ms (/ (- (pii-time) t0) 1000)))

(defun main ()
	(defq sc (scene-begin (cat "Every node busy with apps, " (str +out) " gophers at once, then again with "
		(str +late) " of them going round for ever: the gophers are bots, and the apps are watched for how long they wait.")))
	(when (scene-check sc "five members start and meet" (scene-members sc +scene_ids))
		(defq objects (map (lambda (file)
				(defq data (load (cat "docs/ai_digest/" file)) path (cat "docs/" file))
				(storage-put path data :nil (scene-svc sc (elem-get +scene_ids (% (!) 5))))
				(list path data))
				+files)
			jobs (list (list "bytes" (gopher-bytes)) (list "lines" (gopher-lines)) (list "member" (gopher-count "member"))
				(list "checksum" (gopher-checksum)) (list "longest" (gopher-longest)) (list "head" (gopher-head 5000))))
		(scene-settle sc)
		(defq nodes (lisp-nodes :t))
		;with no apps, to have something to set the rest against
		(bind '(right & quiet_ms &ignore) (send-all sc objects jobs 0))
		(scene-say sc "no apps: " right " of " +out " right in " quiet_ms " ms")
		;every node busy
		(defq apps (start-apps nodes))
		(task-sleep 300000)
		(bind '(right & busy_ms &ignore) (send-all sc objects jobs 0))
		(scene-say sc "every node busy with apps: " right " of " +out " right in " busy_ms " ms")
		(scene-check sc "every gopher right with the nodes busy" (= right +out))
		(scene-check sc "and not much slower for it, the gophers run before the apps"
			(<= busy_ms (+ (* 4 quiet_ms) 2000)))
		(scene-say sc "the apps waited " (stop-apps apps) " ms at most, past what they asked for")
		;and a gopher that never comes back, many of them, while the nodes are busy
		(setq apps (start-apps nodes))
		(task-sleep 300000)
		(bind '(right ended late_ms ended_ms) (send-all sc objects jobs +late))
		(defq waited (stop-apps apps))
		(scene-say sc "with " +late " going round for ever: " right " of " (- +out +late) " right, "
			ended " of " +late " ended, the right ones in " late_ms " ms, all ended in " ended_ms " ms, the apps waited " waited " ms at most")
		(scene-check sc "every gopher that comes back is right" (= right (- +out +late)))
		(scene-check sc "every one that goes round for ever is ended" (= ended +late))
		;they are at the back of the queue: what holds the others is the
		;first try of each, not the second it has after
		(scene-check sc "and the others are not held behind them long" (<= late_ms (+ (* 4 busy_ms) 3000))))
	(scene-end sc))
