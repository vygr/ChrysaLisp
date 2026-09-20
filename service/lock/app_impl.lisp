(import "./app.inc")

(defun path-segments (key)
	(filter (# (nql %0 "")) (split key "/")))

(defun make-node ()
	(pmap :holders (list) :sub_count 0))

(defun trie-conflict? (root segs)
	(defq curr root conflict :nil
		broken (some (#
			(when (nempty? (pfind curr :holders)) (setq conflict :t))
			(unless (setq curr (pfind curr %0)) :t)) segs))
	(cond
		(conflict :t)
		((not broken)
			(or (nempty? (pfind curr :holders))
				(> (pfind curr :sub_count) 0)))))

(defun trie-lock! (root segs reply_id)
	(defq nodes (reduce (# (push %0 (ifn (pfind (last %0) %1)
			(progn
				(defq child (make-node))
				(pinsert (last %0) %1 child)
				child))))
		segs (list root)))
	(push (pfind (last nodes) :holders) reply_id)
	; increment sub_count on all ancestors (excluding target node)
	(pop nodes)
	(each (# (pinsert %0 :sub_count (inc (pfind %0 :sub_count)))) nodes))

(defun trie-unlock! (root segs)
	(defq nodes (reduce (#
			(ifn (defq next (pfind (last %0) %1)) %0
				(push %0 next))) segs (list root))
		curr (last nodes) holders (pfind curr :holders))
	(when (nempty? holders)
		(pop holders)
		; decrement sub_count on ancestors
		(pop nodes)
		(each (# (pinsert %0 :sub_count (dec (pfind %0 :sub_count)))) nodes)))

(defun main ()
	(defq lock_service (mail-declare (task-mbox) "@Lock" "Lock Service 0.3")
		root (make-node) active_locks (list) pending_que (list))
	(while :t
		(let* ((msg (mail-read (task-mbox)))
				(reply_id (getf msg +lock_rpc_reply_id))
				(key (slice msg +lock_rpc_size -1))
				(segs (path-segments key)))
			(case (getf msg +lock_rpc_type)
				(+lock_type_claim
					(if (trie-conflict? root segs)
						(push pending_que key segs reply_id)
						(trie-lock! root segs reply_id)
						(push active_locks key segs reply_id)
						(mail-send reply_id "")))
				(+lock_type_release
					; find and remove from active_locks matching by key
					(defq i 0 len (length active_locks))
					(while (< i len)
						(ifn (eql (elem-get active_locks i) key)
							(setq i (+ i 3))
							(trie-unlock! root (elem-get active_locks (+ i 1)))
							(setq active_locks (erase active_locks i (+ i 3)) i len)))
					(mail-send reply_id "")
					; drain pending requests that are now conflict-free
					(when (nempty? pending_que)
						(defq old_pending pending_que pending_que (list)
							j 0 plen (length old_pending))
						(while (< j plen)
							(defq pk (elem-get old_pending j)
								psegs (elem-get old_pending (+ j 1))
								pr (elem-get old_pending (+ j 2)))
							(if (trie-conflict? root psegs)
								(push pending_que pk psegs pr)
								(trie-lock! root psegs pr)
								(push active_locks pk psegs pr)
								(mail-send pr ""))
							(setq j (+ j 3))))))))
	(mail-forget lock_service))
