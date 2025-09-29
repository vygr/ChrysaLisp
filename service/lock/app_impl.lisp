(import "./app.inc")

(defun main ()
	(defq lock_service (mail-declare (task-mbox) "*Lock" "Lock Service 0.1")
		lock_map (Lmap))
	(while :t
		(let* ((msg (mail-read (task-mbox)))
				(reply_id (getf msg +lock_rpc_reply_id))
				(key (slice msg +lock_rpc_size -1)))
			(case (getf msg +lock_rpc_type)
				(+lock_type_claim
					;claim lock on this key, or just add to que
					(if (defq lock_que (. lock_map :find key))
						(push lock_que reply_id)
						(. lock_map :insert key (setq lock_que (list reply_id))))
					(if (= (length lock_que) 1) (mail-send reply_id "")))
				(+lock_type_release
					;release lock on this key
					(defq lock_que (slice (. lock_map :find key) 1 -1))
					(if (empty? lock_que)
						(. lock_map :erase key)
						(progn
							(. lock_map :insert key lock_que)
							(mail-send (first lock_que) "")))
					(mail-send reply_id "")))))
	(mail-forget lock_service))
