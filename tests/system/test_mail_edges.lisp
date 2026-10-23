(report-header "Mail Edges: empty messages, order, polling, timeouts")

; --- mailboxes ---
(defq me_a (mail-mbox) me_b (mail-mbox))
(assert-eq "mailbox id length" 24 (length me_a))
(assert-true "new mailboxes differ" (nql me_a me_b))
(assert-true "task mailbox is constant" (eql (task-mbox) (task-mbox)))
(assert-eq "node id length" 16 (length (task-nodeid (task-mbox))))
(assert-true "task mailbox is valid" (mail-validate (task-mbox)))

; --- messages arrive whole and in order ---
(mail-send me_a "x")
(assert-eq "send and read" "x" (mail-read me_a))
(mail-send me_a "")
(assert-eq "empty message" "" (mail-read me_a))
(mail-send me_a "a")
(mail-send me_a "b")
(assert-eq "order first" "a" (mail-read me_a))
(assert-eq "order second" "b" (mail-read me_a))

; --- poll and select give the index of a mailbox with mail ---
(assert-eq "poll nothing" :nil (mail-poll (list me_a me_b)))
(mail-send me_b "x")
(assert-eq "poll second" 1 (mail-poll (list me_a me_b)))
(assert-eq "poll does not read" 1 (mail-poll (list me_a me_b)))
(assert-eq "select second" 1 (mail-select (list me_a me_b)))
(assert-eq "read after select" "x" (mail-read me_b))
(assert-eq "poll after read" :nil (mail-poll (list me_a me_b)))

; --- timeouts ---
(assert-eq "read timeout on an empty mailbox" :nil (mail-read-timeout me_a 1000))
;a timed read that gets its mail cancels its timer, so a run of them, each
;with a long timeout, leaves nothing behind to slow the next
(defq me_cnt 0)
(times 2000 (mail-send me_a "t") (if (eql (mail-read-timeout me_a 60000000) "t") (++ me_cnt)))
(assert-eq "a run of timed reads that all get mail" 2000 me_cnt)
(assert-eq "and the mailbox is empty after" :nil (mail-read-timeout me_a 1000))
(mail-timeout me_a 1000 7)
(assert-true "a timer wakes a read" (mail-read me_a))
;a mailbox can have more than one timeout, each set adds one, and each
;comes with the id it was set with, the soonest first whatever order they
;were set in. A time of 0 takes them all away, and those of no other mailbox
(defun me-fired (mbox wait)
	(task-sleep wait)
	(defq out (list))
	(while (mail-poll (list mbox)) (push out (getf (mail-read mbox) +mail_timeout_id)))
	out)
(defq me_t1 (mail-mbox) me_t2 (mail-mbox))
(mail-timeout me_t1 3000 3) (mail-timeout me_t1 1000 1) (mail-timeout me_t1 2000 2)
(assert-list-eq "three timeouts on one mailbox all come, the soonest first" '(1 2 3) (me-fired me_t1 (task-timeout 1)))
(mail-timeout me_t1 (task-timeout 1) 1) (mail-timeout me_t2 (task-timeout 1) 9)
(mail-timeout me_t1 (* 2 (task-timeout 1)) 2) (mail-timeout me_t2 (* 2 (task-timeout 1)) 8)
(mail-timeout me_t1 0 0)
(assert-list-eq "a cancel takes away every one the mailbox has" '() (me-fired me_t1 (* 3 (task-timeout 1))))
(assert-list-eq "and leaves those of another mailbox, set between them" '(9 8) (me-fired me_t2 0))
(mail-timeout me_t1 0 0)
(mail-timeout me_t1 1000 5)
(assert-list-eq "a cancel of none is nothing, and one set after it comes" '(5) (me-fired me_t1 (task-timeout 1)))
(defq me_t0 (pii-time))
(task-sleep 2000)
(assert-true "sleep is at least as long as asked" (>= (- (pii-time) me_t0) 2000))
(assert-true "sleep 0 gives way and returns" (task-sleep 0))

; --- services ---
(assert-list-eq "enquire a service that is not there" '() (mail-enquire "MailEdgeNoSuchService"))
(defq me_key (mail-declare me_a "MailEdgeService" "info"))
(assert-eq "declared service is found" 1 (length (mail-enquire "MailEdgeService")))
(mail-forget me_key)
(assert-eq "forgotten service is gone" 0 (length (mail-enquire "MailEdgeService")))

;a message is a string, not a list
(assert-error "send a list" (mail-send me_a (list 1 2)))

;one said as a name on this node whose task is still there, (mail-here): a
;task that ends without (mail-forget) is still said, its mailbox is gone,
;and it is no guard of one to be the only one
(defq me_said (mail-declare (task-mbox) "ZzHere" "a test"))
(assert-eq "one said here that is here is had" (hex-encode (task-mbox)) (hex-encode (mail-here "ZzHere")))
(assert-eq "and is the service to use" (hex-encode (task-mbox)) (hex-encode (mail-service "ZzHere")))
(mail-forget me_said)
(save "(mail-declare (task-mbox) \qZzGone\q \qa test\q)" "tests/scratch/zz_gone.lisp")
(open-child "tests/scratch/zz_gone.lisp" +kn_call_pin)
(while (empty? (mail-enquire "ZzGone,")) (task-sleep 20000))
;and then has ended, its mailbox no longer here
(while (mail-here "ZzGone") (task-sleep 20000))
(assert-list-eq "one that ended without (mail-forget) is still said, and is not here, nor a service to use" '(:t :nil :nil)
	(list (nempty? (mail-enquire "ZzGone,")) (mail-here "ZzGone") (mail-service "ZzGone")))
(pii-remove "tests/scratch/zz_gone.lisp")
(undef (env) 'me_said)
