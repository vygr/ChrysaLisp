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
