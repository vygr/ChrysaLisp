#!/usr/bin/env lsp

;; LispScript Concurrency Tests (spawn/send/receive)
;; TDD approach - tests for Actor/CSP-style concurrency

(import "lib/test/unittest.inc")
(import "lib/lisp/concurrency.inc")

(deftest-suite "LispScript Concurrency Tests")

; Test 1: Create mailbox
(deftest "Create Mailbox"
	(defq mbox (create-mailbox))
	(assert-not-nil mbox))

; Test 2: Send and receive message
(deftest "Send And Receive Message"
	(defq mbox (create-mailbox))

	; Send a message
	(send-message mbox "Hello")

	; Receive it
	(defq msg (receive-message mbox))
	(assert-eq "Hello" msg))

; Test 3: Multiple messages preserve order
(deftest "Multiple Messages In Order"
	(defq mbox (create-mailbox))

	; Send 3 messages
	(send-message mbox "First")
	(send-message mbox "Second")
	(send-message mbox "Third")

	; Receive in order
	(assert-eq "First" (receive-message mbox))
	(assert-eq "Second" (receive-message mbox))
	(assert-eq "Third" (receive-message mbox)))

; Test 4: Send structured data
(deftest "Send Structured Data"
	(defq mbox (create-mailbox))

	; Send an environment
	(defq data (env))
	(set-insert data :name "Alice")
	(set-insert data :age 30)

	(send-message mbox data)

	; Receive and verify
	(defq received (receive-message mbox))
	(assert-eq "Alice" (get received :name))
	(assert-eq 30 (get received :age)))

; Test 5: Spawn task
(deftest "Spawn Task"
	; Spawn should return a task ID or handle
	(defq task (spawn-task (lambda ()
		(print "Task running"))))

	(assert-not-nil task))

; Test 6: Spawn task that sends message back
(deftest "Spawn Task With Message"
	(defq mbox (create-mailbox))

	; Spawn task that sends to mailbox
	(spawn-task (lambda ()
		(send-message mbox "From spawned task")))

	; Give it time to run (in real impl, would be synchronous)
	; Receive message from spawned task
	(defq msg (receive-message mbox :timeout 1000))
	(assert-eq "From spawned task" msg))

; Test 7: Parent-child communication
(deftest "Parent Child Communication"
	(defq parent-mbox (create-mailbox))

	; Spawn child that sends to parent
	(spawn-task (lambda ()
		(send-message parent-mbox "Child says hi")))

	; Parent receives from child
	(defq msg (receive-message parent-mbox))
	(assert-eq "Child says hi" msg))

; Test 8: Multiple concurrent tasks
(deftest "Multiple Concurrent Tasks"
	(defq mbox (create-mailbox))

	; Spawn 3 tasks
	(spawn-task (lambda () (send-message mbox "Task 1")))
	(spawn-task (lambda () (send-message mbox "Task 2")))
	(spawn-task (lambda () (send-message mbox "Task 3")))

	; Receive all 3 messages (order may vary)
	(defq messages (list))
	(push messages (receive-message mbox))
	(push messages (receive-message mbox))
	(push messages (receive-message mbox))

	; All 3 should be present
	(assert-eq 3 (length messages)))

; Test 9: Task can create and use own mailbox
(deftest "Task Own Mailbox"
	(defq parent-mbox (create-mailbox))

	(spawn-task (lambda ()
		; Create own mailbox
		(defq my-mbox (create-mailbox))

		; Send to self
		(send-message my-mbox "Self message")

		; Receive from self
		(defq msg (receive-message my-mbox))

		; Send result to parent
		(send-message parent-mbox (cat "Got: " msg))))

	; Parent receives final result
	(defq result (receive-message parent-mbox))
	(assert-eq "Got: Self message" result))

; Test 10: Non-blocking receive with timeout
(deftest "Receive With Timeout"
	(defq mbox (create-mailbox))

	; Try to receive from empty mailbox with timeout
	(defq msg (receive-message mbox :timeout 100))

	; Should return nil on timeout
	(assert-eq nil msg))

; Test 11: Send complex nested data
(deftest "Send Nested Data"
	(defq mbox (create-mailbox))

	; Create nested structure
	(defq data (env))
	(set-insert data :user (env))
	(set-insert (get data :user) :name "Bob")
	(set-insert (get data :user) :emails (list "bob@test.com" "bob@work.com"))

	(send-message mbox data)

	(defq received (receive-message mbox))
	(assert-eq "Bob" (get (get received :user) :name))
	(assert-eq 2 (length (get (get received :user) :emails))))

; Test 12: Task spawning from script
(deftest "Spawn From LispScript"
	(defq html "
		<script>
			(defq result-mbox (create-mailbox))

			; Spawn background task
			(spawn-task (lambda ()
				(send-message result-mbox \"Background work done\")))

			; Wait for result
			(defq result (receive-message result-mbox))
		</script>")

	; Execute script
	(defq doc (parse-html html))
	(defq executor (execute-document-scripts doc))
	(defq ctx (. executor :get-context))

	; Verify result
	(assert-eq "Background work done" (. ctx :get-global "result")))

; Test 13: Producer-consumer pattern
(deftest "Producer Consumer Pattern"
	(defq queue (create-mailbox))

	; Producer
	(spawn-task (lambda ()
		(send-message queue "Item 1")
		(send-message queue "Item 2")
		(send-message queue "Item 3")
		(send-message queue :done)))

	; Consumer
	(defq items (list))
	(defq done :nil)

	(while (eql done :nil)
		(defq msg (receive-message queue))
		(if (eql msg :done)
			(setq done :t)
			(push items msg)))

	; Should have received all items
	(assert-eq 3 (length items))
	(assert-eq "Item 1" (elem 0 items)))

; Test 14: Request-response pattern
(deftest "Request Response Pattern"
	(defq request-mbox (create-mailbox))
	(defq response-mbox (create-mailbox))

	; Server task
	(spawn-task (lambda ()
		(defq req (receive-message request-mbox))
		; Process request
		(defq result (cat "Processed: " req))
		; Send response
		(send-message response-mbox result)))

	; Client sends request
	(send-message request-mbox "Do work")

	; Client waits for response
	(defq response (receive-message response-mbox))
	(assert-eq "Processed: Do work" response))

; Test 15: Pipeline pattern (3 stages)
(deftest "Pipeline Pattern"
	(defq stage1-out (create-mailbox))
	(defq stage2-out (create-mailbox))
	(defq stage3-out (create-mailbox))

	; Stage 1: Add prefix
	(spawn-task (lambda ()
		(send-message stage1-out "raw")))

	; Stage 2: Add middle
	(spawn-task (lambda ()
		(defq msg (receive-message stage1-out))
		(send-message stage2-out (cat msg "-processed"))))

	; Stage 3: Add suffix
	(spawn-task (lambda ()
		(defq msg (receive-message stage2-out))
		(send-message stage3-out (cat msg "-done"))))

	; Get final result
	(defq final (receive-message stage3-out))
	(assert-eq "raw-processed-done" final))

; Test 16: Broadcast to multiple receivers
(deftest "Broadcast Pattern"
	(defq mbox1 (create-mailbox))
	(defq mbox2 (create-mailbox))
	(defq mbox3 (create-mailbox))

	; Send same message to all
	(defq broadcast-msg "Broadcast")
	(send-message mbox1 broadcast-msg)
	(send-message mbox2 broadcast-msg)
	(send-message mbox3 broadcast-msg)

	; All receive the same message
	(assert-eq "Broadcast" (receive-message mbox1))
	(assert-eq "Broadcast" (receive-message mbox2))
	(assert-eq "Broadcast" (receive-message mbox3)))

; Test 17: Task can send to multiple mailboxes
(deftest "Send To Multiple Mailboxes"
	(defq mbox1 (create-mailbox))
	(defq mbox2 (create-mailbox))

	(spawn-task (lambda ()
		(send-message mbox1 "For mailbox 1")
		(send-message mbox2 "For mailbox 2")))

	(assert-eq "For mailbox 1" (receive-message mbox1))
	(assert-eq "For mailbox 2" (receive-message mbox2)))

; Test 18: Numeric messages
(deftest "Send Numeric Messages"
	(defq mbox (create-mailbox))

	(send-message mbox 42)
	(send-message mbox 3.14)
	(send-message mbox -100)

	(assert-eq 42 (receive-message mbox))
	(assert-eq 3.14 (receive-message mbox))
	(assert-eq -100 (receive-message mbox)))

; Test 19: Symbol messages
(deftest "Send Symbol Messages"
	(defq mbox (create-mailbox))

	(send-message mbox :start)
	(send-message mbox :stop)
	(send-message mbox :pause)

	(assert-eq :start (receive-message mbox))
	(assert-eq :stop (receive-message mbox))
	(assert-eq :pause (receive-message mbox)))

; Test 20: List messages
(deftest "Send List Messages"
	(defq mbox (create-mailbox))

	(defq my-list (list "a" "b" "c"))
	(send-message mbox my-list)

	(defq received (receive-message mbox))
	(assert-eq 3 (length received))
	(assert-eq "a" (elem 0 received))
	(assert-eq "c" (elem 2 received)))
