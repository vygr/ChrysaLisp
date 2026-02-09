;;;;;;;;;;;;;;;;;;;;;;
; tests/test_system.lisp
;;;;;;;;;;;;;;;;;;;;;;
(report-header "System & Tasking Smoke Tests")

; --- System Functions ---
(assert-true "pii-time" (num? (pii-time)))
(assert-true "kernel-stats" (array? (kernel-stats)))
(assert-true "load-path" (str? (load-path)))
(assert-true "task-flags" (num? (task-flags)))
(assert-true "task-count" (num? (task-count 0)))

; --- Mail & Nodes ---
(assert-true "mail-mbox" (find :netid (type-of (mail-mbox))))
(assert-true "task-nodeid" (str? (task-nodeid)))
