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

(defq st (pii-fstat "README.md"))
(assert-true "pii-fstat" (or (nil? st) (seq? st)))
(defq dl (pii-dirlist "."))
(assert-true "pii-dirlist" (str? dl))

; --- Mail & Nodes ---
(assert-true "mail-mbox" (find :netid (type-of (mail-mbox))))
(assert-true "task-nodeid" (str? (task-nodeid)))
(assert-true "task-mbox" (str? (task-mbox)))
(assert-true "mail-nodes" (seq? (mail-nodes)))
(defq mbox_test (mail-mbox))
(assert-true "mail-validate" (mail-validate mbox_test))
