(report-header "Host: what the node can ask of, and do with, the machine it runs on")

(assert-true "this node has a process id" (> (pii-pid) 0))
(assert-eq "and it is running" :t (pii-alive (pii-pid)))
(assert-eq "no process has id 0" :nil (pii-alive 0))
(assert-eq "nor a negative id" :nil (pii-alive -5))
(assert-true "the machine has a processor" (>= (pii-cpus) 1))
(assert-true "and at least 16MB of memory" (>= (pii-memory) 16777216))
