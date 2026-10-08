;the other end of tests/solo/test_tcp_link.lisp. A node started by that
;test, alone, a network of one. It links to the test's network over TCP,
;by the address of this machine itself, and stays till it is told to go,
;or a minute, so one that is forgotten does not stay for ever
(import "lib/task/pipe.inc")
(pipe-run "link 127.0.0.1:34581" (const prin))
(task-sleep (task-timeout 60))
(pii-exit)
