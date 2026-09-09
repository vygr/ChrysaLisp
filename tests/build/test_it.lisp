;run with ./run_tui.sh -f -s tests/build/test_it.lisp
(import "lib/task/pipe.inc")
(while (< (length (lisp-nodes)) 8) (task-sleep 100000))
(defq out (memory-stream))
(pipe-run "make it | time -s" (# (write-blk out %0)))
(stream-seek out 0 0)
(lines! (# (print %0) (stream-flush (io-stream 'stdout)) (task-sleep 10) :nil) out)
((ffi "service/gui/lisp_deinit"))
