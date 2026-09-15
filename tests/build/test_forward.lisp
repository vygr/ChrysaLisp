;run with ./run_tui.sh -f -s tests/build/test_forward.lisp
(import "lib/task/pipe.inc")

(print)
(print "Scanning for forward references in apps/demos/opcodes/app.lisp...")
(print)

(defq out (memory-stream))
(pipe-run "forward apps/demos/opcodes/app.lisp" (# (write-blk out %0)))
(stream-seek out 0 0)

(defq count 0)
(lines! (lambda (line)
	(when (nempty? line)
		(print line)
		(stream-flush (io-stream 'stdout))
		(++ count)) :nil) out)

(if (= count 0)
	(print "No forward references found - SUCCESS!")
	(print "Found " count " forward reference(s)."))

(pii-exit)
