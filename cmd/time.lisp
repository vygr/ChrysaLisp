(import "class/lisp.inc")
(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: time [options]
	options:
		-h --help: this help info.
	Time the duration of the stdin stream.
	Print result to stderr.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		;get start time
		(defq start (pii-time))
		;pass it all through
		(while (defq c (read-char (io-stream 'stdin)))
			(write-char (io-stream 'stdout) c))
		;get duration
		(defq duration (- (pii-time) start))
		(stream-flush (io-stream 'stdout))
		;wait for the stdout data to flow along...
		(task-sleep 100000)
		(write (io-stream 'stderr)
			(cat "Time:" (char 10) (time-in-seconds duration) " seconds" (char 10)))
		(stream-flush (io-stream 'stderr))
		;wait for the stderr data to flow along...
		(task-sleep 100000)
		;and now exit and send EOF along the pipe.
		))
