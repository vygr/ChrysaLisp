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
		(defq stdin (io-stream 'stdin) stdout (io-stream 'stdout)
			stderr (io-stream 'stderr))
		(while (defq c (read-blk stdin 1024)) (write-blk stdout c))
		;get duration
		(defq duration (- (pii-time) start))
		(stream-flush stdout)
		;wait for the stdout data to flow along...
		(task-sleep 100000)
		(write-line stderr
			(cat "Time:" (char 10) (time-in-seconds duration) " seconds"))
		(stream-flush stderr)
		;wait for the stderr data to flow along...
		(task-sleep 100000)
		;and now exit and send EOF along the pipe.
		))
