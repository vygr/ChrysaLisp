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
		(defq start (pii-time))
		(while (defq c (read-char (io-stream 'stdin)))
			(write-char (io-stream 'stdout) c))
		(stream-flush (io-stream 'stdout))
		(write (io-stream 'stderr)
			(cat "Time:" (char 10)
				(time-in-seconds (- (pii-time) start)) " seconds" (char 10)))
		(task-sleep 100000)
		(stream-flush (io-stream 'stderr))))
