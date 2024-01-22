(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: test [options]

	options:
		-h --help: this help info.
	
	Simple timing test framework.")
))

(defun f1 (lst)
	(map-rev (const identity) lst))

(defun f2 (lst)
	(reduce! -1 0 (const push) (list lst) (cap (length lst) (list))))

(defmacro time-it (name cnt &rest _)
	`(progn
		(print ,name)
		(stream-flush (io-stream 'stdout))
		(task-sleep 10)
		(defq then (pii-time))
		(times ,cnt ~_)
		(print (time-in-seconds (- (pii-time) then)))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq l (range 0 100000))
		(time-it "f1" 1000 (f1 l))
		(time-it "f2" 1000 (f2 l))
		(time-it "f1" 1000 (f1 l))
		(time-it "f2" 1000 (f2 l))
		))
