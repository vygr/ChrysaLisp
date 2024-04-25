(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: test [options]

	options:
		-h --help: this help info.

	Simple timing test framework.")
))

(defun f0 (seq)
	(cond
		((> (length seq) 0)
			(defq out (cap (length seq) (list (slice seq 0 1))))
			(each! 1 -1 (# (unless (eql %0 (last (last out)))
				(push out (slice seq _ (inc _))))) (list seq))
			(apply (const cat) out))
		((rest seq))))

(defun f1 (seq)
	(cond
		((= (length seq) 0)
			(rest seq))
		((array? seq)
			(reduce! 1 -1 (# (if (eql %1 (last %0)) %0 (push %0 %1))) (list seq) (slice seq 0 1)))
		(:t ;string
			(apply (const cat) (reduce (# (if (eql %1 (last %0)) %0 (push %0 %1))) seq (list))))))

(defmacro time-it (name cnt &rest _)
	`(progn
		(print ,name)
		(stream-flush (io-stream 'stdout))
		(task-sleep 100)
		(defq then (pii-time))
		(times ,cnt ~_)
		(print (time-in-seconds (- (pii-time) then)))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq s (map identity "abbttkkddiieowkfkbsk463463775484fgohfskcvbc823765872kuyrgcbkzrckzgvbzgvfyg") c 100000)
		(time-it "f0" c (f0 s))
		(time-it "f1" c (f1 s))
		(time-it "f0" c (f0 s))
		(time-it "f1" c (f1 s))
		(defq s "abbttkkddiieowkfkbsk463463775484fgohfskcvbc823765872kuyrgcbkzrckzgvbzgvfyg" c 100000)
		(time-it "f0" c (f0 s))
		(time-it "f1" c (f1 s))
		(time-it "f0" c (f0 s))
		(time-it "f1" c (f1 s))
		))
