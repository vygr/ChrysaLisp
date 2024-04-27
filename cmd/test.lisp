(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: test [options]

	options:
		-h --help: this help info.

	Simple timing test framework.")
))

(defun f0 (_)
	(defq out (list))
	(walk-list _
		;element func
		(lambda (_) (unless (list? _) (push out _)))
		;enter list func
		(lambda (_) :nil)
		;exit list func
		progn) out)

(defun f1 (lst)
	(defq out (list) stack (list lst 0))
	(while (defq idx (pop stack) lst (pop stack))
		(some! idx -1 :nil (# (cond
			((list? %0) (push stack lst (inc _) %0 0))
			(:t (push out %0) :nil))) (list lst))) out)

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
		(defq s '(1 2 3(4 5)((6 7)(8 9))) c 1000000)
		(time-it "f0" c (f0 s))
		(time-it "f1" c (f1 s))
		(time-it "f0" c (f0 s))
		(time-it "f1" c (f1 s))
		(time-it "f0" c (f0 s))
		(time-it "f1" c (f1 s))
		(time-it "f0" c (f0 s))
		(time-it "f1" c (f1 s))
		))
