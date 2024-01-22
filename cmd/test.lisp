(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: test [options]

	options:
		-h --help: this help info.
	
	Simple timing test framework.")
))

(defun f0 (_f _b)
	(defq _l (list))
	(each! 0 -1 (lambda (_p)
		(if (apply _f (list _p)) (push _l _p))) (list _b)) _l)

(defun f1 (_f _b)
	(defq _l (slice 0 0 _b))
	(each! 0 -1 (lambda (_p)
		(if (apply _f (list _p)) (push _l _p))) (list _b)) _l)

(defun f2 (_f _b)
	(reduce! 0 -1 (lambda (_l _p)
		(if (apply _f (list _p)) (push _l _p) _l)) (list _b) (slice 0 0 _b)))

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
		(defq l (range 0 20000) c 1000)
		(time-it "f0" c (f0 odd? l))
		(time-it "f1" c (f1 odd? l))
		(time-it "f2" c (f2 odd? l))
		(time-it "f0" c (f0 odd? l))
		(time-it "f1" c (f1 odd? l))
		(time-it "f2" c (f2 odd? l))
		))
