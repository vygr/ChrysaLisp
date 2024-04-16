(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: test [options]

	options:
		-h --help: this help info.

	Simple timing test framework.")
))

(defun f0 (s &optional cls)
	(defq i :nil out (list) cls (opt cls " "))
	(each (# (if i
		(when (bfind %0 cls) (push out (slice s i _)) (setq i :nil))
		(unless (bfind %0 cls) (setq i _)))) s)
	(if i (push out (slice s i -1)) out))

(defun f1 (s &optional cls)
	(defq i 0 out (list) l (length s) cls (opt cls " "))
	(while (< i l)
		(if (/= (defq j (bskip cls s i)) i) (setq i j))
		(if (/= (defq j (bskipn cls s i)) i) (push out (slice s i (setq i j)))))
	out)

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
		(defq s "ttt  ttt  tttd  tdtdt  dtd  tdt  dtdt  dtdtd tdt dtd" c 1000000)
		(time-it "f0" c (f0 s))
		(time-it "f1" c (f1 s))
		(time-it "f0" c (f0 s))
		(time-it "f1" c (f1 s))
		))
