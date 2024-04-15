(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: test [options]

	options:
		-h --help: this help info.

	Simple timing test framework.")
))

(defun f0 (s &optional c)
	(defq c (if c (code c) (ascii-code " ")) i (length s))
	(while (and (/= (setq i (dec i)) -1) (eql (code s 1 i) c)))
	(slice s 0 (inc i)))

(defun f1 (s &optional c)
	(setd c " ")
	(while (eql (last s) c) (setq s (slice s 0 -2))) s)

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
		(defq s "   tttttttttdtdtdtdtdtdtdtdtdtdtdtdtdtdtdtdtdtdtdtdtdtd!" c 10000000)
		(time-it "f0" c (f0 s " "))
		(time-it "f1" c (f1 s " "))
		(time-it "f0" c (f0 s " "))
		(time-it "f1" c (f1 s " "))
		))
