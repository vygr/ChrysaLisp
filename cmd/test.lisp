(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: test [options]

	options:
		-h --help: this help info.

	Simple timing test framework.")
))

(defun f0 (&rest seqs)
	; (zip seq ...) -> seq
	(if (= (length (defq out (map! 0 -1 (const cat) (map (const partition) seqs)))) 0)
		(slice (first seqs) 0 0) (apply (const cat) out)))

(defun f1 (&rest seqs)
	; (zip seq ...) -> seq
	(apply (const cat)
		(map! 0 -1 (const cat) (map (const partition) seqs)
			(list (slice (first seqs) 0 0)))))

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
		(defq l (range 0 20000) c 10000)
		(time-it "f0" c (f0 l l l))
		(time-it "f1" c (f1 l l l))
		(time-it "f0" c (f0 l l l))
		(time-it "f1" c (f1 l l l))
		(time-it "f0" c (f0 l l l))
		(time-it "f1" c (f1 l l l))
		(time-it "f0" c (f0 l l l))
		(time-it "f1" c (f1 l l l))
		))
