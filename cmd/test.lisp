(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: test [options]

	options:
		-h --help: this help info.

	Simple timing test framework.")
))

(defun f0 (s &optional c)
	; (trim-start str [str]) -> str
	(defq c (if c (code c) (ascii-code " ")) i -1)
	(while (and (/= (setq i (inc i)) (length s)) (eql (code s 1 i) c)))
	(slice s i -1))

(defun f1 (s &optional c)
	(if (= (defq i (bskip (setd c " ") s 0)) 0) s (slice s i -1)))

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
		(defq s "t!" c 10000000)
		(time-it "f0" c (f0 s (ascii-char 13)))
		(time-it "f1" c (f1 s (ascii-char 13)))
		(time-it "f0" c (f0 s (ascii-char 13)))
		(time-it "f1" c (f1 s (ascii-char 13)))
		))
