(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: test [options]

	options:
		-h --help: this help info.

	Simple timing test framework.

	To be stable and accurate this should be
	run on a single node !

	./run_tui.sh -n 1")
))

(defmacro bits?1 (val &rest masks)
	; (bits? val mask ...) -> :t | :nil
	(if (> (length masks) 1)
		(static-qq (/= 0 (logand ,val ,(num-intern (apply (const logior) (eval-list masks))))))
		(static-qq (/= 0 (logand ,val ~masks)))))

(defmacro bits?2 (val &rest masks)
	; (bits? val mask ...) -> :t | :nil
	(if (> (length masks) 1)
		(static-qq (/= 0 (logand ,val ,(num-intern (apply (const logior) (map (const eval) masks))))))
		(static-qq (/= 0 (logand ,val ~masks)))))

(defmacro bits?3 (val &rest masks)
	; (bits? val mask ...) -> :t | :nil
	(if (> (length masks) 1)
		(static-qq (/= 0 (logand ,val ,(apply (const bit-mask) (map (const eval) masks)))))
		(static-qq (/= 0 (logand ,val ~masks)))))

(defmacro bits?4 (val &rest masks)
	; (bits? val mask ...) -> :t | :nil
	(if (> (length masks) 1)
		`(/= 0 (logand ,val (const (bit-mask ~masks))))
		`(/= 0 (logand ,val ~masks))))

(defun f1 () (macrobind (cat '(bits?1 1 2 3 4 5))))
(defun f2 () (macrobind (cat '(bits?2 1 2 3 4 5))))
(defun f3 () (macrobind (cat '(bits?3 1 2 3 4 5))))
(defun f4 () (macrobind (cat '(bits?4 1 2 3 4 5))))

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
		(defq c 10000000)
		(time-it "f1" c (f1))
		(time-it "f2" c (f2))
		(time-it "f3" c (f3))
		(time-it "f4" c (f4))
		(time-it "f1" c (f1))
		(time-it "f2" c (f2))
		(time-it "f3" c (f3))
		(time-it "f4" c (f4))
		))
