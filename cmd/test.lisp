(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: test [options]

	options:
		-h --help: this help info.
	
	Simple timing test framework.")
))

(defun reverse-list1 (lst)
	; (reverse-list list) -> list
	(map-rev (const identity) lst))

(defun reverse-list2 (lst)
	; (reverse-list list) -> list
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
		(time-it "reverse-list1" 1000 (reverse-list1 l))
		(time-it "reverse-list2" 1000 (reverse-list2 l))
		(time-it "reverse-list1" 1000 (reverse-list1 l))
		(time-it "reverse-list2" 1000 (reverse-list2 l))
		))
