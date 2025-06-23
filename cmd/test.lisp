(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: test [options]

	options:
		-h --help: this help info.

	Simple timing test framework.")
))

(defmacro cback (f e &rest args)
	; (callback lambda env arg ...) -> (#eval `(#apply ,lambda '(,arg ...)) env)
	(list eval (list quasi-quote (list apply (list 'unquote f) (cat (list quote)
		(list (map! (lambda (a) (list 'unquote a)) (list args)))))) e))

(defmacro cback1 (f e &rest args)
	; (callback lambda env arg ...) -> (#eval `(#apply ,lambda ',(#list ~args)) env)
	(list eval (list quasi-quote (list apply (list 'unquote f)
		(list quote (list 'unquote (cat (list list) args))))) e))

(defun f0 (x y z a b c)
	(cback identity (penv) x y z a b c x y z a b c))

(defun f1 (x y z a b c)
	(cback1 identity (penv) x y z a b c x y z a b c))

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
		(time-it "f0" c (f0 1 2 3 "a" "b" "c"))
		(time-it "f1" c (f1 1 2 3 "a" "b" "c"))
		(time-it "f0" c (f0 1 2 3 "a" "b" "c"))
		(time-it "f1" c (f1 1 2 3 "a" "b" "c"))
		(time-it "f0" c (f0 1 2 3 "a" "b" "c"))
		(time-it "f1" c (f1 1 2 3 "a" "b" "c"))
		(time-it "f0" c (f0 1 2 3 "a" "b" "c"))
		(time-it "f1" c (f1 1 2 3 "a" "b" "c"))
		))
