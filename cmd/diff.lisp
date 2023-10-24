(import "class/lisp.inc")
(import "sys/lisp.inc")
(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
{Usage: diff [options] file_a file_b

	options:
		-h --help: this help info.

	Calculate difference between text file a and text file b.
})
))

(defq +max_q 64)

(defun goal-distance ((x y _))
	(setq x (inc x) y (inc y))
	(+ (* x x) (* y y)))

(defun free-wf (wf)
	(defq stack '())
	(while (defq n (pop wf)) (while (bind '(_ _ n) n) (push stack n)))
	(clear stack))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage))
			(= (length args) 3))
		(defq a (file-stream (second args)) b (file-stream (third args)))
		(when (and a b)
			(defq al (list) _ (each-line (# (push al %0)) a) lal (length al)
				bl (list) _ (each-line (# (push bl %0)) b) lbl (length bl))
			(cond
				((and (= lal 0) (= lbl 0)))
				((= lal 0) (each (# (print "+ " _ " " %0)) bl))
				((= lbl 0) (each (# (print "- " _ " " %0)) al))
				(:t (defq wf (list (list (dec lal) (dec lbl) :nil)) nwf (list) run :t)
					(while run
						(each (lambda (n)
							(bind '(a b _) n)
							(push nwf (cond
								((or (/= a -1) (/= b -1))
									(cond
										((= a -1) (list a (dec b) n))
										((= b -1) (list (dec a) b n))
										((eql (elem-get a al) (elem-get b bl))
											(list (dec a) (dec b) n))
										(:t (push nwf (list a (dec b) n))
											(list (dec a) b n))))
								(:t (setq run :nil) n)))) wf)
						(free-wf wf) (setq wf nwf nwf (list))
						(when (>= (length wf) +max_q)
							(setq nwf (sort (# (- (goal-distance %0) (goal-distance %1))) wf))
							(setq wf (slice 0 1 nwf))
							(free-wf nwf)))
					(sort (# (- (goal-distance %0) (goal-distance %1))) wf)
					(defq a -2 b -2 oa 1 n (first wf))
					(free-wf wf)
					(while n
						(bind '(na nb _) n) (setq n (pop n))
						(cond
							((and (/= na a) (/= nb b)))
							((= na a)
								(print "+ " (+ a oa) " " (elem-get nb bl))
								(setq oa (inc oa)))
							(:t (print "- " (+ a oa) " " (elem-get na al))))
						(setq a na b nb)))))))
