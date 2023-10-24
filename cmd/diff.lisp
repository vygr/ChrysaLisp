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

(defq +min_paths 4 +max_paths 64)

(defun goal-distance (p)
	(bind '(x y) p)
	(setq x (inc x) y (inc y))
	(+ (* x x) (* y y)))

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
				(:t (defq ps (list (list (list (dec lal) (dec lbl)))) nps (list) run :t)
					(while run
						(each (lambda (p)
							(bind '(a b) (last p))
							(push nps (cond
								((or (/= a -1) (/= b -1))
									(cond
										((= a -1) (push p (list a (dec b))))
										((= b -1) (push p (list (dec a) b)))
										((eql (elem-get a al) (elem-get b bl))
											(push p (list (dec a) (dec b))))
										(:t (push nps (push (cat p) (list a (dec b))))
											(push p (list (dec a) b)))))
								(:t (setq run :nil) p)))) ps)
						(setq ps (cat nps)) (clear nps)
						(when (> (length ps) +max_paths)
							(setq ps (slice 0 +min_paths (sort (# (-
								(goal-distance (last %0)) (goal-distance (last %1)))) ps)))))
					(defq a -2 b -2 oa 0)
					(each-rev (lambda ((pa pb))
							(cond
								((and (/= pa a) (/= pb b)))
								((= pa a)
									(print "+ " (+ a oa) " " (elem-get pb bl))
									(setq oa (inc oa)))
								(:t (print "- " (+ a oa) " " (elem-get pa al))))
							(setq a pa b pb))
						(first (sort (# (- (length %0) (length %1))) ps))))))))
