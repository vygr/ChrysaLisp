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

(defq +min_paths 4 +max_paths 16)

(defun goal-distance (p)
	(defq _ (slice -3 -1 p))
	(nums-sum (nums-mul (nums-sub goal _ _) _ _)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage))
			(= (length args) 3))
		(defq a (file-stream (second args)) b (file-stream (third args)))
		(when (and a b)
			(defq al (list) _ (each-line (# (push al %0)) a) lal (length al)
				bl (list) _ (each-line (# (push bl %0)) b) lbl (length bl)
				goal (nums lal lbl))
			(cond
				((and (= lal 0) (= lbl 0)))
				((= lal 0) (each (# (print "+ " _ " " %0)) bl))
				((= lbl 0) (each (# (print "- " _ " " %0)) al))
				(:t (defq ps (list (nums 0 0)) nps (list) run :t)
					(while run
						(each (lambda (p)
							(bind '(a b) (slice -3 -1 p))
							(push nps (cond
								((or (/= a lal) (/= b lbl))
									(cond
										((= a lal) (push p a (inc b)))
										((= b lbl) (push p (inc a) b))
										((eql (elem-get a al) (elem-get b bl))
											(push p (inc a) (inc b)))
										(:t (push nps (push (cat p) a (inc b)))
											(push p (inc a) b))))
								(:t (setq run :nil) p)))) ps)
						(setq ps (cat nps)) (clear nps)
						(when (> (length ps) +max_paths)
							(setq ps (slice 0 +min_paths (sort (# (-
								(goal-distance %0) (goal-distance %1))) ps)))))
					(defq a -1 b -1 oa 0)
					(sort (# (- (length %0) (length %1))) ps)
					(each (lambda ((pa pb))
							(cond
								((and (/= pa a) (/= pb b)))
								((= pa a)
									(print "+ " (+ a oa) " " (elem-get b bl))
									(setq oa (inc oa)))
								(:t (print "- " (+ a oa) " " (elem-get a al))))
							(setq a pa b pb))
						(partition 2 (first ps))))))))
