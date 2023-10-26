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

(defun goal-distance (_)
	(nums-sum (nums-mul (nums-sub goal (defq _ (slice -3 -1 _)) _) _ _)))

(defun iron (p)
	(defq out (cap (length p) (list)) a -1 b -1 x 0 y 0 s :diag)
	(each (lambda ((pa pb))
		(if (and (/= pa a) (/= pb b))
			(when (eql s :flat)
				(while (/= x a) (push out (nums x y)) (setq x (inc x)))
				(while (/= y b) (push out (nums x y)) (setq y (inc y)))
				(setq s :diag))
			(when (eql s :diag)
				(while (/= x a) (push out (nums x y)) (setq x (inc x) y (inc y)))
				(setq s :flat)))
		(setq a pa b pb)) p)
	(cond
		((eql s :flat)
			(while (/= x a) (push out (nums x y)) (setq x (inc x)))
			(while (/= y b) (push out (nums x y)) (setq y (inc y))))
		(:t	(while (/= x a) (push out (nums x y)) (setq x (inc x) y (inc y)))))
	(push out (nums a b)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage))
			(= (length args) 3))
		(bind '(a b) (map file-stream (rest args)))
		(when (and a b)
			(defq al (list) la (each-line (# (push al %0)) a) la (length al)
				bl (list) lb (each-line (# (push bl %0)) b) lb (length bl))
			(cond
				((and (= la 0) (= lb 0)))
				((= la 0) (each (# (print "+ " _ " " %0)) bl))
				((= lb 0) (each (# (print "- " _ " " %0)) al))
				(:t (defq ps (list (nums 0 0)) nps (list) goal (nums la lb) run :t)
					(while run
						(each (lambda (p)
							(bind '(a b) (slice -3 -1 p))
							(push nps (cond
								((or (/= a la) (/= b lb))
									(cond
										((= a la) (push p a (inc b)))
										((= b lb) (push p (inc a) b))
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
						(iron (partition 2 (first ps)))))))))
