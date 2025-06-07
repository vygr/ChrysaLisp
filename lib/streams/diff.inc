;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; text stream diff and patch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +min_paths 4 +max_paths 16)

(defun goal-distance (_)
	(nums-dot (nums-sub goal (defq _ (slice _ -3 -1)) _) _))

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
		(:t (while (/= x a) (push out (nums x y)) (setq x (inc x) y (inc y)))))
	(push out (nums a b)))

(defun stream-diff (a b c)
	; (stream-diff a b c)
	;difference between streams a and b, write to stream c
	(defq al (list) la (lines! (# (push al %0)) a) la (length al)
		bl (list) lb (lines! (# (push bl %0)) b) lb (length bl))
	(cond
		((and (= la 0) (= lb 0)))
		((= la 0) (each (# (write-line c (str "+ " (!) " " %0))) bl))
		((= lb 0) (each (# (write-line c (str "- " (!) " " %0))) al))
		(:t (defq ps (list (nums 0 0)) nps (list) goal (nums la lb) run :t)
			(while run
				(each (lambda (p)
					(bind '(a b) (slice p -3 -1))
					(push nps (cond
						((or (/= a la) (/= b lb))
							(cond
								((= a la) (push p a (inc b)))
								((= b lb) (push p (inc a) b))
								((eql (elem-get al a) (elem-get bl b))
									(push p (inc a) (inc b)))
								(:t (push nps (push (cat p) a (inc b)))
									(push p (inc a) b))))
						(:t (setq run :nil) p)))) ps)
				(setq ps (cat nps)) (clear nps)
				(when (> (length ps) +max_paths)
					(task-slice)
					(setq ps (slice (sort ps (# (- (goal-distance %0) (goal-distance %1))))
						0 +min_paths))))
			(defq a -1 b -1 oa 0)
			(sort ps (# (- (length %0) (length %1))))
			(each (lambda ((pa pb))
					(task-slice)
					(cond
						((and (/= pa a) (/= pb b)))
						((= pa a)
							(write-line c (str "+ " (+ a oa) " " (elem-get bl b)))
							(setq oa (inc oa)))
						(:t (write-line c (str "- " (+ a oa) " " (elem-get al a)))))
					(setq a pa b pb))
				(iron (partition (first ps) 2))))))

(defun stream-patch (a b c)
	; (stream-patch a b c)
	;patch stream a with stream b, write to stream c
	(defq ln 0 run :t)
	(while (and run (defq p (read-line b)))
		(task-slice)
		(defq match (matches p "^([-+]) (\d+) (.*)"))
		(cond
			((empty? match) (setq run :nil))
			(:t (bind '((_ (x x1) (x2 x3) (x4 x5))) match)
				(defq op (slice p x x1) l (slice p x4 x5)
					nl (str-to-num (slice p x2 x3)))
				(while (< ln nl) (write-line c (read-line a)) (setq ln (inc ln)))
				(if (eql op "+") (write-line c l) (read-line a))
				(setq ln (inc ln)))))
	(while (defq l (read-line a)) (write-line c l)))

;module
(export-symbols '(stream-diff stream-patch))
(env-pop)
