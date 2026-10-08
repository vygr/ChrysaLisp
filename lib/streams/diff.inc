;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; text stream diff and patch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +min_paths 4 +max_paths 16)

(defun goal-distance (%0)
	(nums-dot (nums-sub goal (defq %0 (slice %0 -3 -1)) %0) %0))

(defun iron (p)
	(defq out (cap (length p) (list)) a -1 b -1 x 0 y 0 s :diag)
	(each (lambda ((pa pb))
		(if (and (/= pa a) (/= pb b))
			(when (eql s :flat)
				(for x a (push out (nums (!) y)))
				(for y b (push out (nums a (!))))
				(setq x a y b s :diag))
			(when (eql s :diag)
				(for x a (push out (nums (!) y)) (++ y))
				(setq x a s :flat)))
		(setq a pa b pb)) p)
	(cond
		((eql s :flat)
			(for x a (push out (nums (!) y)))
			(for y b (push out (nums a (!)))))
		(:t (for x a (push out (nums (!) y)) (++ y))))
	(push out (nums a b)))

(defun diff-range (lo hi)
	;a line, or the first and last of a run of them
	(if (= lo hi) (str lo) (str lo "," hi)))

(defun diff-hunk (c ha hb dels adds)
	;one change, the lines of a that go and the lines of b that come, from
	;after line ha of a and line hb of b. The ironed path has them in
	;blocks, so they are written as blocks, a run of lines deleted, a run
	;added, or the one changed for the other
	(defq na (length dels) nb (length adds))
	(cond
		((and (= na 0) (= nb 0)))
		((= nb 0) (write-line c (str (diff-range (inc ha) (+ ha na)) "d" hb)))
		((= na 0) (write-line c (str ha "a" (diff-range (inc hb) (+ hb nb)))))
		(:t (write-line c (str (diff-range (inc ha) (+ ha na)) "c" (diff-range (inc hb) (+ hb nb))))))
	(each (# (write-line c (str "< " %0))) dels)
	(if (and (> na 0) (> nb 0)) (write-line c "---"))
	(each (# (write-line c (str "> " %0))) adds))

(defun stream-diff (a b c)
	; (stream-diff a b c)
	;difference between streams a and b, write to stream c
	;outputs in standard "Normal diff" format
	(defq al (list) la (lines! (# (push al %0) :nil) a) la (length al)
		bl (list) lb (lines! (# (push bl %0) :nil) b) lb (length bl))
	(cond
		((and (= la 0) (= lb 0)))
		((= la 0) (diff-hunk c 0 0 (list) bl))
		((= lb 0) (diff-hunk c 0 0 al (list)))
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
			(defq a -1 b -1 ha 0 hb 0 dels (list) adds (list))
			(sort ps (# (- (length %0) (length %1))))
			(each (lambda ((pa pb))
					(task-slice)
					(cond
						((and (/= pa a) (/= pb b))
							;a line that is the same, what went before it is a change
							(diff-hunk c ha hb dels adds)
							(clear dels adds)
							(setq ha pa hb pb))
						((= pa a) (push adds (elem-get bl b)))
						(:t (push dels (elem-get al a))))
					(setq a pa b pb))
				(iron (partition (first ps) 2)))
			(diff-hunk c ha hb dels adds))))

(defun stream-patch (a b c)
	; (stream-patch a b c)
	;patch stream a with stream b, write to stream c
	;accepts standard "Normal diff" format
	(defq ln 1 run :t)
	(while (and run (defq p (read-line b)))
		(task-slice)
		(defq match (matches p "^(\\d+)(,\\d+)?([acd])(\\d+)(,\\d+)?$"))
		(cond
			((empty? match)
				; skip unexpected headers or EOF
				(if (or (starts-with "---" p) (starts-with "+++" p)) :t (setq run :nil)))
			(:t (bind '((& (x1 x2) (x3 x4) (x5 x6) (x7 x8) (x9 x10))) match)
				(defq s1 (str-to-num (slice p x1 x2))
					;a part of the header that is not there has an end before its start
					e1 (if (<= x4 x3) s1 (str-to-num (slice p (inc x3) x4)))
					op (slice p x5 x6)
					s2 (str-to-num (slice p x7 x8))
					e2 (if (<= x10 x9) s2 (str-to-num (slice p (inc x9) x10)))
					target_line (if (eql op "a") (inc s1) s1))
				; advance original file output stream, up to the line the
				; operation is at, and not that line itself
				(while (< ln target_line)
					(write-line c (read-line a))
					(++ ln))
				; apply normal diff operation
				(cond
					((eql op "a")
						(times (inc (- e2 s2))
							(write-line c (slice (read-line b) 2 -1))))
					((eql op "d")
						(times (inc (- e1 s1))
							(read-line b)
							(read-line a)
							(++ ln)))
					((eql op "c")
						(times (inc (- e1 s1))
							(read-line b)
							(read-line a)
							(++ ln))
						(read-line b)
						(times (inc (- e2 s2))
							(write-line c (slice (read-line b) 2 -1))))))))
	(while (defq l (read-line a)) (write-line c l)))

;module
(export-symbols '(stream-diff stream-patch))
(env-pop)
