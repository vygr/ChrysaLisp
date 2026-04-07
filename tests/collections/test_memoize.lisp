(report-header "Memoize Macro")

(defq mem_count 0)
(defun test-memo (x)
	(memoize x (progn (++ mem_count) (* x 2)) 11))

(assert-eq "memoize 1st" 20 (test-memo 10))
(assert-eq "memoize 2nd" 20 (test-memo 10))
(assert-eq "memoize count" 1 mem_count)
(assert-eq "memoize other" 40 (test-memo 20))
(assert-eq "memoize count 2" 2 mem_count)
