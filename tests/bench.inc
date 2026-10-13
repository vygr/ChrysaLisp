;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; benchmarks, the timing of a thing, to hold a change to
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;A benchmark module is tests/bench/bench_<name>.lisp, a file of (bench)
;forms. They are not tests, nothing is asserted and (tests) does not run
;them. They are run by the bench command, when the timing of something is
;in question: before a change and after it.

;how many goes each gets, the best is kept, and what has been timed, each
;(name tenths_of_a_ns)
(defq *bench_runs* 5 *bench_results* (list))

(defmacro bench (name per &rest body)
	; (bench name per body)
	;the body is run *bench_runs* times and the best of them kept, as
	;tenths of a ns for each of the per things one go of it does: the
	;elements it goes over, or the times it does a thing. The best, not
	;the mean: what else the machine does only ever adds
	(defq best (gensym) then (gensym))
	`(progn
		(defq ,best +max_int)
		(times *bench_runs*
			(defq ,then (pii-time))
			~body
			(setq ,best (min ,best (- (pii-time) ,then))))
		(push *bench_results* (list ,name (/ (* ,best 10000) ,per)))))
