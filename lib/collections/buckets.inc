;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; what the sets and maps of buckets share
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun buckets-empty (this count width per)
	; (buckets-empty this count width per) -> :t | :nil
	;about count items let go of, a bucket at a time in turn from where it
	;stopped last, so a big one is let go of a part at a time: width lists
	;to a bucket, per elements of its first to an item. :t while there is
	;more. A bucket found empty is a step of the count, so a call is short
	;even when most are
	(defq buckets (get :buckets this) n (length buckets)
		at (ifn (get :empty_at this) 0) bare (ifn (get :empty_bare this) 0))
	(while (and (> count 0) (< bare n))
		(defq bucket (elem-get buckets at) len (length bucket))
		(cond
			((= len 0) (setq bare (+ bare width) count (dec count)))
			(:t (if (= width 1) (clear bucket) (clear bucket (elem-get buckets (inc at))))
				(setq bare 0 count (- count (/ len per)))))
		(setq at (% (+ at width) n)))
	(def this :empty_at at :empty_bare bare)
	(< bare n))

;module
(export-symbols '(buckets-empty))
(env-pop)
