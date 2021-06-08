;;;;;;;;;;;;;;;;;;;
; dictionary object
;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defclass Dictionary (&optional num_buckets) nil
	; (Dictionary [num_buckets]) -> dictionary
	(def this :word_set (xset num_buckets) :word_list (list) :dirty nil)

	(defmethod :insert_word (this word)
		; (. dictionary :insert_word word) -> dictionary
		(. (get :word_set this) :insert word)
		(set this :dirty t)
		this)

	(defmethod :sort (this)
		; (. dictionary :sort) -> dictionary
		(defq word_list (clear (get :word_list this)))
		(. (get :word_set this) :each (# (push word_list %0)))
		(sort cmp word_list)
		(set this :dirty nil)
		this)

	(defmethod :find_matches (this prefix)
		; (. dictionary :find_matches prefix) -> (word ...)
		(if (get :dirty this) (. this :sort))
		(defq word_list (get :word_list this) i nil j 0 k (length word_list))
		;bsearch
		(while (and (/= j -1) (<= j k))
			(setq i (>> (+ j k) 1))
			(if (starts-with prefix (elem i word_list))
				(setq j -1 k -1)
				(if (> (cmp prefix (elem i word_list)) 0)
					(setq j (inc i)) (setq k (dec i)))))
		;bounds
		(cond
			((= j k -1)
				(defq y (some! i 0 nil
						(# (unless (starts-with prefix %0) (inc _)))
					(list word_list)))
				(defq y1 (some! i -1 nil
						(# (unless (starts-with prefix %0) _))
					(list word_list)))
				(unless y (setq y 0))
				(unless y1 (setq y1 (length word_list)))
				(slice y y1 word_list))
			((list))))
	)

;module
(export (penv)
	*class_Dictionary* Dictionary Dictionary?)
(env-pop)
