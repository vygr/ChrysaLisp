;;;;;;;;;;;;;;;;;;;
; dictionary object
;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defclass Dictionary (&optional num_buckets) nil
	; (Dictionary [num_buckets]) -> dictionary
	(def this :word_set (fset num_buckets) :word_list (list) :dirty nil)

	(defmethod :insert_word (this word)
		; (. dictionary :insert_word word) -> dictionary
		(. (get :word_set this) :insert word)
		(lower (:dirty t))
		this)

	(defmethod :sort (this)
		; (. dictionary :sort) -> dictionary
		(defq word_list (clear (get :word_list this)))
		(. (get :word_set this) :each (# (push word_list %0)))
		(sort cmp word_list)
		(lower (:dirty nil))
		this)

	(defmethod :find_matches (this prefix)
		; (. dictionary :find_matches prefix) -> (word ...)
		(if (get :dirty this) (. this :sort))
		(raise :word_list (i nil j 0 k (length word_list)))
		;bsearch
		(while (< j k)
			(setq i (>> (+ j k) 1))
			(if (starts-with prefix (elem i word_list))
				(setq k -1)
				(if (> (cmp prefix (elem i word_list)) 0)
					(setq j (inc i)) (setq k i))))
		;bounds
		(cond
			((= k -1)
				(setq j (some! i 0 nil
						(# (unless (starts-with prefix %0) (inc _)))
					(list word_list)))
				(setq k (some! i -1 nil
						(# (unless (starts-with prefix %0) _))
					(list word_list)))
				(unless j (setq j 0))
				(unless k (setq k (length word_list)))
				(slice j k word_list))
			((list))))

	(defmethod :find_matches_case (this prefix)
		; (. dictionary :find_matches_case prefix) -> (word ...)
		(map (# (cat prefix (slice (length prefix) -1 %0)))
			(. this :find_matches (to-lower prefix))))
	)

;module
(export (penv)
	*class_Dictionary* Dictionary Dictionary?)
(env-pop)
