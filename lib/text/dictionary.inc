;;;;;;;;;;;;;;;;;;;
; dictionary object
;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defclass Dictionary (&optional num_buckets) :nil
	; (Dictionary [num_buckets]) -> dictionary
	(def this :word_set (Fset num_buckets) :word_list (list) :dirty :nil)

	(defmethod :insert_word (word)
		; (. dictionary :insert_word word) -> dictionary
		(. (get :word_set this) :insert word)
		(lower (:dirty :t))
		this)

	(defmethod :sort ()
		; (. dictionary :sort) -> dictionary
		(defq word_list (clear (get :word_list this)))
		(. (get :word_set this) :each (# (push word_list %0)))
		(sort word_list)
		(lower (:dirty :nil))
		this)

	(defmethod :find_matches (prefix)
		; (. dictionary :find_matches prefix) -> (word ...)
		(if (get :dirty this) (. this :sort))
		(raise :word_list (i :nil j 0 k (length word_list)))
		;bsearch
		(while (< j k)
			(setq i (>> (+ j k) 1))
			(if (starts-with prefix (elem-get word_list i))
				(setq k -1)
				(if (> (cmp prefix (elem-get word_list i)) 0)
					(setq j (inc i)) (setq k i))))
		;bounds
		(cond
			((= k -1)
				(setq j (some! (# (if (starts-with prefix %0) :nil (inc (!))))
					(list word_list) :nil i 0))
				(setq k (some! (# (if (starts-with prefix %0) :nil (!)))
					(list word_list) :nil i -1))
				(setd j 0 k -1)
				(slice word_list j k))
			((list))))

	(defmethod :find_matches_case (prefix)
		; (. dictionary :find_matches_case prefix) -> (word ...)
		(unique (map (# (cat prefix (slice %0 (length prefix) -1)))
			(reduce (const merge)
				(map (# (. this :find_matches %0))
					(list prefix (to-lower prefix) (to-upper prefix)))))))
	)

;module
(export-classes '(Dictionary))
(env-pop)
