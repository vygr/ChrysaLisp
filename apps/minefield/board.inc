
;all negative numbers are buttons. Adding 10 results in the value. 
;-1 is a flagged button. 9 is a mine.
(defq ord_vals (list (list -1 -1) (list -1 0) (list -1 1)
	(list 0 -1) (list 0 0) (list 0 1) (list 1 -1) (list 1 0) (list 1 1))
	beginner_settings '(8 8 10) intermediate_settings '(16 16 40) expert_settings '(30 16 99))
(defmacro sum-xy (c1 c2)
	`(list (+ (elem-get 0 ,c1) (elem-get 0 ,c2)) (+ (elem-get 1 ,c1) (elem-get 1 ,c2))))
(defmacro eql-xy (c1 c2)
	`(and (eql (elem-get 0 ,c1) (elm 0 ,c2)) (eql (elem-get 1 ,c1) (elem-get 1 ,c2))))
(defmacro find-xy (c s)
	`(some (lambda (_e) (if (eql-xy ,c _e) _)) ,s))
(defmacro sfind (x s)
	`(some (lambda (_e) (if (eql _e ,x) _)) ,s))
(defmacro in-bounds (c w h)
	`(and (< -1 (elem-get 0 ,c) ,w) (< -1 (elem-get 1 ,c) ,h)))
;need test h or w greater, flip if wrong
(defun coord-elem ((x y) w h) (+ x (* y w)))
(defun elem-coord (e w h) (list (% e w) (/ e w)))

(defun create-game (w h n s)
	(defq a (list) b (list) p (list) x_index 0 y_index 0 m (mine-list (* w h) n s w h))
	(each (lambda (_)
			(defq neighbors (adjacent-elem (list x_index y_index) w h))
			(push a neighbors) (push b 0) (push p "b")
			(if (= x_index (dec w)) (setq x_index 0 y_index (inc y_index)) (setq x_index (inc x_index))))
		(range 0 (* w h)))
	(board-set-mines m b n s w h) 
	(board-set-values a b) (list b p a))

(defun mine-list (l n s w h)
	(defq count 0 m (list))
	(while (< count n)
		(defq r (% (random l) (/ (pii-time) 1000000)))
		(unless (or (eql r s) (sfind r m) (some (lambda (_) (= _ r)) (adjacent-elem (elem-coord s w h) w h)))
			(setq count (inc count)) (push m r))) m)

(defun board-set-mines (m b n s w h)
	(each (# (elem-set %0 b 9)) (mine-list (length b) n s w h)) b)

(defun board-set-values (a b)
	(each (# (when (/= (elem-get %0 b) 9) (cell-set-value %0 a b))) (range 0 (length b))))

(defun cell-set-value (c a b)
	(defq count 0)
	(each (# (when (= (elem-get %0 b) 9) (setq count (inc count)))) (elem-get c a))
	(elem-set c b count))

(defun adjacent-elem (c w h)
	(defq coords (list))
	(aeach ord_vals
		(defq ord_pos (sum-xy c it))
		(when (in-bounds ord_pos w h)
			(push coords (coord-elem ord_pos w h)))) coords)
