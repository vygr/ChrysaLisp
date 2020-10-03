
;all negative numbers are buttons. Adding 10 results in the value. 
;-1 is a flagged button. 9 is a mine.
(defq ord_vals (list (list -1 -1) (list -1 0) (list -1 1)
	(list 0 -1) (list 0 0) (list 0 1) (list 1 -1) (list 1 0) (list 1 1))
	beginner_settings '(8 8 10) intermediate_settings '(16 16 40) expert_settings '(30 16 99))
(defmacro-bind sum-xy (c1 c2)
	`(list (+ (elem 0 ,c1) (elem 0 ,c2)) (+ (elem 1 ,c1) (elem 1 ,c2))))
(defmacro-bind eql-xy (c1 c2)
	`(and (eql (elem 0 ,c1) (elm 0 ,c2)) (eql (elem 1 ,c1) (elem 1 ,c2))))
(defmacro-bind find-xy (c s)
	`(some (lambda (_e) (if (eql-xy ,c _e) _)) ,s))
(defmacro sfind (x s)
	`(some (lambda (_e) (if (eql _e ,x) _)) ,s))
(defmacro-bind in-bounds (c w h)
	`(and (< -1 (elem 0 ,c) ,w) (< -1 (elem 1 ,c) ,h)))
;need test h or w greater, flip if wrong
(defun-bind coord-elem (c w h) (bind '(x y) c) (+ x (* y w)))
(defun-bind elem-coord (e w h) (list (% e w) (/ e w)))

(defun-bind create-game (w h n s)
	(defq a (list) b (list) p (list) x_index 0 y_index 0 m (mine-list (* w h) n s w h))
	(each (lambda (_)
			(defq neighbors (adjacent-elem (list x_index y_index) w h))
			(push a neighbors) (push b 0) (push p "b")
			(if (= x_index (dec w)) (setq x_index 0 y_index (inc y_index)) (setq x_index (inc x_index))))
		(range 0 (* w h)))
	(board-set-mines m b n s w h) 
	(board-set-values a b) (list b p a))

(defun-bind mine-list (l n s w h)
	(defq count 0 m (list))
	(while (< count n)
		(defq r (% (random l) (/ (time) 1000000)))
		(unless (or (eql r s) (sfind r m) (some (lambda (_) (= _ r)) (adjacent-elem (elem-coord s w h) w h)))
			(setq count (inc count)) (push m r))) m)

(defun-bind board-set-mines (m b n s w h)
	(each (lambda (e) (elem-set e b 9))
		(mine-list (length b) n s w h)) b)

(defun-bind board-set-values (a b)
	(each (lambda (_) (when (/= (elem _ b) 9) (cell-set-value _ a b))) (range 0 (length b))))

(defun-bind cell-set-value (c a b)
	(defq count 0)
	(each (lambda (_) (when (= (elem _ b) 9) (setq count (inc count)))) (elem c a))
	(elem-set c b count))

(defmacro-bind each-in (a &rest r)
	`(each (lambda (_) ~r) ,a))

(defun-bind adjacent-elem (c w h)
	(defq coords (list))
	(each-in ord_vals
		(defq ord_pos (sum-xy c _))
		(when (in-bounds ord_pos w h)
			(push coords (coord-elem ord_pos w h)))) coords)
