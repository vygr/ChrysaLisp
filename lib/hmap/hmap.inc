;generic hash map
(import 'sys/lisp.inc)

(structure '+hmap 0
	(byte 'num_buckets+ 'cmp_fnc+ 'hash_fnc+ 'buckets+))

(defmacro-bind hmap-slot ()
	`(defq b (+ +hmap_buckets+ (* (% ((elem +hmap_hash_fnc+ obj) key) (elem +hmap_num_buckets+ obj)) 2))
		e (some (# (if ((elem +hmap_cmp_fnc+ obj) %0 key) _)) (elem b obj))))

(defun-bind hmap (&optional num_buckets cmp_fnc hash_fnc)
	(setd num_buckets 1 cmp_fnc eql hash_fnc hash)
	(defq obj (cap (+ +hmap_num_buckets+ (* num_buckets 2)) (list)))
	(push obj num_buckets cmp_fnc hash_fnc)
	(while (>= (setq num_buckets (dec num_buckets)) 0)
		(push obj (list) (list)))
	obj)

(defun-bind hmap-insert (obj key val)
	(hmap-slot)
	(cond
		(e (elem-set e (elem (inc b) obj) val))
		(t (push (elem b obj) key) (push (elem (inc b) obj) val))))

(defun-bind hmap-find (obj key)
	(hmap-slot)
	(if e (elem e (elem (inc b) obj))))

(defun-bind hmap-erase (obj key)
	(hmap-slot)
	(when e
		(defq bv (elem (inc b) obj) b (elem b obj))
		(elem-set e b (elem -2 b))
		(elem-set e bv (elem -2 bv))
		(pop b) (pop bv)))

(defun-bind hmap-each (_obj _hf)
	(defq _i (- +hmap_buckets+ 2))
	(while (< (setq _i (+ _i 2)) (length _obj))
		(each _hf (elem _i _obj) (elem (inc _i) _obj)))
	_obj)

(defq o (hmap 10))
(hmap-insert o "cliff" (i2r 45))
(hmap-insert o "bert" 'wendy)
(hmap-insert o "alf" 60)
(hmap-insert o "albert" 1.56)
(hmap-insert o "wally" (list 4 5 6))
(hmap-insert o "tristum" "qwerty")
(hmap-each o (# (print %0 " : " %1)))
(print "wally = " (hmap-find o "wally"))
(print "bert = " (hmap-find o "bert"))
(print "alf = " (hmap-find o "alf"))
(print "tristum = " (hmap-find o "tristum"))
(print o)
(hmap-erase o "tristum")
(print "tristum = " (hmap-find o "tristum"))
(print o)

(defq o (hmap 10
	(# (and (= (elem 0 %0) (elem 0 %1)) (= (elem 1 %0) (elem 1 %1)) (= (elem 2 %0) (elem 2 %1))))
	(# (reduce + %0))))
(hmap-insert o '(1 2 3) 'alf)
(hmap-insert o '(2 3 4) 'bert)
(hmap-insert o '(8 3 7) 'fred)
(hmap-insert o '(8 9 4) 'liz)
(hmap-insert o '(8 9 5) 'chris)
(hmap-each o (# (print %0 " : " %1)))
(print "(2 3 4) = " (hmap-find o '(2 3 4)))
(print o)
