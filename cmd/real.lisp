;imports
(import 'class/lisp.inc)
(import 'cmd/options.inc)

(defq usage `(
(("-h" "--help")
"Usage: real [options]
	options:
		-h --help: this help info.
	+m ++mantisa 2-31: default 31.")
(("+m" "++mantisa")
	,(bind-fun (lambda (o f) (setq mbits (str-to-num f)))))
))

(defun-bind real-unpack (n)
	(list (>>> (<< n (- 64 ebits)) (- 64 ebits)) (>>> n ebits)))

(defun-bind real-pack (e m)
	(+ (<< m ebits) (>> (<< e (- 64 ebits)) (- 64 ebits))))

(defun-bind real-norm (e m)
	(cond
		((> m 0)
			;positive
			(while (>= m (<< 1 mbits))
				(setq m (>>> m 1) e (inc e)))
			(while (< m (<< 1 (dec mbits)))
				(setq m (<< m 1) e (dec e))))
		((< m 0)
			;negative
			(while (<= m (<< -1 mbits))
				(setq m (>>> m 1) e (inc e)))
			(while (> m (<< -1 (dec mbits)))
				(setq m (<< m 1) e (dec e))))
		(t	;zero
			(setq e 0)))
	(list e m))

(defun-bind real-add (n1 n2)
	;unpack
	(bind '(e1 m1) (real-unpack n1))
	(bind '(e2 m2) (real-unpack n2))
	(cond
		;exponents the same
		((= (defq e2 (- e2 e1)) 0) (setq m1 (+ m1 m2)))
		;need to align m1
		((> e2 0) (setq m1 (+ (>>> m1 e2) m2) e1 (+ e1 e2)))
		;need to align m2
		(t (setq m1 (+ m1 (>>> m2 (neg e2))))))
	;normalise and pack
	(apply real-pack (real-norm e1 m1)))

(defun-bind real-sub (n1 n2)
	;unpack
	(bind '(e1 m1) (real-unpack n1))
	(bind '(e2 m2) (real-unpack n2))
	(cond
		;exponents the same
		((= (defq e2 (- e2 e1)) 0) (setq m1 (- m1 m2)))
		;need to align m1
		((> e2 0) (setq m1 (- (>>> m1 e2) m2) e1 (+ e1 e2)))
		;need to align m2
		(t (setq m1 (- m1 (>>> m2 (neg e2))))))
	;normalise and pack
	(apply real-pack (real-norm e1 m1)))

(defun-bind real-mul (n1 n2)
	;unpack
	(bind '(e1 m1) (real-unpack n1))
	(bind '(e2 m2) (real-unpack n2))
	;normalise and pack
	(apply real-pack (real-norm (+ e1 e2 (neg mbits)) (* m1 m2))))

(defun-bind real-div (n1 n2)
	;unpack
	(bind '(e1 m1) (real-unpack n1))
	(bind '(e2 m2) (real-unpack n2))
	;normalise and pack
	(apply real-pack (real-norm (- e1 e2 (inc (* (- 31 mbits) 2))) (/ (<< m1 (- 63 mbits)) m2))))

(defun-bind real-eq (&rest _)
	(apply = _))

(defun-bind real-ne (&rest _)
	(apply /= _))

(defun-bind real-lt (&rest _)
	(if (reduce (lambda (x y)
		(if x (if (< (real-sub x y) 0) y))) _) t))

(defun-bind real-le (&rest _)
	(if (reduce (lambda (x y)
		(if x (if (<= (real-sub x y) 0) y))) _) t))

(defun-bind real-gt (&rest _)
	(if (reduce (lambda (x y)
		(if x (if (> (real-sub x y) 0) y))) _) t))

(defun-bind real-ge (&rest _)
	(if (reduce (lambda (x y)
		(if x (if (>= (real-sub x y) 0) y))) _) t))

(defun-bind int-to-real (n)
	(apply real-pack (real-norm mbits n)))

(defun-bind fixed-to-real (n)
	(apply real-pack (real-norm (- mbits fp_shift) n)))

(defun-bind real-to-int (n)
	;unpack
	(bind '(e m) (real-unpack n))
	;norm to int
	(cond
		((= e mbits) m)
		((< e mbits) (>>> m (+ (neg e) mbits)))
		(t (<< m (- e mbits)))))

(defun-bind real-to-fixed (n)
	;unpack
	(bind '(e m) (real-unpack n))
	;norm to fixed
	(cond
		((= e (- mbits fp_shift)) m)
		((< e (- mbits fp_shift)) (>>> m (+ (neg e) (- mbits fp_shift))))
		(t (<< m (- e (- mbits fp_shift))))))

;initialize pipe details and command args, abort on error
(when (and (defq slave (create-slave)) (defq mbits 31 args (options slave usage)))
	(defq ebits (- 63 mbits))

	(defq n1 (fixed-to-real 2.571) n2 (fixed-to-real -45.599) n3 (int-to-real 0) n4 (real-div (int-to-real 1024) (int-to-real 4)))
	(print "Nums: " (array n1 n2 n3 n4))
	(print "Add: " (array (real-add n2 n1) (real-add n1 n2)))
	(print "Sub: " (array (real-sub n1 n2) (real-sub n2 n1) (real-sub n1 n1)))
	(print "Mul: " (array (real-mul n2 n1) (real-mul n2 n1) (real-mul n1 n3) (real-mul n2 n4)))

	(print "Result Mul: " (array (real-add n1 (real-mul n2 n4))))
	(each (lambda (_)
		(setq n1 (real-add n1 n2))) (range 0 256))
	(print "Result Acc: " (array n1) " " (fixed-to-str (real-to-fixed n1)))

	(print "Cmps:")
	(print (real-eq (int-to-real -2) (int-to-real -2) (int-to-real -2) (int-to-real -2) (int-to-real -2)))
	(print (real-ne (int-to-real -2) (int-to-real -1) (int-to-real 0) (int-to-real 1) (int-to-real 2)))
	(print (real-lt (int-to-real -2) (int-to-real -1) (int-to-real 0) (int-to-real 1) (int-to-real 2)))
	(print (real-le (int-to-real -2) (int-to-real -1) (int-to-real 0) (int-to-real 1) (int-to-real 2)))
	(print (real-gt (int-to-real 2) (int-to-real 1) (int-to-real 0) (int-to-real -1) (int-to-real -2)))
	(print (real-ge (int-to-real 2) (int-to-real 1) (int-to-real 0) (int-to-real -1) (int-to-real -2)))

	(print (real-eq (int-to-real -2) (int-to-real -1) (int-to-real 0) (int-to-real 1) (int-to-real 2)))
	(print (real-ne (int-to-real -2) (int-to-real -1) (int-to-real 2) (int-to-real 1) (int-to-real 2)))
	(print (real-lt (int-to-real -2) (int-to-real -1) (int-to-real 0) (int-to-real 2) (int-to-real 2)))
	(print (real-le (int-to-real -2) (int-to-real -3) (int-to-real 0) (int-to-real 1) (int-to-real 2)))
	(print (real-gt (int-to-real 2) (int-to-real 1) (int-to-real 0) (int-to-real 0) (int-to-real -2)))
	(print (real-ge (int-to-real 2) (int-to-real 1) (int-to-real 0) (int-to-real 1) (int-to-real -2)))

	(print "Preds:")
	(print (some real-eq
		(list (fixed-to-real 3.9876) (fixed-to-real 3.9875))
		(list (fixed-to-real 3.9876) (fixed-to-real 3.9875))
		(list (fixed-to-real 3.9876) (fixed-to-real 3.9876))))
	(print (some real-ne
		(list (fixed-to-real 3.9876) (fixed-to-real 3.9874))
		(list (fixed-to-real 3.9876) (fixed-to-real 3.9875))
		(list (fixed-to-real 3.9876) (fixed-to-real 3.9876))))
	(print (some real-lt
		(list (fixed-to-real 3.9876) (fixed-to-real 3.9875))
		(list (fixed-to-real 3.9876) (fixed-to-real 3.9876))))
	(print (every real-gt
		(list (fixed-to-real 6.9876) (fixed-to-real 23.9875))
		(list (fixed-to-real 4.9876) (fixed-to-real 13.9875))
		(list (fixed-to-real 3.9876) (fixed-to-real 9.9876))
		(list (fixed-to-real -3.9876) (fixed-to-real -9.9876))))
)
