(def (squared) ((lambda (x) (mul x x))))
(def (cubed) ((lambda (x) (mul x x x))))

(def (a b) (t nil))
(def (fn1) ((lambda (f x) (f x))))
(def (fn2) ((lambda (x) x)))
(fn1 fn2 a)
(fn1 fn2 b)
