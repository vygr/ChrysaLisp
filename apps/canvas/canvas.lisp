(defq canvas (pop argv))

(each (lambda (y)
	(each (lambda (x)
		(pixel canvas 0xff0000ff x y)) (range 10 60))) (range 10 100))

(pixel canvas)
