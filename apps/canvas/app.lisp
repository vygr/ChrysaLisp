;math tools
(run 'apps/canvas/math.lisp)

(defq canvas (pop argv) pen-col 0 brush-col 0
	mitre-join 0 bevel-join 1 round-join 2
	butt-cap 0 square-cap 1 tri-cap 2 arrow-cap 3 round-cap 4)

(defun set-pen-col (_) (setq pen-col _))
(defun set-brush-col (_) (setq brush-col _))

(defun fbox (x y w h)
	(defq h (add y h) y (dec y))
	(while (lt (setq y (inc y)) h)
		(pixel canvas brush-col x y w)))

(defun fpoly (&rest _)
	(defq e (list) ys max-long ye min-long)
	(each (lambda (_)
		(reduce (lambda (p1 p2)
			(defq x1 (add (elem 0 p1) fp-half) y1 (bit-asr (add (elem 1 p1) fp-half) fp-shift)
				x2 (add (elem 0 p2) fp-half) y2 (bit-asr (add (elem 1 p2) fp-half) fp-shift))
			(cond
				((lt y1 y2)
					(setq ys (min ys y1) ye (max ye y2))
					(push e (list x1 y1 y2 (div (sub x2 x1) (sub y2 y1)))))
				((gt y1 y2)
					(setq ys (min ys y2) ye (max ye y1))
					(push e (list x2 y2 y1 (div (sub x1 x2) (sub y1 y2))))))
			p2) _ (elem -2 _))) _)
	(sort (lambda (e1 e2)
		(lt (elem 1 e1) (elem 1 e2))) e)
	(defq i 0 j 0 ys (dec ys))
	(while (ne (setq ys (inc ys)) ye)
		(each! i j nil (lambda (_)
			(if (eq ys (elem 2 _)) (elem-set 0 _ min-long))) (list e))
		(while (and (ne j (length e)) (eq ys (elem 1 (elem j e))))
			(setq j (inc j)))
		(sort (lambda (e1 e2)
			(lt (elem 0 e1) (elem 0 e2))) e i j)
		(while (and (ne i (length e)) (eq (elem 0 (elem i e)) min-long))
			(setq i (inc i)))
		(defq k (sub i 2))
		(while (ne (setq k (add k 2)) j)
			(defq e1 (elem k e) e2 (elem (inc k) e)
				x1 (elem 0 e1) x2 (elem 0 e2))
			(elem-set 0 e1 (add x1 (elem 3 e1)))
			(elem-set 0 e2 (add x2 (elem 3 e2)))
			(setq x1 (bit-asr x1 fp-shift) x2 (bit-asr x2 fp-shift))
			(pixel canvas brush-col x1 ys (sub x2 x1)))))

(set-brush-col 0)
(fbox 0 0 512 512)

(set-brush-col 0xff0000ff)
(fpoly
	(list
		(fp-vec 0 0)
		(fp-vec 128 512)
		(fp-vec 256 0)
		(fp-vec 384 512)
		(fp-vec 512 0)
		(fp-vec 16 512)))

(set-brush-col 0xff00ff00)
(fpoly
	(stroke-polyline-2d
		(list)
		(fp-val 30)
		round-join
		round-cap
		round-cap
		(list
			(fp-vec 50 50)
			(fp-vec 350 80)
			(fp-vec 450 480))))

(set-brush-col 0xff00ffff)
(apply fpoly
	(stroke-polygon-2d
		(list)
		(fp-val 5)
		mitre-join
		(stroke-polyline-2d
			(list)
			(fp-val 15)
			bevel-join
			round-cap
			arrow-cap
			(bezier-polyline-2d
				(list)
				(fp-vec 50 470)
				(fp-vec 90 100)
				(fp-vec 200 200)
				(fp-vec 470 40)))))

(set-brush-col 0xffff0000)
(apply fpoly
	(stroke-polygon-2d
		(list)
		(fp-val 15)
		mitre-join
		(stroke-polyline-2d
			(list)
			(fp-val 25)
			bevel-join
			square-cap
			square-cap
			(arc-polyline-2d
				(list)
				(fp-vec 240 256)
				(fp-val 150)
				fp-2pi
				fp-pi))))

(pixel canvas)
