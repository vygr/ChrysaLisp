;import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)

(structure 'work 0
	(long 'parent_id)
	(long 'width)
	(long 'heigth)
	(offset 'ys))

(structure 'reply 0
	(int 'y)
	(offset 'data))

(defun screen ((canvas w h s))
	(defq y -1 w (div (fmul w s) 1.0) h (div (fmul h s) 1.0)
		farm (open-farm "apps/raymarch/child.lisp" 16 kn_call_child)
		com (map (lambda (_)
			(array (task-mailbox) w h)) farm))
	(while (lt (setq y (inc y)) h)
		(push (elem (mod y (length farm)) com) (sub h y 1)))
	(each (lambda (c e)
		(mail-send c e)) com farm)
	(defq i -1)
	(while (lt (setq i (inc i)) h)
		(defq msg (mail-mymail) x -1 y (read-int reply_y msg))
		(while (lt (setq x (inc x)) w)
			(canvas-set-plot canvas (read-int (add reply_data (mul x int_size)) msg) x y))
		(canvas-swap canvas)))

;read args from parent
(screen (mail-mymail))
