;;;;;;;;;;;;;;;;;;;;;;;
; just some fun for now
;;;;;;;;;;;;;;;;;;;;;;;

(defq next_id 0)

(defun new (c)
	(eval (list 'init (defq _ (eval '(new) c))) c) _)

(defq class_base ((lambda ()
	(defun new () (env))
	(defun init (_) (eval `(defq id (setq next_id (inc next_id))) _))
	(defun get_id () id)
	(env))))

(defmacro defclass (c s i &rest b)
	`(defq ,c (eval (list (lambda ()
		(defun init (_)
			(eval (list 'init _) ,s)
			(eval ',i _))
		~b (env))) ,s)))

(defclass class_rect class_base
	(defq x 0 y 0 w 0 h 0)
	(defun get_x () x)
	(defun get_y () y)
	(defun get_w () w)
	(defun get_h () h)
	(defun set_x (_) (setq x _))
	(defun set_y (_) (setq y _))
	(defun set_w (_) (setq w _))
	(defun set_h (_) (setq h _)))

(defclass class_fbox class_rect
	(defq col 0xff00ff00)
	(defun get_col () col)
	(defun set_col (_) (setq col _)))

(defq box (new class_fbox))
