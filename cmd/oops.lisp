;imports
(import 'class/lisp.inc)

;this is just some experiments !

(defmacro class (name &optional super)
	(if super
		`(defq ,name (list (cat (list nil) (slice 1 -1 (elem 0 ,super)))
			(cat (list ,super) (slice 1 -1 (elem 1 ,super)))) ,(sym (str "_" name)) ,super)
		`(defq ,name (list (list nil) (list nil)))))

(defun method (vtable name &optional fun)
	(setq fun (bind-fun fun))
	(cond
		((defq slot (find name (elem 0 vtable)))
			(elem-set slot (elem 1 vtable) fun))
		(t
			(push (elem 0 vtable) name)
			(push (elem 1 vtable) fun))))

(defmacro mcall (vtable name &rest args)
	`((elem ,(find (eval name) (elem 0 (eval vtable))) (elem 1 ,vtable)) ~args))

(defmacro scall (vtable name &rest args)
	(defq vtable (sym (str "_" vtable)))
	`((elem ,(find (eval name) (elem 0 (eval vtable))) (elem 1 ,vtable)) ~args))

(class Obj)
(method Obj 'init)
(method Obj 'deinit)

(method Obj 'init (lambda (o v)
	(def o 'vtable v)))
(method Obj 'deinit (lambda (o)
	(set o 'vtable nil)))

(class Node Obj)
(method Node 'get_children)
(method Node 'sub)
(method Node 'add_child)

(method Node 'init (lambda (o v)
	(scall Node 'init o v)
	(def o 'parent nil 'children (list))))
(method Node 'deinit (lambda (o)
	(mcall Node 'sub o)
	(each (lambda (child)
		(if child (mcall Node 'sub child))) (eval 'children o))
	(set o 'children nil)
	(scall Node 'deinit o)))
(method Node 'get_children (lambda (o)
	(eval 'children o)))
(method Node 'sub (lambda (o)
	(defq parent (eval 'parent o))
	(when parent
		(set o 'parent nil)
		(defq siblings (eval 'children parent))
		(elem-set (find o siblings) siblings nil))))
(method Node 'add_child (lambda (o child)
	(def child 'parent o)
	(push (eval 'children o) child)))

(class View Node)
(method View 'create)
(method View 'get_bounds)
(method View 'set_bounds)

(method View 'create (lambda ()
	(defq o (env -1))
	(mcall View 'init o View) o))
(method View 'init (lambda (o v)
	(scall View 'init o v)
	(def o 'x 0 'y 0 'w 0 'h 0)))
(method View 'get_bounds (lambda (o)
	(eval `(list x y w h) o)))
(method View 'set_bounds (lambda (o x y w h)
	(set o 'x x 'y y 'w w 'h h) o))

(defun dump-tree (view &optional indent)
	(when view
		(setd indent 0)
		(print (pad "" indent) (mcall View 'get_bounds view))
		(each (lambda (view)
			(dump-tree view (+ 4 indent))) (mcall View 'get_children view))))

(defun free-tree (node)
	(when node
		(each free-tree (mcall Node 'get_children node))
		(mcall Node 'deinit node)))

;initialize pipe details and command args, abort on error
(when (defq slave (create-slave))
	(defq p (mcall View 'create)
		c1 (mcall View 'create)
		c2 (mcall View 'create)
		gc1 (mcall View 'create)
		gc2 (mcall View 'create)
		gc3 (mcall View 'create)
		gc4 (mcall View 'create))
	(mcall View 'add_child p c1)
	(mcall View 'add_child p c2)
	(mcall View 'add_child c1 gc1)
	(mcall View 'add_child c1 gc2)
	(mcall View 'add_child c2 gc3)
	(mcall View 'add_child c2 gc4)
	(mcall View 'set_bounds c1 1 2 3 4)
	(mcall View 'set_bounds c2 2 3 4 5)
	(mcall View 'set_bounds gc1 3 4 5 6)
	(mcall View 'set_bounds gc2 4 5 6 7)
	(mcall View 'set_bounds gc3 5 6 7 8)
	(mcall View 'set_bounds gc4 6 7 8 9)
	(dump-tree p)
	(free-tree p)
)
