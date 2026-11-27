;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a graph of places and the links between them, and the route of least
; cost from one to another, A*: the junctions of tunnels, the stops of a
; tour, the cells of a map. A graph is (places links), places a list of
; vec3 doubles, a node its number in it, links a list of a nums each of
; the nodes it links to. What a link costs is asked as the route is
; found, of a function given, so a link can cost more now, or be shut,
; :nil; by default it is the distance. Nothing here knows of tasks: a
; graph is data, and goes in a message as it is
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/consts/numbers.inc")

;module
(env-push)

(defun graph ()
	; (graph) -> graph
	;a graph with no places
	(list (list) (list)))

(defun graph-node (g at)
	; (graph-node g at) -> node
	;a place added, a vec3, and its node
	(bind '(places links) g)
	(push places at)
	(push links (nums))
	(dec (length places)))

(defun graph-link (g a b &optional one_way)
	; (graph-link g a b [one_way]) -> g
	;a linked to b, and b to a unless one way
	(bind '(& links) g)
	(unless (find b (elem-get links a)) (push (elem-get links a) b))
	(and (not one_way) (not (find a (elem-get links b))) (push (elem-get links b) a))
	g)

(defun graph-unlink (g a b &optional one_way)
	; (graph-unlink g a b [one_way]) -> g
	;a no longer linked to b, nor b to a unless one way
	(bind '(& links) g)
	(defq l (elem-get links a))
	(if (defq i (find b l)) (elem-set links a (cat (slice l 0 i) (slice l (inc i) -1))))
	(unless one_way
		(setq l (elem-get links b))
		(if (defq i (find a l)) (elem-set links b (cat (slice l 0 i) (slice l (inc i) -1)))))
	g)

(defun graph-place (g node)
	; (graph-place g node) -> doubles
	(elem-get (first g) node))

(defun graph-links (g node)
	; (graph-links g node) -> nums
	(elem-get (second g) node))

(defun graph-size (g)
	; (graph-size g) -> num
	(length (first g)))

(defun graph-places (g nodes)
	; (graph-places g nodes) -> (doubles ...)
	;the places of a route
	(map (# (elem-get (first g) %0)) nodes))

(defun graph-distance (a b)
	; (graph-distance a b) -> double
	;how far apart two places are
	(sqrt (nums-dot (defq d (nums-sub b a)) d)))

(defun graph-nearest (g at &optional ok_fn)
	; (graph-nearest g at [ok_fn]) -> :nil | node
	;the node nearest a place, of those (ok_fn g node) says will do
	(defq best :nil best_dd +double_0)
	(each (lambda (p)
			(defq dd (nums-dot (defq d (nums-sub p at)) d))
			(and (or (not best) (< dd best_dd)) (or (not ok_fn) (ok_fn g (!)))
				(setq best (!) best_dd dd)))
		(first g))
	best)

;the open set of a search, a binary heap of the nodes by their cost and
;the cost on to the end, each (f node)
(defun heap-push (heap f node)
	; (heap-push heap f node) -> heap
	(push heap (list f node))
	(defq i (dec (length heap)))
	(while (and (> i 0) (< f (first (elem-get heap (defq up (/ (dec i) 2))))))
		(defq e (elem-get heap up))
		(elem-set heap up (elem-get heap i))
		(elem-set heap i e)
		(setq i up))
	heap)

(defun heap-pop (heap)
	; (heap-pop heap) -> (f node)
	;the least, the heap not empty
	(defq top (first heap) e (pop heap) n (length heap) i (if (> n 0) 0))
	(if i (elem-set heap 0 e))
	(while i
		(defq l (inc (* i 2)) r (inc l) m i)
		(and (< l n) (< (first (elem-get heap l)) (first (elem-get heap m))) (setq m l))
		(and (< r n) (< (first (elem-get heap r)) (first (elem-get heap m))) (setq m r))
		(cond
			((= m i) (setq i :nil))
			(:t (elem-set heap i (elem-get heap m))
				(elem-set heap m e)
				(setq i m))))
	top)

(defun graph-route (g from to &optional cost_fn)
	; (graph-route g from to [cost_fn]) -> :nil | (node ...)
	;the route of least cost from node from to node to, its nodes, from
	;first; :nil if there is none. (cost_fn g a b) -> :nil | double, what
	;the link from a to b costs now, :nil shut; the distance if none given.
	;The cost on to the end is guessed by the distance, so a cost never
	;less than the distance finds the least; less, a route still, near it
	(defq places (first g) links (second g) n (length places)
		end (elem-get places to) cost (list) came (list) done (list) heap (list) out :nil)
	(times n (push cost :nil) (push came :nil) (push done :nil))
	(elem-set cost from +double_0)
	(heap-push heap (graph-distance (elem-get places from) end) from)
	(while (and (not out) (nempty? heap))
		(defq node (second (heap-pop heap)))
		(cond
			((elem-get done node))
			((= node to)
				(setq out (list to))
				(while (defq c (elem-get came (last out))) (push out c))
				(setq out (reverse out)))
			(:t (elem-set done node :t)
				(defq here (elem-get cost node) at (elem-get places node))
				(each (lambda (next)
						(unless (elem-get done next)
							(defq step (if cost_fn (cost_fn g node next) (graph-distance at (elem-get places next))))
							(when step
								(defq c (+ here step) was (elem-get cost next))
								(when (or (not was) (< c was))
									(elem-set cost next c)
									(elem-set came next node)
									(heap-push heap (+ c (graph-distance (elem-get places next) end)) next)))))
					(elem-get links node)))))
	out)

;module
(export-symbols
	'(graph graph-node graph-link graph-unlink graph-place graph-links graph-size
	graph-places graph-distance graph-nearest graph-route))
(env-pop)
