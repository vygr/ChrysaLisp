;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The Network Map's own workings, with nothing of the window or the
; drawing in them, so that they can be tested with no desktop,
; tests/system/test_netmap.lisp.
;
; The nodes are a map, a node id to what is known of it: :pos and :vel,
; three reals each, :system, the id of its machine, :tasks, :busy, how
; hard it works, with :idle and :time that it is worked out from, and :links,
; each (peer_node_id bytes_sent) as the node itself said. The links are a
; map made from those, links, each with its two ends :a and :b, :sent,
; :flow, :heat and :bar. These are worked on where they are, in the names
; the app has them by: global_tasks, links, top_rate, zoom and changed.
; (make-bar) is the app's, the thing a link is drawn with.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/vector.inc")

(defq
	;the bedspring. How hard nodes push apart, how stiff a link is and how
	;long it would be, how much a busy link is more, the pull to the
	;middle, what is lost each step, and the step
	+k_push (n2r 0.02) +k_spring (n2r 4.0) +rest (n2r 0.5) +k_heat (n2r 1.0)
	+k_middle (n2r 0.3) +damp (n2r 0.85) +dt (n2r 0.05)
	;the bytes a link carries between polls that is no more than idle
	+quiet 4096
	;how much of the way a link's flow goes, each poll, to what it carried
	;in that poll. A fifth, at 4 polls a second, is a second or so
	+ease (n2r 0.2)
	;how far out the furthest node is drawn. The window is 2 from the
	;middle to an edge, where the middle of it all is
	+fit (n2r 1.65))

(defun machine-color (system)
	; (machine-color system) -> (r g b)
	;the color of a machine, from its id, so it is the same color on every
	;desktop and every time. The id picks where round the colors it is, and
	;it is kept bright, and not too deep, it is whitened by how hard it works
	(memoize system (progn
		(defq h (if (< (length system) +node_id_size) 200
				(% (logand (logxor (get-long system 0) (>>> (get-long system 0) 17)
					(get-long system 8) (>>> (get-long system 8) 29)) 0x7fffffff) 360))
			f (/ (n2r (% h 60)) (const (n2r 60)))
			lo (const (n2r 0.25)) up (+ lo (* f (const (n2r 0.75))))
			down (- +real_1 (* f (const (n2r 0.75)))))
		(case (/ h 60)
			(0 (list +real_1 up lo))
			(1 (list down +real_1 lo))
			(2 (list lo +real_1 up))
			(3 (list lo down +real_1))
			(4 (list up lo +real_1))
			(:t (list +real_1 lo down)))) 31))

(defun node-busy (node idle time)
	; (node-busy node idle time) -> node
	;how hard a node works, :busy, none to all of it. A task count does
	;not say, most tasks are asleep, and one that is flat out is one. It
	;is what of the time since it was last asked the node was not idle,
	;by its own clock, and half of what that was before, so it does not
	;flicker. A node that gives no time, time of 0, is left as it is
	(defq span (- time (get :time node)))
	(when (and (/= (get :time node) 0) (> span 0))
		(def node :busy (* +real_1/2 (+ (get :busy node)
			(/ (n2r (max 0 (min span (- span (- idle (get :idle node)))))) (n2r span))))))
	(def node :idle idle :time time)
	node)

(defun rnd ()
	;somewhere between -1/2 and 1/2
	(/ (n2r (- (random 1000) 500)) (const (n2r 1000))))

(defun links-gather ()
	;the links there are, from what each end says, each once, with how
	;much mail both ends have put on it. How fast that is growing, taken
	;over a second or so, is its flow, and its flow set against the most
	;there has been of late is how hot it is. So heat comes up and goes
	;down smoothly, a flow that lasts is hotter than a burst, and the
	;bedspring is not tugged about
	(defq seen (Fmap 31) now (pii-time))
	(. global_tasks :each (lambda (key node)
		(each (lambda ((peer sent))
			(when (. global_tasks :find peer)
				(defq name (if (< (cmp key peer) 0) (cat key peer) (cat peer key)))
				(. seen :insert name (+ sent (ifn (. seen :find name) 0)))))
			(get :links node))))
	(defq old links)
	(setq links (Fmap 31))
	(. seen :each (lambda (name sent)
		(unless (defq link (. old :find name))
			(setq changed :t)
			(def (setq link (env 1)) :a (slice name 0 +node_id_size) :b (slice name +node_id_size -1)
				:sent sent :flow +real_0 :heat +real_0
				:bar (make-bar)))
		;the most there has been is of what a poll carried, not of the
		;flow, or the busiest link would be white the moment it began
		(defq rate (n2r (max 0 (- sent (get :sent link)))) flow (get :flow link)
			flow (+ flow (* (- rate flow) +ease)))
		(setq top_rate (max top_rate rate))
		(def link :sent sent :flow flow :heat (/ flow top_rate))
		(. links :insert name link)))
	(if (/= (. links :size) (. old :size)) (setq changed :t))
	;the busiest fades, so a burst long gone does not leave all else cold
	;but not to nothing, or the pings of an idle network would be hot
	(setq top_rate (max (const (n2r +quiet)) (* top_rate (const (n2r 0.97))))))

(defun spring-step ()
	;a step of the bedspring
	(defq nodes (list) forces (list))
	(. global_tasks :each (lambda (key node) (push nodes node)
		(push forces (nums-scale (get :pos node) (neg +k_middle)))))
	;every node pushes every other away
	(each! (lambda (node force)
		(defq i (!) p (get :pos node))
		(each! (lambda (node1 force1)
			(defq d (nums-sub p (get :pos node1)) d2 (+ (nums-dot d d) (const (n2r 0.0001)))
				f (nums-scale d (/ +k_push (* d2 (sqrt d2)))))
			(nums-add force f force)
			(nums-sub force1 f force1))
			(list nodes forces) (inc i)))
		(list nodes forces))
	;every link pulls its two ends to the length it would be, a hot one
	;harder and to a shorter length
	(. links :each (lambda (name link)
		(when (and (defq a (. global_tasks :find (get :a link)))
				(defq b (. global_tasks :find (get :b link))))
			(defq d (nums-sub (get :pos b) (get :pos a)) len (sqrt (+ (nums-dot d d) (const (n2r 0.0001))))
				warm (+ +real_1 (* +k_heat (get :heat link)))
				f (nums-scale d (/ (* +k_spring warm (- len (/ +rest warm))) len))
				fa (elem-get forces (find a nodes)) fb (elem-get forces (find b nodes)))
			(nums-add fa f fa)
			(nums-sub fb f fb))))
	(defq reach (const (n2r 0.01)) middle (reals +real_0 +real_0 +real_0))
	(each (lambda (node force)
		(defq vel (get :vel node) pos (get :pos node))
		(nums-scale (nums-add vel (nums-scale force +dt) vel) +damp vel)
		(nums-add pos (nums-scale vel +dt) pos)
		(nums-add middle pos middle))
		nodes forces)
	;kept about its own middle, it does not drift off
	(when (nempty? nodes)
		(nums-scale middle (/ +real_1 (n2r (length nodes))) middle)
		(each (lambda (node)
			(defq pos (get :pos node))
			(nums-sub pos middle pos)
			(setq reach (max reach (nums-dot pos pos)))) nodes))
	;how big it all is, so that it can be drawn to fit, eased
	(setq zoom (+ zoom (* (- (/ +fit (sqrt reach)) zoom) (const (n2r 0.1))))))
