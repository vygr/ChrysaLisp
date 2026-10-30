;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/motion/rostrum.inc - a scene of figures, and the camera over it
;
; A rostrum is the top of a tree of figures, lib/motion/figure.inc, with a
; size for its picture and a camera. Each tick, by how long it has been since
; the last and never by how many frames there have been:
;
;	every figure's components have their turn, a parent before its children
;	every figure is worked out where it is, in the space of the picture
;	those that meet one another are found, and what they fire is fired
;	those told to go are taken out, and what they fire is fired
;
; The camera looks down z at the middle of the picture. With a focus, things
; further off, of more z, are smaller, and a tilted figure tapers. With none
; a tilted figure is only narrower. A figure facing it is one matrix either
; way, in native code. A figure tilted with a focus is a divide for each point.
;
; Whether two figures meet is asked in the picture, as they look. Their kinds
; and meets are bit masks, a figure looks out for those whose kinds are in its
; meets. The broad phase is a sweep along x of the boxes round them, then the
; boxes themselves, then (poly-hit?) of their polygons. A figure with
; children has all of theirs as well as its own.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/collections/collections.inc")
(import "lib/math/polygon.inc")
(import "./comps.inc")

(defun rostrum-picture (m cx cy focus p)
	; (rostrum-picture m cx cy focus p) -> path
	;a new path of the points of a path of a figure in the picture, by its
	;matrix to space and the camera
	(bind '(a b & tx d e & ty g h & tz) m)
	(cond
		((not focus)
			(path-transform (fixeds a b tx d e ty) p (cat p)))
		((and (= g 0.0) (= h 0.0))
			;it faces the camera, all of it is the one way off
			(defq k (/ focus (+ focus tz)))
			(path-transform (fixeds (* a k) (* b k) (+ cx (* (- tx cx) k))
				(* d k) (* e k) (+ cy (* (- ty cy) k))) p (cat p)))
		(:t (defq out (path) i 0 n (length p))
			(while (< i n)
				(defq x (elem-get p i) y (elem-get p (inc i))
					k (/ focus (max 1.0 (+ focus (* g x) (* h y) tz))))
				(push out (+ cx (* (- (+ (* a x) (* b y) tx) cx) k))
					(+ cy (* (- (+ (* d x) (* e y) ty) cy) k)))
				(setq i (+ i 2)))
			out)))

(defun rostrum-paths (m cx cy focus paths)
	; (rostrum-paths m cx cy focus paths) -> paths
	(map (# (rostrum-picture m cx cy focus %0)) paths))

(defun rostrum-pair (a b)
	; (rostrum-pair a b) -> num
	;the one number of two figures, which ever way round
	(defq ia (get :fig_id a) ib (get :fig_id b))
	(+ (<< (min ia ib) 24) (max ia ib)))

(defun rostrum-fire (this other what)
	; (rostrum-fire this other what)
	;what a figure is to do, if it looks out for the other and is not going
	(unless (get :fig_dead this)
		(and (/= 0 (logand (get :fig_meets this) (get :fig_kinds other)))
			(defq fnc (get what this))
			(fnc this other))))

(defun rostrum-faded (col alpha)
	; (rostrum-faded col alpha) -> col
	;a colour with its alpha times that
	(if (>= alpha 1.0) col
		(logior (logand col 0xffffff)
			(<< (n2i (* alpha (n2f (logand (>> col 24) 0xff)))) 24))))

(defun rostrum-view (view paths)
	; (rostrum-view view paths) -> paths
	;the paths moved by the view, which are new ones and can be moved in place
	(when view (each (# (path-transform view %0 %0)) paths))
	paths)

(defun rostrum-mover (rostrum mover)
	; (rostrum-mover rostrum mover) -> paths
	;the polygons of one the sweep has come to, worked out the first time
	(unless (elem-get mover 3)
		(elem-set mover 3 (. rostrum :polygons (second mover))))
	(elem-get mover 3))

(defun rostrum-place-one (node world alpha)
	; (rostrum-place-one node world alpha) -> world
	;by the world and alpha of its parent. Its alpha each time, its world only
	;if it or what it is placed by has changed
	(def node :fig_world_alpha (* alpha (get :fig_alpha node)))
	(cond
		((and (get :fig_local node) (eql world (get :fig_world_by node)))
			(get :fig_world node))
		(:t (defq out (if world (Mat3x4-mul world (. node :local)) (. node :local)))
			(def node :fig_world out :fig_world_by world)
			out)))

(defun rostrum-place (node)
	; (rostrum-place node) -> node
	;where a figure is, by where its parent is. A parent nothing has worked
	;out, one just made, is worked out first, and those above it
	(defq up (. node :get_parent))
	(cond
		((and up (get :fig_world up))
			(rostrum-place-one node (get :fig_world up) (get :fig_world_alpha up)))
		(:t (defq chain (list node))
			(while (and up (not (get :fig_world up)))
				(push chain up)
				(setq up (. up :get_parent)))
			(defq world (if up (get :fig_world up)) alpha (if up (get :fig_world_alpha up) 1.0))
			(each! (lambda (one)
					(setq world (rostrum-place-one one world alpha) alpha (get :fig_world_alpha one)))
				(list chain) -1 0)))
	node)

(defclass Rostrum (width height &optional focus) (Figure "rostrum")
	; (Rostrum width height [focus]) -> rostrum
	;a picture of that size, its top left at 0 0. focus is how far the camera
	;is from z 0, :nil for none, and things at z 0 are their own size
	(def this :ros_width (n2f width) :ros_height (n2f height) :ros_focus (if focus (n2f focus))
		:ros_speed 1.0 :ros_by_depth :nil :ros_time 0.0 :ros_touching (Fmap 31) :ros_order (list) :ros_ticks 0)

	(defmethod :tick (dt)
		; (. rostrum :tick dt) -> rostrum
		;dt seconds have gone
		(setq dt (* (n2f dt) (get :ros_speed this)))
		(def this :ros_time (+ (get :ros_time this) dt))
		(defq order (list) todo (reverse (cat (get :fig_children this))))
		(. this :update dt)
		(def this :fig_world (const (fixeds 1.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 1.0 0.0))
			:fig_world_alpha (get :fig_alpha this))
		;a parent has its turn and is placed before its children are come to,
		;what is still to come is a stack, and a figure added in its turn
		;has its first the next tick
		;A figure can be moved in its own turn, to a group made that moment,
		;which has been worked out by nothing yet, or to one to come. A
		;figure that has been come to this tick is not done again
		(defq tick_no (inc (get :ros_ticks this)))
		(def this :ros_ticks tick_no)
		(while (defq node (pop todo))
			(unless (eql (get :fig_tick_no node) tick_no)
				(def node :fig_tick_no tick_no)
				(. node :update dt)
				(rostrum-place node)
				(push order node)
				(defq kids (get :fig_children node) i (length kids))
				(while (> i 0) (push todo (elem-get kids (setq i (dec i)))))))
		(def this :ros_order order)
		(. this :collide order)
		;those told to go, by a collision too
		(each (lambda (node)
				(when (get :fig_dead node)
					(. node :sub_node)
					(when (defq death (get :fig_on_death node))
						(def node :fig_on_death :nil)
						(death node))))
			order)
		this)

	(defmethod :polygons (node)
		; (. rostrum :polygons figure) -> paths
		;all that it is drawn with in the picture, what is under it too
		(raise :ros_width :ros_height :ros_focus)
		(defq cx (* ros_width 0.5) cy (* ros_height 0.5) out (list) todo (list node))
		(while (defq n (pop todo))
			(bind '(fills strokes &ignore) (. n :flat))
			(defq m (get :fig_world n))
			(when m
				(each (# (push out (rostrum-picture m cx cy ros_focus %0))) fills)
				(each (# (push out (rostrum-picture m cx cy ros_focus %0))) strokes))
			(each (# (push todo %0)) (get :fig_children n)))
		out)

	(defmethod :box (node)
		; (. rostrum :box figure) -> :nil | (x y x1 y1)
		;the box in the picture round all it is drawn with, what is under it
		;too. A tilted figure's is by the corners of its box
		(raise :ros_width :ros_height :ros_focus)
		(defq cx (* ros_width 0.5) cy (* ros_height 0.5) out :nil todo (list node))
		(while (defq n (pop todo))
			(bind '(& & box) (. n :flat))
			(defq m (get :fig_world n))
			(when (and box m)
				(bind '(x y x1 y1) box)
				(defq c (rostrum-picture m cx cy ros_focus (path x y x1 y x1 y1 x y1))
					b (list (min (elem-get c 0) (elem-get c 2) (elem-get c 4) (elem-get c 6))
						(min (elem-get c 1) (elem-get c 3) (elem-get c 5) (elem-get c 7))
						(max (elem-get c 0) (elem-get c 2) (elem-get c 4) (elem-get c 6))
						(max (elem-get c 1) (elem-get c 3) (elem-get c 5) (elem-get c 7))))
				(setq out (if out (vector-box-join-2d out b) b)))
			(each (# (push todo %0)) (get :fig_children n)))
		out)

	(defmethod :collide (order)
		; (. rostrum :collide order) -> rostrum
		;who has come to touch whom, who still does, and who no longer does,
		;and what they fire
		(defq movers (list) old (get :ros_touching this) now (Fmap 31))
		(each (lambda (node)
				(and (not (get :fig_dead node))
					(or (/= 0 (get :fig_kinds node)) (/= 0 (get :fig_meets node)))
					(defq box (. this :box node))
					(push movers (list (first box) node box :nil))))
			order)
		;the sweep along x, those whose boxes are still open are the only ones
		;that can meet the next
		(sort movers (lambda (a b) (cond ((< (first a) (first b)) -1) ((> (first a) (first b)) 1) (0))))
		(defq open (list))
		(each (lambda (mover)
				(bind '(x node box &ignore) mover)
				(setq open (filter (# (>= (elem-get (third %0) 2) x)) open))
				(each (lambda (other)
						(defq b (second other))
						(and (or (/= 0 (logand (get :fig_meets node) (get :fig_kinds b)))
								(/= 0 (logand (get :fig_meets b) (get :fig_kinds node))))
							(vector-collide-boxes-2d box (third other))
							(poly-hit? (rostrum-mover this mover) (rostrum-mover this other))
							(. now :insert (rostrum-pair node b) (list node b))))
					open)
				(push open mover))
			movers)
		;those that were and are not, those that are and were not, and those
		;that still are. A figure told to go has no more to do with this
		(. old :each (lambda (key pair)
			(unless (. now :find key)
				(bind '(a b) pair)
				(rostrum-fire a b :fig_on_leave)
				(rostrum-fire b a :fig_on_leave))))
		(. now :each (lambda (key pair)
			(bind '(a b) pair)
			(defq what (if (. old :find key) :fig_on_stay :fig_on_enter))
			(rostrum-fire a b what)
			(rostrum-fire b a what)))
		(def this :ros_touching now)
		this)

	(defmethod :draw (canvas &optional view)
		; (. rostrum :draw canvas [view]) -> rostrum
		;the figures on a canvas, by a 3x2 matrix from the picture to it, as
		;they were worked out by the last tick. By z, the furthest first, if
		;:ros_by_depth is set, else in the order of the tree
		(raise :ros_width :ros_height :ros_focus :ros_by_depth :ros_order)
		(defq cx (* ros_width 0.5) cy (* ros_height 0.5))
		(when ros_by_depth
			(setq ros_order (map (# (second %0))
				(sort (map (# (list (elem-get (get :fig_world %0) 11) %0)) ros_order)
					(lambda (a b) (cond ((> (first a) (first b)) -1) ((< (first a) (first b)) 1) (0)))))))
		(each (lambda (node)
				(unless (get :fig_dead node)
					(bind '(fills strokes &ignore) (. node :flat))
					(defq m (get :fig_world node) alpha (get :fig_world_alpha node) rule (get :fig_rule node))
					(when (and m (> alpha 0.0))
						;a colour with no alpha is there to be met, not seen
						(and (nempty? fills) (/= 0 (logand (get :fig_fill node) 0xff000000))
							(.-> canvas (:set_color (rostrum-faded (get :fig_fill node) alpha))
								(:fpoly 0.0 0.0 rule (rostrum-view view (rostrum-paths m cx cy ros_focus fills)))))
						(and (nempty? strokes) (/= 0 (logand (get :fig_stroke node) 0xff000000))
							(.-> canvas (:set_color (rostrum-faded (get :fig_stroke node) alpha))
								(:fpoly 0.0 0.0 +winding_none_zero
									(rostrum-view view (rostrum-paths m cx cy ros_focus strokes))))))))
			ros_order)
		this)
	)
