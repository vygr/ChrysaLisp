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

(import "././collections/collections.inc")
(import "././math/polygon.inc")
(import "./comps.inc")

(defun rostrum-picture (m cx cy focus p)
	; (rostrum-picture m cx cy focus p) -> path
	;a new path of the points of a path of a figure in the picture, by its
	;matrix to space and the camera
	(bind '(a b & tx d e & ty g h & tz) m)
	(cond
		((not focus)
			(mat3x2-vec2-mul (fixeds a b tx d e ty) p (cat p)))
		((and (= g 0.0) (= h 0.0))
			;it faces the camera, all of it is the one way off
			(defq k (/ focus (+ focus tz)))
			(mat3x2-vec2-mul (fixeds (* a k) (* b k) (+ cx (* (- tx cx) k))
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
	;the one number of two figures, which ever way round, by the order the
	;rostrum first came to them in, the same each time a scene is run
	(defq ia (get :fig_turn a) ib (get :fig_turn b))
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
	(when view (each (# (mat3x2-vec2-mul view %0 %0)) paths))
	paths)

(defq +rostrum_bands 16)

(defun rostrum-fill (canvas col col2 rule polys)
	; (rostrum-fill canvas col col2 rule polys)
	;polygons of the picture filled in a colour, or if there is a second
	;going from the one at the top to the other at the bottom, in bands of
	;rows that the canvas is clipped to in turn
	(cond
		((= col2 0)
			(.-> canvas (:set_color col) (:fpoly 0.0 0.0 rule polys)))
		(:t (bind '((& y) (& y1)) (vector-bounds-2d polys))
			(bind '(cx cy cx1 cy1) (. canvas :get_clip))
			(defq h (/ (- y1 y) (n2f +rostrum_bands)) i 0 top (n2i y))
			(while (< i +rostrum_bands)
				(defq foot (if (= i (dec +rostrum_bands)) (inc (n2i y1)) (n2i (+ y (* h (n2f (inc i)))))))
				(and (< top foot) (< (max top cy) (min foot cy1))
					(.-> canvas (:set_clip cx (max top cy) cx1 (min foot cy1))
						(:set_color (motion-mix col col2 (/ (+ (n2f i) 0.5) (n2f +rostrum_bands))))
						(:fpoly 0.0 0.0 rule polys)))
				(setq top foot i (inc i)))
			(. canvas :set_clip cx cy cx1 cy1))))

(defun rostrum-figure (canvas node m alpha cx cy focus view glow)
	; (rostrum-figure canvas node m alpha cx cy focus view glow)
	;one figure drawn by a matrix to space and an alpha: its halo if glow,
	;the widest first, its fill, then its stroke. A colour with no alpha is
	;there to be met, not seen
	(bind '(fills strokes &ignore) (. node :flat))
	(and glow (/= 0 (logand (defq col (get :fig_glow node)) 0xff000000)) (> (get :fig_glow_size node) 0.0)
		(each (# (.-> canvas (:set_color (rostrum-faded col alpha))
					(:fpoly 0.0 0.0 +winding_none_zero (rostrum-view view (rostrum-paths m cx cy focus %0)))))
			(. node :glow)))
	(and (nempty? fills) (/= 0 (logand (defq col (get :fig_fill node)) 0xff000000))
		(rostrum-fill canvas (rostrum-faded col alpha)
			(if (/= 0 (defq col2 (get :fig_fill2 node))) (rostrum-faded col2 alpha) 0)
			(get :fig_rule node) (rostrum-view view (rostrum-paths m cx cy focus fills))))
	(and (nempty? strokes) (/= 0 (logand (defq col (get :fig_stroke node)) 0xff000000))
		(.-> canvas (:set_color (rostrum-faded col alpha))
			(:fpoly 0.0 0.0 +winding_none_zero (rostrum-view view (rostrum-paths m cx cy focus strokes))))))

(defun rostrum-mover (rostrum mover)
	; (rostrum-mover rostrum mover) -> paths
	;the polygons of one the sweep has come to, worked out the first time
	(unless (elem-get mover 3)
		(elem-set mover 3 (. rostrum :polygons (second mover) :t)))
	(elem-get mover 3))

(defun rostrum-place-one (node world alpha)
	; (rostrum-place-one node world alpha) -> world
	;by the world and alpha of its parent. Its alpha each time, its world only
	;if it or what it is placed by has changed
	(def node :fig_world_alpha (* alpha (get :fig_alpha node)))
	(cond
		((and (get :fig_local node) (eql world (get :fig_world_by node)))
			(get :fig_world node))
		(:t (defq out (if world (mat3x4-mul world (. node :local)) (. node :local)))
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

;the step a rostrum goes on by, :advance takes as many as the time owes
(defq +ros_step (/ 1.0 60.0)
	;what a copy of a figure shares and does not make anew: its class, and
	;what is never changed in place, a new outline is a new list
	+ros_shared ''(:vtable :fig_outline :fig_flat :fig_glow_flat))

(defun rostrum-kind (x)
	; (rostrum-kind x) -> :env | :fmap | :list | :nil
	;what a copy of a scene makes anew: figures, components and the like,
	;maps, and lists that are not code. The rest it shares
	(cond
		((not (list? x)) :nil)
		((Fmap? x) :fmap)
		((find :hmap (type-of x)) :env)
		((and (nempty? x) (func? (first x))) :nil)
		(:list)))

(defclass Rostrum (width height &optional focus seed) (Figure "rostrum")
	; (Rostrum width height [focus seed]) -> rostrum
	;a picture of that size, its top left at 0 0. focus is how far the camera
	;is from z 0, :nil for none, and things at z 0 are their own size. seed
	;starts its own random numbers, (:random), so that a scene made again
	;with the same seed and gone on by the same steps is the same scene
	(def this :ros_width (n2f width) :ros_height (n2f height) :ros_focus (if focus (n2f focus))
		:ros_speed 1.0 :ros_by_depth :nil :ros_time 0.0 :ros_touching (Fmap 31) :ros_order (list) :ros_ticks 0
		:ros_owed 0.0 :ros_turns 0 :ros_seed (logand (ifn seed 1) 0x7fffffffffffffff))

	(defmethod :random (n)
		; (. rostrum :random n) -> 0 to n - 1
		;the rostrum's own random numbers, from its seed, so a scene that
		;takes all of its from here is the same each time it is run
		(defq seed (+ (* (get :ros_seed this) 6364136223846793005) 1442695040888963407))
		(def this :ros_seed seed)
		(% (>> seed 33) n))

	(defmethod :copy ()
		; (. rostrum :copy) -> rostrum
		;all of it as it is now, made anew: each figure, each of its
		;components, and what the scene keeps on them, that refer to each
		;other as they did, so the copy goes on as this would have and this
		;is not changed by it. Kept every so often it is a place to seek
		;from. A weak link a component keeps, Tie's, is to what it was
		(defq made (Fmap 101) todo (list this) olds (list))
		;first each thing it holds is made, once, however many hold it
		(while (defq x (pop todo))
			(unless (. made :find (defq key (weak-ref x)))
				(defq kind (rostrum-kind x))
				(. made :insert key (case kind
					(:env (env-copy x 31))
					(:fmap (Fmap 31))
					(:t (cat x))))
				(push olds x)
				(case kind
					(:env (each (lambda ((k v))
							(and (not (find k +ros_shared)) (rostrum-kind v) (push todo v)))
						(tolist x)))
					(:fmap (. x :each (lambda (k v) (if (rostrum-kind v) (push todo v)))))
					(:t (each (# (if (rostrum-kind %0) (push todo %0))) x)))))
		;then what each new one holds is the new one of what it held
		(each (lambda (x)
				(defq new (. made :find (weak-ref x)))
				(case (rostrum-kind x)
					(:env (each (lambda ((k v))
							(and (not (find k +ros_shared)) (rostrum-kind v)
								(def new k (. made :find (weak-ref v)))))
						(tolist x)))
					(:fmap (. x :each (lambda (k v)
						(. new :insert k (if (rostrum-kind v) (. made :find (weak-ref v)) v)))))
					(:t (each (lambda (v)
							(if (rostrum-kind v) (elem-set new (!) (. made :find (weak-ref v)))))
						x))))
			olds)
		;and each child's weak link up is to its new parent
		(defq top (. made :find (weak-ref this)))
		(. top :each_node (lambda (node)
			(each (# (. %0 :set_parent node)) (get :node_children node))))
		top)

	(defmethod :advance (secs)
		; (. rostrum :advance secs) -> rostrum
		;secs seconds of the wall clock have gone, times :ros_speed: as many
		;steps of +ros_step as that comes to are taken, what is left over
		;is owed to the next. A scene goes on by the same steps however fast
		;it is drawn, so it can be run again to any step
		(defq owed (+ (get :ros_owed this) (* (n2f secs) (get :ros_speed this))))
		(while (>= owed +ros_step)
			(setq owed (- owed +ros_step))
			(. this :tick +ros_step))
		(def this :ros_owed owed)
		this)

	(defmethod :tick (dt)
		; (. rostrum :tick dt) -> rostrum
		;dt seconds have gone, one step
		(setq dt (n2f dt))
		(def this :ros_time (+ (get :ros_time this) dt))
		(defq order (list) todo (reverse (cat (get :node_children this))))
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
				;its number in this rostrum, by when it was first come to
				(unless (get :fig_turn node)
					(defq turn (inc (get :ros_turns this)))
					(def this :ros_turns turn)
					(def node :fig_turn turn))
				(. node :update dt)
				(rostrum-place node)
				(push order node)
				(defq kids (get :node_children node) i (length kids))
				(while (> i 0) (push todo (elem-get kids (setq i (dec i)))))))
		(def this :ros_order order)
		;the places of those that leave a trail, so many of the last
		(each (lambda (node)
				(when (> (defq n (get :fig_trail node)) 0)
					(defq was (ifn (get :fig_trail_was node) (list)))
					(push was (get :fig_world node))
					(def node :fig_trail_was (if (> (length was) n) (slice was (- (length was) n) -1) was))))
			order)
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

	(defmethod :polygons (node &optional flat)
		; (. rostrum :polygons figure [flat]) -> paths
		;all that it is drawn with in the picture, what is under it too, as
		;the camera has it, or with flat as it is with no focus
		(raise :ros_width :ros_height :ros_focus)
		(defq cx (* ros_width 0.5) cy (* ros_height 0.5) out (list))
		(if flat (setq ros_focus :nil))
		(. node :each_node (lambda (n)
				(bind '(fills strokes &ignore) (. n :flat))
				(when (defq m (get :fig_world n))
					(each (# (push out (rostrum-picture m cx cy ros_focus %0))) fills)
					(each (# (push out (rostrum-picture m cx cy ros_focus %0))) strokes))))
		out)

	(defmethod :box (node &optional flat)
		; (. rostrum :box figure [flat]) -> :nil | (x y x1 y1)
		;the box in the picture round all it is drawn with, what is under it
		;too, as the camera has it, or with flat as it is with no focus. A
		;tilted figure's is by the corners of its box
		(raise :ros_width :ros_height :ros_focus)
		(defq cx (* ros_width 0.5) cy (* ros_height 0.5) out :nil)
		(if flat (setq ros_focus :nil))
		(. node :each_node (lambda (n)
				(bind '(& & box) (. n :flat))
				(defq m (get :fig_world n))
				(when (and box m)
					(bind '(x y x1 y1) box)
					(defq c (rostrum-picture m cx cy ros_focus (path x y x1 y x1 y1 x y1))
						b (list (min (elem-get c 0) (elem-get c 2) (elem-get c 4) (elem-get c 6))
							(min (elem-get c 1) (elem-get c 3) (elem-get c 5) (elem-get c 7))
							(max (elem-get c 0) (elem-get c 2) (elem-get c 4) (elem-get c 6))
							(max (elem-get c 1) (elem-get c 3) (elem-get c 5) (elem-get c 7))))
					(setq out (if out (vector-box-join-2d out b) b)))))
		out)

	(defmethod :collide (order)
		; (. rostrum :collide order) -> rostrum
		;who has come to touch whom, who still does, and who no longer does,
		;and what they fire. Asked flat, with no focus: the focus is the
		;camera's, how the scene is seen, and what the scene does is the
		;same whichever, so a run sought to is the run that was seen
		(defq movers (list) old (get :ros_touching this) now (Fmap 31))
		(each (lambda (node)
				(and (not (get :fig_dead node))
					(or (/= 0 (get :fig_kinds node)) (/= 0 (get :fig_meets node)))
					(defq box (. this :box node :t))
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
					(defq m (get :fig_world node) alpha (get :fig_world_alpha node))
					(when (and m (> alpha 0.0))
						;the places it was, the oldest and faintest first, with no halo
						(when (defq was (get :fig_trail_was node))
							(defq n (n2f (inc (length was))))
							(each (# (rostrum-figure canvas node %0 (* alpha 0.5 (/ (n2f (inc (!))) n))
									cx cy ros_focus view :nil))
								was))
						(rostrum-figure canvas node m alpha cx cy ros_focus view :t))))
			ros_order)
		this))
