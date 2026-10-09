;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/cwb/doc.inc - a whiteboard document, the shapes of a .cwb file
;
; A document is a size, a background, and layers. A layer is a list of
; items, the last on top. An item is a shape or a group of items.
;
; A shape is an SVG path, the d of a <path>, as text: M L H V C S Q T A Z
; and their lower case, (path-gen-paths) in gui/path/lisp.inc reads it. It
; has a fill, a stroke, and a transform, a matrix of 3 by 2, that puts it
; where it is in what holds it. To move, turn or size a shape is to change
; its matrix, the path is as it was drawn. A shape of kind :text has its
; outline from a font and a string in place of a path.
;
; Every item has an id, a number, its own in the document for as long as
; it lives, and kept in the file. It is how a script, a tool or a person
; says which.
;
; A shape is flattened to polygons when it is first drawn, and they are
; kept with it till it is changed. They are not saved.
;
; A file is the tree of this, (tree-save), text that can be read and
; written by hand, see (cwb-save). docs/ai_digest/whiteboard.md.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;this is under the canvas, which loads a .cwb as an image by it, so it
;takes what a canvas takes and not the canvas, lib/image/cwb.inc
(import "gui/path/lisp.inc")
(import "gui/canvas/struct.inc")
(import "gui/font/lisp.inc")
(import "lib/math/vector.inc")
(import "lib/collections/collections.inc")

(defq +cwb_version 4)

;the slots of an item, a list. None is named size, an enum has a size
;of its own by that name. The first few are of both kinds, then those
;of a shape, the items of a group, and what is kept and not saved
(enums +cwb 0
	(enum type id name m props)
	(enum kind d fill stroke width rule join cap1 cap2 text font font_size)
	(enum items)
	(enum cache))

;the name of each slot in a file, and what a slot has when the file does
;not say. A list quoted twice, a + constant that is a list is put in place
;of its name as code is read
(defq +cwb_keys ''(:type :id :name :m :props
		:kind :d :fill :stroke :width :rule :join :cap1 :cap2 :text :font :font_size
		:items :cache)
	+cwb_defaults ''(:shape 0 "" :nil :nil
		:path "" 0 0xff000000 2.0 :nonzero :round :round :round "" "fonts/OpenSans-Regular.ctf" 24
		:nil :nil))

;;;;;;;;;;
; matrices
;;;;;;;;;;

;a matrix is six numbers, a b tx c d ty: x' = ax + by + tx, y' = cx + dy + ty,
;as (path-transform) takes it. :nil where a matrix can be is no change

(defun cwb-mat (&optional a b tx c d ty)
	; (cwb-mat [a b tx c d ty]) -> matrix
	(fixeds (ifn a 1.0) (ifn b 0.0) (ifn tx 0.0) (ifn c 0.0) (ifn d 1.0) (ifn ty 0.0)))

(defun cwb-mat-mul (ma mb)
	; (cwb-mat-mul ma mb) -> matrix | :nil
	;the one matrix that does what mb does and then what ma does
	(cond
		((not ma) mb)
		((not mb) ma)
		(:t (bind '(a0 a1 a2 a3 a4 a5) ma)
			(bind '(b0 b1 b2 b3 b4 b5) mb)
			(fixeds (+ (* a0 b0) (* a1 b3)) (+ (* a0 b1) (* a1 b4)) (+ (* a0 b2) (* a1 b5) a2)
				(+ (* a3 b0) (* a4 b3)) (+ (* a3 b1) (* a4 b4)) (+ (* a3 b2) (* a4 b5) a5)))))

(defun cwb-mat-move (tx ty)
	; (cwb-mat-move tx ty) -> matrix
	(fixeds 1.0 0.0 (n2f tx) 0.0 1.0 (n2f ty)))

(defun cwb-mat-scale (sx &optional sy cx cy)
	; (cwb-mat-scale sx [sy cx cy]) -> matrix
	;about a point, default the origin
	(setd sy sx cx 0.0 cy 0.0)
	(setq sx (n2f sx) sy (n2f sy) cx (n2f cx) cy (n2f cy))
	(fixeds sx 0.0 (- cx (* sx cx)) 0.0 sy (- cy (* sy cy))))

(defun cwb-mat-turn (angle &optional cx cy)
	; (cwb-mat-turn angle [cx cy]) -> matrix
	;by an angle in radians, the way from the x axis to the y axis, about
	;a point, default the origin
	(setd cx 0.0 cy 0.0)
	(setq cx (n2f cx) cy (n2f cy))
	(defq c (cos (n2f angle)) s (sin (n2f angle)))
	(fixeds c (neg s) (- cx (- (* c cx) (* s cy))) s c (- cy (+ (* s cx) (* c cy)))))

(defun cwb-mat-invert (m)
	; (cwb-mat-invert m) -> matrix | :nil
	;the matrix that undoes it. One that flattens everything to a line
	;has none, and that of no change is given
	(when m
		(bind '(a b tx c d ty) m)
		(defq det (- (* a d) (* b c)))
		(if (= det 0.0) (cwb-mat)
			(progn
				(defq ia (/ d det) ib (/ (neg b) det) ic (/ (neg c) det) id (/ a det))
				(fixeds ia ib (neg (+ (* ia tx) (* ib ty))) ic id (neg (+ (* ic tx) (* id ty))))))))

(defun cwb-mat-point (m x y)
	; (cwb-mat-point m x y) -> (x y)
	(setq x (n2f x) y (n2f y))
	(if m
		(progn
			(bind '(a b tx c d ty) m)
			(list (+ (* a x) (* b y) tx) (+ (* c x) (* d y) ty)))
		(list x y)))

(defun cwb-mat-paths (m paths)
	; (cwb-mat-paths m paths) -> paths
	;new paths, each moved by the matrix, the same ones if it is :nil
	(if m (map (# (path-transform m %0 (cat %0))) paths) paths))

;;;;;;;
; items
;;;;;;;

(defun cwb-slot (key)
	; (cwb-slot key) -> index
	(ifn (find key +cwb_keys) (throw "No such key of a whiteboard item !" key)))

(defun cwb-get (item key)
	; (cwb-get item key) -> value
	(elem-get item (cwb-slot key)))

(defun cwb-set (item &rest key_vals)
	; (cwb-set item [key val] ...) -> item
	;set what an item has. What it was flattened to is let go of, unless
	;all that was set was its matrix, its name or its props, which do not
	;change what it is flattened to
	(defq stale :nil)
	(each (lambda ((key val))
		(elem-set item (cwb-slot key) val)
		(unless (find key '(:m :name :props :id)) (setq stale :t)))
		(partition key_vals 2))
	(if stale (elem-set item +cwb_cache :nil))
	item)

(defun cwb-item (type key_vals)
	; (cwb-item type key_vals) -> item
	(defq item (cat +cwb_defaults))
	(elem-set item +cwb_type type)
	(if (eql type :group) (elem-set item +cwb_items (list)))
	(each (lambda ((key val)) (elem-set item (cwb-slot key) val)) (partition key_vals 2))
	item)

(defun cwb-shape (d &rest key_vals)
	; (cwb-shape d [key val] ...) -> shape
	;a shape of that path, black, 2 wide and not filled unless it is said.
	;The keys are :fill :stroke, each a colour, 0 for none, :width, :rule,
	;:nonzero or :evenodd, :join, :miter :bevel or :round, :cap1 and :cap2,
	;its two ends, :butt :square :tri :arrow or :round, :m, :name, :props
	(cwb-item :shape (cat (list :d d) key_vals)))

(defun cwb-text (text x y &rest key_vals)
	; (cwb-text text x y [key val] ...) -> shape
	;words, their left end and the line they sit on at that point. :font
	;and :font_size say which letters, they are filled, black unless it is said
	(defq item (cwb-item :shape (cat (list :kind :text :text text :fill 0xff000000 :stroke 0) key_vals)))
	(elem-set item +cwb_m (cwb-mat-mul (cwb-mat-move x y) (elem-get item +cwb_m)))
	item)

(defun cwb-text-mid (text x y &rest key_vals)
	; (cwb-text-mid text x y [key val] ...) -> shape
	;words with their middle at a point, to label a box by its middle say
	(defq item (apply (const cwb-text) (cat (list text 0 0) key_vals)))
	(when (defq box (third (cwb-flat item)))
		(bind '(bx by bx1 by1) box)
		(elem-set item +cwb_m (cwb-mat-move (- (n2f x) (* 0.5 (+ bx bx1))) (- (n2f y) (* 0.5 (+ by by1))))))
	item)

(defun cwb-group (items &rest key_vals)
	; (cwb-group items [key val] ...) -> group
	(defq item (cwb-item :group key_vals))
	(elem-set item +cwb_items (cat items))
	item)

(defun cwb-shape? (item) (eql (elem-get item +cwb_type) :shape))
(defun cwb-group? (item) (eql (elem-get item +cwb_type) :group))

;;;;;;;;;;;;;;;;;;;;;;;;
; paths of common shapes
;;;;;;;;;;;;;;;;;;;;;;;;

(defun cwb-num (n)
	; (cwb-num n) -> str
	;a number as a path has it, no more of it than there is
	(defq s (str (n2f n)))
	(if (find "." s)
		(progn
			(setq s (trim-end s "0"))
			(if (ends-with "." s) (most s) s))
		s))

(defun cwb-d (&rest parts)
	; (cwb-d [cmd | num] ...) -> d
	;a path from its commands and numbers, (cwb-d "M" 0 0 "L" 10 5 "Z")
	(join (map (# (if (str? %0) %0 (cwb-num %0))) parts) " "))

(defun cwb-d-line (x y x1 y1)
	; (cwb-d-line x y x1 y1) -> d
	(cwb-d "M" x y "L" x1 y1))

(defun cwb-d-points (points &optional closed)
	; (cwb-d-points points [closed]) -> d
	;a line through each point in turn, points is x y x y ..., back to
	;the first if closed
	(defq out (list "M"))
	(each (lambda ((x y))
		(if (= (!) 1) (push out "L"))
		(push out x y)) (partition points 2))
	(if closed (push out "Z"))
	(apply (const cwb-d) out))

(defun cwb-d-rect (x y x1 y1 &optional r)
	; (cwb-d-rect x y x1 y1 [r]) -> d
	;a box from one corner to the other, its corners round by r if given
	(defq x (n2f x) y (n2f y) x1 (n2f x1) y1 (n2f y1))
	(if (> x x1) (defq x_ x x x1 x1 x_))
	(if (> y y1) (defq y_ y y y1 y1 y_))
	(defq r (min (n2f (ifn r 0)) (* 0.5 (- x1 x)) (* 0.5 (- y1 y))))
	(if (<= r 0.0)
		(cwb-d "M" x y "L" x1 y x1 y1 x y1 "Z")
		(cwb-d "M" (+ x r) y "L" (- x1 r) y "A" r r 0 0 1 x1 (+ y r)
			"L" x1 (- y1 r) "A" r r 0 0 1 (- x1 r) y1
			"L" (+ x r) y1 "A" r r 0 0 1 x (- y1 r)
			"L" x (+ y r) "A" r r 0 0 1 (+ x r) y "Z")))

(defun cwb-d-ellipse (cx cy rx &optional ry)
	; (cwb-d-ellipse cx cy rx [ry]) -> d
	;an ellipse about a point, a circle if only one radius is given
	(defq cx (n2f cx) cy (n2f cy) rx (n2f rx) ry (n2f (ifn ry rx)))
	(cwb-d "M" (+ cx rx) cy "A" rx ry 0 1 1 (- cx rx) cy "A" rx ry 0 1 1 (+ cx rx) cy "Z"))

(defun cwb-d-arc (cx cy r a0 a1 &optional pie)
	; (cwb-d-arc cx cy r a0 a1 [pie]) -> d
	;part of a circle about a point, from one angle round to another the
	;way the angle grows, in radians from the x axis. A slice of pie, with
	;the two lines to the middle, if pie. Once round or more is a circle
	(defq cx (n2f cx) cy (n2f cy) r (n2f r) a0 (n2f a0) a1 (n2f a1) sweep (- a1 a0))
	(cond
		((>= (abs sweep) (const (- +fp_2pi 0.0005))) (cwb-d-ellipse cx cy r))
		(:t (if (< sweep 0.0) (setq sweep (+ sweep +fp_2pi)))
			(defq x0 (+ cx (* r (cos a0))) y0 (+ cy (* r (sin a0)))
				x1 (+ cx (* r (cos a1))) y1 (+ cy (* r (sin a1)))
				large (if (> sweep +fp_pi) 1 0))
			(if pie
				(cwb-d "M" cx cy "L" x0 y0 "A" r r 0 large 1 x1 y1 "Z")
				(cwb-d "M" x0 y0 "A" r r 0 large 1 x1 y1)))))

;;;;;;;;;;;;
; flattening
;;;;;;;;;;;;

(defun cwb-join-of (key)
	(case key (:miter +join_miter) (:bevel +join_bevel) (:t +join_round)))

(defun cwb-cap-of (key)
	(case key (:butt +cap_butt) (:square +cap_square) (:tri +cap_tri) (:arrow +cap_arrow) (:t +cap_round)))

(defun cwb-outline (shape)
	; (cwb-outline shape) -> ((closed path) ...)
	;the lines of a shape, each open or closed, in its own space. The
	;letters of a :text are closed. A line of less than two points is
	;left out
	(if (eql (elem-get shape +cwb_kind) :text)
		;the outlines a font gives are twice the size of the font, as a
		;canvas that is drawn at twice its size and made small wants them.
		;Here a size is a size, words of 24 are as big as words of 24 are
		;on a button
		(map (# (list :t (path-transform (const (fixeds 0.5 0.0 0.0 0.0 0.5 0.0)) %0 %0)))
			(if (nempty? (elem-get shape +cwb_text))
				(font-glyph-paths (create-font (elem-get shape +cwb_font) (n2i (elem-get shape +cwb_font_size)))
					(elem-get shape +cwb_text))
				(list)))
		(filter (# (>= (length (second %0)) 4))
			(if (nempty? (elem-get shape +cwb_d)) (path-gen-paths (elem-get shape +cwb_d)) (list)))))

(defun cwb-flat (shape)
	; (cwb-flat shape) -> (fills strokes (x y x1 y1) | :nil)
	;the polygons a shape is drawn with, those of its fill and those of
	;its stroke, and the box they are all in, in its own space. :nil for
	;the box of a shape that draws nothing. Worked out once and kept
	(unless (defq cache (elem-get shape +cwb_cache))
		(defq outline (cwb-outline shape) fills (list) strokes (list)
			radius (* 0.5 (n2f (elem-get shape +cwb_width)))
			join (cwb-join-of (elem-get shape +cwb_join))
			cap1 (cwb-cap-of (elem-get shape +cwb_cap1)) cap2 (cwb-cap-of (elem-get shape +cwb_cap2)))
		(if (/= (elem-get shape +cwb_fill) 0)
			(each (lambda ((closed p)) (if (>= (length p) 6) (push fills p))) outline))
		(if (and (/= (elem-get shape +cwb_stroke) 0) (> radius 0.0))
			(each (lambda ((closed p))
				(cond
					((every (# (eql %0 (first (partition p 2)))) (partition p 2))
						;a dot, all its points the one point
						(push strokes (path-gen-arc (first p) (second p) 0.0 +fp_2pi radius (path))))
					(closed
						(bind '(p1 p2) (path-stroke-polygon p radius join))
						(push strokes p1 p2))
					(:t (push strokes (path-stroke-polyline p radius join cap1 cap2)))))
				outline))
		(defq polys (cat fills strokes))
		(elem-set shape +cwb_cache (setq cache (list fills strokes
			(if (nempty? polys)
				(progn
					(bind '((x y) (x1 y1)) (vector-bounds-2d polys))
					(list x y x1 y1)))))))
	cache)

;;;;;;;;;;;;;;;;;;;;;;;
; walking a tree of items
;;;;;;;;;;;;;;;;;;;;;;;

(defun cwb-walk (items m fnc)
	; (cwb-walk items m fnc)
	;call (fnc shape matrix) for each shape of the items, and of the
	;groups in them, the one at the back first. The matrix is the one that
	;puts the shape where it is in the space the items are in, given one
	;for that space, or :nil. A list of what is left to do, no function
	;here calls itself
	(defq todo (map (# (list %0 m)) (reverse items)))
	(while (defq next (pop todo))
		(bind '(item m) next)
		(defq m (cwb-mat-mul m (elem-get item +cwb_m)))
		(if (cwb-group? item)
			(each (# (push todo (list %0 m))) (reverse (elem-get item +cwb_items)))
			(fnc item m))))

(defun cwb-box-of (box m)
	; (cwb-box-of box m) -> (x y x1 y1)
	;the box round a box that a matrix has moved
	(bind '(x y x1 y1) box)
	(defq pts (list (cwb-mat-point m x y) (cwb-mat-point m x1 y) (cwb-mat-point m x1 y1) (cwb-mat-point m x y1)))
	(list (reduce (# (min %0 (first %1))) pts (first (first pts)))
		(reduce (# (min %0 (second %1))) pts (second (first pts)))
		(reduce (# (max %0 (first %1))) pts (first (first pts)))
		(reduce (# (max %0 (second %1))) pts (second (first pts)))))

(defun cwb-box-join (a b)
	; (cwb-box-join a b) -> box | :nil
	;the box round two boxes, either can be :nil
	(cond
		((not a) b)
		((not b) a)
		(:t (list (min (first a) (first b)) (min (second a) (second b))
			(max (third a) (third b)) (max (elem-get a 3) (elem-get b 3))))))

(defun cwb-box-meet? (a b)
	; (cwb-box-meet? a b) -> :t | :nil
	(and a b (< (first a) (elem-get b 2)) (< (first b) (elem-get a 2))
		(< (second a) (elem-get b 3)) (< (second b) (elem-get a 3))))

(defun cwb-bounds (items &optional m)
	; (cwb-bounds items [m]) -> (x y x1 y1) | :nil
	;the box round all that the items draw, :nil if they draw nothing
	(defq box :nil)
	(cwb-walk items m (lambda (shape m)
		(if (defq flat_box (third (cwb-flat shape)))
			(setq box (cwb-box-join box (cwb-box-of flat_box m))))))
	box)

;;;;;;;;;;;
; documents
;;;;;;;;;;;

(defun cwb-doc (&optional width height)
	; (cwb-doc [width height]) -> doc
	;a new document, of one empty layer. The background is a colour, 0
	;for none, what is behind shows. The style is what the app draws
	;behind it to work on, :plain :grid :lines or :axis, with :grid the
	;gap, it is not part of the picture
	(scatter (Emap) :version +cwb_version
		:width (ifn width 1024) :height (ifn height 768)
		:background 0 :style :grid :grid 32 :next_id 1
		:layers (list (list "Layer 1" 0 (list))) :props :nil))

(defun cwb-layers (doc) (. doc :find :layers))

(defun cwb-layer (doc &optional index)
	; (cwb-layer doc [index]) -> (name flags items)
	;a layer, default the top one
	(defq layers (cwb-layers doc))
	(elem-get layers (ifn index (dec (length layers)))))

(defun cwb-items (doc &optional index)
	; (cwb-items doc [index]) -> items
	;the items of a layer, default the top one, the list itself
	(third (cwb-layer doc index)))

(defun cwb-new-id (doc)
	; (cwb-new-id doc) -> id
	(defq id (. doc :find :next_id))
	(. doc :insert :next_id (inc id))
	id)

(defun cwb-adopt (doc item)
	; (cwb-adopt doc item) -> item
	;give an item, and each item in it if it is a group, an id of this
	;document, if it has none
	;a list that grows as it is gone along, so ids are in the order of
	;the items, a group and then what is in it
	(defq todo (list item) at 0)
	(while (< at (length todo))
		(defq next (elem-get todo at))
		(++ at)
		(if (= (elem-get next +cwb_id) 0) (elem-set next +cwb_id (cwb-new-id doc)))
		(if (cwb-group? next) (each (# (push todo %0)) (elem-get next +cwb_items))))
	item)

(defun cwb-add (doc item &optional index)
	; (cwb-add doc item [index]) -> item
	;put an item on top of a layer, default the top one
	(push (cwb-items doc index) (cwb-adopt doc item))
	item)

(defun cwb-each (doc fnc)
	; (cwb-each doc fnc)
	;call (fnc item layer_index holder) for every item of the document,
	;those in groups too, the holder is the list it is in
	(each (lambda ((name flags items))
		(defq layer_index (!) todo (list (list items)))
		(while (defq next (pop todo))
			(each (lambda (item)
				(fnc item layer_index (first next))
				(if (cwb-group? item) (push todo (list (elem-get item +cwb_items)))))
				(first next))))
		(cwb-layers doc)))

(defun cwb-find (doc id)
	; (cwb-find doc id) -> :nil | (item layer_index holder)
	;an item by its id, with the layer it is in and the list that holds it
	(defq found :nil)
	(cwb-each doc (lambda (item layer_index holder)
		(if (and (not found) (= (elem-get item +cwb_id) id))
			(setq found (list item layer_index holder)))))
	found)

(defun cwb-remove (doc id)
	; (cwb-remove doc id) -> :nil | item
	;take an item out of the document, a group goes with all in it
	(when (defq found (cwb-find doc id))
		(bind '(item layer_index holder) found)
		(defq index (some (# (if (= (elem-get %0 +cwb_id) id) (!))) holder)
			kept (cat (slice holder 0 index) (slice holder (inc index) -1)))
		(clear holder)
		(each (# (push holder %0)) kept)
		item))

;;;;;;;;;;;;;;;;;;;
; saving and loading
;;;;;;;;;;;;;;;;;;;

(defun cwb-item-out (item)
	; (cwb-item-out item) -> tree
	;an item as it is in a file: its type, then each thing it has that is
	;not what it would have anyway, a key and its value. A group has its
	;items the same way. What is left to do is a list
	(defq root (list) todo (list (list item root)))
	(while (defq next (pop todo))
		(bind '(item out) next)
		(push out (elem-get item +cwb_type))
		(each (lambda (key default)
			(defq val (elem-get item (!)))
			(cond
				((find key '(:type :cache)))
				((eql key :items)
					(when (cwb-group? item)
						(defq outs (map (lambda (&) (list)) val))
						(push out :items outs)
						(each (# (push todo (list %0 %1))) val outs)))
				((and (eql key :m) val) (push out key (apply (const path) (map (const identity) val))))
				((eql key :m))
				((eql val default))
				((push out key val))))
			+cwb_keys +cwb_defaults))
	root)

(defun cwb-item-in (tree)
	; (cwb-item-in tree) -> item
	;an item from what a file has of it. A key that is not known is left.
	;Each item is put in its place in the list that is to hold it, which
	;is made first, with a place for each
	(defq root (list :nil) todo (list (list tree root 0)))
	(while (defq next (pop todo))
		(bind '(tree holder place) next)
		(defq item (cwb-item (if (eql (first tree) :group) :group :shape) (list)))
		(elem-set holder place item)
		(each (lambda ((key val))
			(cond
				((eql key :items)
					(defq kids (map (lambda (&) :nil) val))
					(elem-set item +cwb_items kids)
					(each (# (push todo (list %0 kids (!)))) val))
				((eql key :m) (elem-set item +cwb_m (apply (const fixeds) (map (const identity) val))))
				((defq index (find key +cwb_keys)) (elem-set item index val))))
			(partition (rest tree) 2)))
	(first root))

(defun cwb-save (doc stream)
	; (cwb-save doc stream) -> stream
	(tree-save stream (scatter (Emap)
		:version +cwb_version
		:width (. doc :find :width) :height (. doc :find :height)
		:background (. doc :find :background)
		:style (. doc :find :style) :grid (. doc :find :grid)
		:next_id (. doc :find :next_id)
		:layers (map (lambda ((name flags items))
			(list name flags (map (const cwb-item-out) items))) (cwb-layers doc))
		:props (. doc :find :props))))

(defun cwb-old-in (data)
	; (cwb-old-in data) -> doc
	;a document from a file of before this, version 2 or 3: polygons, in
	;groups in 3. Each polygon list is a shape that is filled, the size is
	;the box round them all and a margin
	(defq doc (cwb-doc) version (. data :find :version)
		groups (if (= version 2)
			(map (lambda ((col polys)) (list :nil (list (list col polys)))) (ifn (. data :find :polygons) (list)))
			(ifn (. data :find :groups) (list))))
	(each (lambda ((bbox strokes &ignore))
		(defq shapes (reduce (lambda (out (col polys &ignore))
			(if (nempty? polys)
				(push out (cwb-shape (join (map (# (cwb-d-points %0 :t)) polys) " ")
					:fill col :stroke 0 :kind :polygon)))
			out) strokes (list)))
		(cond
			((empty? shapes))
			((= (length shapes) 1) (cwb-add doc (first shapes)))
			((cwb-add doc (cwb-group shapes)))))
		groups)
	(when (defq box (cwb-bounds (cwb-items doc)))
		(bind '(x y x1 y1) box)
		(defq margin (max 10.0 (* 0.05 (max (- x1 x) (- y1 y)))) move (cwb-mat-move (- margin x) (- margin y)))
		(each (# (elem-set %0 +cwb_m (cwb-mat-mul move (elem-get %0 +cwb_m)))) (cwb-items doc))
		(. doc :insert :width (n2i (+ (- x1 x) margin margin)))
		(. doc :insert :height (n2i (+ (- y1 y) margin margin))))
	(. doc :insert :style :plain)
	doc)

(defun cwb-load (stream)
	; (cwb-load stream) -> :nil | doc
	;a document from a .cwb file. One from before this version is made
	;into one of this
	;a file that is not a tree of one, any other text, is not a document
	;and one that is not a tree at all throws as it is read, where there
	;are errors to throw
	(when (and stream (defq data (catch (tree-load stream) :t)) (Emap? data)
			(defq version (. data :find :version)) (num? version))
		(cond
			((< version 2) :nil)
			((< version +cwb_version) (cwb-old-in data))
			(:t (defq doc (cwb-doc))
				(each (lambda (key) (if (defq val (. data :find key)) (. doc :insert key val)))
					'(:width :height :background :style :grid :next_id :props))
				(. doc :insert :layers (map (lambda ((name flags items))
					(list name flags (map (const cwb-item-in) items))) (ifn (. data :find :layers) (list))))
				;an item with no id, one written by hand, is given one
				(each (lambda ((name flags items)) (each (# (cwb-adopt doc %0)) items)) (cwb-layers doc))
				doc))))

;;;;;;;;;
; drawing
;;;;;;;;;

(defun cwb-draw-items (canvas items m &optional clip)
	; (cwb-draw-items canvas items m [clip]) -> count
	;draw items on a canvas, by a matrix, :nil for as they are. With a
	;clip, a box in the canvas's space, a shape that is nowhere in it is
	;not drawn. How many shapes were drawn
	(defq count 0)
	(cwb-walk items m (lambda (shape m)
		(bind '(fills strokes box) (cwb-flat shape))
		(when (and box (or (not clip) (cwb-box-meet? clip (cwb-box-of box m))))
			(++ count)
			(if (nempty? fills)
				(.-> canvas (:set_color (elem-get shape +cwb_fill))
					(:fpoly 0.0 0.0 (if (eql (elem-get shape +cwb_rule) :evenodd) +winding_odd_even +winding_none_zero)
						(cwb-mat-paths m fills))))
			(if (nempty? strokes)
				(.-> canvas (:set_color (elem-get shape +cwb_stroke))
					(:fpoly 0.0 0.0 +winding_none_zero (cwb-mat-paths m strokes)))))))
	count)

(defun cwb-draw (canvas doc &optional m clip skip)
	; (cwb-draw canvas doc [m clip skip]) -> count
	;draw every layer that is not hidden, the first at the back. skip is
	;ids, of items of the layers, that are not drawn, those being moved
	;about, which are drawn over the rest
	(defq count 0)
	(each (lambda ((name flags items))
		(unless (bits? flags 1)
			(setq count (+ count (cwb-draw-items canvas
				(if (and skip (nempty? skip)) (filter (# (not (find (elem-get %0 +cwb_id) skip))) items) items)
				m clip)))))
		(cwb-layers doc))
	count)

;;;;;;;;;;;;;
; what is hit
;;;;;;;;;;;;;

(defun cwb-shape-hit? (shape m x y &optional tol)
	; (cwb-shape-hit? shape m x y [tol]) -> :t | :nil
	;is a point on what a shape draws, its fill or its stroke, or within
	;tol of it, default 0
	(bind '(fills strokes box) (cwb-flat shape))
	(setq x (n2f x) y (n2f y))
	(defq tol (n2f (ifn tol 0)))
	(when (and box (progn
			(bind '(bx by bx1 by1) (cwb-box-of box m))
			(and (>= x (- bx tol)) (<= x (+ bx1 tol)) (>= y (- by tol)) (<= y (+ by1 tol)))))
		;the point is taken into the shape's own space, what it is drawn
		;with is not moved
		(defq inv (cwb-mat-invert m)
			pts (if (> tol 0.0)
				(map (lambda ((dx dy)) (cwb-mat-point inv (+ x (* dx tol)) (+ y (* dy tol))))
					'((0.0 0.0) (1.0 0.0) (-1.0 0.0) (0.0 1.0) (0.0 -1.0) (0.7 0.7) (-0.7 0.7) (0.7 -0.7) (-0.7 -0.7)))
				(list (cwb-mat-point inv x y)))
			rule (if (eql (elem-get shape +cwb_rule) :evenodd) +winding_odd_even +winding_none_zero))
		(some (lambda ((px py))
			(defq p (path px py))
			(or (and (nempty? fills) (vector-point-in-polygon p fills rule))
				(and (nempty? strokes) (vector-point-in-polygon p strokes +winding_none_zero))))
			pts)))

(defun cwb-hit (doc x y &optional tol m)
	; (cwb-hit doc x y [tol m]) -> :nil | (item layer_index shape)
	;the item of a layer a point is on, the one on top, in a layer that is
	;not hidden or locked. The item is one of the layer's own, a group if
	;the shape that was hit is in one. m is the matrix the document is
	;seen by, if the point is in the space of a view of it
	(defq found :nil)
	(each! (lambda ((name flags items))
		(unless (or found (bits? flags 1) (bits? flags 2))
			(defq layer_index (!))
			(each! (lambda (item)
				(unless found
					(defq hit :nil)
					(cwb-walk (list item) m (lambda (shape sm)
						(if (and (not hit) (cwb-shape-hit? shape sm x y tol)) (setq hit shape))))
					(if hit (setq found (list item layer_index hit)))))
				(list items) -1 0)))
		(list (cwb-layers doc)) -1 0)
	found)

(defun cwb-hit-box (doc x y &optional m)
	; (cwb-hit-box doc x y [m]) -> :nil | (item layer_index :nil)
	;the item of a layer whose box a point is in, the smallest of them if
	;it is in more than one, in a layer that is not hidden or locked. For
	;a finger, which is not fine enough to be on the line of a box that is
	;not filled, and means the box
	(defq found :nil area :nil x (n2f x) y (n2f y))
	(each (lambda ((name flags items))
		(unless (or (bits? flags 1) (bits? flags 2))
			(defq layer_index (!))
			(each (lambda (item)
				(when (defq box (cwb-bounds (list item) m))
					(bind '(bx by bx1 by1) box)
					(when (and (>= x bx) (>= y by) (<= x bx1) (<= y by1)
							(or (not area) (<= (* (- bx1 bx) (- by1 by)) area)))
						(setq area (* (- bx1 bx) (- by1 by)) found (list item layer_index :nil)))))
				items)))
		(cwb-layers doc))
	found)

(defun cwb-in-box (doc box &optional m)
	; (cwb-in-box doc box [m]) -> ((item layer_index) ...)
	;the items of the layers that are wholly in a box, as a drag round
	;them has it, not those of a layer that is hidden or locked
	(defq out (list))
	(each (lambda ((name flags items))
		(unless (or (bits? flags 1) (bits? flags 2))
			(defq layer_index (!))
			(each (lambda (item)
				(when (defq ib (cwb-bounds (list item) m))
					(if (and (>= (first ib) (first box)) (>= (second ib) (second box))
							(<= (third ib) (third box)) (<= (elem-get ib 3) (elem-get box 3)))
						(push out (list item layer_index)))))
				items)))
		(cwb-layers doc))
	out)
