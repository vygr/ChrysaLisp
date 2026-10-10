;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; diagrams, made and not drawn
;
; A diagram is a whiteboard document, lib/cwb/doc.inc, that a program
; lays out: boxes with words in them, cards with a title and lists, lines
; and arrows between them, a tree. It is saved as a .cwb file and shown
; in a page of the docs as any picture is, or opened on a Whiteboard and
; changed there.
;
; Nothing here draws. Each function gives the items of a part, in a group,
; and how big it is, and the caller says where it goes. Words are as wide
; as their letters are, which is asked of the font, so a box is the size
; of what is in it.
;
; make docs makes the diagrams of the reference with this, a card for
; each class and the tree of them all, cmd/make.lisp, so they are as the
; source is, each time.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./doc.inc")

;the letters, and the colours of the parts
(defq +dia_font "fonts/OpenSans-Regular.ctf" +dia_bold "fonts/OpenSans-Bold.ctf"
	+dia_ink 0xff202428 +dia_dim 0xff5c6670 +dia_line 0xff3c4650
	+dia_paper 0xffffffff +dia_band 0xffd7e6f7 +dia_here 0xffffe8a8 +dia_soft 0xfff2f4f6)

;how wide words are, kept, a word is asked of many times
(defq *dia_widths* (Fmap 101))

(defun dia-width (text size &optional bold)
	; (dia-width text size [bold]) -> width
	;how wide words are in letters of that size
	(defq key (cat (if bold "b" "r") (str size) ":" text))
	(unless (defq w (. *dia_widths* :find key))
		(setq w (if (and (nempty? text) (defq box (third (cwb-flat (cwb-text text 0 0
				:font (if bold +dia_bold +dia_font) :font_size size)))))
			(- (third box) (first box)) 0.0))
		(. *dia_widths* :insert key w))
	w)

(defun dia-words (text x y size &optional bold color)
	; (dia-words text x y size [bold color]) -> shape
	;words, their left end at x and the line they sit on at y
	(cwb-text text x y :font (if bold +dia_bold +dia_font) :font_size size :fill (ifn color +dia_ink)))

(defun dia-move (items x y)
	; (dia-move items x y) -> group
	;items as one thing, put at a point
	(cwb-group items :m (cwb-mat-move x y)))

(defun dia-pill (text &optional size bold fill)
	; (dia-pill text [size bold fill]) -> (group width height)
	;words in a box with round corners, its top left at 0 0
	(setd size 13)
	(defq w (+ (dia-width text size bold) 20.0) h (+ (n2f size) 12.0))
	(list (cwb-group (list
			(cwb-shape (cwb-d-rect 0 0 w h 6) :fill (ifn fill +dia_paper) :stroke +dia_line :width 1.2)
			(dia-words text 10.0 (- h 8.0) size bold)))
		w h))

(defun dia-head (x y x1 y1)
	; (dia-head x y x1 y1) -> shape
	;the head of an arrow that comes from a point and ends at another, a
	;filled triangle big enough to be seen at the width a diagram's lines are
	(defq dx (- (n2f x1) (n2f x)) dy (- (n2f y1) (n2f y)) len (max 0.001 (sqrt (+ (* dx dx) (* dy dy))))
		ux (/ dx len) uy (/ dy len) bx (- (n2f x1) (* 9.0 ux)) by (- (n2f y1) (* 9.0 uy)))
	(cwb-shape (cwb-d "M" x1 y1 "L" (- bx (* 4.0 uy)) (+ by (* 4.0 ux)) (+ bx (* 4.0 uy)) (- by (* 4.0 ux)) "Z")
		:fill +dia_line :stroke 0))

(defun dia-arrow (x y x1 y1 &optional heads)
	; (dia-arrow x y x1 y1 [heads]) -> group
	;a line from a point to a point, an arrow at its far end, or with
	;heads :none no arrow, :both one at each
	(defq items (list (cwb-shape (cwb-d-line x y x1 y1) :stroke +dia_line :width 1.4 :cap1 :butt :cap2 :butt)))
	(unless (eql heads :none) (push items (dia-head x y x1 y1)))
	(if (eql heads :both) (push items (dia-head x1 y1 x y)))
	(cwb-group items))

(defun dia-elbow (x y x1 y1 &optional heads)
	; (dia-elbow x y x1 y1 [heads]) -> shape
	;from a point across, down or up, and across to a point, as the
	;branches of a tree are drawn
	(defq mx (* 0.5 (+ x x1)))
	(cwb-shape (cwb-d "M" x y "L" mx y mx y1 x1 y1) :stroke +dia_line :width 1.2 :join :miter
		:cap1 :butt :cap2 (if (eql heads :arrow) :arrow :butt)))

(defun dia-card (title under sections &optional fill)
	; (dia-card title under sections [fill]) -> (group width height)
	;a card: a band with a title and a line of smaller words under it,
	;then sections, each a heading and its rows, ((heading (row ...)) ...).
	;A section of many rows is set in columns. Its top left is at 0 0
	(defq pad 12.0 pitch 17.0 size 12 gap 22.0 top 0.0 items (list) cols (list)
		band (+ 34.0 (if (and under (nempty? under)) 16.0 0.0))
		width (max 150.0 (+ (dia-width title 18 :t) pad pad)
			(if under (+ (dia-width under 11) pad pad) 0.0)))
	;each section as columns of rows, and how wide that makes it
	(each (lambda ((heading rows))
		(defq n (length rows) across (cond ((> n 36) 3) ((> n 14) 2) (:t 1))
			down (max 1 (/ (+ n across -1) across)) parts (partition rows down)
			widths (map (lambda (part) (reduce (# (max %0 (dia-width %1 size))) part 0.0)) parts))
		(push cols (list heading parts widths down))
		(setq width (max width (+ pad pad (dia-width heading 11 :t))
			(+ pad pad (reduce (const +) widths 0.0) (* gap (n2f (dec (length parts))))))))
		sections)
	;the band
	(setq top band)
	(defq body (list))
	(each (lambda ((heading parts widths down))
		(when (nempty? parts)
			(push body (cwb-shape (cwb-d-line 0 top width top) :stroke +dia_line :width 1.0 :cap1 :butt :cap2 :butt))
			(push body (dia-words heading pad (+ top 16.0) 11 :t +dia_dim))
			(defq x pad)
			(each (lambda (part w)
				(each (lambda (row) (push body (dia-words row x (+ top 20.0 (* pitch (n2f (inc (!)))) -2.0) size))) part)
				(setq x (+ x w gap)))
				parts widths)
			(setq top (+ top 26.0 (* pitch (n2f down)) 6.0))))
		cols)
	(defq height (max top (+ band 8.0)))
	(push items (cwb-shape (cwb-d-rect 0 0 width height 8) :fill +dia_paper :stroke +dia_line :width 1.4))
	;the band is the top of the card, round at its top corners only
	(push items (cwb-shape (cwb-d "M" 0 band "L" 0 8 "Q" 0 0 8 0 "L" (- width 8.0) 0 "Q" width 0 width 8 "L" width band "Z")
		:fill (ifn fill +dia_band) :stroke +dia_line :width 1.4))
	(push items (dia-words title pad 24.0 18 :t))
	(if (and under (nempty? under)) (push items (dia-words under pad 40.0 11 :nil +dia_dim)))
	(each (# (push items %0)) body)
	(list (cwb-group items) width height))

(defun dia-row (pills gap max_width)
	; (dia-row pills gap max_width) -> (group width height)
	;pills, each (group width height), set left to right, onto another
	;line when they would be wider than max_width. Top left at 0 0
	(defq x 0.0 y 0.0 high 0.0 wide 0.0 items (list))
	(each (lambda ((item w h))
		(when (and (> x 0.0) (> (+ x w) max_width))
			(setq x 0.0 y (+ y high gap) high 0.0))
		(push items (dia-move (list item) x y))
		(setq x (+ x w gap) high (max high h) wide (max wide (- x gap))))
		pills)
	(list (cwb-group items) wide (+ y high)))

(defun dia-tree (nodes &optional here)
	; (dia-tree nodes [here]) -> (group width height)
	;a tree, its root at the left and what comes of each thing to the
	;right of it. nodes is ((name parent) ...), parent :nil or a name not
	;among them for a root. here, a name, is marked. Top left at 0 0
	(defq kids (Fmap 31) known (Fmap 31) roots (list) order (list) depth (Fmap 31)
		place (Fmap 31) pitch 30.0 gap 46.0 slot 0 items (list))
	(each (lambda ((name parent)) (. known :insert name :t) (. kids :insert name (list))) nodes)
	(each (lambda ((name parent))
		(if (and parent (. known :find parent))
			(push (. kids :find parent) name)
			(push roots name)))
		nodes)
	;each thing after what it comes of, a list of what is left to do
	(defq todo (map (# (list %0 0)) (reverse (sort roots (const cmp)))))
	(while (defq next (pop todo))
		(bind '(name d) next)
		(push order name)
		(. depth :insert name d)
		(each (# (push todo (list %0 (inc d)))) (reverse (sort (. kids :find name) (const cmp)))))
	;how wide each depth is, and where it starts
	(defq deep (reduce (# (max %0 (. depth :find %1))) order 0)
		widths (map (lambda (d) (reduce (lambda (w name)
			(if (= (. depth :find name) d) (max w (+ (dia-width name 13) 20.0)) w)) order 0.0)) (range 0 (inc deep)))
		starts (list) x 0.0)
	(each (lambda (w) (push starts x) (setq x (+ x w gap))) widths)
	;a leaf has the next line down, what has things come of it is half
	;way between the first and the last of them. From the last back, so
	;that those are placed before it is
	(each (lambda (name) (if (empty? (. kids :find name))
		(progn (. place :insert name (* pitch (n2f slot))) (setq slot (inc slot)))))
		order)
	(each (lambda (name)
		(when (nempty? (defq mine (. kids :find name)))
			(. place :insert name (* 0.5 (+ (reduce (# (min %0 (. place :find %1))) mine 1000000.0)
				(reduce (# (max %0 (. place :find %1))) mine -1000000.0))))))
		(reverse order))
	;the lines first, under the boxes
	(each (lambda (name)
		(defq d (. depth :find name) x0 (+ (elem-get starts d) (dia-width name 13) 20.0) y0 (+ (. place :find name) 12.5))
		(each (lambda (kid)
			(push items (dia-elbow x0 y0 (elem-get starts (inc d)) (+ (. place :find kid) 12.5))))
			(. kids :find name)))
		order)
	(each (lambda (name)
		(bind '(pill w h) (dia-pill name 13 (eql name here) (if (eql name here) +dia_here +dia_paper)))
		(push items (dia-move (list pill) (elem-get starts (. depth :find name)) (. place :find name))))
		order)
	(list (cwb-group items) (- x gap) (+ (* pitch (n2f (max 1 slot))) -5.0)))

(defun dia-hierarchy (nodes &optional alone)
	; (dia-hierarchy nodes [alone]) -> doc
	;the tree of nodes, ((name parent) ...), and under it, in rows, those
	;that come of nothing and have nothing come of them, with alone, a
	;line of words, over them. They are not in the tree, a tree of one each
	(defq parents (Fmap 31) loners (list) rest_of (list) parts (list))
	(each (lambda ((name parent)) (if parent (. parents :insert parent :t))) nodes)
	(each (lambda ((name parent))
		(if (or parent (. parents :find name)) (push rest_of (list name parent)) (push loners name)))
		nodes)
	(bind '(tree w h) (dia-tree rest_of))
	(push parts tree)
	(when (nempty? loners)
		(bind '(row rw rh) (dia-row (map (# (dia-pill %0 13 :nil +dia_soft)) (sort loners (const cmp))) 8.0 (max w 640.0)))
		(push parts (dia-words (ifn alone "And those that come of nothing") 0.0 (+ h 34.0) 12 :t +dia_dim)
			(dia-move (list row) 0.0 (+ h 44.0))))
	(dia-doc parts))

(defun dia-doc (parts &optional pad)
	; (dia-doc parts [pad]) -> doc
	;a document of parts, each a group or a shape, as big as they are and
	;pad round them, default 12. Nothing behind them, the page shows
	(defq doc (cwb-doc 16 16))
	(. doc :insert :style :plain)
	(each (# (cwb-add doc %0)) parts)
	(cwb-fit doc (ifn pad 12))
	doc)

(defun dia-class (name under chain kids sections)
	; (dia-class name under chain kids sections) -> doc
	;the diagram of a class: what it comes of, from the first to it,
	;along the top, its card, name under and sections as (dia-card), and
	;below that the classes that come of it. chain and kids are names
	(defq parts (list) y 0.0 wide 0.0)
	;what it comes of, each with an arrow to the next, itself the last
	(when (nempty? chain)
		(defq x 0.0)
		(each (lambda (parent)
			(bind '(pill w h) (dia-pill parent 12))
			(push parts (dia-move (list pill) x 0.0) (dia-arrow (+ x w 3.0) 12.0 (+ x w 25.0) 12.0))
			(setq x (+ x w 28.0)))
			chain)
		(bind '(pill w h) (dia-pill name 12 :t +dia_here))
		(push parts (dia-move (list pill) x 0.0))
		(setq y 40.0 wide (+ x w)))
	(bind '(card w h) (dia-card name under sections))
	(push parts (dia-move (list card) 0.0 y))
	(setq y (+ y h) wide (max wide w))
	;what comes of it
	(when (nempty? kids)
		(push parts (dia-words (cat (str (length kids)) (if (= (length kids) 1) " class comes of it" " classes come of it"))
			0.0 (+ y 24.0) 11 :t +dia_dim))
		(bind '(row w h) (dia-row (map (# (dia-pill %0 12 :nil +dia_soft)) kids) 8.0 (max wide 620.0)))
		(push parts (dia-move (list row) 0.0 (+ y 32.0))))
	(dia-doc parts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a scene: boxes that are put down by name, and lines between them by name
;
; For a diagram that is thought out and not worked out from data: what
; talks to what, what is made of what, what happens after what. A script
; puts each box where it is to be and gives it a name, then joins names.
; A line knows where the two boxes are, and leaves and arrives at the
; sides that face. docs/diagrams/ has the scripts of those in the docs,
; and make docs runs them.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dia-box (text w h &optional fill size bold)
	; (dia-box text w h [fill size bold]) -> (group width height)
	;a box of a size that is given, with words in the middle of it. text
	;is a str, or a list of them for lines one under another, the first
	;bold if bold is :first
	(setd size 13)
	(defq w (n2f w) h (n2f h) lines (if (str? text) (list text) text) pitch (+ (n2f size) 5.0)
		top (+ (* 0.5 (- h (* pitch (n2f (length lines))))) (n2f size) 1.0)
		items (list (cwb-shape (cwb-d-rect 0 0 w h 6) :fill (ifn fill +dia_paper) :stroke +dia_line :width 1.2)))
	(each (lambda (line)
		(defq strong (or (eql bold :t) (and (eql bold :first) (= (!) 0))))
		(push items (dia-words line (* 0.5 (- w (dia-width line size strong))) (+ top (* pitch (n2f (!)))) size strong)))
		lines)
	(list (cwb-group items) w h))

(defun dia-scene ()
	; (dia-scene) -> scene
	;nothing yet: the lines, the boxes, and where each box that has a name is
	(list (list) (list) (Fmap 31)))

(defun dia-put (scene name part x y)
	; (dia-put scene name part x y) -> scene
	;put a part, (group width height), with its top left at a point. With
	;a name it can be joined to, :nil for one that is only there
	(bind '(item w h) part)
	(push (second scene) (dia-move (list item) x y))
	(if name (. (third scene) :insert name (list (n2f x) (n2f y) w h)))
	scene)

(defun dia-say (scene text x y &optional size bold color)
	; (dia-say scene text x y [size bold color]) -> scene
	;words on the scene, their left end and the line they sit on at a point
	(push (second scene) (dia-words text x y (ifn size 12) bold (ifn color +dia_dim)))
	scene)

(defun dia-side (box way)
	; (dia-side box way) -> (x y)
	;the middle of a side of a box, (x y w h): :left :right :top :bottom
	(bind '(x y w h) box)
	(case way
		(:left (list x (+ y (* 0.5 h))))
		(:right (list (+ x w) (+ y (* 0.5 h))))
		(:top (list (+ x (* 0.5 w)) y))
		(:t (list (+ x (* 0.5 w)) (+ y h)))))

(defun dia-join (scene from to &optional label heads bend)
	; (dia-join scene from to [label heads bend]) -> scene
	;a line from one named box to another, with an arrow at the far end,
	;heads :none or :both for none or two. It leaves and arrives at the
	;sides that face each other. label, words at its middle. bend :elbow
	;for a line that goes across, then up or down, then across
	(defq a (. (third scene) :find from) b (. (third scene) :find to))
	(when (and a b)
		(bind '(ax ay aw ah) a)
		(bind '(bx by bw bh) b)
		(defq dx (- (+ bx (* 0.5 bw)) (+ ax (* 0.5 aw))) dy (- (+ by (* 0.5 bh)) (+ ay (* 0.5 ah)))
			;apart across, if they do not lie one over the other
			across (or (>= bx (+ ax aw)) (>= ax (+ bx bw)))
			p (dia-side a (if across (if (> dx 0.0) :right :left) (if (> dy 0.0) :bottom :top)))
			q (dia-side b (if across (if (> dx 0.0) :left :right) (if (> dy 0.0) :top :bottom))))
		(bind '(px py) p)
		(bind '(qx qy) q)
		(cond
			((and (eql bend :elbow) across)
				(defq mx (* 0.5 (+ px qx)))
				(push (first scene) (cwb-shape (cwb-d "M" px py "L" mx py mx qy qx qy) :stroke +dia_line :width 1.4
					:join :miter :cap1 :butt :cap2 :butt))
				(unless (eql heads :none) (push (first scene) (dia-head mx qy qx qy)))
				(if (eql heads :both) (push (first scene) (dia-head mx py px py))))
			(:t (push (first scene) (dia-arrow px py qx qy heads))))
		(when (and label (nempty? label))
			(defq lw (dia-width label 11) mx (* 0.5 (+ px qx)) my (* 0.5 (+ py qy)))
			;beside an upright line, over one that lies flat
			(push (first scene) (if across
				(dia-words label (- mx (* 0.5 lw)) (- (if (eql bend :elbow) (min py qy) my) 5.0) 11 :nil +dia_dim)
				(dia-words label (+ mx 6.0) (+ my 4.0) 11 :nil +dia_dim)))))
	scene)

(defun dia-scene-doc (scene &optional pad)
	; (dia-scene-doc scene [pad]) -> doc
	;the document of a scene, its lines under its boxes
	(dia-doc (cat (first scene) (second scene)) pad))
