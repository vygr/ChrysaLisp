;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Wavefront .obj and .mtl, read
;
; An .obj is lines: v a position, vt a place on a texture, vn a normal, f a
; face of three or more corners, each corner the position, and if they are
; there the place and the normal, a/b/c, a//c or a/b, counted from 1, or
; from the end if less than 0. usemtl says which material the faces after it
; are of, mtllib which .mtl files say what the materials are. Anything else
; is not looked at.
;
; A face of more than three corners is cut into triangles from its first
; corner, as a fan. What comes back is every position, place and normal as
; they were, and the faces by material, a triangle three corners, a corner
; three numbers, the position, the place and the normal, -1 for one that is
; not there.
;
; An .mtl says of each material, newmtl name, its colour, Kd, how much of it
; is seen, d, or how much not, Tr, and the picture that is its colour,
; map_Kd. Anything else is not looked at.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./vector.inc")

;module
(env-push)

(defun obj-normal (p0 p1 p2)
	; (obj-normal p0 p1 p2) -> reals
	;the normal of a triangle, the way it faces when its corners go round
	;counter clockwise
	(defq n (apply (const reals) (vector-cross-3d (vector-sub p1 p0 (reals 0 0 0)) (vector-sub p2 p0 (reals 0 0 0))))
		d (vector-dot n n))
	(if (= d +real_0) (reals +real_0 +real_0 +real_1) (vector-scale n (recip (sqrt d)))))

(defun obj-index (text count)
	; (obj-index text count) -> index
	;a number of a corner as an index from 0, -1 if there is none
	(cond
		((empty? text) -1)
		((> (defq n (str-as-num text)) 0) (dec n))
		((< n 0) (+ count n))
		(-1)))

(defun obj-corner (text counts)
	; (obj-corner text counts) -> (position place normal)
	;a corner of a face, a, a/b, a//c or a/b/c. split would lose the empty
	;part of a//c, so the slashes are found
	(bind '(positions places normals) counts)
	(cond
		((defq at (find "/" text))
			(defq rest (slice text (inc at) -1) next (find "/" rest))
			(list (obj-index (slice text 0 at) positions)
				(obj-index (if next (slice rest 0 next) rest) places)
				(obj-index (if next (slice rest (inc next) -1) "") normals)))
		((list (obj-index text positions) -1 -1))))

(defun obj-read (stream)
	; (obj-read stream) -> emap
	;:positions reals, three a position, :places reals, two a place,
	;:normals reals, three a normal, :groups ((material nums) ...), the
	;nums three a corner and nine a triangle, in the order the materials
	;were first used, and :libraries (name ...), the .mtl files
	(defq positions (reals) places (reals) normals (reals) groups (Fmap 11) order (list)
		libraries (list) material "" faces :nil)
	(defun obj-group (name)
		(unless (defq got (. groups :find name))
			(. groups :insert name (setq got (nums)))
			(push order name))
		got)
	(setq faces (obj-group material))
	(lines! (lambda (line)
		(when (nempty? (defq words (split (trim line) (const (char-class " \t\r")))))
			(case (first words)
				("v" (push positions (str-to-real (elem-get words 1)) (str-to-real (elem-get words 2))
					(str-to-real (elem-get words 3))))
				("vt" (push places (str-to-real (elem-get words 1))
					(if (> (length words) 2) (str-to-real (elem-get words 2)) +real_0)))
				("vn" (push normals (str-to-real (elem-get words 1)) (str-to-real (elem-get words 2))
					(str-to-real (elem-get words 3))))
				("f" (when (>= (length words) 4)
					(defq counts (list (/ (length positions) 3) (/ (length places) 2) (/ (length normals) 3))
						corners (map! (# (obj-corner %0 counts)) (list words) 1 -1) one (first corners))
					;a fan from the first corner
					(each (lambda (b c) (each (# (push faces (elem-get %0 0) (elem-get %0 1) (elem-get %0 2))) (list one b c)))
						(slice corners 1 -2) (slice corners 2 -1))))
				("usemtl" (setq material (if (> (length words) 1) (elem-get words 1) "") faces (obj-group material)))
				("mtllib" (each! (# (push libraries %0)) (list words) 1 -1))))
		:nil) stream)
	(defq out (Emap))
	(scatter out :positions positions :places places :normals normals :libraries libraries
		;a material no face is of is not given
		:groups (filter (# (nempty? (second %0))) (map (# (list %0 (. groups :find %0))) order)))
	out)

(defun mtl-read (stream)
	; (mtl-read stream) -> fmap
	;each material by its name, an emap: :colour (r g b), :seen 0 to 1,
	;and :map, the file of its picture, if it has one
	(defq out (Fmap 11) now :nil)
	(lines! (lambda (line)
		(when (nempty? (defq words (split (trim line) (const (char-class " \t\r")))))
			(case (first words)
				("newmtl" (when (> (length words) 1)
					(scatter (setq now (Emap)) :colour (list +real_1 +real_1 +real_1) :seen +real_1)
					(. out :insert (elem-get words 1) now)))
				("Kd" (and now (> (length words) 3)
					(. now :insert :colour (map! (const str-to-real) (list words) 1 4))))
				("d" (and now (> (length words) 1) (. now :insert :seen (str-to-real (elem-get words 1)))))
				("Tr" (and now (> (length words) 1) (. now :insert :seen (- +real_1 (str-to-real (elem-get words 1))))))
				;the file is the last word, options before it
				("map_Kd" (and now (> (length words) 1) (. now :insert :map (last words))))))
		:nil) stream)
	out)

(defun obj-flat (obj corners &optional matrix)
	; (obj-flat obj corners [matrix]) -> reals
	;the triangles of a group as a shader is given them, a corner eight
	;numbers: where it is, three, after matrix if one is given, the place
	;on the picture, two, across and down, and its normal, three. A place
	;of an .obj goes up from the bottom, a picture's down from the top, so
	;down is 1 less it. A corner with no place is at 0 0, and a triangle
	;whose corners have no normal is given the one of its face
	(defq positions (. obj :find :positions) places (. obj :find :places) normals (. obj :find :normals)
		out (reals) at 0 count (length corners))
	(while (< at count)
		(defq tri (map (lambda (i)
				(defq p (* 3 (elem-get corners (+ at (* i 3)))) t (elem-get corners (+ at (* i 3) 1))
					n (elem-get corners (+ at (* i 3) 2)))
				(list (slice positions p (+ p 3))
					(if (>= t 0) (reals (elem-get places (* t 2)) (- +real_1 (elem-get places (inc (* t 2))))) (reals +real_0 +real_0))
					(if (>= n 0) (slice normals (* n 3) (+ (* n 3) 3)) :nil)))
				'(0 1 2))
			face (obj-normal (first (first tri)) (first (second tri)) (first (third tri))))
		(each (lambda ((p t n))
				(if matrix (setq p (slice (mat4x4-vec4-mul matrix (cat p (reals +real_1)) (reals +real_0 +real_0 +real_0 +real_0)) 0 3)))
				(push out (elem-get p 0) (elem-get p 1) (elem-get p 2) (elem-get t 0) (elem-get t 1))
				(defq n (ifn n face))
				(push out (elem-get n 0) (elem-get n 1) (elem-get n 2)))
			tri)
		(setq at (+ at 9)))
	out)

(defun obj-fit (obj)
	; (obj-fit obj) -> mat4x4
	;the matrix that puts the whole of it in a ball of radius 1 about 0
	(defq positions (. obj :find :positions) fours (reals))
	(each (# (push fours (elem-get positions (* %0 3)) (elem-get positions (inc (* %0 3)))
			(elem-get positions (+ (* %0 3) 2)) +real_1))
		(range 0 (/ (length positions) 3)))
	(bind '((cx cy cz) scale) (vector-bounds-sphere fours +vec4_size))
	(defq scale (if (> scale +real_0) (recip scale) +real_1))
	(reals
		scale +real_0 +real_0 (* scale (neg cx))
		+real_0 scale +real_0 (* scale (neg cy))
		+real_0 +real_0 scale (* scale (neg cz))
		+real_0 +real_0 +real_0 +real_1))

;module
(export-symbols '(obj-read mtl-read obj-flat obj-fit))
(env-pop)
