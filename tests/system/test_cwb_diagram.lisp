(report-header "Whiteboard diagrams: cards, rows and trees laid out by a program")

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/cwb/diagram.inc")

;words are as wide as their letters, and a box is the size of what is in it
(assert-true "longer words are wider, and bold wider than not"
	(and (> (dia-width "a longer word" 12) (dia-width "word" 12) 0.0) (> (dia-width "word" 12 :t) (dia-width "word" 12))))
(assert-eq "no words are no width" 0.0 (dia-width "" 12))
(bind '(dg_pill dg_w dg_h) (dia-pill ":canvas" 13))
(assert-true "a pill is its words and a little round them" (and (cwb-group? dg_pill) (= dg_w (+ (dia-width ":canvas" 13) 20.0)) (= dg_h 25.0)))

;a card is as wide as its widest row, and as tall as its rows
(bind '(dg_card dg_cw dg_ch) (dia-card "Thing" "what it is" (list (list "fields" '("int a" "int b")) (list "methods" '(":one")))))
(bind '(dg_card2 dg_cw2 dg_ch2) (dia-card "Thing" "what it is" (list (list "fields" '("int a" "int b" "a very much longer row than the others are")))))
(assert-true "a card with a longer row is wider, with a section more it is taller" (and (> dg_cw2 dg_cw) (> dg_ch dg_ch2)))
(bind '(dg_many dg_mw dg_mh) (dia-card "Thing" :nil (list (list "methods" (map (# (cat ":a_method_with_a_name_" (str %0))) (range 0 30))))))
(bind '(dg_few dg_fw dg_fh) (dia-card "Thing" :nil (list (list "methods" (map (# (cat ":a_method_with_a_name_" (str %0))) (range 0 14))))))
(assert-true "thirty rows are set in two columns, a little taller than fourteen and not twice" (and (> dg_mw dg_fw) (< dg_mh (* 1.3 dg_fh))))

;a row goes onto another line when it is too wide
(bind '(dg_row dg_rw dg_rh) (dia-row (map (# (dia-pill %0)) '("one" "two" "three" "four" "five" "six")) 8.0 120.0))
(assert-true "six pills in a row no wider than 120 are on more than one line" (and (<= dg_rw 120.0) (> dg_rh 50.0)))

;a tree: each thing to the right of what it comes of, and what things come of half way between them
(defq dg_nodes '(("root" :nil) ("b" "root") ("a" "root") ("a2" "a") ("a1" "a") ("lost" "nowhere")))
(bind '(dg_tree dg_tw dg_th) (dia-tree dg_nodes "a1"))
(defun dg-at (name)
	;where the pill of a name is, the top left of its group, by its words
	(some (lambda (item)
		(if (and (cwb-group? item) (some (# (and (cwb-group? %0) (some (# (and (cwb-shape? %0) (eql (cwb-get %0 :text) name)))
				(elem-get %0 +cwb_items)))) (elem-get item +cwb_items)))
			(map (const n2i) (slice (elem-get item +cwb_m) 2 3))))
		(elem-get dg_tree +cwb_items)))
(defun dg-xy (name)
	(some (lambda (item)
		(if (and (cwb-group? item) (some (# (and (cwb-group? %0) (some (# (and (cwb-shape? %0) (eql (cwb-get %0 :text) name)))
				(elem-get %0 +cwb_items)))) (elem-get item +cwb_items)))
			(list (elem-get (elem-get item +cwb_m) 2) (elem-get (elem-get item +cwb_m) 5))))
		(elem-get dg_tree +cwb_items)))
(assert-true "what comes of a thing is to the right of it, and its own further right still"
	(< (first (dg-xy "root")) (first (dg-xy "a")) (first (dg-xy "a1"))))
(assert-true "things that come of the same are one under another, in the order of their names"
	(and (= (first (dg-xy "a")) (first (dg-xy "b"))) (< (second (dg-xy "a1")) (second (dg-xy "a2")) (second (dg-xy "b")))))
(assert-true "what things come of is half way between the first and the last of them"
	(and (= (second (dg-xy "a")) (* 0.5 (+ (second (dg-xy "a1")) (second (dg-xy "a2")))))
		(= (second (dg-xy "root")) (* 0.5 (+ (second (dg-xy "a")) (second (dg-xy "b")))))))
(assert-true "one whose parent is not among them is a root too, at the left" (= (first (dg-xy "lost")) (first (dg-xy "root"))))

;the diagram of a class, and of them all, are documents the size of what is in them, that save and load
(defq dg_doc (dia-class ":canvas" "gui/canvas/class.inc" '(":obj" ":view") '(":kid")
	(list (list "fields" '("ptr pixmap")) (list "static methods" '(":create" ":fill")))))
(assert-true "the diagram of a class is a document with its parts in it, and a size"
	(and (> (. dg_doc :find :width) 100) (> (. dg_doc :find :height) 100) (>= (length (cwb-items dg_doc)) 5)))
(defq dg_stream (string-stream (cat "")))
(cwb-save dg_doc dg_stream)
(defq dg_back (cwb-load (string-stream (str dg_stream))))
(assert-list-eq "it is saved and loaded as any document is" (list (. dg_doc :find :width) (. dg_doc :find :height) (length (cwb-items dg_doc)))
	(list (. dg_back :find :width) (. dg_back :find :height) (length (cwb-items dg_back))))
(defq dg_all (dia-hierarchy '((":obj" :nil) (":num" ":obj") (":sys_mem" :nil) (":sys_task" :nil))))
(assert-true "the tree of them all has those that come of nothing set apart under it"
	(> (. dg_all :find :height) 100))
