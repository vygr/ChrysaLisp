(report-header "Hash tree: a tree of hashes, the top says all of it, and two are compared from the top down")

(import "lib/hash/tree.inc")

(defq ht_things '(("a.txt" "H1" "420") ("lib/b.txt" "H2" "420") ("lib/sub/c.txt" "H3" "493")
		("lib/sub/deep/d.txt" "H4" "420") ("with space.txt" "H5" "420"))
	ht_tree (hash-tree ht_things) ht_root (hash-tree-root ht_tree))
(assert-eq "the top has a hash, 64 hex digits" 64 (length ht_root))
(assert-eq "the same things give the same top" ht_root (hash-tree-root (hash-tree ht_things)))
(assert-eq "in whatever order they are given" ht_root (hash-tree-root (hash-tree (reverse (cat ht_things)))))
(assert-list-eq "the top has its things and its folders, in the order of their names"
	'("a.txt" "lib" "with space.txt") (map (const first) (hash-tree-kids ht_tree "")))
(assert-list-eq "a folder has what is in it" '("b.txt" "sub") (map (const first) (hash-tree-kids ht_tree "lib")))
(assert-list-eq "a folder that is not there has nothing" '() (hash-tree-kids ht_tree "nowhere"))
(assert-list-eq "every thing under a folder" '("lib/sub/c.txt" "lib/sub/deep/d.txt") (hash-tree-under ht_tree "lib/sub"))
(assert-eq "and under the top, all of them" 5 (length (hash-tree-under ht_tree "")))
(assert-eq "a tree of nothing has a top" 64 (length (hash-tree-root (hash-tree (list)))))
(assert-true "that is not the top of something" (nql ht_root (hash-tree-root (hash-tree (list)))))

;a folder as text, and back, a name with a space in it too
(defq ht_text (hash-tree-text (hash-tree-kids ht_tree "")))
(assert-list-eq "a folder goes to text and comes back" (hash-tree-kids ht_tree "") (hash-tree-read ht_text))
(assert-eq "the hash of a folder is the hash of its text" ht_root (to-lower (hex-encode (sha256 ht_text))))

;one thing deep down is changed
(defun ht-with (path hash meta)
	(hash-tree (map (# (if (eql (first %0) path) (list path hash meta) %0)) ht_things)))
(defq ht_other (ht-with "lib/sub/deep/d.txt" "XX" "420"))
(assert-true "a change deep down changes the top" (nql ht_root (hash-tree-root ht_other)))
(assert-true "and every folder on the way to it"
	(every (# (nql (first (. ht_tree :find %0)) (first (. ht_other :find %0)))) '("lib" "lib/sub" "lib/sub/deep")))

(defun ht-walk (mine theirs)
	;compare two trees from the top down, as two machines would, and say
	;what differs, what has only its meta changed, what is gone, and how
	;many folders were looked at
	(defq stack (list "") differ (list) meta (list) gone (list) looked 0)
	(while (defq at (pop stack))
		(setq looked (inc looked))
		(defq pre (if (eql at "") "" (cat at "/")))
		(bind '(d m g into only_mine only_theirs)
			(hash-tree-compare (hash-tree-kids mine at) (hash-tree-kids theirs at)))
		(each (# (push differ (cat pre %0))) d)
		(each (# (push meta (cat pre (first %0)))) m)
		(each (# (push gone (cat pre %0))) g)
		(each (# (push stack (cat pre %0))) into)
		(each (# (each (# (push differ %0)) (hash-tree-under mine (cat pre %0)))) only_mine)
		(each (# (each (# (push gone %0)) (hash-tree-under theirs (cat pre %0)))) only_theirs))
	(list (sort differ) (sort meta) (sort gone) looked))

(assert-list-eq "two the same, the top is looked at and no more" '(() () () 1) (ht-walk ht_tree ht_tree))
(assert-list-eq "one thing changed deep down is found, by the four folders on the way"
	'(("lib/sub/deep/d.txt") () () 4) (ht-walk ht_tree ht_other))
(assert-list-eq "a thing with only its meta changed is told apart"
	'(() ("lib/sub/c.txt") () 3) (ht-walk ht_tree (ht-with "lib/sub/c.txt" "H3" "420")))
(defq ht_less (hash-tree (filter (# (not (starts-with "lib/sub/" (first %0)))) ht_things)))
(assert-list-eq "a folder they have not, every thing in it is to go"
	'(("lib/sub/c.txt" "lib/sub/deep/d.txt") () () 2) (ht-walk ht_tree ht_less))
(assert-list-eq "a folder only they have, every thing in it is gone"
	'(() () ("lib/sub/c.txt" "lib/sub/deep/d.txt") 2) (ht-walk ht_less ht_tree))
(defq ht_more (hash-tree (cat ht_things '(("lib/new.txt" "H6" "420") ("zz/y.txt" "H7" "420")))))
(assert-list-eq "new things, here and in a new folder"
	'(("lib/new.txt" "zz/y.txt") () () 2) (ht-walk ht_more ht_tree))

;a big tree with one change is a handful of folders, not all of it
(defq ht_big (list))
(each (lambda (i) (each (lambda (j) (each (lambda (k)
	(push ht_big (list (cat "d" (str i) "/e" (str j) "/f" (str k) ".txt") (str (+ (* i 10000) (* j 100) k)) "420")))
	(range 0 10))) (range 0 10))) (range 0 10))
(defq ht_b1 (hash-tree ht_big)
	ht_b2 (hash-tree (map (# (if (eql (first %0) "d7/e3/f5.txt") (list (first %0) "changed" "420") %0)) ht_big)))
(assert-list-eq "1000 things in 111 folders, one changed, 3 folders are looked at"
	'(("d7/e3/f5.txt") () () 3) (ht-walk ht_b1 ht_b2))
