;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/hash/tree.inc - a tree of hashes
;
; A set of things that have names like paths, a/b/c, and a hash each. A
; folder has a hash of its own, the hash of what is in it, the name, the
; kind and the hash of each thing, and so on up. The hash of the top says
; all of it. Two that have the same top have the same everything, in one
; number, and two that differ are compared a folder at a time from the top
; down, going only into the folders whose hashes differ. What is the same,
; however much of it there is, is never looked at.
;
; It is what sync compares two trees of files by, lib/sync/sync.inc, and
; it is not about files, a thing is a name, a hash, and a word of what
; else is to be said of it. The folders of one side can be asked for over
; a link, a folder is a few lines of text.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././crypto/sha256.inc")

;what is in a folder, each one of these. The kind is "f" a thing or "d" a
;folder. meta is a word, no spaces, of what else there is to say of a
;thing, its mode say, and is part of what the folder's hash is of. A
;folder has "-"
(enums +hash_kid 0
	(enum name kind hash meta))

(defun hash-tree-text (kids)
	; (hash-tree-text kids) -> str
	;a folder as text, a line each, in the order of their names. It is
	;what a folder's hash is the hash of, and what goes in a message
	(apply (const cat) (cat (list "") (map (lambda ((name kind hash meta))
		(cat kind " " hash " " meta " " name (ascii-char 10))) kids))))

(defun hash-tree-read (text)
	; (hash-tree-read text) -> kids
	;a folder from its text. A name may have spaces in it, it is last
	(reduce (lambda (out line)
		(if (>= (length (defq parts (split line " "))) 4)
			(push out (list (join (slice parts 3 -1) " ") (first parts) (second parts) (third parts))))
		out) (split text (ascii-char 10)) (list)))

(defun hash-tree (things)
	; (hash-tree things) -> tree
	;the tree of things, each (path hash [meta]). A map, the path of a
	;folder, "" the top, to (hash kids), the kids in the order of their
	;names. A tree of nothing has a top, with nothing in it
	(defq folders (Fmap 101) tree (Fmap 101))
	(. folders :insert "" (list))
	(each (lambda ((path hash &optional meta))
		;the thing goes in its folder, and each folder on the way up in
		;the one above, the once
		(defq at (rfind "/" path) folder (if at (slice path 0 (dec at)) "")
			name (if at (slice path at -1) path))
		(unless (. folders :find folder) (. folders :insert folder (list)))
		(push (. folders :find folder) (list name "f" hash (ifn meta "-")))
		(while (nempty? folder)
			(defq at (rfind "/" folder) above (if at (slice folder 0 (dec at)) "")
				name (if at (slice folder at -1) folder) made (. folders :find above))
			(unless made (. folders :insert above (setq made (list))))
			;if it is there the ones above are too
			(if (some (# (and (eql (elem-get %0 +hash_kid_kind) "d") (eql (first %0) name))) made)
				(setq folder "")
				(progn (push made (list name "d" "" "-")) (setq folder above)))))
		things)
	;the deepest folders first, so that a folder's folders have their hashes
	(defq paths (list))
	(. folders :each (lambda (path kids) (push paths path)))
	(sort paths (# (- (length (split %1 "/")) (length (split %0 "/")))))
	(each (lambda (path)
		(defq kids (sort (. folders :find path) (# (cmp (first %0) (first %1)))))
		(each (lambda (kid)
			(if (eql (elem-get kid +hash_kid_kind) "d")
				(elem-set kid +hash_kid_hash
					(first (. tree :find (if (eql path "") (first kid) (cat path "/" (first kid))))))))
			kids)
		(. tree :insert path (list (to-lower (hex-encode (sha256 (hash-tree-text kids)))) kids)))
		paths)
	tree)

(defun hash-tree-root (tree)
	; (hash-tree-root tree) -> hash
	;the hash of the top, which is of all of it
	(first (. tree :find "")))

(defun hash-tree-kids (tree folder)
	; (hash-tree-kids tree folder) -> kids
	;what is in a folder, nothing if there is no such folder
	(ifn (second (ifn (. tree :find folder) '("" ()))) (list)))

(defun hash-tree-under (tree folder)
	; (hash-tree-under tree folder) -> paths
	;the path of every thing in a folder and in the folders below it
	(defq out (list) stack (list folder))
	(while (defq at (pop stack))
		(each (lambda ((name kind &rest _))
			(defq path (if (eql at "") name (cat at "/" name)))
			(if (eql kind "d") (push stack path) (push out path)))
			(hash-tree-kids tree at)))
	(sort out))

(defun hash-tree-compare (mine theirs)
	; (hash-tree-compare mine theirs) -> (differ meta gone into only_mine only_theirs)
	;one folder of mine against the same folder of theirs, both as kids.
	;The names of: things I have that they have not, or not the same.
	;Things that are the same but for the meta, each (name meta) with
	;mine. Things they have and I have not. Folders we both have that are
	;not the same, to go into. Folders only I have, and folders only they
	;have. A name that is a thing on one side and a folder on the other is
	;both a thing and a folder that one has and the other has not
	(defq have (Fmap 31) keep (Fmap 31)
		differ (list) meta (list) gone (list) into (list) only_mine (list) only_theirs (list))
	(each (lambda ((name kind hash m)) (. have :insert (cat kind name) (list hash m))) theirs)
	(each (lambda ((name kind hash m))
		(. keep :insert (cat kind name) :t)
		(defq there (. have :find (cat kind name)))
		(cond
			((eql kind "d")
				(cond
					((not there) (push only_mine name))
					((nql hash (first there)) (push into name))))
			((or (not there) (nql hash (first there))) (push differ name))
			((nql m (second there)) (push meta (list name m)))))
		mine)
	(each (lambda ((name kind hash m))
		(unless (. keep :find (cat kind name))
			(push (if (eql kind "d") only_theirs gone) name)))
		theirs)
	(list differ meta gone into only_mine only_theirs))
