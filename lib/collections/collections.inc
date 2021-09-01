;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; collections - ChyrsaLisp data collections library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./emap.inc")
(import "./fmap.inc")
(import "./xmap.inc")
(import "./xset.inc")
(import "./xnode.inc")

(defun map? (_)
	; (map? object) -> t | nil
	; Returns true if argument is a map type
	(or (emap? _) (xmap? _) (fmap? _)))

(defun set? (_)
	(xset? _))

(defun collection? (_)
	; (collection? obj) -> t | nil
	; Returns true if argument is a collection type
	(or (map? _) (set? _)))

(defun indexable? (_)
	(or (collection? _) (list? _)))

; Associative structure functions

(defun entries (_)
	; (entries collection) ->  list | nil
	; Returns a list of lists (key value) from maps and
	; a list from xset, of collection entries
	(defq rlist nil)
	(when (collection? _)
		(setq rlist (list))
		(if (map? _)
			(. _ :each (#(push rlist (list %0 %1))))
			(. _ :each (#(push rlist %0)))))
	rlist)

(defun keys (_)
	; (keys collection) -> list | nil
	; Returns the keys of an associative structure
	(defq res (list))
	(when (defq els (entries _))
		(each (lambda ((_k _v)) (push res _k)) els)) res)

(defun values (_)
	; (values collection) -> list | nil
	; Returns the values of an associative structure
	(defq res (list))
	(when (defq els (entries _))
		(each (lambda ((_k _v)) (push res _v)) els)) res)

(defun gets (kv-map _k &optional if_nil)
	; (gets collection k [if_nil]) -> value | if_nil | nil
	(opt (. kv-map :find _k) if_nil))

(defun gets-in (coll &rest kws)
	; (gets-in collection key-path) -> value | nil
	; Walks into collection to return the last kws value
	; Works on heterogenous collections/trees including lists
	(defq hit nil)
	(when (and (indexable? coll) (nempty? kws))
		(defq
			pick (reverse kws)
			curr coll)
		(while (defq _i (pop pick))
			(cond
				((collection? curr)
					(setq curr (gets curr _i)))
				((list? curr)
					(setq curr (elem _i curr)))
				(t	(throw "Not an indexable collection " (list _i curr)))))
		(setq hit curr))
	hit)

(defun sets! (kv-map _k &optional _v)
	; (sets! collection key value) -> collection
	(cond
		((map? kv-map)
			(. kv-map :insert _k _v))
		((set? kv-map)
			(. kv-map :insert _k)))
	kv-map)

(defun sets-pairs! (kv-map &rest pairs)
	; (sets-pairs! collection))
	; Inserts the key value pairs into map
	(each (lambda ((_k _v))
		(. kv-map :insert _k _v)) (partition 2 pairs))
	kv-map)

(defun sets-in! (kv-map val &rest kws))

(defun updates-in! (kv-map ufnc val &rest kws))

(defun drop! (kv-map _k)
	; (drop! collection key) -> collection
	(. kv-map :erase _k))

(defun merge-into! (kv-map &rest _maps)
	; (merge-into! collection (collections)) -> collection
	; Merges _maps into kv-map
	(each (lambda (_m)				  ; for each map
		(each (lambda ((_k _v))
			(. kv-map :insert _k _v)	; insert pair
			) (entries _m))) _maps)
	kv-map)

(defmacro merges (&rest _maps)
	; (merges (collections)) -> collection
	; Merges collections into a new collection
	(defq kvmap (if (emap? (first _maps)) (emap) (xmap)))
	`(merge-into! ,kvmap ~_maps))

(defmacro pairs-into-kv (pairs)
	; (pairs-into-kv list) -> emap
	; Creates a new emap and consumes key value pairs
	; (pairs-into kv (list :a 0 :b 1 :c 2))
	`(emap-kv ~pairs))

(defun empty (_coll)
	; (empty collection) -> collection | nil
	; Returns an empty collection of same category of _coll or nil
	(cond
		((indexable? _coll)
			(cond
				((emap? _coll) (emap))
				((xmap? _coll) (xmap))
				((xset? _coll) (xset))
				((fmap? _coll) (fmap))
				((list? _coll) (list))))
		(t nil)))

(defun into-map (m lop)
	; (into-map map list-of-pairs) -> map
	; Takes a map type and list of key/value lists
	; and sets into map
	(each (lambda ((_k _v)) (sets! m _k _v)) lop))

(defun into-set (xs loe)
	; (into-set set list-of-elements) -> set
	; Takes a set type and list of values
	; and adds into set
	(each (lambda (_v) (sets! xs _v)) loe))

(defun into-fn (a)
	; (into-fn collection) -> fn
	; Returns the 'into' function for the collection
	; type
	(cond
		((or (emap? a) (xmap? a) (fmap? a)) into-map)
		((xset? a) into-set)
		(t (lambda (&rest _) _))))
