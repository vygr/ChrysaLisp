;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; collections - ChyrsaLisp data collections library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/collections/emap.inc")
(import "lib/collections/xmap.inc")
(import "lib/collections/xset.inc")

(defun-bind map? (_)
    ; (map? object) -> t | nil
    ; Returns true if argument is a map type
    (or (emap? _) (xmap? _)))

(defun-bind set? (_)
    (xset? _))

(defun-bind collection? (_)
    ; (collection? obj) -> t | nil
    ; Returns true if argument is a collection type
    (or (map? _) (set? _)))

(defun-bind indexable? (_)
    (or (collection? _) (lst? _)))

; Associative structure functions

(defun-bind entries (_)
    ; (entries collections) -> list | nil
    ; Returns list of key/value pairs if map
    ; Returns list of values if set
    (defq bk (get :buckets _))
    (cond
        ((map? _)
            (cond
                ((xmap? _)
                    (defq res (list))
                    (each! 0 -1 (lambda (_k _v) (push res (list _k _v))) bk)
                    res)
                (t (tolist bk))))
        ((set? _)
            (first bk))))

(defun-bind keys (_)
    ; (keys collection) -> list | nil
    ; Returns the keys of an associative structure
    (defq res (list))
    (when (defq els (entries _))
        (each (lambda ((_k _v)) (push res _k)) els)) res)

(defun-bind values (_)
    ; (values collection) -> list | nil
    ; Returns the values of an associative structure
    (defq res (list))
    (when (defq els (entries _))
        (each (lambda ((_k _v)) (push res _v)) els)) res)

(defun-bind gets (kv-map _k &optional if_nil)
    ; (gets collection k [if_nil]) -> value | if_nil | nil
    (opt (. kv-map :find _k) if_nil))

(defun-bind gets-in (coll &rest kws)
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
                ((lst? curr)
                    (setq curr (elem _i curr)))
                (t
                    (throw "Not an indexable collection " (list _i curr)))))
        (setq hit curr))
    hit)

(defun-bind sets! (kv-map _k &optional _v)
    ; (sets! collection key value) -> collection
    (cond
        ((map? kv-map)
            (. kv-map :insert _k _v))
        ((set? kv-map)
            (. kv-map :insert _k)))
    kv-map)

(defun-bind sets-pairs! (kv-map &rest pairs)
    ; (sets-pairs! collection))
    (each (lambda ((_k _v))
        (. kv-map :insert _k _v)) (partition 2 pairs))
    kv-map)

(defun-bind sets-in! (kv-map val &rest kws))

(defun-bind updates-in! (kv-map ufnc val &rest kws))

(defun-bind drop! (kv-map _k)
    ;(drop! collection key) -> collection
    (. kv-map :erase _k))

(defun-bind merge-into! (kv-map &rest _maps)
    ; (merge-into! collection (collections)) -> collection
    ; Merges _maps into kv-map
    (each (lambda (_m)                  ; for each map
        (each (lambda ((_k _v))
            (. kv-map :insert _k _v)    ; insert pair
            ) (entries _m))) _maps)
    kv-map)

(defmacro-bind merges (&rest _maps)
    ; (merges (collections)) -> collection
    ; Merges collections into a new collection
    (defq kvmap (if (emap? (first _maps)) (emap) (xmap)))
    `(merge-into! ,kvmap ~_maps))

(defmacro-bind pairs-into-kv (pairs)
    `(emap-kv ~pairs))
