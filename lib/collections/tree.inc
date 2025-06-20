;;;;;;;;;;;;;;;;;;;;;;;;;;;
; collection tree load/save
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun tree-type (collection)
	; (tree-type collection) -> type
	(if (eql (defq type (last (type-of collection))) :hmap)
		(setq type (last (. collection :type_of)))) type)

(defun tree-collection? (type)
	; (tree-collection? type) -> :nil | type
	(some (# (if (eql %0 type) %0))
		'(:list :array :path :Emap :Lmap :Fmap :Fset)))

(defun tree-encode (atom)
	; (tree-encode atom) -> atom
	(cond
		((sym? atom) atom)
		((num? atom) atom)
		((str? atom) (cat {"} (id-encode atom) {"}))
		((throw "Invalid tree encode !" atom))))

(defun tree-decode (atom)
	; (tree-decode atom) -> atom
	(cond
		((sym? atom) atom)
		((num? atom) atom)
		((str? atom) (id-decode atom))
		((throw "Invalid tree decode !" atom))))

(defun tree-buckets (collection type)
	; (tree-buckets collection type) -> num
	(if (find type '(:list :array :path))
		(length collection)
		(if (eql type :Lmap) 1
			(get :num_buckets collection))))

(defun tree-node ((type &optional buckets))
	; (tree-node ((type [buckets])) -> collection
	(setd buckets 1)
	(case type
		(:Lmap (Lmap))
		(:Emap (Emap buckets))
		(:Fmap (Fmap buckets))
		(:Fset (Fset buckets))
		(:array (cap buckets (array)))
		(:path (cap buckets (path)))
		(:t (cap buckets (list)))))

(defun tree-save (stream root &optional key_filters)
	; (tree-save stream tree [key_filters]) -> tree
	(setd key_filters '())
	(defq out (list) stack (list root out)
		ws (const (pad "" 8 (ascii-char 9))) wc 1)
	;convert to typed list tree
	(while (defq l (pop stack) c (pop stack))
		(push l (list (defq type (tree-type c)) (tree-buckets c type)))
		(case type
			((:Emap :Lmap :Fmap)
				(. c :each (lambda (k v)
					(when (notany (# (eql k %0)) key_filters)
						(if (tree-collection? (tree-type v))
							(push stack v (setq v (list))))
						(push l (list k v))))))
			(:Fset
				(. c :each (lambda (k)
					(when (notany (# (eql k %0)) key_filters)
						(if (tree-collection? (tree-type k))
							(push stack k (setq k (list))))
						(push l k)))))
			(:t (each (lambda (k)
					(if (tree-collection? (tree-type k))
						(push stack k (setq k (list))))
					(push l k)) c))))
	;pretty write the typed list tree
	(push stack out 1)
	(write-line stream (str "(" (first out)))
	(while (defq i (pop stack) l (pop stack))
		(defq p (pad "" wc ws))
		(bind '(type buckets) (first l))
		(cond
			((< i (length l))
				(case type
					((:Emap :Lmap :Fmap)
						(bind '(k v) (elem-get l i))
						(push stack l (inc i))
						(setq k (tree-encode k))
						(cond
							((list? v)
								;indent
								(++ wc)
								(push stack v 1)
								(write-line stream (str p k))
								(write-line stream (str p "(" (first v))))
							(:t (write-line stream (str p k " " (tree-encode v))))))
					((:array :path)
						(defq k (elem-get l i))
						(cond
							((list? k)
								;indent
								(push stack l (inc i))
								(++ wc)
								(push stack k 1)
								(write-line stream (str p "(" (first k))))
							(:t	(push stack l (length l))
								(each (# (write-line stream (cat p (slice (str %0) 1 -2))))
									(partition (slice l 1 -1) 16)))))
					(:t (defq k (elem-get l i))
						(push stack l (inc i))
						(cond
							((list? k)
								;indent
								(++ wc)
								(push stack k 1)
								(write-line stream (str p "(" (first k))))
							(:t (write-line stream (str p (tree-encode k))))))))
			(:t ;outdent
				(write-line stream (cat (rest p) ")"))
				(-- wc))))
	root)

(defun tree-load (stream)
	; (tree-load stream) -> tree
	(defq items (first (read stream)) root (tree-node (first items))
		stack (list (rest items) root))
	(while (defq node (pop stack) items (pop stack))
		(case (tree-type node)
			((:Emap :Lmap :Fmap)
				(each (lambda ((k v))
					(if (list? v)
						(push stack (rest v) (setq v (tree-node (first v))))
						(setq v (tree-decode v)))
					(. node :insert (tree-decode k) v)) (partition items 2)))
			(:Fset
				(each (lambda (k)
					(if (list? k)
						(push stack (rest k) (setq k (tree-node (first k))))
						(setq k (tree-decode k)))
					(. node :insert k)) items))
			(:t (each (lambda (k)
					(if (list? k)
						(push stack (rest k) (setq k (tree-node (first k))))
						(setq k (tree-decode k)))
					(push node k)) items))))
	root)

;module
(export-symbols '(tree-load tree-save))
(env-pop)
