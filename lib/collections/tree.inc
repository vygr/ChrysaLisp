;;;;;;;;;;;;;;;;;;;;;;;;;;;
; collection tree load/save
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +char_class_ascii (const (cat " !" {""}
	"#$%&'()*+,-./0123456789:;<=>?@"
	"ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`"
	"abcdefghijklmnopqrstuvwxyz{|}~")))

(defun tree-type (collection)
	; (tree-type collection) -> type
	(if (eql (defq type (last (type-of collection))) :hmap)
		(setq type (last (. collection :type_of)))) type)

(defun tree-collection? (type)
	; (tree-collection? type) -> :nil | type
	(some (# (if (eql %0 type) %0))
		'(:list :pset :pmap :array :path :Emap :Lmap :Fmap :Lset :Fset)))

(defq +tree_chunk_size 512)

(defun tree-encode-chunk (atom)
	(if (/= (rbskip +char_class_ascii (defq esc (escape atom)) -1) 0)
		(hex-encode atom)
		(cat "[" esc "]")))

(defun tree-encode (atom)
	; (tree-encode atom) -> atom
	(cond
		((sym? atom) atom)
		((num? atom) atom)
		((str? atom)
			(defq enc (tree-encode-chunk atom))
			(if (<= (length enc) +tree_chunk_size)
				(cat "\q" enc "\q")
				(defq chunks (reduce (lambda (p off)
						(push p (cat "\q" (slice enc off (min (length enc) (+ off +tree_chunk_size))) "\q")))
					(range 0 (length enc) +tree_chunk_size) (list)))
				(cat "(:cat\n\t\t" (join chunks "\n\t\t") ")")))
		((throw "Invalid tree encode !" atom))))

(defun tree-decode-chunk (chunk)
	(if (eql (first chunk) "[")
		(slice chunk 1 -2)
		(hex-decode chunk)))

(defun tree-decode (atom)
	; (tree-decode atom) -> atom
	(cond
		((and (list?? atom) (eql (first atom) :cat))
			(tree-decode-chunk (apply (const cat) (rest atom))))
		((sym? atom) atom)
		((num? atom) atom)
		((str? atom) (tree-decode-chunk atom))
		((throw "Invalid tree decode !" atom))))

(defun tree-buckets (collection type)
	; (tree-buckets collection type) -> num
	(if (find type '(:list :pset :pmap :array :path))
		(length collection)
		(if (find type '(:Lmap :Lset)) 1
			(get :num_buckets collection))))

(defun tree-node ((type &optional buckets))
	; (tree-node ((type [buckets])) -> collection
	(setd buckets 1)
	(case type
		(:Lset (Lset))
		(:Lmap (Lmap))
		(:Emap (Emap buckets))
		(:Fmap (Fmap buckets))
		(:Fset (Fset buckets))
		(:array (cap buckets (array)))
		(:path (cap buckets (path)))
		(:pset (cap buckets (pset)))
		(:pmap (cap buckets (pmap)))
		(:t (cap buckets (list)))))

(defun tree-save (stream root &optional key_filters)
	; (tree-save stream tree [key_filters]) -> tree | :nil
	(when stream
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
				((:Fset :Lset)
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
								((list?? v)
									;indent
									(++ wc)
									(push stack v 1)
									(write-line stream (str p k))
									(write-line stream (str p "(" (first v))))
								(:t (write-line stream (str p k " " (tree-encode v))))))
						((:array :path)
							(defq k (elem-get l i))
							(cond
								((list?? k)
									;indent
									(push stack l (inc i))
									(++ wc)
									(push stack k 1)
									(write-line stream (str p "(" (first k))))
								(:t (push stack l (length l))
									(each (# (write-line stream (cat p (slice (str %0) 1 -2))))
										(partition (slice l 1 -1) 16)))))
						(:t (defq k (elem-get l i))
							(push stack l (inc i))
							(cond
								((list?? k)
									;indent
									(++ wc)
									(push stack k 1)
									(write-line stream (str p "(" (first k))))
								(:t (write-line stream (str p (tree-encode k))))))))
				(:t ;outdent
					(write-line stream (cat (rest p) ")"))
					(-- wc))))
		root))

(defun tree-load (stream)
	; (tree-load stream) -> tree | :nil
	(when stream
		(defq items (first (read stream)) root (tree-node (first items))
			stack (list (rest items) root))
		(while (defq node (pop stack) items (pop stack))
			(case (tree-type node)
				((:Emap :Lmap :Fmap)
					(each (lambda ((k v))
						(if (and (list?? v) (nql (first v) :cat))
							(push stack (rest v) (setq v (tree-node (first v))))
							(setq v (tree-decode v)))
						(. node :insert (tree-decode k) v)) (partition items 2)))
				((:Fset :Lset)
					(each (lambda (k)
						(if (and (list?? k) (nql (first k) :cat))
							(push stack (rest k) (setq k (tree-node (first k))))
							(setq k (tree-decode k)))
						(. node :insert k)) items))
				(:t (each (lambda (k)
						(if (and (list?? k) (nql (first k) :cat))
							(push stack (rest k) (setq k (tree-node (first k))))
							(setq k (tree-decode k)))
						(push node k)) items))))
		root))

;module
(export-symbols '(tree-load tree-save tree-encode tree-decode tree-decode-chunk))
(env-pop)