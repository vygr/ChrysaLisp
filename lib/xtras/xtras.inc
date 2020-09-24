;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; xtras - ChrysaLisp extra forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defq xtras_defined t)

; Predicates

(defun-bind nil? (_)
  ; (nil? o) -> bool
  ; Returns true if nil
  (eql _ nil))

(defun-bind neg? (_)
  ; (neg? num) -> bool
  ; Returns true if negative number
  (< _ 0))

(defun-bind pos? (_)
  ; (pos? num) -> bool
  ; Returns true if positive number
  (> _ 0))

(defun-bind zero? (_)
  ; (zero? num) -> bool
  ; Returns true if argument is equal to zero (0)
  (= _ 0))

(defun-bind one? (_)
  ; (one? num) -> bool
  ; Returns true if argument is equal to one (1)
  (= _ 1))

(defun-bind odd? (_)
  ; (odd? num) -> bool
  ; Returns true if argument is equal to odd number
  (one? (logand 1 _)))

(defun-bind even? (_)
  ; Returns true if argument is equal to even number
  ; (even? num) -> bool
  (not (odd? _)))

(defun-bind neql? (a b)
  ; (neql? value value) -> bool
  ; Returns true if values are not eql
  (not (eql a b)))

(defun-bind dict? (_)
  ; (dict? form) -> bool
  ; Returns true if argument is a dictionary type (env)
  (env? _))

(defun-bind props? (_)
  ; (props? el) -> bool
  ; Returns true if argument is a properties type
  (when (and
    (lst? _)
      (eql (getp _ :super_clz) :clz_properties))
    t))

(defun-bind def? (_ &optional e)
  ; (def? sym [env]) -> bool
  ; Returns t if sym is defined in environment
  ; Defaults to current environment if not provided
  (setd e (env))
  (not (eql (get _ e) nil)))

(defun-bind ndef? (_ &optional e)
  ; (ndef? sym [env]) -> bool
  ; Returns t if sym is not defined in environment
  (not (def? _ e)))

(defun-bind kw? (_)
  ; (kw? el) -> bool
  ; Returns true if argument is a keyword
  (and (sym? _) (starts-with ":" _)))

(defun-bind truthy? (_)
  (cond
    ((lst? _) (nempty? _))
    (t (not (nil? _)))))

; Convenience

(defq +numeric_chars+ "0123456789")
(defq +plus_minus+ "-+")

(defun-bind str-is-ints? (s)
  ; (str-is-ints? s) -> :true | :false
  (defq instr
    (if (find (first s) +plus_minus+)
      (rest s)
      s))
  (reduced-reduce
      (lambda (acc el)
        (if  (find el +numeric_chars+)
          acc
          (reduced :false))
        ) instr :true))

(defun-bind ident (_)
  ; (ident el) -> el
  _)

(defun-bind efind (p k)
  ; (efind list key) -> index | nil
  ; Returns the index of key in list or properties
  ; Used when keys are number and/or string
  (defq fx nil)
  (each! 0 -1
    (lambda (s)
      (when (eql s k)
        (setq fx _))) (list p))
  fx)

(defun-bind kw (_)
  (if (and (not (kw? _)) (or (str? _) (sym? _)))
    (sym (str ":" _))
    (throw "Expected symbol or string, found " _)))

(defun-bind strip-rest (in-args)
  ; (strip-rest in-args)
  ; Returns inner list if nested from passing
  ; arguments through multiple &rest constructs
  (if (and
        (nempty? in-args)
        (lst? (first in-args)))
      (first in-args)
      in-args))

; Sequence forms

(defmacro-bind interleave (seq1 seq2)
  ; (interleave seq1 seq2) -> list
  ; Returns a list of the first item in each seq, then the second etc.
  `(zip ,seq1 ,seq2))

(defun-bind chunk (cnt seq)
  ; (chunk 2 '(0 1 2 3')) -> ((0 1) (2 3))
  ; Chunks up a sequence into list of sequence by cnt
  (cond
    ((< cnt (length seq))
      (defq
        base (reverse (copy seq))
        res (list))
      (while (not (empty? base))
        (defq nl (list))
        (times cnt (push nl (pop base)))
        (push res nl))
      res)
    (t (list seq))))

; (defun-bind unions (&rest lists)
;   ; (unions [list]...) -> list
;   (defq nl (list))
;   (each (curry merge nl) lists))

; (defun-bind unions-with (base &rest lists)
;   ; (unions-with list [list]...) -> list'
;   (each (curry merge base) lists))

; Properties (a.k.a. list of pairs [k v]) forms

(defmacro-bind properties (&rest pairs)
  ; (properties [k v]...) -> list
  (push pairs :super_clz :clz_properties)
  `(list ~pairs))

(defun-bind entries (_)
  ; (entries properties) -> entry list
  ; Returns a list of entries where each entry: (key value)
  (when (props? _)
    (defq c (chunk 2 _))
    (reduce (lambda (acc e)
      (if (or (eql (first e) :clz) (eql (first e) :super_clz))
        acc
        (push acc e)))
      c (list))))

(defun-bind keys (prop)
  ; (keys properties) -> key list
  (map (#(elem 0 %0)) (entries prop)))

(defun-bind values (prop)
  ; (values properties) -> value list
  (map (#(elem 1 %0)) (entries prop)))

(defun-bind getp (p k &optional if_nil)
  ; (getp properties key [if_nil]) -> val | if_nil | nil
  (defq fx nil)
  (cond
    ((or (kw? k) (sym? k))
      (setq fx (find k p)))
    ((or (num? k) (str? k))
      (setq fx (efind p k)))
    (t nil))
  (if fx (elem (inc fx) p) (opt nil if_nil)))

(defun-bind getp-in (p &rest kws)
  ; (getp-in (properties kw...)) -> value | nil
  ; Returns the value in a nested properties structure
  ; where kws is a sequence (list) of keys. Returns nil
  ; if a key is not present
  (defq hit nil)
  (cond
    ((not (props? p))
      (throw "Not a properties structure " p))
    ((empty? kws)
      (throw "No keys path provided " kws))
    (t
      (defq
        keep_going t
        last_nest  p
        pick (reverse kws)
        fkw  (last pick))
      (while (and keep_going (not (empty? pick)))
        (defq v (getp last_nest fkw))
        (pop pick)
        (cond
          ((not v)
            (setq keep_going nil))
          ((empty? pick)
            (setq
              keep_going nil
              hit v))
          (t
            (setq
              fkw       (last pick)
              last_nest v))))))
    hit)

(defun-bind setp! (p k v &optional add_if_nil)
  ; (setp properties k value [add_if_nil]) -> properties
  (defq ki (if (kw? k) (find k p) (efind p k)))
  (if (not ki)
    (if add_if_nil
      (push p k v)
      (throw (str "Key " k " does not exist") p))
    (elem-set (inc ki) p v))
  p)

(defun-bind setsp! (p &rest pairs)
  ; (setsp properties [k v] ...) -> properties
  (each (#(setp! p (first %0) (second %0) t)) (chunk 2 pairs))
  p)

(defun-bind setsp-in! (p val &rest kws)
  ; (setsp-in! properties value kw...) -> properties
  ; Sets the value in a nested properties structure
  ; where kws is a sequence of keys.
  (cond
    ((not (props? p))
      (throw "Not a properties structure " p))
    ((empty? kws)
      (throw "No keys path provided " kws))
    (t
      (defq
        keep_going t
        last_nest  p
        pick (reverse kws)
        fkw  (last pick)
        trg  (first pick))
      (while (and keep_going (not (empty? pick)))
        (defq v (getp last_nest fkw))
        (pop pick)
        (cond
          ((not v)
            (setq keep_going nil))
          ((empty? pick)
            (if (eql trg fkw)
              (setp! last_nest trg val))
            (setq keep_going nil))
          (t (setq
            fkw       (last pick)
            last_nest v))))))
  p)

(defun-bind update-in! (p ufnc &rest kws)
  ; (update-in! properties ufnc kw...)) -> value | nil
  ; Updates the value found in the key-leaf of kws with the
  ; result of calling ufnc with the current value
  ; Returns nil if a key is not present
  (defq hit nil)
  (cond
    ((not (props? p))
      (throw "Not a properties structure " p))
    ((empty? kws)
      (throw "No keys path provided " kws))
    (t
      (defq
        keep_going t
        last_nest  p
        pick (reverse kws)
        fkw  (last pick))
      (while (and keep_going (not (empty? pick)))
        (defq v (getp last_nest fkw))
        (pop pick)
        (cond
          ; If key not found exit
          ((not v)
            (setq keep_going nil))
          ; Otherwise, if we are at leaf we
          ; have our target entry
          ((empty? pick)
            (setq
              keep_going nil
              hit (ufnc (elem (inc (efind last_nest fkw)) last_nest)))
            (setp! last_nest fkw hit))
          (t
            (setq
              fkw       (last pick)
              last_nest v))))))
    hit)

(defun-bind pmerge (&rest props)
  ; (pmerge properties...) -> properties
  ; Returns a new properties structure that consists of
  ; the rest of the properties structures merged into
  ; it
  (reduce
    (#(each (lambda (e)
      (setp! %0 (first e) (second e) t)) (chunk 2 %1)))
    props (properties)))

(defun-bind move-entry-forward! (p sp ep)
  ; (move-entry-forward! properties start-key-index end-key-index)
  ; Moves the properties k/v pair forward from key index sp to index ep
  (while (/= sp ep)
    (defq tsp (+ sp 2))
    (swap p sp tsp)
    (swap p (inc sp) (+ sp 3))
    (setq sp tsp))
  p)

(defun-bind move-entry-backward! (p sp ep)
  ; (move-entry-backward! properties start-key-index end-key-index)
  ; Moves the properties k/v pair backward from key index sp to index ep
  (while (/= sp ep)
    (defq tsp (- sp 2))
    (swap p sp tsp)
    (swap p (inc sp) (dec sp))
    (setq sp tsp))
  p)

(defun-bind pdrop! (p k)
  ; (pdrop properties key) -> properties
  ; Drops a properties key/pair
  (when (props? p)
    (defq
      sp (efind p k)
      ep (- (length p) 2))
    (when sp
      (move-entry-forward! p sp ep)
      (times 2 (pop p)))))

; Privatize functions

(defmacro private (nm)
  ; (private nm) -> env
  `(macroexpand (defq ,nm (env -1))))

(defmacro defunp (e n a &rest _)
  ; (defunp env name args form) -> lambda
  `(def ,e ,n (lambda ,a ~_)))

(defmacro defunp-bind (e n a &rest _)
  ;(defun-bind name ([arg ...]) body)
  `(def ,e ,n (lambda ,a ~(prebind (macroexpand _)))))

(defmacro callp (e name &rest _)
  ; (callp env name &rest args) -> form invoked with args
  `((get ,name ,e) ~_))

; Walkers

(defun walk-tree (tree fnc)
  (defq stack (if (> (length tree) 0) (list tree 0) '()))
  (while (defq i (pop stack) l (pop stack))
    (defq e (elem i l))
    (if (< (setq i (inc i)) (length l))
        (push stack l i))
    (if (lst? e)
      (if (> (length e) 0) (push stack e 0))
      (fnc e))))

(defun-bind walk-tree-recur (inner-fn outer-fn tree)
  ; (walk-tree-recur inner-fn outer-fn tree) -> tree
  ; Recusively walk tree and handles dictionaries
  (cond
    ((lst? tree)
      (outer-fn
        (apply list
          (map (lambda (el)
            (if (dict? el)
              (walk-tree-recur inner-fn outer-fn el)
              (inner-fn el))) tree))))
    ((dict? tree)
      (map (curry walk-tree-recur inner-fn outer-fn) (dict-entries tree)))
    (t (outer-fn tree))))

(defun-bind pre-walk-recur (fn tree)
  ; (pre-walk-recur fn tree)
  (walk-tree-recur (curry pre-walk-recur fn) ident (fn tree)))

(defun-bind post-walk-recur (fn tree)
  ; (post-walk-recur fn tree)
  (walk-tree-recur (curry post-walk-recur fn) fn tree))

; Take and drop forms

(defun-bind take (cnt seq)
  ; (take count sequence) -> sequence
  ; Returns the first `cnt` of elements from seq
  ; providing a negative cnt returns the original sequence
  ; example:
  ; (defq myL2 (list -2 -1 0 -1 -2 3))
  ; (take 2 '(0 1 2 3)) -> (0 1)
  (defq len (length seq))
  (cond
    ((neg? cnt) seq)
    (t (slice 0 (cond ((> cnt len) len) (t cnt)) seq))))


(defun-bind take-last (cnt seq)
  ; (take-last count sequence) -> sequence
  ; Returns the last 'cnt' of elements from seq
  ; providing a negative cnt returns the original sequence
  ; example:
  ; (defq myL2 (list -2 -1 0 -1 -2 3))
  ; (take-last 2 '(0 1 2 3)) ->  (2 3)
  (reverse (take cnt (reverse seq))))

(defun-bind take-while (pred seq)
  ; (take-while predicate sequence) -> sequence
  ; Returns successive element of seq while pred returns true
  ; example:
  ; (defq myL2 (list -2 -1 0 -1 -2 3))
  ; (take-while neg? '(-1 -2 0 -1 -2)) -> (-1 -2)
  (reduced-reduce
    (lambda (acc el)
      (cond
        ((pred el) (push acc el))
        (t (reduced acc))))
    seq (list)))

(defun-bind take-unless (pred seq)
  ; (take-unless predicate sequence) -> sequence
  ; Returns successive element of seq unless pred returns true
  ; example:
  ; (defq myL2 (list -2 -1 0 -1 -2 3))
  ; (take-unless (# (< %0 0)) '(-1 -2 0 -1 -2)) -> (0)
  (reduced-reduce
    (lambda (acc el)
      (cond
        ((pred el) acc)
        (t (push acc el))))
    seq (list)))

(defun-bind drop (cnt seq)
  ; (drop count sequence) -> sequence
  ; Returns a sequence with the first `cnt` of elements removed
  ; providing a negative cnt returns the original sequence
  ; example:
  ; (defq myL2 (list -2 -1 0 -1 -2 3))
  ; (drop 2 myL2)       ; -> (0 -1 -2 3)
  (defq len (length seq))
  (cond
    ((neg? cnt) seq)
    (t (slice (cond ((> cnt len) -1) (t cnt)) -1 seq))))

(defun-bind drop-last (cnt seq)
  ; (drop-last count sequence) -> sequence
  ; providing a negative cnt returns the original sequence
  ; example:
  ; (defq myL2 (list -2 -1 0 -1 -2 3))
  ; (drop-last 2 myL2)  ; -> (-2 -1 0 -1)
  (reverse (drop cnt (reverse seq))))

(defun-bind drop-while (pred seq)
  ; (drop-while predicate seq) -> sequence
  ; Drops successive element of seq while pred returns true
  ; example:
  ; (defq myL2 (list -2 -1 0 -1 -2 3))
  ; (drop-while (# (< %0 0)) myL2)  ; -> (0 -1 -2 3)
  (slice (length (take-while pred seq)) -1 seq))

(defun-bind drop-unless (pred seq)
  ; (drop-unless predicate sequence) -> sequence
  ; Drops elements from sequence unless predicate returns true
  ; example:
  ; (defq myL2 (list -2 -1 0 -1 -2 3))
  ; (drop-unless (# (< %0 0)) myL2) ; -> (-2 -1 -1 -2)
  (reduced-reduce
    (lambda (acc el)
      (cond
        ((pred el) (push acc el))
        (t acc)))
    seq (list)))

; Data Object AST (Node Tree)

(defun Node (ttype &optional value)
  (properties
      :type         ttype
      :value        value
      :children     (list)))

(defun DocStartNode ()
  (Node :docstart "---"))

(defun DocEndNode ()
  (Node :docend "..."))

(defun MapNode ()
  (Node :map))

(defun MapEntryNode ()
  (Node :map_entry))

(defun KeyNode ()
  (Node :key))

(defun ValueNode ()
  (Node :value))

(defun SequenceNode ()
  (Node :seq))

(defun ScalarNode (stype value)
  (setp! (Node :scalar value) :stype stype t))

(defun add-child-node! (p c)
  (when (not (lst? (defq chs (getp p :children))))
    (throw "Children of node is not a list" p))
  (push chs c)
  c)

(defun me-key? (_)
  (eql (getp _ :clz) :me_key))

(defun me-value? (_)
  (eql (getp _ :clz) :me_value))

(defun prop-me? (_)
  (eql (getp _ :clz) :prop_me))

(defun make-me-key (_)
  (list :clz :me_key :value _))

(defun make-me-value (_)
  (list :clz :me_value :value _))

(defun split-entries (p)
  (reduce
    (lambda (acc el)
      (push acc
        (list :clz :prop_me :entries
              (push
                (push (list) (make-me-key (first el)))
                (make-me-value (second el))))))
    (entries p) (list)))

; AST Node Context Stack

(defun Context ()
  ; (Context) -> properties
  ; Constructs a common node stack context
  (properties
    :current nil
    :path    (list)))

(defun set-context! (cntxt n)
  ; (set-context! node) -> node
  ; Makes the current context 'node'
  ; after adding 'node' to current's children
  (defq crn (getp cntxt :current))
  ; Stack node in path for un-setting
  (push (getp cntxt :path) n)
  ; If first
  (when crn
    (add-child-node! crn n))
  (setp! cntxt :current n)
  n)

(defun unset-context! (cntxt)
  ; (unset-context!) -> node | nil
  ; Set's context to most recent in path
  (defq
    npath (getp cntxt :path)  ; Setup
    lnode (pop npath))          ; Pop path stack
  (when (truthy? npath)
    (setp! cntxt :current (last npath)))
  nil)

(defun add-to-context! (cntxt n)
  ; (add-to-context! node) -> node | nil
  (when (truthy? (defq crn (getp cntxt :current)))
    (add-child-node! crn n)))

(defun obj-to-node (cntxt odata)
  ; (obj-to-node Context object)
  ; Builds an common Node AST from object
  (cond
    ((lst? odata)
        (cond
          ; Map
          ((props? odata)
           (set-context! cntxt (MapNode))
           (each (#(obj-to-node cntxt %0)) (split-entries odata))
           (unset-context! cntxt))
          ; Map Entry
          ((prop-me? odata)
           (set-context! cntxt (MapEntryNode))
           (each (#(obj-to-node cntxt %0)) (getp odata :entries))
           (unset-context! cntxt))
          ; Key
          ((me-key? odata)
           (set-context! cntxt (KeyNode))
           (obj-to-node cntxt (getp odata :value))
           (unset-context! cntxt))
          ; Value
          ((me-value? odata)
           (set-context! cntxt (ValueNode))
           (obj-to-node cntxt (getp odata :value))
           (unset-context! cntxt))
          ; Sequence
          (t
            (set-context! cntxt (SequenceNode))
            (each (#(obj-to-node cntxt %0)) odata)
            (unset-context! cntxt))))
    ((or (eql odata t) (eql odata nil))
     (add-to-context! cntxt (ScalarNode :boolean odata)))
    ((kw? odata)
     (add-to-context! cntxt (ScalarNode :keyword odata)))
    ((sym? odata)
     (add-to-context! cntxt (ScalarNode :symbol odata)))
    ((str? odata)
     (add-to-context! cntxt (ScalarNode :string odata)))
    ((num? odata)
     (add-to-context! cntxt (ScalarNode :number odata)))
    (t
     (throw "Unknown type found in obj-to-node" odata))))

