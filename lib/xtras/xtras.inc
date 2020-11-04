;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; xtras - ChrysaLisp extra forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defq xtras_defined t)

; Constants

(defq
  +nl+          (char 0x0a)
  +eof+         (char 0x00)
  +dblq+        (char 0x22)
  +singq+       (char 0x27)
  +kw_ind+      (char 0x3a)
  +upper_chars+ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  +lower_chars+ "abcdefghijklmnopqrstuvwxyz"
  +bin_prefix+  "0bB"
  +oct_prefix+  "0oO"
  +hex_prefix+  "0xX"
  +oct_chars+   "01234567"
  +bin_chars+   "01"
  +plus_minus+  "-+"
  +dec_point+   "."
  +numeric_chars+ (cat +oct_chars+ "89")
  +hex_chars+ (cat +numeric_chars+ (slice 0 6 +upper_chars+) (slice 0 6 +lower_chars+))
  +alpha_num+ (cat +numeric_chars+ +upper_chars+ +lower_chars+))

; Predicates

(defun-bind numcharset? (_ cset)
  ; (numcharset? val charset) -> t | nil
  (reduced-reduce (#(if (find %1 cset) %0 (reduced nil))) _ t))

(defun-bind intstr? (_)
  ; (intstr? val) -> t | nil
  (cond
    ((not (find +dec_point+ _))
      (numcharset? _ +numeric_chars+))
    (t
      nil)))

(defun-bind decstr? (_)
  ; (decstr? val) -> t | nil
  (if (find +dec_point+ _)
    (progn
      (defq
        psplit (split _ +dec_point+))
      (and
        (= (length psplit) 2)
        (numcharset? (first psplit) +numeric_chars+)
        (numcharset? (second psplit) +numeric_chars+)))
    nil))

(defun-bind _!base10str? (_ prefix charset)
  ; (_!base10str? val prefixchars charset) -> t | nil
  (cond
    ((and
      (eql (first _) (first prefix))
      (or
        (eql (second _) (second prefix))
        (eql (second _) (last prefix))))
      (numcharset? (slice 2 -1 _) charset))
    (t
      nil)))

(defun-bind hexstr? (_)
  (_!base10str? _ +hex_prefix+ +hex_chars+))

(defun-bind octstr? (_)
  (_!base10str? _ +oct_prefix+ +oct_chars+))

(defun-bind binstr? (_)
  (_!base10str? _ +bin_prefix+ +bin_chars+))

(defun-bind numstr-type (_)
  ; (numstr-type el) -> numeric string tuple
  ; Returns the numeric string type tuple, where:
  ; Elem 0 = 1 for positive, -1 for negative
  ; Elem 1 = :hexidecimal | :octal | :binary | :integer | :decimal | :nan
  ; Elem 2 = unprefixed input base portion of string
  ; Elem 3 = decimal portion of string iff Elem 1 is :decimal | nil
  ; Elem 4 = base 16 | 10 | 8 | 2 | nil
  (when (nempty? _)
    (defq res (list
      (if (eql (first _) "-")
        (progn (setq _ (rest _)) -1)
        (if (eql (first _) "+")
          (progn (setq _ (rest _)) 1)
          1))))
      (cond
        ((hexstr? _ )
          (push res :hexidecimal (slice 2 -1 _) nil 16))
        ((octstr? _ )
          (push res :octal (slice 2 -1 _) nil 8))
        ((binstr? _ )
          (push res :binary (slice 2 -1 _) nil 2))
        ((intstr? _)
          (push res :integer _ nil 10))
        ((decstr? _)
          (defq sp (split _ +dec_point+))
          (push res :decimal (first sp) (second sp) 10))
        (t (push res :nan _ nil nil)))))

(defun-bind numstr? (_)
  ; (numstr? string) -> t | nil
  ; Tests whether string is numeric form
  (not (eql (second (numstr-type _)) :nan)))

(defun-bind to-num (s)
  ; (to-num string) -> number
  (bind '(mu ty wn dn ba)  (numstr-type s))
  (when (not (eql ty :nan))
    (case ty
      ((:integer)
        (* (str-to-num wn) mu))
      ((:decimal)
        (* (i2f mu) (+ (i2f (str-to-num wn))
          (/ (i2f (str-to-num dn)) (i2f (pow ba (length dn)))))))
      ((:hexidecimal :octal :binary)
        (defq num 0)
        (each (# (setq num (+ (* num ba) (char-to-num %0)))) wn)
        (* mu num)))))

(defun-bind zero? (_)
  ; (zero? num) -> bool
  ; Returns true if argument is equal to zero (0)
  (= _ 0))

(defun-bind one? (_)
  ; (one? num) -> bool
  ; Returns true if argument is equal to one (1)
  (= _ 1))

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
  (emap? _))

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

(defun-bind str-is-ints? (s)
  ; (str-is-ints? s) -> :true | :false
  ; TODO: Deprecate for numstr?
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
  ; (kw el) -> keyword
  ; Converts a string or symbol to a keyword
  ; example:
  ; (kw "foo") -> :foo
  ; (kw ":foo") -> :foo
  (if (and (not (kw? _)) (or (str? _) (sym? _)))
    (if (eql (first _) +kw_ind+)
      (sym _)
      (sym (str +kw_ind+ _)))
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
  ; example:
  ; (interleave '(:a :b :c) '(0 1 2)) -> (:a 0 :b 1 :c 2)
  `(zip ,seq1 ,seq2))

; Properties (a.k.a. list of pairs [k v]) forms
; Deprecated and is using an `emap` under the covers

(defmacro-bind properties (&rest pairs)
  ; (properties [k v]...) -> list
  ; Creates a properties list object
  `(xmap-kv ~pairs))

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
  ; (take-last 2 '(0 1 2 3)) ->  (2 3)
  (reverse (take cnt (reverse seq))))

(defun-bind take-while (pred seq)
  ; (take-while predicate sequence) -> sequence
  ; Returns successive element of seq while pred returns true
  ; example:
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
  (emap-kv
      :type         ttype
      :value        value
      :children     (list)))

(defun DocStartNode ()
  (Node :docstart "---"))

(defun DocEndNode ()
  (Node :docend "..."))

(defun MapNode ()
  (Node :map))

(defun EMapNode ()
  (Node :emap))

(defun KeyNode (val)
  (Node :me_key val))

(defun ValueNode (val)
  (Node :me_value val))

(defun MapEntryNode (_k _v)
  (sets-pairs! (Node :map_entry)
    :mkey (KeyNode _k )
    :mval (ValueNode _v)))

(defun SetNode ()
  (Node :set))

(defun SequenceNode ()
  (Node :seq))

(defun ScalarNode (stype value)
  (sets! (Node :scalar value) :stype stype))

(defun add-child-node! (p c)
  (when (not (lst? (defq chs (gets p :children))))
    (throw "Children of node is not a list" p))
  (push chs c)
  c)

(defun map-me? (_)
  (eql (gets _ :type) :map_entry))

(defun me-key? (_)
  (eql (gets _ :type) :me_key))

(defun me-value? (_)
  (eql (gets _ :type) :me_value))

; AST Node Context Stack

(defun Context ()
  ; (Context) -> properties
  ; Constructs a common node stack context
  (emap-kv
    :current nil
    :path    (list)))

(defun set-context! (cntxt n)
  ; (set-context! node) -> node
  ; Makes the current context 'node'
  ; after adding 'node' to current's children
  (defq crn (gets cntxt :current))
  ; Stack node in path for un-setting
  (push (gets cntxt :path) n)
  ; If first
  (when crn
    (add-child-node! crn n))
  (sets! cntxt :current n)
  n)

(defun unset-context! (cntxt)
  ; (unset-context!) -> node | nil
  ; Set's context to most recent in path
  (defq
    npath (gets cntxt :path)  ; Setup
    lnode (pop npath))          ; Pop path stack
  (when (truthy? npath)
    (sets! cntxt :current (last npath)))
  nil)

(defun add-to-context! (cntxt n)
  ; (add-to-context! node) -> node | nil
  (when (truthy? (defq crn (gets cntxt :current)))
    (add-child-node! crn n)))

(defun obj-to-node (cntxt odata)
  ; (obj-to-node Context object)
  ; Builds an common Node AST from object
  (cond
    ((indexable? odata)
        (cond
          ; Lists
          ((lst? odata)
            (set-context! cntxt (SequenceNode))
            (each (#(obj-to-node cntxt %0)) odata)
            (unset-context! cntxt))
          ; Map Entry
          ((map-me? odata)
            (set-context! cntxt odata)
            (obj-to-node cntxt (gets odata :mkey))
            (obj-to-node cntxt (gets odata :mval))
            (unset-context! cntxt))
          ; Key
          ((me-key? odata)
            (set-context! cntxt odata)
            (obj-to-node cntxt (gets odata :value))
            (unset-context! cntxt))
          ; Value
          ((me-value? odata)
            (set-context! cntxt odata)
            (obj-to-node cntxt (gets odata :value))
            (unset-context! cntxt))
          ; Maps
          ((map? odata)
            (set-context! cntxt (MapNode))
            (each (lambda ((_k _v))
              (obj-to-node cntxt (MapEntryNode _k _v)))
              (entries odata))
            (unset-context! cntxt))
          (t (throw "Unknown type found in obj-to-node" odata))))
    ((or (eql odata t) (eql odata nil))
     (add-to-context! cntxt (ScalarNode :boolean odata)))
    ((kw? odata)
     (add-to-context! cntxt (ScalarNode :keyword odata)))
    ((sym? odata)
     (add-to-context! cntxt (ScalarNode :symbol odata)))
    ((str? odata)
     (add-to-context! cntxt (ScalarNode :string (cat +dblq+ odata +dblq+))))
    ((num? odata)
     (add-to-context! cntxt (ScalarNode :number odata)))
    (t
     (throw "Unknown type found in obj-to-node" odata))))

; Pretty Printers
(defq ppopts
  (emap-kv
    :map-delims   "{}"
    :list-delims  "()"
    :indent       -1
    :indent_space  2))

(defq pp_boolean
      (xmap-kv
        t   "true"
        nil "false"))

(defun indent+ (cntxt)
  (defq ci (gets cntxt :indent))
  (sets! cntxt :indent (inc ci)))

(defun indent- (cntxt)
  (defq ci (gets cntxt :indent))
  (sets! cntxt :indent (dec ci)))

(defun padded= (cntxt)
  (pad "" (* (gets cntxt :indent_space) (gets cntxt :indent))))

(defun padded- (cntxt)
  (pad "" (* (gets cntxt :indent_space) (dec (gets cntxt :indent)))))

(defun pp-write-pad (sstrm cntxt)
  (write sstrm (padded= cntxt)))

(defun pp-niw (sstrm cntxt el)
  (write sstrm el))

(defun pp-iw (sstrm cntxt el)
  (write (pp-write-pad sstrm cntxt) el))

(defun pp-iwcr (sstrm cntxt el)
  (write (pp-iw sstrm cntxt el) +nl+))

(defun pp-map-container (sstrm cntxt el)
  (write (pp-write-pad (write sstrm +nl+) cntxt) (str el +nl+)))

(defun node-to-pprint (sstrm in-args ast)
  (defq recur (curry node-to-pprint sstrm in-args))
  (case (gets ast :type)
    ((:map)
      (indent+ in-args)
      (pp-map-container sstrm in-args (first (gets in-args :map-delims)))
      (indent+ in-args)
      (each recur (gets ast :children))
      (indent- in-args)
      (if (= (gets in-args :indent) 1)
        (pp-iwcr sstrm in-args (last (gets in-args :map-delims)))
        (pp-iw sstrm in-args (last (gets in-args :map-delims))))
      (indent- in-args))
    ((:set)
      ; TODO
      )
    ((:seq)
     (indent+ in-args)
     (pp-niw sstrm in-args (first (gets in-args :list-delims)))
     (indent+ in-args)
     (each recur (gets ast :children))
     (indent- in-args)
     (pp-niw sstrm in-args (last (gets in-args :list-delims)))
     (indent- in-args))
    ((:map_entry)
     (each recur (gets ast :children)))
    ((:key)
      (pp-iw sstrm in-args "")
      (each recur (gets ast :children)))
    ((:value)
      (each recur (gets ast :children))
      (pp-iwcr sstrm in-args ""))
    ((:scalar)
     (if (eql (gets ast :stype) :boolean)
        (pp-niw sstrm in-args (str (gets pp_boolean (gets ast :value)) " "))
        (pp-niw sstrm in-args (str (gets ast :value) " "))))
    (t (throw "Unknown " ast))))

(defun-bind pprint-to-strm (sstrm obj out-args)
  (defq context
    (cond
      ((or (empty? out-args) (odd? out-args))
        (merges (Context) ppopts))
      (t
        (merges (Context) ppopts out-args))))
  (obj-to-node context obj)
  (node-to-pprint sstrm context (gets context :current))
  sstrm)

(defun-bind pprint (obj &rest out-args)
  (print (str (pprint-to-strm (string-stream (cat "")) obj out-args)))
  nil)
