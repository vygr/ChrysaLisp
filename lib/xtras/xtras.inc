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
      (> (length _) 2)
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

(defun-bind ident (_)
  ; (ident el) -> el
  _)

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
    (t seq)))

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

(defun-bind prop-entries (_)
  (when (props? _)
    (defq c (chunk 2 _))
    (reduce (lambda (acc e)
      (if (or (eql (first e) :clz) (eql (first e) :super_clz))
        acc
        (push acc e)))
      c (list))))

(defun-bind prop-keys (prop)
  ; (prop-keys prop) -> key list
  (map (#(elem 0 %0)) (prop-entries prop)))

(defun-bind prop-values (prop)
  ; (prop-keys values) -> value list
  (map (#(elem 1 %0)) (prop-entries prop)))

(defun-bind efind (p k)
  ; (efind properties key) -> index | nil
  ; Used when keys are number and/or string
  (defq fx nil)
  ; (print k)
  (each! 0 -1
    (lambda (s)
      ; (print s)
      (when (eql s k)
        ; (print "(eql s k) => "(eql s k))
        (setq fx _))) (list p))
  fx)

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

(defun-bind get-in (p &rest kws)
  ; (get-in (properties kw...)) -> value | nil
  ; Returns the value in a nested properties structure
  ; where kws is a sequence (list) of keys. Returns nil
  ; if a key is not present
  (defq hit nil)
  (cond
    ((not (props? p))
      (throw "Not a properties structure " p))
    ((empty? kws)
      (throw "No keys argument provided " kws))
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
      (push (push p k) v)
      (throw (str "Key " k " does not exist") p))
    (elem-set (inc ki) p v))
  p)

(defun-bind setsp! (p &rest pairs)
  ; (setsp properties [k v] ...) -> properties
  (each (#(setp! p (first %0) (second %0) t)) (chunk 2 pairs))
  p)

(defun-bind pmerge (&rest props)
  ; (pmerge properties...) -> properties
  ; Returns a properties structure that consists of
  ; the rest of the properties structure merged into
  ; the first
  (reduce
    (#(each (lambda (e)
      (setp! %0 (first e) (second e) t)) (chunk 2 %1)))
    props (properties)))

(defun-bind movekpf! (p sp ep)
  ; (movekpf properties start-key-index end-key-index)
  ; Moves the properties k/v pair forward from key index sp to index ep
  (while (/= sp ep)
    (defq tsp (+ sp 2))
    (swap p sp tsp)
    (swap p (inc sp) (+ sp 3))
    (setq sp tsp))
  p)

(defun-bind movekpb! (p sp ep)
  ; (movekpb properties start-key-index end-key-index)
  ; Moves the properties k/v pair backward from key index sp to index ep
  (while (/= sp ep)
    (defq tsp (- sp 2))
    (swap p sp tsp)
    (swap p (inc sp) (dec sp))
    (setq sp tsp))
  p)

(defun-bind pdrop! (p k)
  ; (pdrop properties key) -> properties

  (when (props? p)
    (defq
      sp (efind p k)
      ep (- (length p) 2))
    (when sp
      (movekpf! p sp ep)
      (times 2 (pop p)))))

; Dictionary forms

(defun-bind dict-entries (e)
  ; (dict-entries e) -> list
  ; Returns list of properties for e
  (tolist e))

(defmacro-bind make-dict (name &rest pairs)
  ; (make-dict name [:key val...]) -> dictionary
  ; Creates a new dictionary with optional property value pairs
  (if (> (length pairs) 0)
    `(progn
      (defq ,name (env -1)) (def ,name ~pairs) ,name)
    `(defq ,name (env -1))))

(defmacro-bind dictionary (&optional bk)
  ; (dictionary [blocks | -blocks]) -> dictionary
  (setd bk -1)
  `(env ,bk))

(defun-bind to-dict (seq1 seq2)
  ; (to-dict seq1 seq2) -> dictionary
  (defq td (dictionary))
  (each (# (def td (first %0) (second %0))) (map list seq1 seq2))
  td)

(defun-bind dict-get (dict property &optional ifnil)
  ; (dict-get dict property &optional ifnil) -> nil | ifnil
  ; Returns a property or optional ifnil value
  (defq tmp (get property dict))
  (when (not tmp)
    (setq tmp ifnil))
  tmp)

(defun-bind dict-drop! (dict &rest dkeys)
  ; (dict-drop! dict key...) -> dictionary
  (each (#(undef dict %0)) dkeys)
  dict)

(defun-bind dict-put! (dict property value &optional add_ifnil)
  ; (dict-put dict property value &optional add_ifnil) -> dictionary
  ; Updates the property value of a dictionary
  (if (not (get property dict))
    (if add_ifnil
      (def dict property value))
    (set dict property value))
  dict)

(defmacro-bind dict-puts! (dict &rest pairs)
  `(def ,dict ~pairs))

(defun-bind dict-merge (&rest dlist)
  ; (dict-merge dict0 ...) -> list
  ; Merges multiple dictionaries to a list of property lists
  (defq
    ebase (dict-entries (first dlist))
    kset  (list))
  (each (lambda (el) (push (push kset (first el)) _)) ebase)
  (each (lambda (el)
    (reduce (lambda (acc el)
      (defq hit (find (first el) kset))
      (cond
        (hit
          (elem-set 1 (elem (elem (inc hit) kset) acc) (second el))
          acc)
        (t
          (push (push kset (first el)) (/ (length kset) 2))
          (push acc el)))) el ebase)) (map dict-entries (rest dlist)))
  ebase)

(defun-bind dict-merge-to! (dict &rest dlist)
  ; (dict-merge-to dict dict0 ...) -> dictionary
  ; Merges multiple dictionaries to a dictionary
  (reduce (lambda (acc el) (def acc (first el) (second el)) acc)
    (apply dict-merge dlist) dict))

(defun-bind dict-keys (dict)
  ; (dict-keys dict) -> key list
  (map (#(elem 0 %0)) (dict-entries dict)))

(defun-bind dict-values (dict)
  ; (dict-values dict) -> value list
  (map (#(elem 1 %0)) (dict-entries dict)))

; Normalize some dictionary and properties functions

(defun-bind entries (e)
  ; (entries e) -> ((k v) ...) | nil
  (cond
    ((dict? e)
      (dict-entries e))
    ((props? e)
      (prop-entries e))
    (t nil)))

(defun-bind entry-cnt (e)
  ; (entry-cnt e) -> num
  (cond
    ((dict? e)
      (length (dict-entries e)))
    ((props? e)
      (dec (/ (length e) 2)))
    (t 0)))

(defun-bind keys (e)
  ; (keys e) -> list | nil
  (cond
    ((dict? e)
      (dict-keys e))
    ((props? e)
      (prop-keys e))
    (t nil)))

(defun-bind values (e)
  ; (values e) -> list | nil
  (cond
    ((dict? e)
      (dict-values e))
    ((props? e)
      (prop-values e))
    (t nil)))

; Test cases
; (make-dict foo :a 0 :b 1) ; -> ((:a 0) (:b 1))
; (make-dict bar :a 2 :c 3) ; -> ((:a 2) (:c 3))
; (make-dict baz :d 5 :c 7) ; -> ((:d 5) (:c 7))
; (dict-merge foo bar baz)  ; -> ((:b 1) (:a 2) (:c 7) (:d 5))
; (apply dict-merge (list foo bar baz))  ; -> ((:b 1) (:a 2) (:c 7) (:d 5))
; (dict-merge-to! (make-dict bingo) foo bar baz) ; -> ((:d 5) (:b 1) (:a 2) (:c 7))
; (dict-drop! bingo :d :c)

; Privatize functions

(defmacro private (nm)
  ; (private nm) -> env
  `(macroexpand (make-dict ,nm)))

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


; Test utilities
; (defun visit (_)
;   (print "Visiting " _)
;   _)

; (defun outf (_)
;   (print "Outter = " _)
;   _)
; (defun inf (_)
;   (print "Inner = " _)
;   _)

; (defq t0 (list 0 1 2 3 (make-dict foo :a 7 :b 6) (list 0 1 2 (list 3 4 5))))
; (defq t1 (list 0 1 2 3 (list 0 1 2 (list 3 4 5))))
; (defq t2 (list 0 1 2 (make-dict foo :a 1 :b 7) (list 0 1 2 (list 3 4 5))))
; (make-dict bar :a 0 :b 1 :c (list foo))

; Take and drop forms

(defun-bind take (cnt seq)
  ; (take cnt seq) -> seq
  ; Returns the first `cnt` of elements from seq
  ; providing a negative cnt returns the original sequence
  ; example: (take 2 '(0 1 2 3)) -> (0 1)
  (defq len (length seq))
  (cond
    ((neg? cnt) seq)
    (t (slice 0 (cond ((> cnt len) len) (t cnt)) seq))))


(defun-bind take-last (cnt seq)
  ; (take-last cnt seq) -> seq
  ; Returns the last 'cnt' of elements from seq
  ; providing a negative cnt returns the original sequence
  ; example: (take-last 2 '(0 1 2 3)) ->  (2 3)
  (reverse (take cnt (reverse seq))))

(defun-bind take-while (pred seq)
  ; (take-while pred seq)
  ; Returns successive element of seq while pred returns true
  ; example: (take-while neg? '(-1 -2 0 -1 -2)) -> (-1 -2)
  (reduced-reduce
    (lambda (acc el)
      (cond
        ((pred el) (push acc el))
        (t (reduced acc))))
    seq (list)))

(defun-bind take-unless (pred seq)
  ; (take-unless pred seq)
  ; Returns successive element of seq unless pred returns true
  ; example: (take-unless (# (< %0 0)) '(-1 -2 0 -1 -2)) -> (0)
  (reduced-reduce
    (lambda (acc el)
      (cond
        ((pred el) acc)
        (t (push acc el))))
    seq (list)))

(defun-bind drop (cnt seq)
  ; (drop cnt seq) -> seq
  ; Returns a sequence with the first `cnt` of elements removed
  ; providing a negative cnt returns the original sequence
  (defq len (length seq))
  (cond
    ((neg? cnt) seq)
    (t (slice (cond ((> cnt len) -1) (t cnt)) -1 seq))))

(defun-bind drop-last (cnt seq)
  ; (drop-last cnt seq) -> seq
  ; providing a negative cnt returns the original sequence
  (reverse (drop cnt (reverse seq))))

(defun-bind drop-while (pred seq)
  ; (drop-while pred seq)
  ; Drops successive element of seq while pred returns true
  (slice (length (take-while pred seq)) -1 seq))

(defun-bind drop-unless (pred seq)
  (reduced-reduce
    (lambda (acc el)
      (cond
        ((pred el) (push acc el))
        (t acc)))
    seq (list)))

; Test cases
; (defq myL2 (list -2 -1 0 -1 -2 3))
; (take 2 myL2)       ; -> (-2 -1)
; (take-last 2 myL2)  ; -> (-2 3)
; (take-while (# (< %0 0)) myL2)  ; -> (-2 -1)
; (take-unless (# (< %0 0)) myL2) ; -> (0 3)
; (drop 2 myL2)       ; -> (0 -1 -2 3)
; (drop-last 2 myL2)  ; -> (-2 -1 0 -1)
; (drop-while (# (< %0 0)) myL2)  ; -> (0 -1 -2 3)
; (drop-unless (# (< %0 0)) myL2) ; -> (-2 -1 -1 -2)

