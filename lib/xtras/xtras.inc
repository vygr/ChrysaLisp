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

(defun-bind efind (p k)
  ; (efind properties key) -> index | nil
  ; Returns the index of key in properties
  ; Used when keys are number and/or string
  (defq fx nil)
  (each! 0 -1
    (lambda (s)
      (when (eql s k)
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
  ; Drops a properties key/pair
  (when (props? p)
    (defq
      sp (efind p k)
      ep (- (length p) 2))
    (when sp
      (movekpf! p sp ep)
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

