;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; xtras - ChrysaLisp extra forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Predicates

(defun-bind neg? (el)
  (< el 0))

(defun-bind pos? (el)
  (> el 0))

(defun-bind zero? (el)
  (= el 0))

; Properties (a.k.a. dictionary a.k.a. map)

; (defmacro make-dict (name &rest pairs)
;   ; (make-dict sym [:key val...]) -> dict
;   `(progn
;     (defq ,name (env -101))
;     (when (> (length ',pairs) 0)
;       (def ,name ~pairs))
;     ,name))

(defun-bind env-list (_)
  (first (read (string-stream (str _)) (ascii-code " "))))

(defmacro make-dict (name &rest pairs)
; (make-dict sym [:key val...]) -> dict
  (if (> (length pairs) 0)
    `(progn
      (defq ,name (env -101)) (def ,name ~pairs) ,name)
    `(defq ,name (env -101))))

(defun dict-get (dict property &optional ifnil)
  ; (dict-get dict property &optional ifnil) -> nil | ifnil
  (defq tmp (get property dict))
  (when (not tmp)
    (setq tmp ifnil))
  tmp)

(defun dict-put (dict property value &optional add_ifnil)
  ; (dict-put dict property value &optional add_ifnil) -> dict
  (if (not (get property dict))
    (if add_ifnil
      (def dict property value))
    (set dict property value))
  dict)

(defun-bind dict-merge (dict0 dict1)
  ; (dict-merge dict0 dict1) -> list
  ; Merges two dictionaries and produces a list of lists
  )

(defun-bind dict-merge-to (dict2 dict0 dict1)
  ; (dict-merge dict2 dict0 dict1) -> dict3
  ; Merges dict0 and dict1 producing dict3
  )

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
        ((pred el) acc)
        (t (push acc el))))
    seq (list)))


(defq myL (list 9 8 7 6 5 4 3 2 1 0))
(defq myL2 (list -2 -1 0 -1 -2 3))
