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

; Forms

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

(defq myL (list 9 8 7 6 5 4 3 2 1 0))
(defq myL2 (list -2 -1 0 -1 -2 3))
