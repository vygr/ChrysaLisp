;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, gophers to send: the start of a library of them
;
; Each is what a gopher sets out with, (state visit), to give to
; (storage-walk path state visit) or (storage-walk-send). visit is the
; function, quoted, as a gopher carries it: given what it has, a block,
; which block it is and how many there are, it gives what it has now and
; the block it wants next, :nil when it is done. What comes back is what
; it has at the end.
;
; They keep ChrysaLisp's rules, nothing calls itself, and go round with
; while, so each has the tick put in its loops, ./gopher.lisp. A find is
; not asked from the end of what it looks in: there it throws.
;
;	(gopher-bytes)		how many bytes
;	(gopher-lines)		how many line ends
;	(gopher-count word)	how many times a word is there, one that is
;				cut in two by the end of a block too
;	(gopher-checksum)	every byte added up, 32 bits
;	(gopher-longest)	the longest line, (longest now)
;	(gopher-head n)		the first n bytes, and no block more than
;				it takes: a gopher that stops early
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gopher-bytes ()
	; (gopher-bytes) -> (state visit)
	(list 0 '(lambda (n data at count) (list (+ n (length data)) (inc at)))))

(defun gopher-lines ()
	; (gopher-lines) -> (state visit)
	(list 0 '(lambda (n data at count)
		(defq from 0)
		(while (and (< from (length data)) (defq i (find "\n" data from)))
			(setq n (inc n) from (inc i)))
		(list n (inc at)))))

(defun gopher-count (word)
	; (gopher-count word) -> (state visit)
	;what it has is the count and the end of the last block, as long as
	;the word less one, so one that runs over from one block to the next
	;is found
	(list (list 0 "") (list 'lambda '(state data at count)
		(list 'bind ''(n tail) 'state)
		(list 'defq 'text '(cat tail data) 'from 0 'past 0 'word word)
		;a find of a str of more than one character finds its first one, so
		;that, and then the word is looked at where it was found
		'(while (and (< from (length text)) (defq i (find (first word) text from)))
			(if (and (<= (+ i (length word)) (length text)) (eql (slice text i (+ i (length word))) word))
				(setq n (inc n) from (+ i (length word)) past from)
				(setq from (inc i))))
		;what is kept for the next block: the end, as long as the word less
		;one, but none of a word found already
		'(list (list n (slice text (max past (- (length text) (dec (length word)))) -1)) (inc at)))))

(defun gopher-checksum ()
	; (gopher-checksum) -> (state visit)
	(list 0 '(lambda (sum data at count)
		(defq i 0 n (length data))
		(while (< i n)
			(setq sum (logand (+ sum (code data 1 i)) 0xffffffff) i (inc i)))
		(list sum (inc at)))))

(defun gopher-longest ()
	; (gopher-longest) -> (state visit)
	;the longest line, and how long the one that has not ended yet is
	(list (list 0 0) '(lambda ((best now) data at count)
		(defq from 0)
		(while (and (< from (length data)) (defq i (find "\n" data from)))
			(setq best (max best (+ now (- i from))) now 0 from (inc i)))
		(list (list best (+ now (- (length data) from))) (inc at)))))

(defun gopher-head (most)
	; (gopher-head n) -> (state visit)
	;the first n bytes, it goes no further than the block they end in
	(list "" (list 'lambda '(text data at count)
		(list 'defq 'most most)
		'(setq text (cat text (slice data 0 (min (length data) (- most (length text))))))
		'(list text (if (< (length text) most) (inc at))))))

(defun gopher-fold (gopher data size)
	; (gopher-fold gopher data size) -> state
	;what a gopher comes back with from an object of that data cut in
	;blocks of that size, worked out here, block by block as it would go:
	;to check one against
	(bind '(state visit) gopher)
	;made to run as the runner makes it, from a copy, the form is changed
	;in being made so
	(defq f (exec (copy visit)) count (/ (+ (length data) (dec size)) size) at 0)
	(while (and at (< -1 at count))
		(bind '(state at) (f state (slice data (* at size) (min (length data) (* (inc at) size))) at count)))
	state)
