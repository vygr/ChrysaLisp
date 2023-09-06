;;;;;;;;;;;;;;;;;;;;;
; regexp search class
;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +bracket_chars "()" +assert_chars "^$" +op_cat "'" +op_tac "`"
	+op_chars "+*?|" +op_presidence "+*?'|`()")
(defmacro is-bracket-char? (_) `(find ,_ +bracket_chars))
(defmacro is-assert-char? (_) `(find ,_ +assert_chars))
(defmacro is-esc-char? (_) `(eql ,_ ,(ascii-char 92)))
(defmacro is-op-char? (_) `(find ,_ +op_chars))

(defun output (op token) (push out_types op) (push out_tokens token))
(defun concat? (&optional o) (if concat (output :op (opt o concat))))

(defun cache (elem) ;graph element cache
	(defq c (const (Fmap 11)) k (str elem))
	(unless (defq e (. c :find k))
		(. c :insert k (setq e elem))) e)

(defun srange (label) ;start of sub match
	(while (<= (length sl) label) (push sl (list 0 0)))
	(elem-set 0 (elem-get label sl) si) 1)

(defun erange (label) ;end of sub match
	(elem-set 1 (elem-get label sl) si) 1)

(defun tokenize (line)
	(defq state :space concat :nil token :nil
		out_types (clear '()) out_tokens (clear '()))
	(each (lambda (c)
		(case state
			(:space
				(cond
					((is-esc-char? c)
						(concat?)
						(setq state :esc))
					((defq _ (is-op-char? c))
						(output :op c)
						(setq concat (elem-get _ (static-q (+op_cat +op_cat +op_cat :nil)))))
					((defq _ (is-bracket-char? c))
						(concat? (elem-get _ (list concat +op_tac)))
						(output (elem-get _ '(:lrb :rrb)) c)
						(setq concat (elem-get _ (static-q (+op_tac +op_cat)))))
					((is-assert-char? c)
						(concat?)
						(output :assert c)
						(setq concat +op_cat))
					((eql c "[")
						(concat?)
						(setq token (clear '()) state :class))
					((eql c ".")
						(concat?)
						(output :wild c)
						(setq concat +op_cat))
					(:t (concat?)
						(output :lit c)
						(setq concat +op_cat))))
			(:class
				(cond
					((eql c "]")
						(output :class (apply cat token))
						(setq concat +op_cat state :space))
					(:t (push token c))))
			(:esc
				(case c
					("w" (output :white c))
					("b" (output :black c))
					("q" (output :lit (ascii-char 34)))
					("s" (output :lit (ascii-char 32)))
					("r" (output :lit (ascii-char 13)))
					("f" (output :lit (ascii-char 12)))
					("n" (output :lit (ascii-char 10)))
					("t" (output :lit (ascii-char 9)))
					(:t (output :lit c)))
				(setq concat +op_cat state :space)))) line)
	(list out_tokens out_types))

(defun postfix (tokenize_output)
	(defq stack (clear '()) parens (clear '())
		out_tokens (clear '()) out_types (clear '()) label -1)
	(each! 0 -1 (lambda (token type)
		(case type
			(:lrb
				(push stack token)
				(push parens (setq label (inc label)))
				(output :lrb label))
			(:rrb
				(output :rrb (pop parens))
				(while (and (not (eql "(" (setq token (pop stack)))) token)
					(output :op token)))
			(:op ;precidence
				(defq p (find token +op_presidence) x :t)
				(while (and x (/= 0 (length stack)))
					(defq ps (find (elem-get -2 stack) +op_presidence))
					(cond
						((>= p ps) (output :op (pop stack)))
						(:t (setq x :nil))))
				(push stack token))
			(:t ;:lit :wild :class :white :black :assert
				(output type token)))) tokenize_output)
	(while (defq token (pop stack)) (output :op token))
	(list out_tokens out_types))

(defun compile (postfix_output)
	(defq stack (clear '()))
	(each! 0 -1 (lambda (token type)
		(push stack (case type
			(:lit (cache (static-qq (((eql s ,token) 1)))))
			(:wild (static-q ((:t 1))))
			(:white (static-q (((<= (code s) 32) 1))))
			(:black (static-q (((> (code s) 32) 1))))
			(:class (cache (if (eql (second token) "-")
				(static-qq (((<= ,(code (first token)) (code s) ,(code (last token))) 1)))
				(static-qq (((find s ,token) 1))))))
			(:assert (case token
				("^" (static-q ((:nil (if (= si -1) 1)))))
				("$" (static-q ((:nil (if (= si (dec (length text))) 1)))))))
			(:lrb (cache (static-qq ((:nil (srange ,token))))))
			(:rrb (cache (static-qq ((:nil (erange ,token))))))
			(:op (defq a (pop stack)) (case token
				((+op_cat +op_tac) (cat (pop stack) a))
				("|" (defq b (pop stack) ob (+ (length b) 2) oa (inc (length a)))
					(cat (cache `((:nil 1 ,ob))) b (cache `((:nil ,oa))) a))
				("?" (defq oa (inc (length a)))
					(cat (cache `((:nil 1 ,oa))) a))
				("*" (defq b (length a) oa (+ b 2) ob (neg b))
					(cat (cache `((:nil 1 ,oa))) a (cache `((:nil 1 ,ob)))))
				("+" (defq oa (neg (length a)))
					(cat a (cache `((:nil 1 ,oa)))))
				))))) postfix_output) (pop stack))

(defun insertelem (cpat next index elem)
	(defq stack (slice 1 -1 elem))
	(while (defq offset (pop stack))
		;offsets can be expressions !
		(if (setq offset (eval offset))
			(if (= (defq new_index (+ index offset)) (length cpat))
				;stopped, so matched somthing !
				(setq end (max end si))
				;not stopped
				(if (elem-get 0 (defq elem (elem-get new_index cpat)))
					;not a split
					(unless (find new_index next) (push next new_index))
					;a split
					(each! 1 -1 (# (if (setq %0 (eval %0))
						(push stack (+ offset %0)))) (list elem)))))) next)

(defun firstelem (cpat current)
	(if (elem-get 0 (defq elem (elem-get 0 cpat)))
		;not a split
		(push current 0)
		;a split
		(insertelem cpat current 0 elem)))

(defun search (text cpat start)
	; (search text cpat start) -> -1 | end
	;for each char in text
	(defq end -2 si (dec start) current (firstelem cpat (list)) next (list))
	(some! start -1 :nil
		(lambda (s) (defq si _)
			;for each current elem
			(each (lambda (index)
				(defq elem (elem-get index cpat))
				(when (eval (elem-get 0 elem))
					;matched text char, so insert next elems
					(insertelem cpat next index elem))) current)
			;swap to new elems
			(defq tmp current) (setq current next next (clear tmp))
			;break out if no elems
			(= (length current) 0))
		(list text)) (inc end))

(defclass Regexp (&optional num_buckets) :nil
	; (Regexp) -> regexp
	(def this :meta_cache (Fmap num_buckets))

	(defmethod :compile (this pattern)
		; (. regexp :compile pattern) -> :nil | meta
		(raise :meta_cache)
		(unless (defq cpat (. meta_cache :find pattern))
			(setq cpat (catch (compile (postfix (tokenize pattern))) :t))
			(if (eql cpat :t) (setq cpat :nil))
			(. meta_cache :insert pattern cpat))
		cpat)

	(defmethod :match? (this text pattern &optional cpat)
		; (. regexp :match? text pattern &optional meta) -> :t | :nil
		(defq cpat (opt cpat (. this :compile pattern)))
		(when cpat
			(defq x -1 x1 -1 sl '())
			(while (and (= x1 -1) (< (setq x (inc x)) (length text)))
				(setq x1 (search text cpat x)))
			(/= x1 -1)))

	(defmethod :search (this text pattern &optional cpat)
		; (. regexp :search text pattern &optional meta) -> (matches submatches)
		(defq out (list) outs (list) cpat (opt cpat (. this :compile pattern)))
		(when cpat
			(defq x -1)
			(while (< (setq x (inc x)) (length text))
				(defq sl (clear '()))
				(when (> (defq x1 (search text cpat x)) x)
					(push out (list x x1))
					(push outs (map (lambda ((s s1))
						(if (< (max s 0) s1) (slice (inc s) (inc s1) text) (slice 0 0 text))) sl))
					(setq x (dec x1)))))
		(list out outs))
	)

;module
(export-classes Regexp)
(env-pop)
