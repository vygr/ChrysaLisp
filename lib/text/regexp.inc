;;;;;;;;;;;;;;;;;;;;;
; regexp search class
;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +bracket_chars "()" +assert_chars "^$!" +op_cat "'" +op_tac "`"
	+op_chars "+*?|" +op_presidence "+*?'|`()")
(defmacro is-bracket-char? (_) `(find ,_ +bracket_chars))
(defmacro is-assert-char? (_) `(find ,_ +assert_chars))
(defmacro is-esc-char? (_) `(eql ,_ ,(ascii-char 92)))
(defmacro is-op-char? (_) `(find ,_ +op_chars))

(defun output (op token) (push out_types op) (push out_tokens token))
(defun concat? (&optional o) (if concat (output :op (opt o concat))))

(defun cache (elem) ;graph element cache
	(defq k (str elem)) (memoize k elem 11))

(defun tokenize (line)
	(defq state :space concat :nil token :nil
		out_types (list) out_tokens (list))
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
						(setq token (list) state :class))
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
						(output :class (apply (const cat) token))
						(setq concat +op_cat state :space))
					(:t (push token c))))
			(:esc
				(case c
					(:t (output :lit c))
					("r" (output :lit (char-class "\r")))
					("f" (output :lit (char-class "\f")))
					("v" (output :lit (char-class "\v")))
					("n" (output :lit (char-class "\n")))
					("t" (output :lit (char-class "\t")))
					("s" (output :class " \t"))
					("S" (output :class "^ \r\f\v\n\t"))
					("d" (output :class "0-9"))
					("D" (output :class "^0-9"))
					("l" (output :class "a-z"))
					("u" (output :class "A-Z"))
					("a" (output :class "A-Za-z"))
					("p" (output :class "A-Za-z0-9"))
					("w" (output :class "A-Za-z0-9_"))
					("W" (output :class "^A-Za-z0-9_"))
					("x" (output :class "A-Fa-f0-9")))
				(setq concat +op_cat state :space)))) line)
	(unless (eql state :space) (throw "Regexp bad state !" state))
	(list out_tokens out_types))

(defun postfix (tokenize_output)
	(defq stack (list) parens (list)
		out_tokens (list) out_types (list) label -2)
	(each! 0 -1 (lambda (token type)
		(case type
			(:lrb
				(push stack token)
				(push parens (setq label (+ label 2)))
				(output :lrb label))
			(:rrb
				(unless (setq type (pop parens)) (throw "Regexp bad parens !" parens))
				(output :rrb (inc type))
				(until (eql "(" (setq token (pop stack)))
					(output :op token)))
			(:op ;precidence
				(defq p (find token +op_presidence) x :t)
				(while (and x (/= 0 (length stack)))
					(defq ps (find (last stack) +op_presidence))
					(cond
						((>= p ps) (output :op (pop stack)))
						(:t (setq x :nil))))
				(push stack token))
			(:t ;:lit :wild :class :assert
				(output type token)))) tokenize_output)
	(while (defq token (pop stack)) (output :op token))
	(if (nempty? parens) (throw "Regexp bad parens !" parens))
	(list out_tokens out_types))

(defun compile (postfix_output)
	(defq stack (list))
	(each! 0 -1 (lambda (token type)
		(push stack (case type
			(:lit (cache (static-qq (((eql s ,token) 1)))))
			(:wild (static-q ((:t 1))))
			(:class
				(if (eql (first token) "^")
					(defq n :t token (rest token))
					(defq n :nil))
				(setq token (char-class token))
				(cache (if n
					(static-qq (((not (bfind s ,token)) 1)))
					(static-qq (((bfind s ,token) 1))))))
			(:assert (case token
				("^" (static-q ((:nil (if (<= si 0) 1)))))
				("$" (static-q ((:nil (if (>= si tl) 1)))))
				("!" (static-q ((:nil (if (or (<= si 0) (>= si tl)
					(bfind s +char_class_not_whole_word)
					(bfind (elem-get si text) +char_class_not_whole_word)) 1)))))))
			(:lrb (cache (static-qq ((:nil
				(progn (defq i (pop trace))
					(while (<= (length trace) ,token) (push trace 0 0))
					(elem-set ,token (push trace i) si) 1))))))
			(:rrb (cache (static-qq ((:nil
				(progn (elem-set ,token trace si) 1))))))
			(:op (defq a (pop stack))
				(and (find token +op_chars) (notany (const first) a)
					(throw "Regexp bad operator !" token))
				(case token
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

(defun insertelem (cpat next seen trace elem)
	(defq stack (rest elem) index (last trace))
	(while (defq offset (pop stack))
		;offsets can be expressions !
		(if (setq offset (eval offset))
			(if (= (defq new_index (+ index offset)) (length cpat))
				;stopped, so matched somthing !
				(if (> si end) (setq end si end_trace trace))
				;not stopped
				(if (first (defq elem (elem-get new_index cpat)))
					;not a split
					(unless (find new_index seen)
						(push seen new_index)
						(push next (push (slice 0 -2 trace) new_index)))
					;a split
					(each! 1 -1 (# (if (setq %0 (eval %0))
						(push stack (+ offset %0)))) (list elem)))))) next)

(defun insertelem-no-trace (cpat next index elem)
	(defq stack (rest elem))
	(while (defq offset (pop stack))
		;offsets can be expressions !
		(if (setq offset (eval offset))
			(if (= (defq new_index (+ index offset)) (length cpat))
				;stopped, so matched somthing !
				(if (> si end) (setq end si))
				;not stopped
				(if (first (defq elem (elem-get new_index cpat)))
					;not a split
					(unless (find new_index next) (push next new_index))
					;a split
					(each! 1 -1 (# (if (setq %0 (eval %0))
						(push stack (+ offset %0)))) (list elem)))))) next)

(defun firstelem (cpat current)
	(if (first (defq elem (first cpat)))
		;not a split
		(push current (array 0))
		;a split
		(insertelem cpat current (list) (array 0) elem)))

(defun firstelem-no-trace (cpat current)
	(if (first (defq elem (first cpat)))
		;not a split
		(push current 0)
		;a split
		(insertelem-no-trace cpat current 0 elem)))

(defun search (text cpat start)
	; (search text cpat start) -> (list submatches {-1 | end})
	(defq end -1 end_trace (array 0) tl (length text)
		si start s (if (<= si 0) " " (elem-get (dec si) text))
		current (firstelem cpat (list)) seen (list) next (list))
	(some! start -1 :nil
		(lambda (s) (defq si (inc _))
			;for each current trace
			(each (# (if (eval (first (defq elem (elem-get (last %0) cpat))))
				;matched text char, so insert next traces
				(insertelem cpat next seen %0 elem))) current)
			;swap to new traces
			(defq tmp current)
			(setq current next next tmp)
			(clear next seen)
			;break out if no traces
			(= (length current) 0))
		(list text))
	(pop end_trace)
	(list (partition 2 end_trace) end))

(defun match (text cpat start)
	; (match text cpat start) -> -1 | end
	(defq end -1 tl (length text) trace (array 0)
		si start s (if (<= si 0) " " (elem-get (dec si) text))
		current (firstelem-no-trace cpat (list)) next (list))
	(some! start -1 :nil
		(lambda (s) (defq si (inc _))
			;for each current index
			(each (# (if (eval (first (defq elem (elem-get %0 cpat))))
				;matched text char, so insert next index
				(insertelem-no-trace cpat next %0 elem))) current)
			;swap to new indexs
			(defq tmp current)
			(setq current next next (clear tmp))
			;break out if no traces OR found any match !
			(or (/= end -1) (= (length current) 0)))
		(list text)) end)

(defclass Regexp (&optional num_buckets) :nil
	; (Regexp) -> regexp
	(def this :meta_cache (Fmap num_buckets))

	(defmethod :compile (pattern)
		; (. regexp :compile pattern) -> :nil | meta
		(raise :meta_cache)
		(setq pattern (cat "(" pattern ")"))
		(unless (defq cpat (. meta_cache :find pattern))
			(setq cpat (catch (compile (postfix (tokenize pattern))) :t))
			(. meta_cache :insert pattern cpat))
		(unless (eql cpat :t) cpat))

	(defmethod :match? (text pattern &optional cpat)
		; (. regexp :match? text pattern &optional meta) -> :t | :nil
		(defq cpat (opt cpat (. this :compile pattern)))
		(when cpat
			(defq x 0 x1 -1)
			(while (and (= x1 -1) (< x (length text)))
				(setq x1 (match text cpat x) x (inc x)))
			(/= x1 -1)))

	(defmethod :search (text pattern &optional cpat)
		; (. regexp :search text pattern &optional meta) -> matches
		(when (defq out (list) cpat (opt cpat (. this :compile pattern)))
			(defq x 0)
			(while (< x (length text))
				(cond
					((> (bind '(sl x1) (search text cpat x)) x)
						(push out sl)
						(setq x x1))
					((setq x (inc x))))))
		out)
	)

;module
(export-classes '(Regexp))
(env-pop)
