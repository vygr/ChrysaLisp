;;;;;;;;;;;;;;;;;;;;;
; regexp search class
;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +bracket_chars "()" +op_chars "+*?|" +op_presidence "+*?,|()")
(defmacro is-bracket-char? (_) `(find ,_ +bracket_chars))
(defmacro is-esc-char? (_) `(eql ,_ ,(ascii-char 92)))
(defmacro is-op-char? (_) `(find ,_ +op_chars))

(defun concat? () (when concat (push out_types :op) (push out_tokens ",")))
(defun output (op token) (push out_types op) (push out_tokens token))

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
						(setq concat (elem-get _ '(:t :t :t :nil))))
					((defq _ (is-bracket-char? c))
						(if (< _ 1) (concat?))
						(output (elem-get _ '(:lrb :rrb)) c)
						(setq concat (elem-get _ '(:nil :t))))
					((eql c "[")
						(concat?)
						(setq token (clear '()) state :class))
					((eql c ".")
						(concat?)
						(output :wild c)
						(setq concat :t))
					((eql c "^")
						(concat?)
						(output :sol c)
						(setq concat :t))
					((eql c "$")
						(concat?)
						(output :eol c)
						(setq concat :t))
					(:t (concat?)
						(output :lit c)
						(setq concat :t))))
			(:class
				(cond
					((eql c "]")
						(output :class (apply cat token))
						(setq concat :t state :space))
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
				(setq concat :t state :space)))) line)
	(list out_tokens out_types))

(defun postfix (tokenize_output)
	(defq stack '() out_tokens (clear '()) out_types (clear '()))
	(each! 0 -1 (lambda (token type)
		(case type
			(:lrb
				(push stack token))
			(:rrb
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
			(:t ;:lit :wild :class :white :black :sol :eol
				(output type token)))) tokenize_output)
	(while (defq _ (pop stack)) (output :op _))
	(list out_tokens out_types))

(defun compile (postfix_output)
	(defq stack (clear '()))
	(each! 0 -1 (lambda (token type)
		(case type
			(:lit (push stack (static-qq (((eql s ,token) 1)))))
			(:wild (push stack (static-q ((:t 1)))))
			(:white (push stack (static-q (((<= (code s) 32) 1)))))
			(:black (push stack (static-q (((> (code s) 32) 1)))))
			(:sol (push stack (static-q ((:nil (if (= si 0) 1))))))
			(:eol (push stack (static-q ((:nil (if (= si (dec (length text))) 1))))))
			(:class (push stack
				(if (eql (second token) "-")
					(static-qq (((<= ,(code (first token)) (code s) ,(code (last token))) 1)))
					(static-qq (((find s ,token) 1))))))
			(:op (case token
				(","
					(defq b (pop stack) a (pop stack))
					(push stack (cat a b)))
				("|"
					(defq b (pop stack) a (pop stack) oa (+ (length a) 2) ob (inc (length b)))
					(push stack (cat `((:nil 1 ,oa)) a `((:nil ,ob)) b)))
				("?"
					(defq a (pop stack) oa (inc (length a)))
					(push stack (cat `((:nil 1 ,oa)) a)))
				("*"
					(defq a (pop stack) b (length a) oa (+ b 2) ob (neg b))
					(push stack (cat `((:nil 1 ,oa)) a `((:nil 1 ,ob)))))
				("+"
					(defq a (pop stack) oa (neg (length a)))
					(push stack (cat a `((:nil 1 ,oa)))))
				)))) postfix_output)
	(pop stack))

(defun addstate (creg next index state)
	(defq stack (slice 1 -1 state))
	(while (defq offset (pop stack))
		;offsets can be expressions !
		(if (setq offset (eval offset))
			(if (= (defq new_index (+ index offset)) (length creg))
				;stopped, so matched somthing !
				(setq end (max end si))
				;not a stop state
				(if (elem-get 0 (defq state (elem-get new_index creg)))
					;not a split
					(unless (find new_index next) (push next new_index))
					;a split
					(each! 1 -1 (# (if (setq %0 (eval %0))
						(push stack (+ offset %0)))) (list state))))))
	next)

(defun firststate (creg current)
	(if (elem-get 0 (defq state (elem-get 0 creg)))
		;not a split
		(push current 0)
		;a split
		(addstate creg current 0 state)))

(defun search (text creg start)
	; (search text creg start) -> -1 | end
	;for each char in text
	(defq end -2 si start current (firststate creg (list)) next (list))
	(some! start -1 :nil
		(lambda (s) (defq si _)
			;for each current state
			(each (lambda (index)
				(defq state (elem-get index creg))
				(when (eval (elem-get 0 state))
					;matched text char, so insert next states
					(addstate creg next index state))) current)
			;swap to new states
			(defq tmp current)
			(setq current next next (clear tmp))
			;break out if no states
			(= (length current) 0))
		(list text)) (inc end))

(defclass Regexp (&optional num_buckets) :nil
	; (Regexp) -> regexp
	(def this :meta_cache (Fmap num_buckets))

	(defmethod :compile (this pattern)
		; (. regexp :compile pattern) -> :nil | meta
		(raise :meta_cache)
		(unless (defq creg (. meta_cache :find pattern))
			(setq creg (catch (compile (postfix (tokenize pattern))) :t))
			(if (eql creg :t) (setq creg :nil))
			(. meta_cache :insert pattern creg))
		creg)

	(defmethod :match? (this text pattern &optional creg)
		; (. regexp :match? text pattern &optional meta) -> :t | :nil
		(defq creg (opt creg (. this :compile pattern)))
		(when creg
			(defq x -1 x1 -1)
			(while (and (= x1 -1) (< (setq x (inc x)) (length text)))
				(setq x1 (search text creg x)))
			(/= x1 -1)))

	(defmethod :search (this text pattern &optional creg)
		; (. regexp :search text pattern &optional meta) -> indexes
		(defq out (list) creg (opt creg (. this :compile pattern)))
		(when creg
			(defq x -1)
			(while (< (setq x (inc x)) (length text))
				(when (> (defq x1 (search text creg x)) x)
					(push out (list x x1))
					(setq x (dec x1)))))
		out)
	)

;module
(export-classes
	Regexp)
(env-pop)
