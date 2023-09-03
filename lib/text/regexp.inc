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

(defun cache (elem) ;graph element cache
	(defq c (const (Fmap 11)) k (str elem))
	(unless (defq e (. c :find k))
		(. c :insert k (setq e elem))) e)

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
		(push stack (case type
			(:lit (cache (static-qq (((eql s ,token) 1)))))
			(:wild (static-q ((:t 1))))
			(:white (static-q (((<= (code s) 32) 1))))
			(:black (static-q (((> (code s) 32) 1))))
			(:sol (static-q ((:nil (if (= si 0) 1)))))
			(:eol (static-q ((:nil (if (= si (dec (length text))) 1)))))
			(:class (cache (if (eql (second token) "-")
				(static-qq (((<= ,(code (first token)) (code s) ,(code (last token))) 1)))
				(static-qq (((find s ,token) 1))))))
			(:op (defq a (pop stack)) (case token
				("," (cat (pop stack) a))
				("|" (defq b (pop stack) ob (+ (length b) 2) oa (inc (length a)))
					(cat (cache `((:nil 1 ,ob))) b (cache `((:nil ,oa))) a))
				("?" (defq oa (inc (length a)))
					(cat (cache `((:nil 1 ,oa))) a))
				("*" (defq b (length a) oa (+ b 2) ob (neg b))
					(cat (cache `((:nil 1 ,oa))) a (cache `((:nil 1 ,ob)))))
				("+" (defq oa (neg (length a)))
					(cat a (cache `((:nil 1 ,oa)))))
				))))) postfix_output) (pop stack))

(defun addelem (creg next index elem)
	(defq stack (slice 1 -1 elem))
	(while (defq offset (pop stack))
		;offsets can be expressions !
		(if (setq offset (eval offset))
			(if (= (defq new_index (+ index offset)) (length creg))
				;stopped, so matched somthing !
				(setq end (max end si))
				;not stopped
				(if (elem-get 0 (defq elem (elem-get new_index creg)))
					;not a split
					(unless (find new_index next) (push next new_index))
					;a split
					(each! 1 -1 (# (if (setq %0 (eval %0))
						(push stack (+ offset %0)))) (list elem))))))
	next)

(defun firstelem (creg current)
	(if (elem-get 0 (defq elem (elem-get 0 creg)))
		;not a split
		(push current 0)
		;a split
		(addelem creg current 0 elem)))

(defun search (text creg start)
	; (search text creg start) -> -1 | end
	;for each char in text
	(defq end -2 si start current (firstelem creg (list)) next (list))
	(some! start -1 :nil
		(lambda (s) (defq si _)
			;for each current elem
			(each (lambda (index)
				(defq elem (elem-get index creg))
				(when (eval (elem-get 0 elem))
					;matched text char, so insert next elems
					(addelem creg next index elem))) current)
			;swap to new elems
			(defq tmp current)
			(setq current next next (clear tmp))
			;break out if no elems
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
