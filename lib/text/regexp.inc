;;;;;;;;;;;;;;;;;;;;;
; regexp search class
;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defq +op_presidence "+*?,|()" +op_chars "+*?|" +bracket_chars "([])")
(defmacro is-op-char? (_) (list 'find _ +op_chars))
(defmacro is-esc-char? (_) (list 'find _ "\\"))
(defmacro is-bracket-char? (_) (list 'find _ +bracket_chars))

(defun comma? () (when comma (push out_types :op) (push out_tokens ",")))
(defun output (op token) (push out_types op) (push out_tokens token))

(defun tokenize (line)
	(defq state :space comma :nil out_types (clear '()) out_tokens (clear '()))
	(each (lambda (c)
		(while c
			(case state
				(:space
					(cond
						((is-esc-char? c)
							(comma?)
							(setq state :esc))
						((defq _ (is-op-char? c))
							(output :op c)
							(setq comma (elem-get _ '(:t :t :t :nil))))
						((defq _ (is-bracket-char? c))
							(if (< _ 2) (comma?))
							(output (elem-get _ '(:lrb :lsb :rsb :rrb)) c)
							(setq comma (elem-get _ '(:nil :nil :t :t))))
						((eql c ".")
							(comma?)
							(output :wild c)
							(setq comma :t))
						((eql c "^")
							(comma?)
							(output :sol c)
							(setq comma :t))
						((eql c "$")
							(comma?)
							(output :eol c)
							(setq comma :t))
						(:t	(comma?)
							(output :lit c)
							(setq comma :t)))
					(setq c :nil))
				(:esc
					(case c
						("w" (output :white c))
						("b" (output :black c))
						("r" (output :lit (ascii-char 13)))
						("n" (output :lit (ascii-char 10)))
						("t" (output :lit (ascii-char 9)))
						(:t (output :lit c)))
					(setq c :nil comma :t state :space))))) line)
;; (print "tokenize")
;; (each (# (print %0 " -> " %1)) out_types out_tokens)
;; (print)
	(list out_tokens out_types))

(defun polish (tokenize_output)
	(defq stack '() out_tokens (clear '()) out_types (clear '()))
	(each! 0 -1 (lambda (token type)
		(case type
			((:lrb :lsb)
				(push stack token))
			(:rrb
				(while (and (not (eql "(" (setq token (pop stack)))) token)
					(output :op token)))
			(:rsb
				(while (and (not (eql "[" (setq token (pop stack)))) token)
					(output :op token))
				(output :op "[]"))
			(:op ;precidence
				(defq p (find token +op_presidence) x :t)
				(while (and x (/= 0 (length stack)))
					(defq ps (find (elem-get -2 stack) +op_presidence))
					(cond
						((>= p ps) (output :op (pop stack)))
						(:t	(setq x :nil))))
				(push stack token))
			(:t	;:lit :wild :white :black :sol :eol
				(output type token)))) tokenize_output)
	(while (defq _ (pop stack)) (output :op _))
;; (print "polish")
;; (each (# (print %0 " -> " %1)) out_types out_tokens)
;; (print)
	(list out_tokens out_types))

(defun compile (rpn_output)
	(defq stack (clear '()))
	(each! 0 -1 (lambda (token type)
		(case type
			(:lit (push stack `(((,eql s ,token) 1))))
			(:wild (push stack `((:t 1))))
			(:white (push stack `(((,<= (,cmp s " ") 0) 1))))
			(:black (push stack `(((,> (,cmp s " ") 0) 1))))
			(:sol (push stack `((:nil (,if (,= si 0) 1)))))
			(:eol (push stack `((:nil (,if (,= si (- (,length text) 1)) 1)))))
			(:op (case token
				(","
					(defq b (pop stack) a (pop stack))
					(push stack (cat a b)))
				("|"
					(defq b (pop stack) a (pop stack))
					(push stack (cat
						`((:nil 1 ,(+ (length a) 2))) a
						`((:nil ,(+ (length b) 1))) b)))
				("?"
					(defq a (pop stack))
					(push stack (cat
						`((:nil 1 ,(+ (length a) 1))) a)))
				("*"
					(defq a (pop stack) b (length a))
					(push stack (cat
						`((:nil 1 ,(+ b 2))) a `((:nil 1 ,(neg b))))))
				("+"
					(defq a (pop stack))
					(push stack (cat
						a `((:nil 1 ,(neg (length a)))))))
				)))) rpn_output)
;; (print "compile")
;; (each print (elem-get 0 stack))
;; (print)
	(pop stack))

(defun addstate (creg next index state)
	(defq stack (slice 1 -1 state))
	(while (defq offset (pop stack))
		;offsets can be expressions !
		(if (setq offset (eval offset))
			(if (= (defq new_index (+ index offset)) (length creg))
				;end state
				(unless (find new_index next) (push next new_index))
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

(defun match (text creg start)
	; (match text creg start) -> :nil | end
	;break out imediatly if no states
	(if (/= (length (defq si start current (firststate creg (list)))) 0)
		;break out imediatly if any end states
		(if (find (length creg) current)
			(length text)
			;for each char in text
			(and (defq next (list) end (some! start -1 :nil
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
					(cond
						;break out if no states
						((= (length current) 0) -1)
						;break out if any end states
						((find (length creg) current) (inc si))))
				(list text))) (> end -1) end))))

(defclass Regexp () :nil
	; (Regexp) -> regexp
	(def this :meta_cache (fmap))

	(defmethod :compile (this pattern)
		; (. regexp :compile pattern) -> :nil | meta
		(raise :meta_cache)
		(unless (defq creg (. meta_cache :find pattern))
			(defq creg (if (= (length pattern) 0)
				:nil (compile (polish (tokenize pattern)))))
			(. meta_cache :insert pattern creg))
		creg)

	(defmethod :search (this text pattern &optional creg)
		; (. regexp :search text pattern &optional meta) -> indexes
		(defq out (list) creg (opt creg (. this :compile pattern)))
		(when creg
			(defq x -1)
			(while (< (setq x (inc x)) (length text))
				(when (defq x1 (match text creg x))
					(push out (list x x1))
					(setq x (dec x1)))))
		out)
	)

;module
(export-classes
	Regexp)
(env-pop)
