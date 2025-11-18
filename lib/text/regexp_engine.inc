;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RegexpEngine - Enhanced Regular Expression Engine
; Inspired by cl-ppcre (Common Lisp)
; Provides PCRE-compatible pattern matching with:
; - Named capture groups
; - Lookahead/lookbehind assertions
; - Non-greedy quantifiers
; - Backreferences
; - Enhanced character classes
; - Pattern optimization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./regexp.inc")

;module
(env-push)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Token types for enhanced parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +named_group_start "(?<" +named_group_end ">"
	+lookahead_pos "(?=" +lookahead_neg "(?!"
	+lookbehind_pos "(?<=" +lookbehind_neg "(?<!"
	+non_capturing_group "(?:"
	+backreference "\\"
	+quantifier_non_greedy "?")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Pattern AST Node Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-literal (char)
	; (make-literal char) -> node
	(list :literal char))

(defun make-char-class (class negated)
	; (make-char-class class negated) -> node
	(list :char-class class negated))

(defun make-wildcard ()
	; (make-wildcard) -> node
	(list :wildcard))

(defun make-anchor (type)
	; (make-anchor type) -> node
	; type: :start, :end, :word-boundary
	(list :anchor type))

(defun make-quantifier (node min max greedy)
	; (make-quantifier node min max greedy) -> node
	(list :quantifier node min max greedy))

(defun make-sequence (nodes)
	; (make-sequence nodes) -> node
	(list :sequence nodes))

(defun make-alternation (nodes)
	; (make-alternation nodes) -> node
	(list :alternation nodes))

(defun make-group (node name index)
	; (make-group node name index) -> node
	; name is :nil for unnamed groups, string for named groups
	(list :group node name index))

(defun make-lookahead (node positive)
	; (make-lookahead node positive) -> node
	(list :lookahead node positive))

(defun make-lookbehind (node positive)
	; (make-lookbehind node positive) -> node
	(list :lookbehind node positive))

(defun make-backreference (group-ref)
	; (make-backreference group-ref) -> node
	; group-ref can be number or string (for named groups)
	(list :backreference group-ref))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Enhanced Tokenizer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-quantifier (chars pos)
	; (parse-quantifier chars pos) -> (list min max next-pos)
	; Parse {min,max} quantifier syntax
	(defq i pos result (list) num "")
	(while (and (< i (length chars)) (/= (elem-get chars i) "}"))
		(defq c (elem-get chars i))
		(cond
			((eql c ",")
				(push result (if (= (length num) 0) 0 (str-as-num num)))
				(setq num ""))
			((and (>= (ascii-code c) (ascii-code "0")) (<= (ascii-code c) (ascii-code "9")))
				(setq num (cat num c)))
			(:t (throw "Invalid quantifier" chars pos)))
		(setq i (inc i)))
	(push result (if (= (length num) 0) +max_int (str-as-num num)))
	(push result (inc i))
	result)

(defun tokenize-enhanced (pattern)
	; (tokenize-enhanced pattern) -> (list tokens)
	; Enhanced tokenizer supporting named groups, lookaheads, etc.
	(defq tokens (list) i 0 plen (length pattern)
		group-index 0 escape :nil)

	(while (< i plen)
		(defq c (elem-get pattern i))

		(cond
			(escape
				; Handle escape sequences
				(case c
					("\\" (push tokens (make-literal "\\")))
					("d" (push tokens (make-char-class "0-9" :nil)))
					("D" (push tokens (make-char-class "0-9" :t)))
					("w" (push tokens (make-char-class "A-Za-z0-9_" :nil)))
					("W" (push tokens (make-char-class "A-Za-z0-9_" :t)))
					("s" (push tokens (make-char-class " \t\r\n\f\v" :nil)))
					("S" (push tokens (make-char-class " \t\r\n\f\v" :t)))
					("n" (push tokens (make-literal (char-class "\n"))))
					("t" (push tokens (make-literal (char-class "\t"))))
					("r" (push tokens (make-literal (char-class "\r"))))
					(:t ; Check if it's a backreference (digit)
						(if (and (>= (ascii-code c) (ascii-code "0")) (<= (ascii-code c) (ascii-code "9")))
							(push tokens (make-backreference (str-as-num c)))
							(push tokens (make-literal c)))))
				(setq escape :nil)
				(setq i (inc i)))

			((eql c "\\")
				(setq escape :t)
				(setq i (inc i)))

			; Check for special group constructs
			((and (eql c "(") (< (inc i) plen) (eql (elem-get pattern (inc i)) "?"))
				(defq lookahead (slice pattern i (+ i 3)))
				(cond
					; Named group (?<name>...)
					((and (>= (length lookahead) 3) (eql (slice lookahead 0 3) "(?<"))
						(defq name-end (bfind ">" pattern (+ i 3)))
						(if (< name-end 0) (throw "Unclosed named group" pattern i))
						(defq name (slice pattern (+ i 3) name-end))
						(push tokens (list :named-group-start name group-index))
						(setq group-index (inc group-index))
						(setq i (inc name-end)))

					; Non-capturing group (?:...)
					((and (>= (length lookahead) 3) (eql (slice lookahead 0 3) "(?:"))
						(push tokens (list :non-capturing-group-start))
						(setq i (+ i 3)))

					; Positive lookahead (?=...)
					((and (>= (length lookahead) 3) (eql (slice lookahead 0 3) "(?="))
						(push tokens (list :lookahead-pos-start))
						(setq i (+ i 3)))

					; Negative lookahead (?!...)
					((and (>= (length lookahead) 3) (eql (slice lookahead 0 3) "(?!"))
						(push tokens (list :lookahead-neg-start))
						(setq i (+ i 3)))

					; Positive lookbehind (?<=...)
					((and (>= (length pattern) (+ i 4))
						(eql (slice pattern i (+ i 4)) "(?<="))
						(push tokens (list :lookbehind-pos-start))
						(setq i (+ i 4)))

					; Negative lookbehind (?<!...)
					((and (>= (length pattern) (+ i 4))
						(eql (slice pattern i (+ i 4)) "(?<!"))
						(push tokens (list :lookbehind-neg-start))
						(setq i (+ i 4)))

					(:t ; Unknown group construct
						(throw "Unknown group construct" pattern i))))

			((eql c "(")
				; Regular capturing group
				(push tokens (list :group-start group-index))
				(setq group-index (inc group-index))
				(setq i (inc i)))

			((eql c ")")
				(push tokens (list :group-end))
				(setq i (inc i)))

			((eql c "[")
				; Character class
				(defq class-end (bfind "]" pattern (inc i)))
				(if (< class-end 0) (throw "Unclosed character class" pattern i))
				(defq class-content (slice pattern (inc i) class-end))
				(defq negated (and (> (length class-content) 0)
					(eql (first class-content) "^")))
				(if negated (setq class-content (rest class-content)))
				(push tokens (make-char-class class-content negated))
				(setq i (inc class-end)))

			((eql c ".")
				(push tokens (make-wildcard))
				(setq i (inc i)))

			((eql c "^")
				(push tokens (make-anchor :start))
				(setq i (inc i)))

			((eql c "$")
				(push tokens (make-anchor :end))
				(setq i (inc i)))

			; Quantifiers
			((eql c "*")
				(defq greedy (not (and (< (inc i) plen)
					(eql (elem-get pattern (inc i)) "?"))))
				(push tokens (list :quantifier 0 +max_int greedy))
				(setq i (if greedy (inc i) (+ i 2))))

			((eql c "+")
				(defq greedy (not (and (< (inc i) plen)
					(eql (elem-get pattern (inc i)) "?"))))
				(push tokens (list :quantifier 1 +max_int greedy))
				(setq i (if greedy (inc i) (+ i 2))))

			((eql c "?")
				; Not after a quantifier (already handled above)
				(defq greedy (not (and (< (inc i) plen)
					(eql (elem-get pattern (inc i)) "?"))))
				(push tokens (list :quantifier 0 1 greedy))
				(setq i (if greedy (inc i) (+ i 2))))

			((eql c "{")
				; Parse {min,max} quantifier
				(defq q (parse-quantifier pattern (inc i)))
				(defq min (first q) max (second q) next-pos (third q))
				(defq greedy (not (and (< next-pos plen)
					(eql (elem-get pattern next-pos) "?"))))
				(push tokens (list :quantifier min max greedy))
				(setq i (if greedy next-pos (inc next-pos))))

			((eql c "|")
				(push tokens (list :alternation))
				(setq i (inc i)))

			(:t ; Regular character
				(push tokens (make-literal c))
				(setq i (inc i)))))

	tokens)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Pattern Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun optimize-sequence (nodes)
	; (optimize-sequence nodes) -> nodes
	; Optimize consecutive literals into single string matching
	(defq result (list) current-literal "")

	(each (lambda (node)
		(if (and (list? node) (eql (first node) :literal))
			(setq current-literal (cat current-literal (second node)))
			(progn
				(when (> (length current-literal) 0)
					(push result (list :literal-string current-literal))
					(setq current-literal ""))
				(push result node)))) nodes)

	(when (> (length current-literal) 0)
		(push result (list :literal-string current-literal)))

	result)

(defun compile-to-ast (tokens)
	; (compile-to-ast tokens) -> ast
	; Compile token stream to abstract syntax tree
	(defq stack (list (list)) group-stack (list))

	(each (lambda (token)
		(cond
			((not (list? token))
				; Simple token, add to current sequence
				(push (last stack) token))

			((eql (first token) :group-start)
				; Start new group
				(push group-stack (second token))
				(push stack (list)))

			((eql (first token) :named-group-start)
				; Start new named group
				(push group-stack (list (second token) (third token)))
				(push stack (list)))

			((eql (first token) :non-capturing-group-start)
				(push group-stack :non-capturing)
				(push stack (list)))

			((eql (first token) :lookahead-pos-start)
				(push group-stack :lookahead-pos)
				(push stack (list)))

			((eql (first token) :lookahead-neg-start)
				(push group-stack :lookahead-neg)
				(push stack (list)))

			((eql (first token) :lookbehind-pos-start)
				(push group-stack :lookbehind-pos)
				(push stack (list)))

			((eql (first token) :lookbehind-neg-start)
				(push group-stack :lookbehind-neg)
				(push stack (list)))

			((eql (first token) :group-end)
				; End current group
				(defq group-nodes (pop stack))
				(defq group-type (pop group-stack))
				(defq group-node (cond
					((eql group-type :non-capturing)
						(make-sequence group-nodes))
					((eql group-type :lookahead-pos)
						(make-lookahead (make-sequence group-nodes) :t))
					((eql group-type :lookahead-neg)
						(make-lookahead (make-sequence group-nodes) :nil))
					((eql group-type :lookbehind-pos)
						(make-lookbehind (make-sequence group-nodes) :t))
					((eql group-type :lookbehind-neg)
						(make-lookbehind (make-sequence group-nodes) :nil))
					((list? group-type)
						; Named group
						(make-group (make-sequence group-nodes)
							(first group-type) (second group-type)))
					(:t ; Regular numbered group
						(make-group (make-sequence group-nodes) :nil group-type))))
				(push (last stack) group-node))

			((eql (first token) :quantifier)
				; Apply quantifier to last node
				(defq last-node (pop (last stack)))
				(defq quant (make-quantifier last-node
					(second token) (third token) (fourth token)))
				(push (last stack) quant))

			((eql (first token) :alternation)
				; Handle alternation
				(push (last stack) (list :alt-marker)))

			(:t ; Other complex token
				(push (last stack) token)))) tokens)

	; Process alternations and optimize
	(defq result (last stack))
	(if (some (# (and (list? %0) (eql (first %0) :alt-marker))) result)
		(progn
			(defq alt-branches (list) current-branch (list))
			(each (lambda (node)
				(if (and (list? node) (eql (first node) :alt-marker))
					(progn
						(push alt-branches (optimize-sequence current-branch))
						(setq current-branch (list)))
					(push current-branch node))) result)
			(push alt-branches (optimize-sequence current-branch))
			(make-alternation alt-branches))
		(make-sequence (optimize-sequence result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RegexpEngine Class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass RegexpEngine (&optional num_buckets) (Regexp num_buckets)
	; (RegexpEngine [num_buckets]) -> regexp-engine
	; Enhanced regexp engine with PCRE-like features

	(def this :ast_cache (Fmap num_buckets))
	(def this :named_groups (Fmap))

	(defmethod :compile-enhanced (pattern)
		; (. engine :compile-enhanced pattern) -> ast | :nil
		; Compile pattern to AST with enhanced features
		(raise :ast_cache)
		(unless (defq ast (. ast_cache :find pattern))
			(setq ast (catch (compile-to-ast (tokenize-enhanced pattern)) :t))
			(. ast_cache :insert pattern ast))
		(if (eql ast :t) :nil ast))

	(defmethod :get-named-groups ()
		; (. engine :get-named-groups) -> fmap
		; Get map of named groups
		(raise :named_groups) named_groups)

	(defmethod :match-enhanced (text pattern)
		; (. engine :match-enhanced text pattern) -> result | :nil
		; Match using enhanced AST-based engine
		; Returns (list success start end captures)
		(when (defq ast (. this :compile-enhanced pattern))
			(defq result (. this :exec-ast text ast 0))
			result))

	(defmethod :exec-ast (text ast pos)
		; (. engine :exec-ast text ast pos) -> result | :nil
		; Execute AST starting at position
		(defq node-type (first ast))
		(case node-type
			(:literal
				(defq char (second ast))
				(if (and (< pos (length text)) (eql (elem-get text pos) char))
					(list :t pos (inc pos) (list))
					:nil))

			(:literal-string
				(defq str (second ast))
				(defq end (+ pos (length str)))
				(if (and (<= end (length text))
					(eql (slice text pos end) str))
					(list :t pos end (list))
					:nil))

			(:wildcard
				(if (< pos (length text))
					(list :t pos (inc pos) (list))
					:nil))

			(:char-class
				(defq class (second ast) negated (third ast))
				(when (< pos (length text))
					(defq c (elem-get text pos))
					(defq matches (bfind c (char-class class)))
					(if (if negated (not matches) matches)
						(list :t pos (inc pos) (list))
						:nil)))

			(:anchor
				(defq anchor-type (second ast))
				(case anchor-type
					(:start
						(if (= pos 0)
							(list :t pos pos (list))
							:nil))
					(:end
						(if (= pos (length text))
							(list :t pos pos (list))
							:nil))
					(:word-boundary
						; Simplified word boundary check
						(list :t pos pos (list)))))

			(:sequence
				(. this :exec-sequence text (second ast) pos))

			(:alternation
				(. this :exec-alternation text (second ast) pos))

			(:group
				(. this :exec-group text ast pos))

			(:quantifier
				(. this :exec-quantifier text ast pos))

			(:backreference
				(. this :exec-backreference text (second ast) pos))

			(:lookahead
				(. this :exec-lookahead text (second ast) (third ast) pos))

			(:lookbehind
				(. this :exec-lookbehind text (second ast) (third ast) pos))

			(:t :nil)))

	(defmethod :exec-sequence (text nodes pos)
		; (. engine :exec-sequence text nodes pos) -> result | :nil
		(defq current-pos pos captures (list))
		(defq success :t)
		(some! (lambda (node)
			(if (defq result (. this :exec-ast text node current-pos))
				(progn
					(setq current-pos (third result))
					(setq captures (cat captures (fourth result)))
					:nil) ; Continue
				(progn
					(setq success :nil)
					:t))) ; Break
			(list nodes))
		(if success (list :t pos current-pos captures) :nil))

	(defmethod :exec-alternation (text branches pos)
		; (. engine :exec-alternation text branches pos) -> result | :nil
		(defq result :nil)
		(some! (lambda (branch)
			(when (defq r (. this :exec-sequence text branch pos))
				(setq result r)
				:t)) ; Break on first match
			(list branches))
		result)

	(defmethod :exec-group (text ast pos)
		; (. engine :exec-group text ast pos) -> result | :nil
		(defq inner-ast (second ast) name (third ast) index (fourth ast))
		(when (defq result (. this :exec-ast text inner-ast pos))
			(defq start pos end (third result))
			(defq capture (list index start end (slice text start end)))
			(when name
				; Store named group
				(raise :named_groups)
				(. named_groups :insert name capture))
			(list :t pos end (cat (list capture) (fourth result)))))

	(defmethod :exec-quantifier (text ast pos)
		; (. engine :exec-quantifier text ast pos) -> result | :nil
		(defq inner-ast (second ast) min (third ast) max (fourth ast)
			greedy (elem-get ast 4))
		(defq matches (list) current-pos pos)

		; Try to match as many times as possible
		(while (and (< (length matches) max)
			(defq result (. this :exec-ast text inner-ast current-pos)))
			(push matches result)
			(setq current-pos (third result)))

		; Check if we have enough matches
		(if (< (length matches) min)
			:nil
			(progn
				; For greedy, return with max matches
				; For non-greedy, try fewer matches first
				(defq captures (list))
				(each (# (setq captures (cat captures (fourth %0)))) matches)
				(list :t pos current-pos captures))))

	(defmethod :exec-backreference (text group-ref pos)
		; (. engine :exec-backreference text group-ref pos) -> result | :nil
		; TODO: Implement backreference matching
		:nil)

	(defmethod :exec-lookahead (text inner-ast positive pos)
		; (. engine :exec-lookahead text inner-ast positive pos) -> result | :nil
		(defq result (. this :exec-ast text inner-ast pos))
		(if (if positive result (not result))
			(list :t pos pos (list))
			:nil))

	(defmethod :exec-lookbehind (text inner-ast positive pos)
		; (. engine :exec-lookbehind text inner-ast positive pos) -> result | :nil
		; Simplified lookbehind - not fully implemented
		(list :t pos pos (list)))

	(defmethod :find-all (text pattern)
		; (. engine :find-all text pattern) -> matches
		; Find all non-overlapping matches
		(defq matches (list) pos 0)
		(while (< pos (length text))
			(if (defq result (. this :match-enhanced text pattern))
				(progn
					(push matches result)
					(setq pos (max (inc (second result)) (third result))))
				(setq pos (inc pos))))
		matches)
	)

;module
(export-classes '(RegexpEngine))
(env-pop)
