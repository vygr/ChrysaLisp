;; HTML Tokenizer
;; Tokenizes HTML into tags, text, and comments

(import "lib/class/class.inc")

(defq
	TOKEN_START_TAG 1
	TOKEN_END_TAG 2
	TOKEN_TEXT 3
	TOKEN_COMMENT 4
	TOKEN_DOCTYPE 5
	TOKEN_EOF 6)

(defclass html-tokenizer (&optional _method) :nil
	; HTML tokenizer
	(def this
		:input ""
		:pos 0
		:length 0
		:has_more :t)

	(defmethod :init (html-string)
		(def this
			:input html-string
			:pos 0
			:length (length html-string)
			:has_more (> (length html-string) 0))
		this)

	(defmethod :peek (&optional offset)
		; Peek at character at current position + offset
		(defq off (if offset offset 0))
		(defq idx (+ (get :pos this) off))
		(defq len (get :length this))
		(if (< idx len)
			(elem-get (get :input this) idx)
			:nil))

	(defmethod :advance (&optional count)
		; Advance position by count characters
		(defq cnt (if count count 1))
		(defq new-pos (min (+ (get :pos this) cnt) (get :length this)))
		(def this :pos new-pos)
		this)

	(defmethod :skip-whitespace ()
		; Skip whitespace characters
		(defq whitespace (list " " "\t" "\n" "\r"))
		(while (and (< (get :pos this) (get :length this))
				(some! (lambda (c) (eql (. this :peek) c)) whitespace))
			(. this :advance))
		this)

	(defmethod :read-until (char-or-fn)
		; Read until character or predicate function returns true
		(defq result "")
		(while (< (get :pos this) (get :length this))
			(defq ch (. this :peek))
			(defq stop
				(if (str? char-or-fn)
					(eql ch char-or-fn)
					(char-or-fn ch)))
			(if stop
				(break)
				(progn
					(setq result (cat result ch))
					(. this :advance))))
		result)

	(defmethod :read-tag-name ()
		; Read tag name (alphanumeric and -)
		(. this :read-until (lambda (ch)
			(not (or (and (>= ch "a") (<= ch "z"))
				(and (>= ch "A") (<= ch "Z"))
				(and (>= ch "0") (<= ch "9"))
				(eql ch "-")
				(eql ch "_")
				(eql ch ":"))))))

	(defmethod :read-attribute-name ()
		; Read attribute name
		(. this :read-tag-name))

	(defmethod :read-attribute-value ()
		; Read attribute value (quoted or unquoted)
		; TODO: Implement proper quoted value parsing
		"")

	(defmethod :read-attributes ()
		; Read tag attributes as an environment
		; TODO: Implement full attribute parsing
		(env))

	(defmethod :next-token ()
		; Get next token
		; Returns (list type data attributes)
		; TODO: Implement tokenization
		(list TOKEN_EOF :nil :nil)))
