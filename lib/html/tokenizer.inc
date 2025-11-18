;; HTML Tokenizer
;; Tokenizes HTML into tags, text, and comments

(defq
	TOKEN_START_TAG 1
	TOKEN_END_TAG 2
	TOKEN_TEXT 3
	TOKEN_COMMENT 4
	TOKEN_DOCTYPE 5
	TOKEN_EOF 6)

(defclass html-tokenizer nil
	(defq
		input ""
		pos 0
		length 0))

(defmethod :init html-tokenizer (html-string)
	(setq this-input html-string
		this-pos 0
		this-length (length html-string))
	this)

(defmethod :peek html-tokenizer (&optional offset)
	; Peek at character at current position + offset
	(defq off (if offset offset 0))
	(defq idx (+ this-pos off))
	(if (< idx this-length)
		(char-at this-input idx)
		nil))

(defmethod :advance html-tokenizer (&optional count)
	; Advance position by count characters
	(defq cnt (if count count 1))
	(setq this-pos (min (+ this-pos cnt) this-length))
	this)

(defmethod :skip-whitespace html-tokenizer ()
	; Skip whitespace characters
	(while (and (< this-pos this-length)
			(some! 0 -1 (lambda (c) (= (. this :peek) c))
				'(" " "\t" "\n" "\r")))
		(. this :advance))
	this)

(defmethod :read-until html-tokenizer (char-or-fn)
	; Read until character or predicate function returns true
	(defq result "")
	(while (< this-pos this-length)
		(defq ch (. this :peek))
		(defq stop
			(if (str? char-or-fn)
				(= ch char-or-fn)
				(char-or-fn ch)))
		(if stop
			(break)
			(progn
				(setq result (cat result ch))
				(. this :advance))))
	result)

(defmethod :read-tag-name html-tokenizer ()
	; Read tag name (alphanumeric and -)
	(. this :read-until (lambda (ch)
		(not (or (and (>= ch "a") (<= ch "z"))
			(and (>= ch "A") (<= ch "Z"))
			(and (>= ch "0") (<= ch "9"))
			(= ch "-")
			(= ch "_")
			(= ch ":"))))))

(defmethod :read-attribute-name html-tokenizer ()
	; Read attribute name
	(. this :read-tag-name))

(defmethod :read-attribute-value html-tokenizer ()
	; Read attribute value (quoted or unquoted)
	(. this :skip-whitespace)
	(defq ch (. this :peek))
	(cond
		((= ch "\"")
			(. this :advance)  ; Skip opening quote
			(defq value (. this :read-until "\""))
			(. this :advance)  ; Skip closing quote
			value)
		((= ch "'")
			(. this :advance)  ; Skip opening quote
			(defq value (. this :read-until "'"))
			(. this :advance)  ; Skip closing quote
			value)
		(:t
			; Unquoted value
			(. this :read-until (lambda (ch)
				(or (= ch " ") (= ch "\t") (= ch "\n") (= ch "\r") (= ch ">")))))))

(defmethod :read-attributes html-tokenizer ()
	; Read tag attributes as an environment
	(defq attrs (env))
	(while :t
		(. this :skip-whitespace)
		(defq ch (. this :peek))
		(when (or (= ch ">") (= ch "/") (nil? ch))
			(break))

		; Read attribute name
		(defq name (lower-case (. this :read-attribute-name)))
		(when (= name "")
			(break))

		; Skip whitespace and check for =
		(. this :skip-whitespace)
		(if (= (. this :peek) "=")
			(progn
				(. this :advance)  ; Skip =
				(defq value (. this :read-attribute-value))
				(set-insert attrs name value))
			; Boolean attribute (no value)
			(set-insert attrs name :t)))
	attrs)

(defmethod :next-token html-tokenizer ()
	; Get next token
	; Returns (list type data attributes)
	(when (>= this-pos this-length)
		(return (list TOKEN_EOF nil nil)))

	(defq ch (. this :peek))

	; Check for tag
	(if (= ch "<")
		(progn
			(. this :advance)  ; Skip <
			(defq next-ch (. this :peek))

			(cond
				; Comment
				((and (= next-ch "!") (= (. this :peek 1) "-") (= (. this :peek 2) "-"))
					(. this :advance 3)  ; Skip !--
					(defq comment (. this :read-until "-"))
					; Skip -->
					(when (= (. this :peek) "-")
						(. this :advance))
					(when (= (. this :peek) "-")
						(. this :advance))
					(when (= (. this :peek) ">")
						(. this :advance))
					(list TOKEN_COMMENT comment nil))

				; DOCTYPE
				((and (= next-ch "!") (or (= (. this :peek 1) "D") (= (. this :peek 1) "d")))
					(. this :advance)  ; Skip !
					(defq doctype (. this :read-until ">"))
					(. this :advance)  ; Skip >
					(list TOKEN_DOCTYPE doctype nil))

				; End tag
				((= next-ch "/")
					(. this :advance)  ; Skip /
					(defq tag-name (lower-case (. this :read-tag-name)))
					(. this :skip-whitespace)
					(when (= (. this :peek) ">")
						(. this :advance))
					(list TOKEN_END_TAG tag-name nil))

				; Start tag
				(:t
					(defq tag-name (lower-case (. this :read-tag-name)))
					(defq attrs (. this :read-attributes))
					; Check for self-closing
					(defq self-closing :nil)
					(when (= (. this :peek) "/")
						(setq self-closing :t)
						(. this :advance))
					(when (= (. this :peek) ">")
						(. this :advance))
					(list TOKEN_START_TAG tag-name attrs))))

		; Text content
		(progn
			(defq text (. this :read-until "<"))
			(list TOKEN_TEXT text nil))))

(defmethod :has-more html-tokenizer ()
	; Check if more tokens available
	(< this-pos this-length))
