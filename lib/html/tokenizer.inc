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
			(char-at (get :input this) idx)
			:nil))

	(defmethod :advance (&optional count)
		; Advance position by count characters
		(defq cnt (if count count 1))
		(defq new-pos (min (+ (get :pos this) cnt) (get :length this)))
		(def this :pos new-pos)
		this)

	(defmethod :skip-whitespace ()
		; Skip whitespace characters
		(while (and (< (get :pos this) (get :length this))
				(some! 0 -1 (lambda (c) (eql (. this :peek) c))
					'(" " "\t" "\n" "\r")))
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
		(. this :skip-whitespace)
		(defq ch (. this :peek))
		(cond
			((eql ch "\"")
				(. this :advance)  ; Skip opening quote
				(defq value (. this :read-until "\""))
				(. this :advance)  ; Skip closing quote
				value)
			((eql ch "'")
				(. this :advance)  ; Skip opening quote
				(defq value (. this :read-until "'"))
				(. this :advance)  ; Skip closing quote
				value)
			(:t
				; Unquoted value
				(. this :read-until (lambda (ch)
					(or (eql ch " ") (eql ch "\t") (eql ch "\n") (eql ch "\r") (eql ch ">")))))))

	(defmethod :read-attributes ()
		; Read tag attributes as an environment
		(defq attrs (env))
		(while :t
			(. this :skip-whitespace)
			(defq ch (. this :peek))
			(when (or (eql ch ">") (eql ch "/") (nil? ch))
				(break))

			; Read attribute name
			(defq name (to-lower (. this :read-attribute-name)))
			(when (eql name "")
				(break))

			; Skip whitespace and check for =
			(. this :skip-whitespace)
			(if (eql (. this :peek) "=")
				(progn
					(. this :advance)  ; Skip =
					(defq value (. this :read-attribute-value))
					(set attrs (sym name) value))
				; Boolean attribute (no value)
				(set attrs (sym name) :t)))
		attrs)

	(defmethod :next-token ()
		; Get next token
		; Returns (list type data attributes)
		(when (>= (get :pos this) (get :length this))
			(return (list TOKEN_EOF :nil :nil)))

		(defq ch (. this :peek))

		; Check for tag
		(if (eql ch "<")
			(progn
				(. this :advance)  ; Skip <
				(defq next-ch (. this :peek))

				(cond
					; Comment
					((and (eql next-ch "!") (eql (. this :peek 1) "-") (eql (. this :peek 2) "-"))
						(. this :advance 3)  ; Skip !--
						(defq comment (. this :read-until "-"))
						; Skip -->
						(when (eql (. this :peek) "-")
							(. this :advance))
						(when (eql (. this :peek) "-")
							(. this :advance))
						(when (eql (. this :peek) ">")
							(. this :advance))
						(list TOKEN_COMMENT comment :nil))

					; DOCTYPE
					((and (eql next-ch "!") (or (eql (. this :peek 1) "D") (eql (. this :peek 1) "d")))
						(. this :advance)  ; Skip !
						(defq doctype (. this :read-until ">"))
						(. this :advance)  ; Skip >
						(list TOKEN_DOCTYPE doctype :nil))

					; End tag
					((eql next-ch "/")
						(. this :advance)  ; Skip /
						(defq tag-name (to-lower (. this :read-tag-name)))
						(. this :skip-whitespace)
						(when (eql (. this :peek) ">")
							(. this :advance))
						(list TOKEN_END_TAG tag-name :nil))

					; Start tag
					(:t
						(defq tag-name (to-lower (. this :read-tag-name)))
						(defq attrs (. this :read-attributes))
						; Check for self-closing
						(defq self-closing :nil)
						(when (eql (. this :peek) "/")
							(setq self-closing :t)
							(. this :advance))
						(when (eql (. this :peek) ">")
							(. this :advance))
						(list TOKEN_START_TAG tag-name attrs))))

			; Text content
			(progn
				(defq text (. this :read-until "<"))
				(list TOKEN_TEXT text :nil)))))
