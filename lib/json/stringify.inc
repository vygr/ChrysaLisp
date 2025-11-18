;; JSON Stringify
;; Converts ChrysaLisp data structures to JSON strings

(defun json-escape-string (str)
	; Escape special characters in JSON strings
	(defq result "")
	(each! 0 -1 (lambda (c)
		(cond
			((= c (code "\"")) (setq result (cat result "\\\"")))
			((= c (code "\\")) (setq result (cat result "\\\\")))
			((= c (code "\n")) (setq result (cat result "\\n")))
			((= c (code "\r")) (setq result (cat result "\\r")))
			((= c (code "\t")) (setq result (cat result "\\t")))
			(:t (setq result (cat result (char c))))))
		str)
	result)

(defun json-stringify (value)
	; Convert ChrysaLisp value to JSON string
	(cond
		; nil → null
		((eql value nil)
			"null")

		; :t → true
		((eql value :t)
			"true")

		; :nil → false
		((eql value :nil)
			"false")

		; Number → number
		((num? value)
			(str value))

		; String → "string"
		((str? value)
			(cat "\"" (json-escape-string value) "\""))

		; List → array [...]
		((list? value)
			(if (= (length value) 0)
				"[]"
				(progn
					(defq parts (list))
					(each! 0 -1 (lambda (item)
						(push parts (json-stringify item)))
						value)
					(cat "[" (apply cat (map-insert ", " parts)) "]"))))

		; Environment (hash map) → object {...}
		(:t
			; Assume it's an environment
			(catch
				(progn
					(defq keys (env-keys value))
					(if (= (length keys) 0)
						"{}"
						(progn
							(defq parts (list))
							(each! 0 -1 (lambda (key)
								(defq key_str
									(cond
										((sym? key) (slice (str key) 1))  ; Remove leading :
										((str? key) key)
										(:t (str key))))
								(defq val (get value key))
								(push parts (cat "\"" key_str "\":" (json-stringify val))))
								keys)
							(cat "{" (apply cat (map-insert "," parts)) "}"))))
				(lambda (e)
					; If env-keys fails, treat as unknown type
					(cat "\"<unknown type>\""))))))

(defun map-insert (sep items)
	; Insert separator between list items
	; (map-insert ", " (list "a" "b" "c")) => (list "a" ", " "b" ", " "c")
	(if (= (length items) 0)
		(list)
		(progn
			(defq result (list (first items)))
			(each! 1 -1 (lambda (item)
				(push result sep)
				(push result item))
				items)
			result)))
