;; JSON Parser
;; Converts JSON strings to ChrysaLisp data structures

(defun json-unescape-string (str)
	; Unescape JSON string escapes
	(defq result "")
	(defq i 0)
	(while (< i (length str))
		(defq c (code str i))
		(if (= c (code "\\"))
			; Escape sequence
			(progn
				(setq i (inc i))
				(when (< i (length str))
					(defq next (code str i))
					(cond
						((= next (code "\"")) (setq result (cat result "\"")))
						((= next (code "\\")) (setq result (cat result "\\")))
						((= next (code "n")) (setq result (cat result "\n")))
						((= next (code "r")) (setq result (cat result "\r")))
						((= next (code "t")) (setq result (cat result "\t")))
						(:t (setq result (cat result (char next)))))))
			; Regular character
			(setq result (cat result (char c))))
		(setq i (inc i)))
	result)

(defun json-skip-whitespace (str pos)
	; Skip whitespace characters and return new position
	(while (and (< pos (length str))
				(or (= (code str pos) (code " "))
					(= (code str pos) (code "\n"))
					(= (code str pos) (code "\r"))
					(= (code str pos) (code "\t"))))
		(setq pos (inc pos)))
	pos)

(defun json-parse-string (str pos)
	; Parse JSON string starting at pos (after opening ")
	; Returns (list value new-pos)
	(defq start pos)
	(defq result "")
	(while (< pos (length str))
		(defq c (code str pos))
		(cond
			; End of string
			((= c (code "\""))
				(ret (list (json-unescape-string result) (inc pos))))
			; Escape sequence
			((= c (code "\\"))
				(setq result (cat result (slice str pos (+ pos 2))))
				(setq pos (+ pos 2)))
			; Regular character
			(:t
				(setq result (cat result (char c)))
				(setq pos (inc pos)))))
	(throw "Unterminated string"))

(defun json-parse-number (str pos)
	; Parse JSON number starting at pos
	; Returns (list value new-pos)
	(defq start pos)
	(defq has_dot :nil)

	; Handle negative sign
	(when (and (< pos (length str)) (= (code str pos) (code "-")))
		(setq pos (inc pos)))

	; Parse digits
	(while (< pos (length str))
		(defq c (code str pos))
		(cond
			((and (>= c (code "0")) (<= c (code "9")))
				(setq pos (inc pos)))
			((and (= c (code ".")) (not has_dot))
				(setq has_dot :t)
				(setq pos (inc pos)))
			(:t (break))))

	(defq num_str (slice str start pos))
	(list (num num_str) pos))

(defun json-parse-value (str pos)
	; Parse any JSON value starting at pos
	; Returns (list value new-pos)
	(setq pos (json-skip-whitespace str pos))

	(when (>= pos (length str))
		(throw "Unexpected end of JSON"))

	(defq c (code str pos))

	(cond
		; String
		((= c (code "\""))
			(json-parse-string str (inc pos)))

		; Object
		((= c (code "{"))
			(json-parse-object str (inc pos)))

		; Array
		((= c (code "["))
			(json-parse-array str (inc pos)))

		; Number (including negative)
		((or (and (>= c (code "0")) (<= c (code "9")))
			 (= c (code "-")))
			(json-parse-number str pos))

		; true
		((and (<= (+ pos 4) (length str))
			  (= (slice str pos (+ pos 4)) "true"))
			(list :t (+ pos 4)))

		; false
		((and (<= (+ pos 5) (length str))
			  (= (slice str pos (+ pos 5)) "false"))
			(list :nil (+ pos 5)))

		; null
		((and (<= (+ pos 4) (length str))
			  (= (slice str pos (+ pos 4)) "null"))
			(list nil (+ pos 4)))

		(:t (throw (cat "Unexpected character: " (char c))))))

(defun json-parse-array (str pos)
	; Parse JSON array starting after opening [
	; Returns (list array new-pos)
	(setq pos (json-skip-whitespace str pos))
	(defq result (list))

	; Check for empty array
	(when (and (< pos (length str)) (= (code str pos) (code "]")))
		(ret (list result (inc pos))))

	; Parse array elements
	(while :t
		(setq pos (json-skip-whitespace str pos))
		(bind '(value new_pos) (json-parse-value str pos))
		(push result value)
		(setq pos (json-skip-whitespace new_pos))

		(when (>= pos (length str))
			(throw "Unterminated array"))

		(defq c (code str pos))
		(cond
			; End of array
			((= c (code "]"))
				(ret (list result (inc pos))))
			; Continue with next element
			((= c (code ","))
				(setq pos (inc pos)))
			(:t (throw "Expected , or ] in array"))))

	(throw "Unterminated array"))

(defun json-parse-object (str pos)
	; Parse JSON object starting after opening {
	; Returns (list env new-pos)
	(setq pos (json-skip-whitespace str pos))
	(defq result (env))

	; Check for empty object
	(when (and (< pos (length str)) (= (code str pos) (code "}")))
		(ret (list result (inc pos))))

	; Parse object properties
	(while :t
		(setq pos (json-skip-whitespace str pos))

		; Parse key (must be string)
		(when (or (>= pos (length str)) (!= (code str pos) (code "\"")))
			(throw "Expected string key in object"))

		(bind '(key new_pos) (json-parse-string str (inc pos)))
		(setq pos (json-skip-whitespace new_pos))

		; Expect colon
		(when (or (>= pos (length str)) (!= (code str pos) (code ":")))
			(throw "Expected : after object key"))
		(setq pos (inc pos))

		; Parse value
		(bind '(value new_pos) (json-parse-value str pos))
		(set-insert result (sym (cat ":" key)) value)
		(setq pos (json-skip-whitespace new_pos))

		(when (>= pos (length str))
			(throw "Unterminated object"))

		(defq c (code str pos))
		(cond
			; End of object
			((= c (code "}"))
				(ret (list result (inc pos))))
			; Continue with next property
			((= c (code ","))
				(setq pos (inc pos)))
			(:t (throw "Expected , or } in object"))))

	(throw "Unterminated object"))

(defun json-parse (json_str)
	; Parse JSON string and return ChrysaLisp value
	; Throws error if JSON is invalid
	(when (or (not json_str) (= (length json_str) 0))
		(throw "Empty JSON string"))

	(bind '(value pos) (json-parse-value json_str 0))

	; Verify no trailing content
	(setq pos (json-skip-whitespace json_str pos))
	(when (< pos (length json_str))
		(throw "Unexpected content after JSON value"))

	value)
