;; Simple XML Parser for LispScript
;; Builds data structures from XML strings

(defun xml-unescape-text (text)
	; Decode XML entities in text
	(defq result text)
	(setq result (str-replace result "&lt;" "<"))
	(setq result (str-replace result "&gt;" ">"))
	(setq result (str-replace result "&quot;" "\""))
	(setq result (str-replace result "&apos;" "'"))
	(setq result (str-replace result "&amp;" "&"))  ; Must be last!
	result)

(defun str-replace (str old new)
	; Replace all occurrences of old with new in str
	(if (or (eql str nil) (= (length str) 0))
		""
		(progn
			(defq result (list))
			(defq pos 0)
			(defq old-len (length old))

			(while (< pos (length str))
				(defq found (str-find str old pos))
				(if found
					(progn
						; Add text before match
						(when (> found pos)
							(push result (slice pos found str)))
						; Add replacement
						(push result new)
						; Move past match
						(setq pos (+ found old-len)))
					(progn
						; No more matches - add rest
						(when (< pos (length str))
							(push result (slice pos (length str) str)))
						(setq pos (length str)))))

			(apply cat result))))

(defun str-find (str pattern &optional start)
	; Find pattern in str, return position or nil
	(when (eql start nil) (setq start 0))

	(defq pattern-len (length pattern))
	(defq str-len (length str))

	(catch
		(progn
			(while (<= (+ start pattern-len) str-len)
				(when (eql (slice start (+ start pattern-len) str) pattern)
					(throw start))
				(setq start (+ start 1)))
			:nil)
		(lambda (result) result)))

(defun str-trim (str)
	; Trim whitespace from string
	(if (or (eql str nil) (= (length str) 0))
		""
		(progn
			(defq start 0)
			(defq end (length str))

			; Trim leading
			(while (and (< start end)
					(defq ch (slice start (+ start 1) str))
					(or (eql ch " ") (eql ch "\t") (eql ch "\n") (eql ch "\r")))
				(setq start (+ start 1)))

			; Trim trailing
			(while (and (> end start)
					(defq ch (slice (- end 1) end str))
					(or (eql ch " ") (eql ch "\t") (eql ch "\n") (eql ch "\r")))
				(setq end (- end 1)))

			(if (> end start)
				(slice start end str)
				""))))

(defun xml-skip-whitespace (xml pos)
	; Skip whitespace and return new position
	(while (and (< pos (length xml))
				(defq ch (slice pos (+ pos 1) xml))
				(or (eql ch " ") (eql ch "\t") (eql ch "\n") (eql ch "\r")))
		(setq pos (+ pos 1)))
	pos)

(defun xml-parse-element (xml pos)
	; Parse an XML element starting at pos
	; Returns (list element new-pos)

	; Skip whitespace
	(setq pos (xml-skip-whitespace xml pos))

	; Expect '<'
	(when (or (>= pos (length xml)) (not (eql (slice pos (+ pos 1) xml) "<")))
		(throw "Expected '<'"))

	(setq pos (+ pos 1))

	; Check for special tags
	(cond
		; Comment: <!--
		((and (< (+ pos 3) (length xml))
			  (eql (slice pos (+ pos 3) xml) "!--"))
			(setq pos (+ pos 3))
			; Find -->
			(defq end-pos (str-find xml "-->" pos))
			(when (eql end-pos nil)
				(throw "Unclosed comment"))
			(list :nil (+ end-pos 3)))

		; Processing instruction: <?
		((and (< pos (length xml))
			  (eql (slice pos (+ pos 1) xml) "?"))
			(setq pos (+ pos 1))
			; Find ?>
			(defq end-pos (str-find xml "?>" pos))
			(when (eql end-pos nil)
				(throw "Unclosed processing instruction"))
			(list :nil (+ end-pos 2)))

		; CDATA: <![CDATA[
		((and (< (+ pos 8) (length xml))
			  (eql (slice pos (+ pos 8) xml) "![CDATA["))
			(setq pos (+ pos 8))
			; Find ]]>
			(defq end-pos (str-find xml "]]>" pos))
			(when (eql end-pos nil)
				(throw "Unclosed CDATA"))
			(defq text (slice pos end-pos xml))
			(defq element (env))
			(set-insert element :text text)
			(list element (+ end-pos 3)))

		; DOCTYPE or other <!
		((and (< pos (length xml))
			  (eql (slice pos (+ pos 1) xml) "!"))
			; Skip to >
			(defq end-pos (str-find xml ">" pos))
			(when (eql end-pos nil)
				(throw "Unclosed declaration"))
			(list :nil (+ end-pos 1)))

		; Regular element
		(:t
			; Parse tag name
			(defq name-start pos)
			(while (and (< pos (length xml))
						(defq ch (slice pos (+ pos 1) xml))
						(not (or (eql ch " ") (eql ch "\t") (eql ch "\n") (eql ch "\r")
								 (eql ch ">") (eql ch "/"))))
				(setq pos (+ pos 1)))

			(defq tag-name (slice name-start pos xml))
			(when (= (length tag-name) 0)
				(throw "Empty tag name"))

			; Create element
			(defq element (env))
			(set-insert element :name tag-name)

			; Parse attributes
			(setq pos (xml-skip-whitespace xml pos))
			(defq attrs (env))

			(while (and (< pos (length xml))
						(not (or (eql (slice pos (+ pos 1) xml) ">")
								 (eql (slice pos (+ pos 2) xml) "/>"))))
				; Parse attribute name
				(defq attr-start pos)
				(while (and (< pos (length xml))
							(defq ch (slice pos (+ pos 1) xml))
							(not (or (eql ch "=") (eql ch " ") (eql ch "\t"))))
					(setq pos (+ pos 1)))

				(defq attr-name (slice attr-start pos xml))

				; Skip to '='
				(setq pos (xml-skip-whitespace xml pos))
				(when (and (< pos (length xml)) (eql (slice pos (+ pos 1) xml) "="))
					(setq pos (+ pos 1))
					(setq pos (xml-skip-whitespace xml pos))

					; Parse attribute value (quoted)
					(defq quote (slice pos (+ pos 1) xml))
					(when (or (eql quote "\"") (eql quote "'"))
						(setq pos (+ pos 1))
						(defq value-start pos)
						(while (and (< pos (length xml))
									(not (eql (slice pos (+ pos 1) xml) quote)))
							(setq pos (+ pos 1)))

						(defq attr-value (xml-unescape-text (slice value-start pos xml)))
						(set-insert attrs (sym attr-name) attr-value)

						(setq pos (+ pos 1))))  ; Skip closing quote

				(setq pos (xml-skip-whitespace xml pos)))

			; Store attributes if any
			(when (> (env-count attrs) 0)
				(set-insert element :attributes attrs))

			; Check for self-closing tag
			(if (and (< (+ pos 1) (length xml))
					 (eql (slice pos (+ pos 2) xml) "/>"))
				; Self-closing
				(list element (+ pos 2))

				; Has content - parse children
				(progn
					; Skip '>'
					(when (and (< pos (length xml)) (eql (slice pos (+ pos 1) xml) ">"))
						(setq pos (+ pos 1)))

					; Parse content until closing tag
					(defq children (list))
					(defq text-parts (list))

					(while (< pos (length xml))
						(setq pos (xml-skip-whitespace xml pos))

						; Check for closing tag
						(when (and (< (+ pos 1) (length xml))
								   (eql (slice pos (+ pos 2) xml) "</"))
							; Found closing tag
							(setq pos (+ pos 2))

							; Verify tag name matches
							(defq close-name-start pos)
							(while (and (< pos (length xml))
										(not (eql (slice pos (+ pos 1) xml) ">")))
								(setq pos (+ pos 1)))

							(defq close-name (slice close-name-start pos xml))
							(when (not (eql tag-name close-name))
								(throw (cat "Mismatched tags: " tag-name " != " close-name)))

							; Skip '>'
							(setq pos (+ pos 1))

							; Break loop
							(setq pos (length xml)))

						; Check for child element
						(when (and (< pos (length xml))
								   (eql (slice pos (+ pos 1) xml) "<")
								   (not (eql (slice pos (+ pos 2) xml) "</")))
							; Parse child element
							(bind '(child new-pos) (xml-parse-element xml pos))
							(setq pos new-pos)

							; Add child if not nil (comments/PIs return nil)
							(when child
								(push children child)))

						; Otherwise it's text
						(when (and (< pos (length xml))
								   (not (eql (slice pos (+ pos 1) xml) "<")))
							; Parse text until next '<'
							(defq text-start pos)
							(while (and (< pos (length xml))
										(not (eql (slice pos (+ pos 1) xml) "<")))
								(setq pos (+ pos 1)))

							(defq text (str-trim (slice text-start pos xml)))
							(when (> (length text) 0)
								(push text-parts (xml-unescape-text text)))))

					; Store children
					(when (> (length children) 0)
						(set-insert element :children children))

					; Store text
					(when (> (length text-parts) 0)
						(set-insert element :text (apply cat text-parts)))

					(list element pos))))))

(defun xml-parse-string (xml-string)
	; Parse XML string into ChrysaLisp data structure
	; Returns element structure:
	;   (env :name "tagname"
	;        :attributes (env :attr1 "val1" ...)
	;        :text "text content"
	;        :children (list child1 child2 ...))

	(defq pos 0)
	(defq root nil)

	; Skip initial whitespace and declarations
	(while (and (< pos (length xml-string)) (eql root nil))
		(setq pos (xml-skip-whitespace xml-string pos))

		(when (< pos (length xml-string))
			(bind '(element new-pos) (xml-parse-element xml-string pos))
			(setq pos new-pos)

			; First real element is root
			(when element
				(setq root element))))

	root)

(defun sym (str)
	; Convert string to symbol
	(read (cat ":" str)))

(defun env-count (e)
	; Count entries in environment
	(length (env-keys e)))
