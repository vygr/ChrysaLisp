;; CSS Parser and Styling
;; Parses and applies CSS styles to HTML elements

(import "lib/html/dom.inc")

;; CSS Rule structure: (selector properties)
;; Properties is an environment of property-name -> value

(defclass css-stylesheet nil
	(defq
		rules (list)
		element_styles (env)))

(defmethod :init css-stylesheet ()
	(setq this-rules (list)
		this-element_styles (env))
	this)

(defmethod :add-rule css-stylesheet (selector properties)
	; Add a CSS rule
	(push this-rules (list selector properties))
	this)

(defmethod :get-rules css-stylesheet ()
	; Get all rules
	this-rules)

(defmethod :compute-styles css-stylesheet (element)
	; Compute computed styles for an element
	; Returns environment of property -> value
	(defq computed (env))

	; Apply rules in order (later rules override earlier)
	(each! 0 -1 (lambda ((selector props))
		(when (. this :selector-matches? selector element)
			; Merge properties
			(each! 0 -1 (lambda ((prop val))
				(set-insert computed prop val))
				(tolist props))))
		this-rules)

	computed)

(defmethod :selector-matches? css-stylesheet (selector element)
	; Check if selector matches element
	; Supports: tag, .class, #id, *
	(cond
		((= selector "*")
			:t)
		((starts-with "#" selector)
			; ID selector
			(defq id (rest selector))
			(= id (. element :get-attribute "id")))
		((starts-with "." selector)
			; Class selector
			(defq class (rest selector))
			(defq elem_classes (split (. element :get-attribute "class") " "))
			(some! 0 -1 (lambda (c) (= c class)) elem_classes))
		(:t
			; Tag selector
			(= (lower-case selector) (lower-case (. element 'node_name))))))

;; CSS Parser
(defclass css-parser nil
	(defq
		input ""
		pos 0
		length 0))

(defmethod :init css-parser (css-string)
	(setq this-input css-string
		this-pos 0
		this-length (length css-string))
	this)

(defmethod :peek css-parser (&optional offset)
	; Peek at character
	(defq off (if offset offset 0))
	(defq idx (+ this-pos off))
	(if (< idx this-length)
		(char-at this-input idx)
		nil))

(defmethod :advance css-parser (&optional count)
	; Advance position
	(defq cnt (if count count 1))
	(setq this-pos (min (+ this-pos cnt) this-length))
	this)

(defmethod :skip-whitespace css-parser ()
	; Skip whitespace and comments
	(while (and (< this-pos this-length)
			(or
				(some! 0 -1 (lambda (c) (= (. this :peek) c))
					'(" " "\t" "\n" "\r"))
				; Check for comments
				(and (= (. this :peek) "/") (= (. this :peek 1) "*"))))
		(if (and (= (. this :peek) "/") (= (. this :peek 1) "*"))
			; Skip comment
			(progn
				(. this :advance 2)
				(while (and (< this-pos this-length)
						(not (and (= (. this :peek) "*") (= (. this :peek 1) "/"))))
					(. this :advance))
				(. this :advance 2))
			; Skip whitespace
			(. this :advance)))
	this)

(defmethod :read-until css-parser (char-or-chars)
	; Read until character or set of characters
	(defq result "")
	(defq chars (if (str? char-or-chars) (list char-or-chars) char-or-chars))
	(while (< this-pos this-length)
		(defq ch (. this :peek))
		(if (some! 0 -1 (lambda (c) (= ch c)) chars)
			(break)
			(progn
				(setq result (cat result ch))
				(. this :advance))))
	result)

(defmethod :read-selector css-parser ()
	; Read CSS selector
	(. this :skip-whitespace)
	(trim (. this :read-until "{")))

(defmethod :read-property css-parser ()
	; Read CSS property name
	(. this :skip-whitespace)
	(trim (. this :read-until ":")))

(defmethod :read-value css-parser ()
	; Read CSS property value
	(. this :skip-whitespace)
	(trim (. this :read-until '(";" "}"))))

(defmethod :parse css-parser ()
	; Parse CSS and return stylesheet
	(defq stylesheet (css-stylesheet :init))

	(while (< this-pos this-length)
		(. this :skip-whitespace)
		(when (>= this-pos this-length)
			(break))

		; Read selector
		(defq selector (. this :read-selector))
		(when (= selector "")
			(break))

		; Skip {
		(when (= (. this :peek) "{")
			(. this :advance))

		; Read properties
		(defq properties (env))
		(while (and (< this-pos this-length) (/= (. this :peek) "}"))
			(. this :skip-whitespace)
			(defq prop (. this :read-property))
			(when (> (length prop) 0)
				; Skip :
				(when (= (. this :peek) ":")
					(. this :advance))

				(defq value (. this :read-value))

				; Skip ; if present
				(when (= (. this :peek) ";")
					(. this :advance))

				(set-insert properties (lower-case prop) value)))

		; Skip }
		(when (= (. this :peek) "}")
			(. this :advance))

		; Add rule to stylesheet
		(. stylesheet :add-rule selector properties))

	stylesheet)

;; Helper function to parse CSS
(defun parse-css (css-string)
	; Parse CSS string and return stylesheet
	(defq parser (css-parser :init css-string))
	(. parser :parse))

;; Color parsing
(defun parse-css-color (color-str)
	; Parse CSS color to ARGB
	; Supports: #RGB, #RRGGBB, rgb(r,g,b), color names
	(cond
		((starts-with "#" color-str)
			; Hex color
			(defq hex (rest color-str))
			(if (= (length hex) 3)
				; #RGB -> #RRGGBB
				(setq hex (cat
					(char-at hex 0) (char-at hex 0)
					(char-at hex 1) (char-at hex 1)
					(char-at hex 2) (char-at hex 2))))
			; Convert to ARGB (add alpha)
			(+ 0xff000000 (str-to-num hex 16)))

		((starts-with "rgb(" color-str)
			; rgb(r, g, b)
			(defq rgb (split (slice (rest (rest (rest (rest color-str)))) 0 -1) ","))
			(defq r (str-to-num (trim (first rgb))))
			(defq g (str-to-num (trim (second rgb))))
			(defq b (str-to-num (trim (third rgb))))
			(+ 0xff000000 (<< r 16) (<< g 8) b))

		; Color names (basic set)
		((= color-str "black") 0xff000000)
		((= color-str "white") 0xffffffff)
		((= color-str "red") 0xffff0000)
		((= color-str "green") 0xff00ff00)
		((= color-str "blue") 0xff0000ff)
		((= color-str "yellow") 0xffffff00)
		((= color-str "cyan") 0xff00ffff)
		((= color-str "magenta") 0xffff00ff)
		((= color-str "gray") 0xff808080)
		((= color-str "grey") 0xff808080)

		; Default to black
		(:t 0xff000000)))

;; Size parsing
(defun parse-css-size (size-str)
	; Parse CSS size (px, em, %, etc.) to pixels
	; For now, just handle px and numbers
	(cond
		((ends-with "px" size-str)
			(str-to-num (slice size-str 0 -2)))
		((ends-with "em" size-str)
			(* (str-to-num (slice size-str 0 -2)) 14))  ; Assume 14px = 1em
		((ends-with "%" size-str)
			(str-to-num (slice size-str 0 -1)))  ; Return percentage value
		(:t
			(str-to-num size-str))))
