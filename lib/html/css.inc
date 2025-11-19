;; CSS Parser and Styling
;; Parses and applies CSS styles to HTML elements

(import "lib/class/class.inc")
(import "lib/html/dom.inc")

;; CSS Rule structure: (selector properties)
;; Properties is an environment of property-name -> value

(defclass css-stylesheet (&optional _method) :nil
	; CSS stylesheet with rules

	; Initialize fields
	(def this
		:rules (list)
		:element_styles (env))

	(defmethod :init ()
		(def this
			:rules (list)
			:element_styles (env))
		this)

	(defmethod :add-rule (selector properties)
		; Add a CSS rule
		(defq rules-list (get :rules this))
		(push rules-list (list selector properties))
		(def this :rules rules-list)
		this)

	(defmethod :get-rules ()
		; Get all rules
		(get :rules this))

	(defmethod :compute-styles (element)
		; Compute computed styles for an element
		; Returns environment of property -> value
		(defq computed (env))
		(defq rules-list (get :rules this))

		; Apply rules in order (later rules override earlier)
		(each! 0 -1 (lambda ((selector props))
			(when (. this :selector-matches? selector element)
				; Merge properties
				(each! 0 -1 (lambda ((prop val))
					(set computed prop val))
					(tolist props))))
			rules-list)

		computed)

	(defmethod :selector-matches? (selector element)
		; Check if selector matches element
		; Supports: tag, .class, #id, *
		(cond
			((eql selector "*")
				:t)
			((starts-with "#" selector)
				; ID selector
				(defq id (rest selector))
				(eql id (. element :get-attribute "id")))
			((starts-with "." selector)
				; Class selector
				(defq class (rest selector))
				(defq elem_classes (split (. element :get-attribute "class") " "))
				(some! (lambda (c) (eql c class)) elem_classes))
			(:t
				; Tag selector
				(eql (to-lower selector) (to-lower (. element 'node_name)))))))

;; CSS Parser
(defclass css-parser (&optional _method) :nil
	; CSS parser

	; Initialize fields
	(def this
		:input ""
		:pos 0
		:length 0)

	(defmethod :init (css-string)
		(def this
			:input css-string
			:pos 0
			:length (length css-string))
		this)

	(defmethod :peek (&optional offset)
		; Peek at character
		(defq off (if offset offset 0))
		(defq idx (+ (get :pos this) off))
		(defq len (get :length this))
		(if (< idx len)
			(elem-get (get :input this) idx)
			:nil))

	(defmethod :advance (&optional count)
		; Advance position
		(defq cnt (if count count 1))
		(defq current-pos (get :pos this))
		(defq len (get :length this))
		(def this :pos (min (+ current-pos cnt) len))
		this)

	(defmethod :skip-whitespace ()
		; Skip whitespace and comments
		(defq pos (get :pos this))
		(defq len (get :length this))
		(while (and (< pos len)
				(or
					(some! (lambda (c) (eql (. this :peek) c))
						'(" " "\t" "\n" "\r"))
					; Check for comments
					(and (eql (. this :peek) "/") (eql (. this :peek 1) "*"))))
			(if (and (eql (. this :peek) "/") (eql (. this :peek 1) "*"))
				; Skip comment
				(progn
					(. this :advance 2)
					(while (and (< (get :pos this) (get :length this))
							(not (and (eql (. this :peek) "*") (eql (. this :peek 1) "/"))))
						(. this :advance))
					(. this :advance 2))
				; Skip whitespace
				(. this :advance))
			(setq pos (get :pos this)))
		this)

	(defmethod :read-until (char-or-chars)
		; Read until character or set of characters
		(defq result "")
		(defq chars (if (str? char-or-chars) (list char-or-chars) char-or-chars))
		(while (< (get :pos this) (get :length this))
			(defq ch (. this :peek))
			(if (some! (lambda (c) (eql ch c)) chars)
				(break)
				(progn
					(setq result (cat result ch))
					(. this :advance))))
		result)

	(defmethod :read-selector ()
		; Read CSS selector
		(. this :skip-whitespace)
		(trim (. this :read-until "{")))

	(defmethod :read-property ()
		; Read CSS property name
		(. this :skip-whitespace)
		(trim (. this :read-until ":")))

	(defmethod :read-value ()
		; Read CSS property value
		(. this :skip-whitespace)
		(trim (. this :read-until '(";" "}"))))

	(defmethod :parse ()
		; Parse CSS and return stylesheet
		(defq stylesheet (css-stylesheet))
		(. stylesheet :init)

		(while (< (get :pos this) (get :length this))
			(. this :skip-whitespace)
			(when (>= (get :pos this) (get :length this))
				(break))

			; Read selector
			(defq selector (. this :read-selector))
			(when (eql selector "")
				(break))

			; Skip {
			(when (eql (. this :peek) "{")
				(. this :advance))

			; Read properties
			(defq properties (env))
			(while (and (< (get :pos this) (get :length this)) (not (eql (. this :peek) "}")))
				(. this :skip-whitespace)
				(defq prop (. this :read-property))
				(when (> (length prop) 0)
					; Skip :
					(when (eql (. this :peek) ":")
						(. this :advance))

					(defq value (. this :read-value))

					; Skip ; if present
					(when (eql (. this :peek) ";")
						(. this :advance))

					(set properties (to-lower prop) value)))

			; Skip }
			(when (eql (. this :peek) "}")
				(. this :advance))

			; Add rule to stylesheet
			(. stylesheet :add-rule selector properties))

		stylesheet))

;; Helper function to parse CSS
(defun parse-css (css-string)
	; Parse CSS string and return stylesheet
	(defq parser (css-parser))
	(. parser :init css-string)
	(. parser :parse))

;; Color parsing
(defun parse-css-color (color-str)
	; Parse CSS color to ARGB
	; Supports: #RGB, #RRGGBB, rgb(r,g,b), color names
	(cond
		((starts-with "#" color-str)
			; Hex color
			(defq hex (rest color-str))
			(if (eql (length hex) 3)
				; #RGB -> #RRGGBB
				(setq hex (cat
					(elem-get hex 0) (elem-get hex 0)
					(elem-get hex 1) (elem-get hex 1)
					(elem-get hex 2) (elem-get hex 2))))
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
		((eql color-str "black") 0xff000000)
		((eql color-str "white") 0xffffffff)
		((eql color-str "red") 0xffff0000)
		((eql color-str "green") 0xff00ff00)
		((eql color-str "blue") 0xff0000ff)
		((eql color-str "yellow") 0xffffff00)
		((eql color-str "cyan") 0xff00ffff)
		((eql color-str "magenta") 0xffff00ff)
		((eql color-str "gray") 0xff808080)
		((eql color-str "grey") 0xff808080)

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

