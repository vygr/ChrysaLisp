;; HTML Canvas Renderer
;; Renders HTML DOM to a ChrysaLisp Canvas with CSS styling

(import "lib/html/dom.inc")
(import "lib/html/css.inc")
(import "gui/canvas/lisp.inc")
(import "gui/font/lisp.inc")

;; Color definitions
(defq
	COLOR_BLACK 0xff000000
	COLOR_WHITE 0xffffffff
	COLOR_LINK 0xff0000ff
	COLOR_VISITED 0xff800080
	COLOR_GRAY 0xff808080
	COLOR_LIGHTGRAY 0xffc0c0c0
	COLOR_TABLE_BORDER 0xff000000)

;; Font styles
(defq
	FONT_NORMAL 0
	FONT_BOLD 1
	FONT_ITALIC 2)

(defclass html-canvas-renderer nil
	(defq
		document nil
		canvas nil
		stylesheet nil
		x 0
		y 0
		max_width 800
		font_normal nil
		font_bold nil
		font_size 14
		line_height 18
		margin 10
		current_color COLOR_BLACK
		current_bg_color :nil
		in_link :nil
		in_bold :nil
		in_italic :nil
		table_cells (list)
		list_level 0
		clickable_regions (list)
		link_start_x 0
		link_start_y 0
		link_href nil))

(defmethod :init html-canvas-renderer (doc cnv width &optional stylesheet)
	(setq this-document doc
		this-canvas cnv
		this-stylesheet stylesheet
		this-x 0
		this-y 0
		this-max_width (if width width 800)
		this-font_normal (create-font "fonts/Hack-Regular.ctf" 14)
		this-font_bold (create-font "fonts/Hack-Bold.ctf" 14)
		this-font_size 14
		this-line_height 18
		this-margin 10
		this-current_color COLOR_BLACK
		this-current_bg_color :nil
		this-in_link :nil
		this-in_bold :nil
		this-in_italic :nil
		this-table_cells (list)
		this-list_level 0
		this-clickable_regions (list)
		this-link_start_x 0
		this-link_start_y 0
		this-link_href nil)
	this)

(defmethod :apply-element-styles html-canvas-renderer (element)
	; Apply CSS styles to current rendering context
	(when (and this-stylesheet (= (. element 'node_type) NODE_ELEMENT))
		(defq styles (. this-stylesheet :compute-styles element))

		; Apply color
		(when (defq color (get styles "color"))
			(setq this-current_color (parse-css-color color)))

		; Apply background-color
		(when (defq bg-color (get styles "background-color"))
			(setq this-current_bg_color (parse-css-color bg-color)))

		; Apply font-size
		(when (defq size (get styles "font-size"))
			(setq this-font_size (parse-css-size size))
			(setq this-line_height (+ this-font_size 4)))

		; Apply font-weight
		(when (defq weight (get styles "font-weight"))
			(if (or (= weight "bold") (>= (str-to-num weight) 700))
				(setq this-in_bold :t))))
	this)

(defmethod :get-font html-canvas-renderer ()
	; Get current font based on style
	(if this-in_bold
		this-font_bold
		this-font_normal))

(defmethod :draw-text html-canvas-renderer (text)
	; Draw text at current position
	(defq font (. this :get-font))
	(when (and font text (> (length text) 0))
		; Get text dimensions
		(bind '(w h) (font-glyph-bounds font text))

		; Word wrap if necessary
		(when (> (+ this-x w) (- this-max_width this-margin))
			(setq this-x this-margin)
			(setq this-y (+ this-y this-line_height)))

		; Draw the text
		(defq color (if this-in_link COLOR_LINK this-current_color))
		(.-> this-canvas
			(:set_color color)
			(:text font this-x this-y text))

		; Move cursor
		(setq this-x (+ this-x w))
		w))

(defmethod :newline html-canvas-renderer ()
	; Move to new line
	(setq this-x this-margin)
	(setq this-y (+ this-y this-line_height))
	this)

(defmethod :add-space html-canvas-renderer ()
	; Add a space
	(defq font (. this :get-font))
	(bind '(w h) (font-glyph-bounds font " "))
	(setq this-x (+ this-x w))
	this)

(defmethod :draw-box html-canvas-renderer (x y w h color)
	; Draw a box outline
	(.-> this-canvas
		(:set_color color)
		(:box x y w h))
	this)

(defmethod :draw-filled-box html-canvas-renderer (x y w h color)
	; Draw a filled box
	(.-> this-canvas
		(:set_color color)
		(:fbox x y w h))
	this)

(defmethod :draw-line html-canvas-renderer (x1 y1 x2 y2 color)
	; Draw a line
	(.-> this-canvas
		(:set_color color)
		(:line x1 y1 x2 y2))
	this)

(defmethod :render-text-node html-canvas-renderer (node)
	; Render a text node
	(defq text (. node :get-text))
	; Split into words and render with wrapping
	(defq words (split text " "))
	(each! 0 -1 (lambda (word)
		(when (> (length word) 0)
			(. this :draw-text word)
			(. this :add-space)))
		words))

(defmethod :render-heading html-canvas-renderer (node level)
	; Render a heading (h1-h6)
	(. this :newline)
	(. this :newline)
	(defq old_bold this-in_bold)
	(setq this-in_bold :t)

	; Increase font size for headings
	(defq old_size this-font_size)
	(setq this-font_size (+ 14 (* 2 (- 7 level))))
	(setq this-line_height (+ this-font_size 4))

	(each! 0 -1 (lambda (child)
		(. this :render-node child))
		(. node 'child_nodes))

	(setq this-in_bold old_bold)
	(setq this-font_size old_size)
	(setq this-line_height (+ old_size 4))
	(. this :newline)
	(. this :newline))

(defmethod :render-paragraph html-canvas-renderer (node)
	; Render a paragraph
	(. this :newline)
	(each! 0 -1 (lambda (child)
		(. this :render-node child))
		(. node 'child_nodes))
	(. this :newline))

(defmethod :render-list html-canvas-renderer (node is-ordered)
	; Render a list (ul/ol)
	(. this :newline)
	(setq this-list_level (inc this-list_level))
	(defq item_num 1)
	(each! 0 -1 (lambda (child)
		(when (= (. child 'node_type) NODE_ELEMENT)
			(when (= (lower-case (. child 'node_name)) "li")
				; Indent for list item
				(defq indent (* this-list_level 20))
				(setq this-x (+ this-margin indent))

				; Draw bullet or number
				(if is-ordered
					(. this :draw-text (cat (str item_num) ". "))
					(. this :draw-text "• "))

				; Render list item content
				(each! 0 -1 (lambda (li_child)
					(. this :render-node li_child))
					(. child 'child_nodes))

				(. this :newline)
				(setq item_num (inc item_num)))))
		(. node 'child_nodes))
	(setq this-list_level (dec this-list_level))
	(. this :newline))

(defmethod :render-table html-canvas-renderer (node)
	; Render a table
	(. this :newline)
	(defq start_y this-y)
	(defq rows (list))

	; Collect all rows
	(each! 0 -1 (lambda (child)
		(when (= (. child 'node_type) NODE_ELEMENT)
			(defq tag (lower-case (. child 'node_name)))
			(cond
				((or (= tag "tr"))
					(push rows child))
				((or (= tag "thead") (= tag "tbody") (= tag "tfoot"))
					; Collect rows from sections
					(each! 0 -1 (lambda (section_child)
						(when (and (= (. section_child 'node_type) NODE_ELEMENT)
								(= (lower-case (. section_child 'node_name)) "tr"))
							(push rows section_child)))
						(. child 'child_nodes))))))
		(. node 'child_nodes))

	; Calculate column widths (simple fixed width for now)
	(defq col_width 150)
	(defq table_x this-margin)

	; Render rows
	(defq row_y start_y)
	(each! 0 -1 (lambda (row)
		(defq col_x table_x)
		(defq max_row_height this-line_height)

		; Render cells
		(each! 0 -1 (lambda (cell)
			(when (= (. cell 'node_type) NODE_ELEMENT)
				(defq cell_tag (lower-case (. cell 'node_name)))
				(when (or (= cell_tag "td") (= cell_tag "th"))
					; Draw cell border
					(. this :draw-box col_x row_y col_width (* 2 this-line_height) COLOR_TABLE_BORDER)

					; Render cell content
					(setq this-x (+ col_x 5))
					(setq this-y (+ row_y 5))
					(defq old_bold this-in_bold)
					(when (= cell_tag "th")
						(setq this-in_bold :t))

					(each! 0 -1 (lambda (cell_child)
						(. this :render-node cell_child))
						(. cell 'child_nodes))

					(setq this-in_bold old_bold)
					(setq col_x (+ col_x col_width)))))
			(. row 'child_nodes))

		(setq row_y (+ row_y (* 2 this-line_height))))
		rows)

	(setq this-y row_y)
	(setq this-x this-margin)
	(. this :newline))

(defmethod :render-link html-canvas-renderer (node)
	; Render a link and track its clickable region
	(defq old_link this-in_link)
	(setq this-in_link :t)

	; Record link start position and href
	(defq start_x this-x)
	(defq start_y this-y)
	(defq href (. node :get-attribute "href"))

	; Render link content
	(each! 0 -1 (lambda (child)
		(. this :render-node child))
		(. node 'child_nodes))

	; Record clickable region (simple bounding box)
	(when href
		(defq end_x this-x)
		(defq end_y this-y)

		; Calculate bounding box (handle single vs multi-line)
		(defq width (if (= start_y end_y)
			(- end_x start_x)
			(- this-max_width start_x)))  ; First line if wrapped
		(defq height (+ (- end_y start_y) this-line_height))

		; Store clickable region
		(push this-clickable_regions (list start_x start_y width height href)))

	(setq this-in_link old_link))

(defmethod :render-image html-canvas-renderer (node)
	; Render an image placeholder
	(defq alt (. node :get-attribute "alt"))
	(defq src (. node :get-attribute "src"))
	(. this :draw-filled-box this-x this-y 100 50 COLOR_LIGHTGRAY)
	(. this :draw-text (if alt (cat "[" alt "]") "[Image]"))
	(. this :newline))

(defmethod :render-form html-canvas-renderer (node)
	; Render a form (just render children for now)
	(. this :newline)
	(each! 0 -1 (lambda (child)
		(. this :render-node child))
		(. node 'child_nodes))
	(. this :newline))

(defmethod :render-input html-canvas-renderer (node)
	; Render an input field placeholder
	(defq type (. node :get-attribute "type"))
	(defq value (. node :get-attribute "value"))
	(. this :draw-box this-x this-y 150 this-line_height COLOR_GRAY)
	(when value
		(setq this-x (+ this-x 5))
		(. this :draw-text value)
		(setq this-x (- this-x 5)))
	(setq this-x (+ this-x 155))
	(. this :add-space))

(defmethod :render-button html-canvas-renderer (node)
	; Render a button
	(. this :draw-filled-box this-x this-y 80 this-line_height COLOR_LIGHTGRAY)
	(. this :draw-box this-x this-y 80 this-line_height COLOR_BLACK)
	(setq this-x (+ this-x 5))
	(each! 0 -1 (lambda (child)
		(. this :render-node child))
		(. node 'child_nodes))
	(setq this-x (+ this-x 5))
	(. this :add-space))

(defmethod :render-node html-canvas-renderer (node)
	; Render a DOM node
	(cond
		((= (. node 'node_type) NODE_TEXT)
			(. this :render-text-node node))

		((= (. node 'node_type) NODE_COMMENT)
			; Skip comments
			nil)

		((= (. node 'node_type) NODE_ELEMENT)
			; Save current styles
			(defq old_color this-current_color)
			(defq old_bg_color this-current_bg_color)
			(defq old_font_size this-font_size)
			(defq old_line_height this-line_height)
			(defq old_bold this-in_bold)

			; Apply CSS styles for this element
			(. this :apply-element-styles node)

			(defq tag-name (lower-case (. node :get-tag-name)))

			(cond
				((or (= tag-name "h1") (= tag-name "h2") (= tag-name "h3")
					 (= tag-name "h4") (= tag-name "h5") (= tag-name "h6"))
					(defq level (str-to-num (rest tag-name)))
					(. this :render-heading node level))

				((= tag-name "p")
					(. this :render-paragraph node))

				((= tag-name "br")
					(. this :newline))

				((= tag-name "hr")
					(. this :newline)
					(. this :draw-line this-margin this-y (- this-max_width this-margin) this-y COLOR_GRAY)
					(. this :newline))

				((= tag-name "ul")
					(. this :render-list node :nil))

				((= tag-name "ol")
					(. this :render-list node :t))

				((= tag-name "table")
					(. this :render-table node))

				((= tag-name "a")
					(. this :render-link node))

				((= tag-name "img")
					(. this :render-image node))

				((or (= tag-name "strong") (= tag-name "b"))
					(defq old_bold this-in_bold)
					(setq this-in_bold :t)
					(each! 0 -1 (lambda (child)
						(. this :render-node child))
						(. node 'child_nodes))
					(setq this-in_bold old_bold))

				((or (= tag-name "em") (= tag-name "i"))
					(defq old_italic this-in_italic)
					(setq this-in_italic :t)
					(each! 0 -1 (lambda (child)
						(. this :render-node child))
						(. node 'child_nodes))
					(setq this-in_italic old_italic))

				((= tag-name "form")
					(. this :render-form node))

				((= tag-name "input")
					(. this :render-input node))

				((= tag-name "button")
					(. this :render-button node))

				((or (= tag-name "div") (= tag-name "section") (= tag-name "article")
					 (= tag-name "header") (= tag-name "footer") (= tag-name "main")
					 (= tag-name "nav") (= tag-name "aside"))
					; Just render children
					(each! 0 -1 (lambda (child)
						(. this :render-node child))
						(. node 'child_nodes)))

				((or (= tag-name "html") (= tag-name "body") (= tag-name "head"))
					; Just render children
					(each! 0 -1 (lambda (child)
						(. this :render-node child))
						(. node 'child_nodes)))

				; Default: render children
				(:t
					(each! 0 -1 (lambda (child)
						(. this :render-node child))
						(. node 'child_nodes))))

			; Restore styles
			(setq this-current_color old_color)
			(setq this-current_bg_color old_bg_color)
			(setq this-font_size old_font_size)
			(setq this-line_height old_line_height)
			(setq this-in_bold old_bold))

		; Document node
		((= (. node 'node_type) NODE_DOCUMENT)
			(each! 0 -1 (lambda (child)
				(. this :render-node child))
				(. node 'child_nodes)))))

(defmethod :render html-canvas-renderer ()
	; Render the document to the canvas
	; Clear canvas
	(. this-canvas :fill COLOR_WHITE)

	; Reset position and clickable regions
	(setq this-x this-margin)
	(setq this-y this-margin)
	(setq this-clickable_regions (list))

	; Render document
	(. this :render-node this-document)

	; Return total height
	this-y)

(defmethod :get-clickable-regions html-canvas-renderer ()
	; Return list of clickable regions
	this-clickable_regions)

(defmethod :find-link-at html-canvas-renderer (x y)
	; Find which link (if any) was clicked at position (x, y)
	; Returns href or nil
	(defq result nil)
	(each! 0 -1 (lambda ((rx ry rw rh href))
		(when (and (>= x rx) (<= x (+ rx rw))
				   (>= y ry) (<= y (+ ry rh)))
			(setq result href)))
		this-clickable_regions)
	result)
