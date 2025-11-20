;; HTML Canvas Renderer
;; Renders HTML DOM to a ChrysaLisp Canvas with CSS styling

(import "lib/html/dom.inc")
(import "lib/html/css.inc")
(import "gui/canvas/lisp.inc")
(import "gui/font/lisp.inc")
(import "gui/pixmap/lisp.inc")
(import "lib/class/class.inc")

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

(defclass html-canvas-renderer (&optional _method) :nil
	(def this :document :nil)
	(def this :canvas :nil)
	(def this :stylesheet :nil)
	(def this :base_path "")
	(def this :x 0)
	(def this :y 0)
	(def this :max_width 800)
	(def this :font_normal :nil)
	(def this :font_bold :nil)
	(def this :font_size 14)
	(def this :line_height 18)
	(def this :margin 10)
	(def this :current_color COLOR_BLACK)
	(def this :current_bg_color :nil)
	(def this :in_link :nil)
	(def this :in_bold :nil)
	(def this :in_italic :nil)
	(def this :table_cells (list))
	(def this :list_level 0)
	(def this :clickable_regions (list))
	(def this :link_start_x 0)
	(def this :link_start_y 0)
	(def this :link_href :nil)
	(def this :text_fragments (list))
	(def this :selection_start :nil)
	(def this :selection_end :nil)

	(defmethod :init (doc cnv width &optional stylesheet base-path)
		(def this :document doc)
		(def this :canvas cnv)
		(def this :stylesheet stylesheet)
		(def this :base_path (if base-path base-path ""))
		(def this :x 0)
		(def this :y 0)
		(def this :max_width (if width width 800))
		(def this :font_normal (create-font "fonts/Hack-Regular.ctf" 14))
		(def this :font_bold (create-font "fonts/Hack-Bold.ctf" 14))
		(def this :font_size 14)
		(def this :line_height 18)
		(def this :margin 10)
		(def this :current_color COLOR_BLACK)
		(def this :current_bg_color :nil)
		(def this :in_link :nil)
		(def this :in_bold :nil)
		(def this :in_italic :nil)
		(def this :table_cells (list))
		(def this :list_level 0)
		(def this :clickable_regions (list))
		(def this :link_start_x 0)
		(def this :link_start_y 0)
		(def this :link_href :nil)
		(def this :text_fragments (list))
		(def this :selection_start :nil)
		(def this :selection_end :nil)
		this)

	(defmethod :apply-element-styles (element)
		; Apply CSS styles to current rendering context
		(when (and (get :stylesheet this) (eql (. element 'node_type) NODE_ELEMENT))
			(defq styles (. (get :stylesheet this) :compute-styles element))

			; Apply color
			(when (defq color (get styles "color"))
				(def this :current_color (parse-css-color color)))

			; Apply background-color
			(when (defq bg-color (get styles "background-color"))
				(def this :current_bg_color (parse-css-color bg-color)))

			; Apply font-size
			(when (defq size (get styles "font-size"))
				(def this :font_size (parse-css-size size))
				(def this :line_height (+ (get :font_size this) 4)))

			; Apply font-weight
			(when (defq weight (get styles "font-weight"))
				(if (or (eql weight "bold") (>= (parse-number weight) 700))
					(def this :in_bold :t))))
		this)

	(defmethod :get-font ()
		; Get current font based on style
		(if (get :in_bold this)
			(get :font_bold this)
			(get :font_normal this)))

	(defmethod :draw-text (text)
		; Draw text at current position and track for selection
		(defq font (. this :get-font))
		(when (and font text (> (length text) 0))
			; Get text dimensions
			(bind '(w h) (font-glyph-bounds font text))

			; Word wrap if necessary
			(when (> (+ (get :x this) w) (- (get :max_width this) (get :margin this)))
				(def this :x (get :margin this))
				(def this :y (+ (get :y this) (get :line_height this))))

			; Record position for text selection
			(defq start_x (get :x this))
			(defq start_y (get :y this))

			; Draw the text
			(defq color (if (get :in_link this) COLOR_LINK (get :current_color this)))
			(.-> (get :canvas this)
				(:set_color color)
				(:text font (get :x this) (get :y this) text))

			; Track this text fragment for selection
			(push (get :text_fragments this) (list start_x start_y w (get :line_height this) text))

			; Move cursor
			(def this :x (+ (get :x this) w))
			w))

	(defmethod :newline ()
		; Move to new line
		(def this :x (get :margin this))
		(def this :y (+ (get :y this) (get :line_height this)))
		this)

	(defmethod :add-space ()
		; Add a space
		(defq font (. this :get-font))
		(bind '(w h) (font-glyph-bounds font " "))
		(def this :x (+ (get :x this) w))
		this)

	(defmethod :draw-box (x y w h color)
		; Draw a box outline
		(.-> (get :canvas this)
			(:set_color color)
			(:box x y w h))
		this)

	(defmethod :draw-filled-box (x y w h color)
		; Draw a filled box
		(.-> (get :canvas this)
			(:set_color color)
			(:fbox x y w h))
		this)

	(defmethod :draw-line (x1 y1 x2 y2 color)
		; Draw a line
		(.-> (get :canvas this)
			(:set_color color)
			(:line x1 y1 x2 y2))
		this)

	(defmethod :render-text-node (node)
		; Render a text node
		(defq text (. node :get-text))
		; Split into words and render with wrapping
		(defq words (split text " "))
		(each! (lambda (word)
			(when (> (length word) 0)
				(. this :draw-text word)
				(. this :add-space)))
			words))

	(defmethod :render-heading (node level)
		; Render a heading (h1-h6)
		(. this :newline)
		(. this :newline)
		(defq old_bold (get :in_bold this))
		(def this :in_bold :t)

		; Increase font size for headings
		(defq old_size (get :font_size this))
		(def this :font_size (+ 14 (* 2 (- 7 level))))
		(def this :line_height (+ (get :font_size this) 4))

		(each! (lambda (child)
			(. this :render-node child))
			(. node 'child_nodes))

		(def this :in_bold old_bold)
		(def this :font_size old_size)
		(def this :line_height (+ old_size 4))
		(. this :newline)
		(. this :newline))

	(defmethod :render-paragraph (node)
		; Render a paragraph
		(. this :newline)
		(each! (lambda (child)
			(. this :render-node child))
			(. node 'child_nodes))
		(. this :newline))

	(defmethod :render-list (node is-ordered)
		; Render a list (ul/ol)
		(. this :newline)
		(def this :list_level (inc (get :list_level this)))
		(defq item_num 1)
		(each! (lambda (child)
			(when (eql (. child 'node_type) NODE_ELEMENT)
				(when (eql (to-lower (. child 'node_name)) "li")
					; Indent for list item
					(defq indent (* (get :list_level this) 20))
					(def this :x (+ (get :margin this) indent))

					; Draw bullet or number
					(if is-ordered
						(. this :draw-text (cat (str item_num) ". "))
						(. this :draw-text "• "))

					; Render list item content
					(each! (lambda (li_child)
						(. this :render-node li_child))
						(. child 'child_nodes))

					(. this :newline)
					(setq item_num (inc item_num)))))
			(. node 'child_nodes))
		(def this :list_level (dec (get :list_level this)))
		(. this :newline))

	(defmethod :render-table (node)
		; Render a table
		(. this :newline)
		(defq start_y (get :y this))
		(defq rows (list))

		; Collect all rows
		(each! (lambda (child)
			(when (eql (. child 'node_type) NODE_ELEMENT)
				(defq tag (to-lower (. child 'node_name)))
				(cond
					((or (eql tag "tr"))
						(push rows child))
					((or (eql tag "thead") (eql tag "tbody") (eql tag "tfoot"))
						; Collect rows from sections
						(each! (lambda (section_child)
							(when (and (eql (. section_child 'node_type) NODE_ELEMENT)
									(eql (to-lower (. section_child 'node_name)) "tr"))
								(push rows section_child)))
							(. child 'child_nodes))))))
			(. node 'child_nodes))

		; Calculate column widths (simple fixed width for now)
		(defq col_width 150)
		(defq table_x (get :margin this))

		; Render rows
		(defq row_y start_y)
		(each! (lambda (row)
			(defq col_x table_x)
			(defq max_row_height (get :line_height this))

			; Render cells
			(each! (lambda (cell)
				(when (eql (. cell 'node_type) NODE_ELEMENT)
					(defq cell_tag (to-lower (. cell 'node_name)))
					(when (or (eql cell_tag "td") (eql cell_tag "th"))
						; Draw cell border
						(. this :draw-box col_x row_y col_width (* 2 (get :line_height this)) COLOR_TABLE_BORDER)

						; Render cell content
						(def this :x (+ col_x 5))
						(def this :y (+ row_y 5))
						(defq old_bold (get :in_bold this))
						(when (eql cell_tag "th")
							(def this :in_bold :t))

						(each! (lambda (cell_child)
							(. this :render-node cell_child))
							(. cell 'child_nodes))

						(def this :in_bold old_bold)
						(setq col_x (+ col_x col_width)))))
				(. row 'child_nodes))

			(setq row_y (+ row_y (* 2 (get :line_height this)))))
			rows)

		(def this :y row_y)
		(def this :x (get :margin this))
		(. this :newline))

	(defmethod :render-link (node)
		; Render a link and track its clickable region
		(defq old_link (get :in_link this))
		(def this :in_link :t)

		; Record link start position and href
		(defq start_x (get :x this))
		(defq start_y (get :y this))
		(defq href (. node :get-attribute "href"))

		; Render link content
		(each! (lambda (child)
			(. this :render-node child))
			(. node 'child_nodes))

		; Record clickable region (simple bounding box)
		(when href
			(defq end_x (get :x this))
			(defq end_y (get :y this))

			; Calculate bounding box (handle single vs multi-line)
			(defq width (if (eql start_y end_y)
				(- end_x start_x)
				(- (get :max_width this) start_x)))  ; First line if wrapped
			(defq height (+ (- end_y start_y) (get :line_height this)))

			; Store clickable region
			(push (get :clickable_regions this) (list start_x start_y width height href)))

		(def this :in_link old_link))

	(defmethod :render-image (node)
		; Render an image from file
		(defq alt (. node :get-attribute "alt"))
		(defq src (. node :get-attribute "src"))
		(defq width-attr (. node :get-attribute "width"))
		(defq height-attr (. node :get-attribute "height"))

		; Resolve file path (handle file:// URLs and relative paths)
		(defq file-path src)
		(when (starts-with "file://" src)
			(setq file-path (slice 7 (length src) src)))

		; If relative path and we have a base path, resolve it
		(when (and (get :base_path this) (not (starts-with "/" file-path)))
			(setq file-path (cat (get :base_path this) file-path)))

		; Try to load the pixmap
		(defq pixmap :nil)
		(catch
			(progn
				(when (path? file-path)
					(setq pixmap (pixmap-load file-path))))
			(lambda (e)
				(print "Failed to load image: " file-path " - " e)))

		; Render image or fallback placeholder
		(if pixmap
			(progn
				; Get pixmap dimensions
				(defq info (pixmap-info file-path))
				(defq img-width (first info))
				(defq img-height (elem 1 info))

				; Use specified dimensions if provided
				(when width-attr
					(setq img-width (parse-number width-attr)))
				(when height-attr
					(setq img-height (parse-number height-attr)))

				; Draw the pixmap at current position
				; Note: Would need canvas blit method to render pixmap
				; For now, draw a colored box with dimensions
				(. this :draw-filled-box (get :x this) (get :y this) img-width img-height COLOR_LIGHTGRAY)

				; Add to clickable regions if inside a link
				(when (get :in_link this)
					(push (get :clickable_regions this) (list (get :x this) (get :y this)
						(+ (get :x this) img-width) (+ (get :y this) img-height) (get :link_href this))))

				; Advance position
				(def this :x (+ (get :x this) img-width))
				(def this :y (+ (get :y this) img-height))
				(. this :newline))

			; Fallback placeholder if image failed to load
			(progn
				(. this :draw-filled-box (get :x this) (get :y this) 100 50 COLOR_LIGHTGRAY)
				(. this :draw-text (if alt (cat "[" alt "]") "[Image]"))
				(. this :newline))))

	(defmethod :render-canvas (node)
		; Render a canvas element
		(defq width-attr (. node :get-attribute "width"))
		(defq height-attr (. node :get-attribute "height"))

		; Default canvas dimensions: 300x150
		(defq canvas-width (if width-attr (parse-number width-attr) 300))
		(defq canvas-height (if height-attr (parse-number height-attr) 150))

		; Get the canvas context if it exists
		(defq ctx (. node :get-context "2d"))

		; Save starting position
		(defq start-x (get :x this))
		(defq start-y (get :y this))

		; Draw canvas border (to visualize canvas area)
		(. this :draw-box (get :x this) (get :y this) canvas-width canvas-height COLOR_GRAY)

		; If context exists, execute drawing commands
		(when ctx
			(import "lib/html/canvas_2d.inc")
			(defq commands (. ctx :get-commands))

			(each! (lambda (cmd)
				(defq cmd-type (get cmd :type))

				(cond
					; Rectangle drawing
					((eql cmd-type :fillRect)
						(defq x (+ start-x (get cmd :x)))
						(defq y (+ start-y (get cmd :y)))
						(defq w (get cmd :width))
						(defq h (get cmd :height))
						(defq color (parse-color (get cmd :fill_style)))
						(when color
							(. this :draw-filled-box x y w h color)))

					((eql cmd-type :strokeRect)
						(defq x (+ start-x (get cmd :x)))
						(defq y (+ start-y (get cmd :y)))
						(defq w (get cmd :width))
						(defq h (get cmd :height))
						(defq color (parse-color (get cmd :stroke_style)))
						(when color
							(. this :draw-box x y w h color)))

					((eql cmd-type :clearRect)
						(defq x (+ start-x (get cmd :x)))
						(defq y (+ start-y (get cmd :y)))
						(defq w (get cmd :width))
						(defq h (get cmd :height))
						(. this :draw-filled-box x y w h COLOR_WHITE))

					; Path drawing
					((eql cmd-type :stroke)
						(defq path (get cmd :path))
						(defq color (parse-color (get cmd :stroke_style)))
						(when (and path color)
							(draw-path this path start-x start-y color)))

					((eql cmd-type :fill)
						(defq path (get cmd :path))
						(defq color (parse-color (get cmd :fill_style)))
						(when (and path color)
							(draw-filled-path this path start-x start-y color)))

					; Text drawing
					((eql cmd-type :fillText)
						(defq text (get cmd :text))
						(defq x (+ start-x (get cmd :x)))
						(defq y (+ start-y (get cmd :y)))
						(defq color (parse-color (get cmd :fill_style)))
						(when color
							(defq old-x (get :x this))
							(defq old-y (get :y this))
							(defq old-color (get :current_color this))
							(def this :x x)
							(def this :y y)
							(def this :current_color color)
							(. this :draw-text text)
							(def this :x old-x)
							(def this :y old-y)
							(def this :current_color old-color)))

					((eql cmd-type :strokeText)
						; For now, render same as fillText
						(defq text (get cmd :text))
						(defq x (+ start-x (get cmd :x)))
						(defq y (+ start-y (get cmd :y)))
						(defq color (parse-color (get cmd :stroke_style)))
						(when color
							(defq old-x (get :x this))
							(defq old-y (get :y this))
							(defq old-color (get :current_color this))
							(def this :x x)
							(def this :y y)
							(def this :current_color color)
							(. this :draw-text text)
							(def this :x old-x)
							(def this :y old-y)
							(def this :current_color old-color)))))
				commands))

		; Move rendering position past canvas
		(def this :y (+ start-y canvas-height))
		(. this :newline))

	(defmethod :render-form (node)
		; Render a form (just render children for now)
		(. this :newline)
		(each! (lambda (child)
			(. this :render-node child))
			(. node 'child_nodes))
		(. this :newline))

	(defmethod :render-input (node)
		; Render an input field placeholder
		(defq type (. node :get-attribute "type"))
		(defq value (. node :get-attribute "value"))
		(. this :draw-box (get :x this) (get :y this) 150 (get :line_height this) COLOR_GRAY)
		(when value
			(def this :x (+ (get :x this) 5))
			(. this :draw-text value)
			(def this :x (- (get :x this) 5)))
		(def this :x (+ (get :x this) 155))
		(. this :add-space))

	(defmethod :render-button (node)
		; Render a button
		(. this :draw-filled-box (get :x this) (get :y this) 80 (get :line_height this) COLOR_LIGHTGRAY)
		(. this :draw-box (get :x this) (get :y this) 80 (get :line_height this) COLOR_BLACK)
		(def this :x (+ (get :x this) 5))
		(each! (lambda (child)
			(. this :render-node child))
			(. node 'child_nodes))
		(def this :x (+ (get :x this) 5))
		(. this :add-space))

	(defmethod :render-node (node)
		; Render a DOM node
		(cond
			((eql (. node 'node_type) NODE_TEXT)
				(. this :render-text-node node))

			((eql (. node 'node_type) NODE_COMMENT)
				; Skip comments
				:nil)

			((eql (. node 'node_type) NODE_ELEMENT)
				; Save current styles
				(defq old_color (get :current_color this))
				(defq old_bg_color (get :current_bg_color this))
				(defq old_font_size (get :font_size this))
				(defq old_line_height (get :line_height this))
				(defq old_bold (get :in_bold this))

				; Apply CSS styles for this element
				(. this :apply-element-styles node)

				(defq tag-name (to-lower (. node :get-tag-name)))

				(cond
					((or (eql tag-name "h1") (eql tag-name "h2") (eql tag-name "h3")
						 (eql tag-name "h4") (eql tag-name "h5") (eql tag-name "h6"))
						(defq level (parse-number (rest tag-name)))
						(. this :render-heading node level))

					((eql tag-name "p")
						(. this :render-paragraph node))

					((eql tag-name "br")
						(. this :newline))

					((eql tag-name "hr")
						(. this :newline)
						(. this :draw-line (get :margin this) (get :y this) (- (get :max_width this) (get :margin this)) (get :y this) COLOR_GRAY)
						(. this :newline))

					((eql tag-name "ul")
						(. this :render-list node :nil))

					((eql tag-name "ol")
						(. this :render-list node :t))

					((eql tag-name "table")
						(. this :render-table node))

					((eql tag-name "a")
						(. this :render-link node))

					((eql tag-name "img")
						(. this :render-image node))

					((eql tag-name "canvas")
						(. this :render-canvas node))

					((or (eql tag-name "strong") (eql tag-name "b"))
						(defq old_bold (get :in_bold this))
						(def this :in_bold :t)
						(each! (lambda (child)
							(. this :render-node child))
							(. node 'child_nodes))
						(def this :in_bold old_bold))

					((or (eql tag-name "em") (eql tag-name "i"))
						(defq old_italic (get :in_italic this))
						(def this :in_italic :t)
						(each! (lambda (child)
							(. this :render-node child))
							(. node 'child_nodes))
						(def this :in_italic old_italic))

					((eql tag-name "form")
						(. this :render-form node))

					((eql tag-name "input")
						(. this :render-input node))

					((eql tag-name "button")
						(. this :render-button node))

					((or (eql tag-name "div") (eql tag-name "section") (eql tag-name "article")
						 (eql tag-name "header") (eql tag-name "footer") (eql tag-name "main")
						 (eql tag-name "nav") (eql tag-name "aside"))
						; Just render children
						(each! (lambda (child)
							(. this :render-node child))
							(. node 'child_nodes)))

					((or (eql tag-name "html") (eql tag-name "body") (eql tag-name "head"))
						; Just render children
						(each! (lambda (child)
							(. this :render-node child))
							(. node 'child_nodes)))

					; Default: render children
					(:t
						(each! (lambda (child)
							(. this :render-node child))
							(. node 'child_nodes))))

				; Restore styles
				(def this :current_color old_color)
				(def this :current_bg_color old_bg_color)
				(def this :font_size old_font_size)
				(def this :line_height old_line_height)
				(def this :in_bold old_bold))

			; Document node
			((eql (. node 'node_type) NODE_DOCUMENT)
				(each! (lambda (child)
					(. this :render-node child))
					(. node 'child_nodes)))))

	(defmethod :render ()
		; Render the document to the canvas
		; Clear canvas
		(. (get :canvas this) :fill COLOR_WHITE)

		; Reset position, clickable regions, and text fragments
		(def this :x (get :margin this))
		(def this :y (get :margin this))
		(def this :clickable_regions (list))
		(def this :text_fragments (list))

		; Render document
		(. this :render-node (get :document this))

		; Render selection highlight if active
		(when (and (get :selection_start this) (get :selection_end this))
			(. this :render-selection))

		; Return total height
		(get :y this))

	(defmethod :get-clickable-regions ()
		; Return list of clickable regions
		(get :clickable_regions this))

	(defmethod :find-link-at (x y)
		; Find which link (if any) was clicked at position (x, y)
		; Returns href or nil
		(defq result :nil)
		(each! (lambda ((rx ry rw rh href))
			(when (and (>= x rx) (<= x (+ rx rw))
					   (>= y ry) (<= y (+ ry rh)))
				(setq result href)))
			(get :clickable_regions this))
		result)

	(defmethod :set-selection-start (x y)
		; Set selection start point
		(def this :selection_start (list x y))
		this)

	(defmethod :set-selection-end (x y)
		; Set selection end point
		(def this :selection_end (list x y))
		this)

	(defmethod :clear-selection ()
		; Clear selection
		(def this :selection_start :nil)
		(def this :selection_end :nil)
		this)

	(defmethod :render-selection ()
		; Render selection highlight
		(when (and (get :selection_start this) (get :selection_end this))
			(bind '(x1 y1) (get :selection_start this))
			(bind '(x2 y2) (get :selection_end this))

			; Ensure x1,y1 is before x2,y2
			(when (or (> y1 y2) (and (eql y1 y2) (> x1 x2)))
				(defq tmp_x x1)
				(defq tmp_y y1)
				(setq x1 x2)
				(setq y1 y2)
				(setq x2 tmp_x)
				(setq y2 tmp_y))

			; Highlight each text fragment in selection
			(each! (lambda ((fx fy fw fh text))
				(defq fx2 (+ fx fw))
				(defq fy2 (+ fy fh))

				; Check if fragment intersects with selection
				(defq in_selection :nil)

				; Same line selection
				(when (and (eql y1 y2) (>= fy y1) (<= fy y1))
					(when (and (< fx x2) (> fx2 x1))
						(setq in_selection :t)))

				; Multi-line selection
				(when (< y1 y2)
					; First line
					(when (and (>= fy y1) (< fy (+ y1 fh)) (>= fx x1))
						(setq in_selection :t))

					; Middle lines
					(when (and (> fy y1) (< fy y2))
						(setq in_selection :t))

					; Last line
					(when (and (>= fy y2) (< fy (+ y2 fh)) (<= fx x2))
						(setq in_selection :t)))

				; Draw highlight
				(when in_selection
					(. this :draw-filled-box fx fy fw fh 0xffadd8e6)))  ; Light blue
				(get :text_fragments this)))
		this)

	(defmethod :get-selected-text ()
		; Get selected text as string
		(defq result "")
		(when (and (get :selection_start this) (get :selection_end this))
			(bind '(x1 y1) (get :selection_start this))
			(bind '(x2 y2) (get :selection_end this))

			; Ensure x1,y1 is before x2,y2
			(when (or (> y1 y2) (and (eql y1 y2) (> x1 x2)))
				(defq tmp_x x1)
				(defq tmp_y y1)
				(setq x1 x2)
				(setq y1 y2)
				(setq x2 tmp_x)
				(setq y2 tmp_y))

			; Collect text from fragments in selection
			(each! (lambda ((fx fy fw fh text))
				(defq fx2 (+ fx fw))

				; Check if fragment is in selection
				(defq in_selection :nil)

				; Same line selection
				(when (and (eql y1 y2) (>= fy y1) (<= fy y1))
					(when (and (< fx x2) (> fx2 x1))
						(setq in_selection :t)))

				; Multi-line selection
				(when (< y1 y2)
					; First line
					(when (and (>= fy y1) (< fy (+ y1 fh)) (>= fx x1))
						(setq in_selection :t))

					; Middle lines
					(when (and (> fy y1) (< fy y2))
						(setq in_selection :t))

					; Last line
					(when (and (>= fy y2) (< fy (+ y2 fh)) (<= fx x2))
						(setq in_selection :t)))

				(when in_selection
					(setq result (cat result text " "))))
				(get :text_fragments this)))

		result))

;; Helper Functions

(defun is-hex-color-str (prefix str)
	; Check if string starts with prefix (used for color parsing)
	(when (and str (>= (length str) (length prefix)))
		(eql (slice 0 (length prefix) str) prefix)))

(defun parse-color (color-str)
	; Parse CSS color string to ChrysaLisp color value
	; Supports: #RGB, #RRGGBB, named colors
	(when color-str
		(cond
			; Hex colors
			((is-hex-color-str "#" color-str)
				(defq hex (rest color-str))
				(cond
					; #RGB format
					((eql (length hex) 3)
						(defq r (hex-to-num (slice 0 1 hex)))
						(defq g (hex-to-num (slice 1 2 hex)))
						(defq b (hex-to-num (slice 2 3 hex)))
						(argb 255 (* r 17) (* g 17) (* b 17)))

					; #RRGGBB format
					((eql (length hex) 6)
						(defq r (hex-to-num (slice 0 2 hex)))
						(defq g (hex-to-num (slice 2 4 hex)))
						(defq b (hex-to-num (slice 4 6 hex)))
						(argb 255 r g b))

					(:t COLOR_BLACK)))

			; Named colors
			((eql color-str "red") COLOR_RED)
			((eql color-str "green") COLOR_GREEN)
			((eql color-str "blue") COLOR_BLUE)
			((eql color-str "black") COLOR_BLACK)
			((eql color-str "white") COLOR_WHITE)
			((eql color-str "gray") COLOR_GRAY)

			(:t COLOR_BLACK))))

(defun hex-to-num (hex-str)
	; Convert hex string to number
	(defq result 0)
	(defq chars (explode hex-str))
	(each! (lambda (ch)
		(setq result (* result 16))
		(cond
			((and (>= ch (code "0")) (<= ch (code "9")))
				(setq result (+ result (- ch (code "0")))))
			((and (>= ch (code "a")) (<= ch (code "f")))
				(setq result (+ result (- ch (- (code "a") 10)))))
			((and (>= ch (code "A")) (<= ch (code "F")))
				(setq result (+ result (- ch (- (code "A") 10)))))))
		chars)
	result)

(defun draw-path (renderer path offset-x offset-y color)
	; Draw a stroked path
	; Path is a list of path commands (moveTo, lineTo, arc, etc.)
	(when (and path (> (length path) 0))
		(defq first-cmd (elem 0 path))
		(defq start-x (+ offset-x (get first-cmd :x)))
		(defq start-y (+ offset-y (get first-cmd :y)))
		(defq current-x start-x)
		(defq current-y start-y)

		(each! 1 -1 (lambda (cmd)
			(defq cmd-type (get cmd :type))
			(cond
				((eql cmd-type :moveTo)
					(setq current-x (+ offset-x (get cmd :x)))
					(setq current-y (+ offset-y (get cmd :y))))

				((eql cmd-type :lineTo)
					(defq next-x (+ offset-x (get cmd :x)))
					(defq next-y (+ offset-y (get cmd :y)))
					(. renderer :draw-line current-x current-y next-x next-y color)
					(setq current-x next-x)
					(setq current-y next-y))

				((eql cmd-type :closePath)
					; Draw line back to start
					(. renderer :draw-line current-x current-y start-x start-y color))))
			path)))

(defun draw-filled-path (renderer path offset-x offset-y color)
	; Draw a filled path
	; For now, just draw the outline (proper polygon filling would require more work)
	(draw-path renderer path offset-x offset-y color))
