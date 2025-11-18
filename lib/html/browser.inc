;; HTML Browser Widget
;; A ChrysaLisp GUI widget for displaying HTML

(import "gui/canvas/lisp.inc")
(import "gui/scroll/lisp.inc")
(import "lib/html/parser.inc")
(import "lib/html/canvas_renderer.inc")

(defclass html-browser-view () (Canvas)
	; HTML browser view widget
	(def this :html_content "" :document nil :renderer nil :rendered_height 0 :scroll_y 0 :current_file "" :base_path "")

	(defmethod :set_html (html)
		; Set HTML content and render
		(set this :html_content html)
		(set this :document (parse-html html))
		(. this :render_html)
		this)

	(defmethod :get_document ()
		; Get the parsed document
		(get :document this))

	(defmethod :render_html ()
		; Render HTML to canvas
		(defq doc (get :document this))
		(when (and doc (getf this +canvas_pixmap 0))
			; Create renderer
			(bind '(w h) (. this :get_size))
			(defq renderer (html-canvas-renderer :init doc this w))

			; Store renderer for click handling
			(set this :renderer renderer)

			; Render document
			(defq height (. renderer :render))
			(set this :rendered_height height)

			; Mark as dirty to redraw
			(. this :dirty))
		this)

	(defmethod :draw ()
		; Draw the canvas
		((const (super)) this)
		this)

	(defmethod :layout ()
		; Layout the view
		(. this :render_html)
		this)

	(defmethod :pref_size ()
		; Preferred size
		(defq height (get :rendered_height this))
		(list 800 (max 600 height)))

	(defmethod :load_file (filepath)
		; Load an HTML file from the filesystem
		(defq full_path filepath)

		; Store current file and base path
		(set this :current_file full_path)
		(defq last_slash (find-rev "/" full_path))
		(if last_slash
			(set this :base_path (slice full_path 0 (inc last_slash)))
			(set this :base_path ""))

		; Read file content
		(when (path? full_path)
			(defq html (load full_path))
			(. this :set_html html)
			:t))

	(defmethod :resolve_path (href)
		; Resolve a relative path to absolute
		(cond
			; Absolute path
			((starts-with "/" href) href)

			; Relative path
			(:t (cat (get :base_path this) href))))

	(defmethod :navigate (href)
		; Navigate to a link (local file only)
		(defq resolved_path (. this :resolve_path href))
		(. this :load_file resolved_path))

	(defmethod :handle_click (x y)
		; Handle mouse click at position (x, y)
		(defq renderer (get :renderer this))
		(when renderer
			(defq href (. renderer :find-link-at x y))
			(when href
				(. this :navigate href)
				:t))
		nil)

	(defmethod :handle_mouse_down (x y)
		; Handle mouse down - start selection
		(defq renderer (get :renderer this))
		(when renderer
			; Clear previous selection
			(. renderer :clear-selection)

			; Start new selection
			(. renderer :set-selection-start x y)
			(set this :is_selecting :t))
		this)

	(defmethod :handle_mouse_drag (x y)
		; Handle mouse drag - update selection
		(when (get :is_selecting this)
			(defq renderer (get :renderer this))
			(when renderer
				(. renderer :set-selection-end x y)
				(. this :render_html)))  ; Re-render to show selection
		this)

	(defmethod :handle_mouse_up (x y)
		; Handle mouse up - finalize selection
		(when (get :is_selecting this)
			(set this :is_selecting :nil)
			(defq renderer (get :renderer this))
			(when renderer
				(. renderer :set-selection-end x y)
				(. this :render_html)))  ; Final render with selection
		this)

	(defmethod :get_selected_text ()
		; Get currently selected text
		(defq renderer (get :renderer this))
		(if renderer
			(. renderer :get-selected-text)
			""))

	(defmethod :clear_selection ()
		; Clear text selection
		(defq renderer (get :renderer this))
		(when renderer
			(. renderer :clear-selection)
			(. this :render_html))
		this))
