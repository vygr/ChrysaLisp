;; HTML Browser Widget
;; A ChrysaLisp GUI widget for displaying HTML

(import "gui/canvas/lisp.inc")
(import "gui/scroll/lisp.inc")
(import "lib/html/parser.inc")
(import "lib/html/canvas_renderer.inc")

(defclass html-browser-view () (Canvas)
	; HTML browser view widget
	(def this :html_content "" :document nil :rendered_height 0 :scroll_y 0)

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
		(list 800 (max 600 height))))
