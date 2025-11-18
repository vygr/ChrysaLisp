#!/usr/bin/env lsp

;; HTML Browser Application
;; Demonstrates HTML parsing, CSS styling, and graphical rendering

(import "././login/env.inc")
(import "gui/lisp.inc")
(import "service/clipboard/app.inc")
(import "lib/html/parser.inc")
(import "lib/html/css.inc")
(import "lib/html/canvas_renderer.inc")

(enums +event 0
	(enum close max min))

;; Sample HTML with CSS
(defq *sample_html* "
<!DOCTYPE html>
<html>
<head>
	<title>ChrysaLisp HTML Browser Demo</title>
</head>
<body>
	<h1>Welcome to the ChrysaLisp HTML Browser!</h1>

	<p>This is a <strong>fully functional</strong> HTML browser written entirely in Lisp.</p>

	<h2>Features</h2>
	<ul>
		<li>HTML parsing and DOM construction</li>
		<li>CSS styling support</li>
		<li>Graphical rendering</li>
		<li>All HTML elements supported</li>
	</ul>

	<h2>Styled Elements</h2>
	<p class=\"highlight\">This paragraph has CSS styling applied.</p>
	<p id=\"special\">This paragraph has a unique ID.</p>

	<h2>Links</h2>
	<p>Check out <a href=\"https://github.com/vygr/ChrysaLisp\">ChrysaLisp on GitHub</a>!</p>

	<h2>Tables</h2>
	<table>
		<tr>
			<th>Feature</th>
			<th>Status</th>
		</tr>
		<tr>
			<td>Parser</td>
			<td>Complete</td>
		</tr>
		<tr>
			<td>Renderer</td>
			<td>Complete</td>
		</tr>
		<tr>
			<td>CSS</td>
			<td>Complete</td>
		</tr>
	</table>

	<h2>Forms</h2>
	<form>
		<input type=\"text\" value=\"Enter text here\" />
		<button>Submit</button>
	</form>

	<hr>

	<p><em>Ported from KDE KHTML to ChrysaLisp</em></p>
</body>
</html>
")

;; Sample CSS
(defq *sample_css* "
h1 {
	color: #0066cc;
	font-size: 24px;
	font-weight: bold;
}

h2 {
	color: #006600;
	font-size: 18px;
}

.highlight {
	color: #ff0000;
	background-color: #ffff00;
}

#special {
	color: #800080;
	font-weight: bold;
}

p {
	color: #000000;
}

a {
	color: #0000ff;
}

table {
	border: 1px solid #000000;
}
")

;; Custom canvas view for HTML rendering
(defclass HTML-Canvas () (Canvas-base)
	(def this :html_doc nil :stylesheet nil :renderer nil :rendered_height 0 :current_file "" :base_path "")

	(defmethod :set_content (html css)
		; Set HTML and CSS content
		(set this :html_doc (parse-html html))
		(set this :stylesheet (parse-css css))
		(. this :render_content)
		this)

	(defmethod :load_file (filepath)
		; Load an HTML file from filesystem
		(defq full_path filepath)

		; Store current file and base path
		(set this :current_file full_path)
		(defq last_slash (find-rev "/" full_path))
		(if last_slash
			(set this :base_path (slice full_path 0 (inc last_slash)))
			(set this :base_path ""))

		; Read file and load content
		(when (path? full_path)
			(defq html (load full_path))
			(. this :set_content html "")
			:t))

	(defmethod :resolve_path (href)
		; Resolve relative path to absolute
		(cond
			; Absolute path
			((starts-with "/" href) href)

			; Relative path
			(:t (cat (get :base_path this) href))))

	(defmethod :navigate (href)
		; Navigate to a link
		(defq resolved_path (. this :resolve_path href))
		(. this :load_file resolved_path))

	(defmethod :handle_click (x y)
		; Handle mouse click at position
		(defq renderer (get :renderer this))
		(when renderer
			(defq href (. renderer :find-link-at x y))
			(when href
				(print "Navigating to: " href)
				(. this :navigate href)
				:t))
		nil)

	(defmethod :render_content ()
		; Render HTML with CSS to canvas
		(when (and (get :html_doc this) (getf this +canvas_pixmap 0))
			(bind '(w h) (. this :get_size))
			(defq renderer (html-canvas-renderer :init
				(get :html_doc this)
				this
				w
				(get :stylesheet this)))

			; Store renderer for click handling
			(set this :renderer renderer)

			; Render document
			(defq height (. renderer :render))
			(set this :rendered_height height)

			; Swap canvas to display
			(. this :swap 0)
			(. this :dirty))
		this)

	(defmethod :layout ()
		; Layout callback
		(. this :render_content)
		this)

	(defmethod :pref_size ()
		; Preferred size
		(list 800 (max 600 (get :rendered_height this)))))

;; Create UI
(ui-window *window* ()
	(ui-title-bar _ "HTML Browser" (0xea19 0xea1b 0xea1a) +event_close)
	(ui-scroll browser_scroll (+scroll_flag_vertical +scroll_flag_horz)
		(defq *canvas* (HTML-Canvas))))

;; Initialize canvas with size
(defq w 800 h 600)
(.-> *canvas*
	(:set_size w h)
	(:canvas_alloc 0 w h +argb_white 1))

;; Load initial HTML file or sample content
(defq *initial_file* "demo/html_pages/index.html")
(if (path? *initial_file*)
	(. *canvas* :load_file *initial_file*)
	(. *canvas* :set_content *sample_html* *sample_css*))

;; Layout window
(bind '(x y w h) (apply view-fit
	(cat (. *window* :get_pos) (. *window* :pref_size))))
(. *window* :change x y w h)

;; Event loop
(defun main ()
	; Open window
	(. *window* :connect_inks)

	; Event loop
	(defq mouse_down :nil)
	(while :t
		(defq msg (mail-read (task-mailbox)))
		(cond
			((= (getf msg +mail_timeout) 0)
				; Timeout - check for events
				(task-sleep 10000))

			((= (getf msg +ev_msg_type) +ev_type_mouse)
				(defq mx (getf msg +ev_msg_mouse_x))
				(defq my (getf msg +ev_msg_mouse_y))
				(defq buttons (getf msg +ev_msg_mouse_buttons))

				; Get canvas position
				(bind '(cx cy) (. *canvas* :get_pos))

				; Convert to canvas-relative coordinates
				(defq rel_x (- mx cx))
				(defq rel_y (- my cy))

				(cond
					; Mouse down - start selection or navigate
					((and (= buttons 1) (not mouse_down))
						(setq mouse_down :t)
						(. *canvas* :handle_mouse_down rel_x rel_y))

					; Mouse drag - update selection
					((and (= buttons 1) mouse_down)
						(. *canvas* :handle_mouse_drag rel_x rel_y))

					; Mouse up - finalize selection or click link
					((and (= buttons 0) mouse_down)
						(setq mouse_down :nil)
						(. *canvas* :handle_mouse_up rel_x rel_y)

						; If no selection was made, check for link click
						(when (= (length (. *canvas* :get_selected_text)) 0)
							(. *canvas* :handle_click rel_x rel_y)))))

			((= (getf msg +ev_msg_type) +ev_type_key)
				(defq keycode (getf msg +ev_msg_key_keycode))
				(defq key_mod (getf msg +ev_msg_key_key))

				(cond
					; Ctrl+C - Copy to clipboard
					((and (= keycode +char_c) (logand key_mod +ev_key_mod_control))
						(defq selected_text (. *canvas* :get_selected_text))
						(when (> (length selected_text) 0)
							(clip-put-rpc selected_text)
							(print "Copied to clipboard: " selected_text)))

					; Escape - Clear selection or close
					((= keycode +char_esc)
						(if (> (length (. *canvas* :get_selected_text)) 0)
							(. *canvas* :clear_selection)
							(break)))))

			((= (getf msg +ev_msg_target_id) +event_close)
				; Close window
				(break))))

	; Clean up
	(. *window* :hide))
