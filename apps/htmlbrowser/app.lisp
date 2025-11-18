#!/usr/bin/env lsp

;; HTML Browser Application
;; Demonstrates HTML parsing, CSS styling, and graphical rendering

(import "././login/env.inc")
(import "gui/lisp.inc")
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
	(def this :html_doc nil :stylesheet nil :rendered_height 0)

	(defmethod :set_content (html css)
		; Set HTML and CSS content
		(set this :html_doc (parse-html html))
		(set this :stylesheet (parse-css css))
		(. this :render_content)
		this)

	(defmethod :render_content ()
		; Render HTML with CSS to canvas
		(when (and (get :html_doc this) (getf this +canvas_pixmap 0))
			(bind '(w h) (. this :get_size))
			(defq renderer (html-canvas-renderer :init
				(get :html_doc this)
				this
				w
				(get :stylesheet this)))

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

;; Set HTML and CSS content
(. *canvas* :set_content *sample_html* *sample_css*)

;; Layout window
(bind '(x y w h) (apply view-fit
	(cat (. *window* :get_pos) (. *window* :pref_size))))
(. *window* :change x y w h)

;; Event loop
(defun main ()
	; Open window
	(. *window* :connect_inks)

	; Event loop
	(while :t
		(defq msg (mail-read (task-mailbox)))
		(cond
			((= (getf msg +mail_timeout) 0)
				; Timeout - check for events
				(task-sleep 10000))
			((or (= (getf msg +ev_msg_target_id) +event_close)
				 (and (= (getf msg +ev_msg_type) +ev_type_key)
					  (= (getf msg +ev_msg_key_keycode) +char_esc)))
				; Close window
				(break))))

	; Clean up
	(. *window* :hide))
