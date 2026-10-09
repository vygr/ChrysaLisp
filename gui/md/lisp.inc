;;;;;;;;;;;
; md widget
;;;;;;;;;;;

(import "lib/consts/colors.inc")
(import "././flow/lisp.inc")
(import "././grid/lisp.inc")
(import "././text/lisp.inc")
(import "././backdrop/lisp.inc")
(import "././scroll/lisp.inc")
(import "././font/lisp.inc")

;module
(env-push)

(bits +state 0
	(bit code bold italic bold_italic highlight strike))

(defq
	+md_doc_font "fonts/OpenSans-Regular.ctf"
	+md_doc_font_bold "fonts/OpenSans-Bold.ctf"
	+md_doc_font_italic "fonts/OpenSans-Italic.ctf"
	+md_doc_font_bold_italic "fonts/OpenSans-BoldItalic.ctf"
	+md_terminal_font "fonts/Hack-Regular.ctf"
	+md_symbol_font "fonts/Symbols.ctf"
	+md_min_cell_width 96)

(defun md-font-name (f)
	(cond
		((str? f) f)
		(f (first (font-info f)))
		(:t +md_doc_font)))

(defun md-link-file (file target)
	; (md-link-file file target) -> path
	;the file a link of a document is to. It is from the folder the
	;document is in, unless it starts at the root with a /. Each .. is a
	;folder up, each . is no move, and what is after a # is a place in
	;the file, not part of its name
	(defq out (if (starts-with "/" target) (list) (most (split file "/"))))
	(each (lambda (part)
		(cond
			((eql part "."))
			((eql part "..") (if (nempty? out) (pop out)))
			((push out part))))
		(split (first (split (cat target " ") "#")) (const (char-class "/ "))))
	(join out "/"))

(defun link-text (line)
	; (link-text line) -> line
	;each link of the line, [text](target) or ![text](target), is made
	;the words " <l:target> text </l> ", for (format-words) to make the
	;text a Link. One with no text, no target, or a space in its target
	;is left as it is written
	(defq out (list) done 0)
	(each (lambda (((x1 x2)))
		(when (and (>= x1 done) (defq open (rfind "[" (slice line done x1)))
				(defq close (find ")" line x2)))
			(defq open (+ done open) text (slice line open x1) target (slice line x2 close)
				start (if (and (> open (inc done)) (eql (elem-get line (- open 2)) "!")) (- open 2) (dec open)))
			(when (and (nempty? text) (nempty? target) (not (find " " target)))
				(push out (slice line done start) " <l:" target "> " text " </l> ")
				(setq done (inc close)))))
		(substr line "]("))
	(if (empty? out) line (apply (const cat) (push out (slice line done -1)))))

(defun parse-text (line &optional links)
	;links, if the links of the text are to be made Links. They are not for
	;an Md that was given no :link_event, nothing would follow one, and it
	;is shown as it is written, with where it goes to be read
	(defq idxs (nums) quoted (list) match (matches line "\q[^\q]+\q|`[^`]+`"))
	(when (nempty? match)
		(defq x 0)
		(each (lambda (((x1 x2)))
			(push quoted (split (cond
				((eql (elem-get line x1) "`")
					(push idxs x x1 0 -1) (slice line (inc x1) (dec x2)))
				(:t (push idxs x x1 4 9) (slice line x1 x2)))))
			(setq x x2)) match)
		(setq line (splice line " <c> <q> </c> " (push idxs x -1))))
	(if links (setq line (link-text line)))
	(setq line (reduce! (lambda (line pattern tag c)
		(cond
			((nempty? (defq match (matches line pattern)))
				(defq idxs (clear idxs) x 0)
				(each (lambda (((x1 x2)))
					(push idxs x x1 0 5 (+ x1 c) (- x2 c) 4 -1)
					(setq x x2)) match)
				(splice line tag (push idxs x -1)))
			(line)))
		'(("~~[^~]+~~" "==[^=]+==" "\\*\\*\\*[^*]+\\*\\*\\*" "\\*\\*[^*]+\\*\\*" "\\*[^*]+\\*")
		(" <s> </s> " " <h> </h> " " <x> </x> " " <b> </b> " " <i> </i> ")
		(2 2 3 2 1)) line))
	(list line quoted))

(defun restore-quotes (words quoted)
	(when (nempty? quoted)
		(reach (# (if (eql %0 "<q>") (elem-set words (!) (pop quoted)))) words)
		(setq words (flatten words)))
	words)

(defun parse-line (line &optional links)
	(bind '(tagged_text quoted) (parse-text line links))
	(restore-quotes (split tagged_text +char_class_white_space) quoted))

(defun word-state (word state)
	(case word
		("<c>" (logior state +state_code))
		("</c>" (logand state (lognot +state_code)))
		("<b>" (logior state +state_bold))
		("</b>" (logand state (lognot +state_bold)))
		("<i>" (logior state +state_italic))
		("</i>" (logand state (lognot +state_italic)))
		("<x>" (logior state +state_bold_italic))
		("</x>" (logand state (lognot +state_bold_italic)))
		("<h>" (logior state +state_highlight))
		("</h>" (logand state (lognot +state_highlight)))
		("<s>" (logior state +state_strike))
		("</s>" (logand state (lognot +state_strike)))
		(:t -1)))

(defun create-word (word state search_widgets flow &optional tail link)
	;tail is what follows the word, a space, or nothing for a part of a
	;word that goes on in the next line. With a link, where it goes, the
	;word is a Link, and is connected to the :link_event of the Md, if
	;whoever made the Md gave it one
	(. flow :add_child (defq text (if link (Link) (Text))))
	(push search_widgets text)
	(def text :text (sym (cat word (ifn tail " "))))
	(when link
		(def text :link link :ink_color +argb_blue)
		(if (defq id (get :link_event flow)) (. text :connect id)))
	(cond
		((bits? state +state_bold_italic) (def text :font (get :font_bold_italic flow)))
		((bits? state +state_bold) (def text :font (get :font_bold flow)))
		((bits? state +state_italic) (def text :font (get :font_italic flow))))
	(if (bits? state +state_code) (def text :ink_color +argb_blue :font (get :font_terminal flow)))
	(if (bits? state +state_strike) (def text :ink_color +argb_grey8))
	(if (bits? state +state_highlight) (def text :color +argb_yellow :ink_color +argb_black))
	text)

(defun md-scale (s zoom)
	(n2i (* (n2f s) (n2f zoom))))

(defun line-flow (this container &optional size)
	(raise :zoom :font_doc (font (get :font this)))
	(if size (setq font (create-font (md-font-name font_doc) (md-scale size zoom))))
	(def (defq flow (Flow)) :flow_flags +flow_right :min_height (second (font-glyph-bounds font "")))
	(. container :add_child flow)
	(when size
		(raise :font_bold :font_italic :font_bold_italic :font_symbol :font_terminal)
		(def flow
			:font font
			:font_bold (create-font (md-font-name font_bold) (md-scale size zoom))
			:font_italic (create-font (md-font-name font_italic) (md-scale size zoom))
			:font_bold_italic (create-font (md-font-name font_bold_italic) (md-scale size zoom))
			:font_symbol (create-font (md-font-name font_symbol) (md-scale size zoom))
			:font_terminal (create-font (md-font-name font_terminal) (md-scale size zoom))))
	flow)

(defun blank-line (this)
	(def (defq text (Text)) :text "")
	(. this :add_child text))

(defun under-line (this size &optional container)
	(when (and size (> size 0))
		(raise :zoom)
		(def (defq backdrop (Backdrop)) :color +argb_black :min_height (max 1 (md-scale size zoom)))
		(. (ifn container this) :add_child backdrop)))

(defun word-parts (word font room)
	; (word-parts word font room) -> (part ...)
	;a word too long for a line, a file path or a link, as the parts that
	;do fit. It is cut after a / _ - . or ) where it can be, and where a
	;run between two of those is still too long, after any character
	(defq atoms (list) atom "")
	(each (lambda (c)
		(setq atom (cat atom c))
		(when (find c "/_-.)")
			(push atoms atom)
			(setq atom ""))) word)
	(if (nempty? atom) (push atoms atom))
	(defq out (list) part "")
	(each (lambda (atom)
		(each (lambda (atom)
				(when (and (nempty? part) (> (first (font-glyph-bounds font (cat part atom))) room))
					(push out part)
					(setq part ""))
				(setq part (cat part atom)))
			(if (> (first (font-glyph-bounds font atom)) room) (map (const identity) atom) (list atom))))
		atoms)
	(if (nempty? part) (push out part))
	out)

(defun format-words (this container words indent style max_width search_widgets &optional underline font_size is_header)
	(defq cnt 0 state 0 link :nil link_end :nil indent_width :nil flow (line-flow this container font_size))
	(if is_header (setq state (logior state +state_bold)))
	(unless (and (eql indent "") (eql style ""))
		(defq text (Text))
		(if (find "*" style)
			(progn
				(defq indent_text (Text) style_text (Text))
				(def indent_text :text (cat indent indent))
				(def text :text (cat (num-to-utf8 +sym_small) " ") :font (get :font_symbol flow))
				(def style_text :text (slice style (bskip +char_class_white_space style -1) -1))
				(.-> flow (:add_child indent_text) (:add_child text) (:add_child style_text)))
			(progn
				(def text :text (cat indent indent style))
				(. flow :add_child text)))
		(setq cnt (setq indent_width (first (. flow :pref_size)))))
	(each (lambda (word)
		(cond
			((starts-with "<l:" word) (setq link (slice word 3 -2)))
			((eql word "</l>") (setq link :nil link_end (last search_widgets)))
			((= (defq next_state (word-state word state)) -1)
				;a full stop or a comma straight after a link sits against it
				(if (and link_end (Link? link_end) (find (first word) ",.;:!?)"))
					(def link_end :text (sym (trim-end (str (get :text link_end))))))
				(setq link_end :nil)
				(defq text (create-word word state search_widgets flow :nil link)
					room (- max_width (ifn indent_width 0)))
				(bind '(w &) (. text :pref_size))
				(defq texts (list text))
				(when (and (> w room) (> (length word) 1))
					;it would not fit a line of its own, so it is cut into
					;parts that do, each a line, the last with what follows it
					(defq parts (word-parts word (get :font text) (- room (- w (first (font-glyph-bounds (get :font text) word))))))
					(when (> (length parts) 1)
						(. text :sub)
						(pop search_widgets)
						(setq texts (map (# (create-word %0 state search_widgets flow
							(if (= (!) (dec (length parts))) " " "") link)) parts))))
				(each (lambda (text)
					(bind '(w &) (. text :pref_size))
					(when (> (++ cnt w) max_width)
						(setq flow (line-flow this container font_size))
						(when indent_width
							(def (defq indent_tab (Backdrop)) :min_width indent_width :color 0)
							(. flow :add_child indent_tab)
							(setq w (+ w indent_width)))
						(. flow :add_child text)
						(setq cnt w))) texts))
			(:t (setq state next_state)))) words)
	(if underline (under-line this underline)))

(defun paragraph (this words indent style search_widgets &optional underline font_size is_header)
	(raise :page_width)
	(format-words this this words indent style page_width search_widgets underline font_size is_header)
	(blank-line this))

(defun separator-cell? (cell)
	(defq trimmed (trim cell " :"))
	(and (nempty? trimmed) (every (# (eql %0 "-")) trimmed)))

(defun table-separator? (cells)
	(and (nempty? cells) (every (const separator-cell?) cells)))

(defun split-cells (line)
	;the cells of a row. One with nothing in it is a cell, it keeps what
	;comes after it in its own column
	(map (const trim) (split (trim line) "|")))

(defun parse-align (cell)
	(defq c (trim cell))
	(cond
		((and (starts-with ":" c) (ends-with ":" c))
			(bit-mask +flow_flag_down +flow_flag_align_hcenter))
		((ends-with ":" c)
			(bit-mask +flow_flag_down +flow_flag_align_hright))
		(:t +flow_down)))

(defun render-row (this container cells q_stack num_cols cell_width alignments search_widgets is_header)
	(def (defq row_grid (Grid))
		:grid_width num_cols :flow_flags +flow_down_fill)
	(. container :add_child row_grid)
	(for 0 num_cols
		(defq cell_text (if (< (!) (length cells)) (elem-get cells (!)) "")
			flags (if (< (!) (length alignments)) (elem-get alignments (!)) +flow_down)
			cell_flow (Flow)
			words (split cell_text +char_class_white_space))
		(when (nempty? q_stack)
			(each (# (if (eql %0 "<q>") (elem-set words (!) (pop q_stack)))) words)
			(setq words (flatten words)))
		(def cell_flow :flow_flags flags)
		(. row_grid :add_child cell_flow)
		(format-words this cell_flow words "" "" cell_width search_widgets :nil :nil is_header))
	row_grid)

(defun try-render-table (this tagged_text quoted search_widgets)
	(when (find "|" tagged_text)
		(defq qlines (split tagged_text "\n")
			raw_rows (filter (const nempty?) (map (const split-cells) qlines))
			has_sep :nil num_cols 0 data_rows (list) headers (list) alignments (list))
		(each! (lambda (cells)
			(cond
				((table-separator? cells)
					(setq has_sep :t
						alignments (map (const parse-align) cells))
					(when (nempty? data_rows)
						(setq headers (pop data_rows))))
				(:t (push data_rows cells)
					(setq num_cols (max num_cols (length cells))))))
			(list raw_rows))
		(when (and (> num_cols 1)
				(or has_sep (and (> (length raw_rows) 1) (every (# (> (length %0) 1)) raw_rows))))
			;a column is its share of the page, and no narrower than a few
			;words. A table of so many columns that it is then wider than
			;the page is put in a scroll, as a wide block of code is
			(raise :page_width :zoom
				(cell_width (max (/ page_width num_cols) (md-scale +md_min_cell_width zoom))
				q_stack (if (nempty? quoted) (reverse quoted))
				table (Flow)))
			(def table :flow_flags +flow_down_fill)
			(. this :add_child table)
			(when (and has_sep (nempty? headers))
				(render-row this table headers q_stack num_cols cell_width alignments search_widgets :t)
				(under-line this 1 table))
			(each (lambda (row)
				(render-row this table row q_stack num_cols cell_width alignments search_widgets :nil))
				data_rows)
			(bind '(tw th) (. table :pref_size))
			(when (> tw page_width)
				(. table :sub)
				(def (defq scroll (Scroll +scroll_flag_horizontal))
					:min_width page_width :min_height th)
				(. scroll :add_child (. table :change 0 0 tw th))
				(. this :add_child scroll))
			(blank-line this)
			:t)))

(defun flush-lines (this lines search_widgets)
	(when (nempty? lines)
		(when (nempty? (defq first_line (first lines) style ""
				indent (slice first_line 0 (bskip +char_class_white_space first_line 0))
				match (matches first_line "^\\s*((\\p\\.)+\\s+|[*-]\\s+)")))
			(bind '(((& cx2) (sx1 sx2) &ignore)) match)
			(setq style (slice first_line sx1 sx2))
			(elem-set lines 0 (slice first_line cx2 -1)))
		(bind '(tagged_text quoted) (parse-text (join lines "\n") (get :link_event this)))
		(cond
			((try-render-table this tagged_text quoted search_widgets))
			(:t (defq words (restore-quotes (split tagged_text +char_class_white_space) quoted))
				(when (nempty? words)
					(paragraph this words indent style search_widgets))))
		(clear lines)))

(defclass Link () (Text)
	; (Link) -> link
	;text that goes somewhere, :link is where. Pressed, it emits, to
	;whatever it was connected to, and the action has its id, from which
	;the one who gets it finds the Link and reads where

	(defmethod :draw ()
		; (. link :draw) -> link
		(.super this :draw)
		(bind '(w h) (. this :get_size))
		(.-> this (:ctx_set_color (get :ink_color this)) (:ctx_filled_box 0 (- h 2) w 1)))

	(defmethod :mouse_up (event)
		; (. link :mouse_up event) -> link
		(bind '(w h) (. this :get_size))
		(defq rx (getf event +ev_msg_mouse_rx) ry (getf event +ev_msg_mouse_ry))
		(if (and (>= rx 0) (>= ry 0) (< rx w) (< ry h)) (. this :emit))
		this)
	)

(defclass Md (&optional text_lines) (Flow)
	; (Md [text_lines]) -> md
	(def this
		:flow_flags +flow_down_fill
		:text_lines (ifn text_lines (list))
		:zoom 1.0
		:page_width 640
		:base_font_size 18
		:search_widgets (list)
		:font_doc +md_doc_font
		:font_bold +md_doc_font_bold
		:font_italic +md_doc_font_italic
		:font_bold_italic +md_doc_font_bold_italic
		:font_terminal +md_terminal_font
		:font_symbol +md_symbol_font)

	(defgetmethod :text_lines)
	(defsetmethod :text_lines)
	(defgetmethod :zoom)
	(defsetmethod :zoom)
	(defgetmethod :page_width)
	(defsetmethod :page_width)
	(defgetmethod :search_widgets)

	(defmethod :render ()
		; (. md :render) -> md
		(each (# (. %0 :sub)) (. this :children))
		(raise :zoom :base_font_size :font_doc :font_bold :font_italic
			:font_bold_italic :font_terminal :font_symbol :page_width :text_lines
			(search_widgets (list)
			base_size (md-scale base_font_size zoom)
			bullet_size (md-scale (/ (* base_font_size 4) 3) zoom)))
		(def this
			:search_widgets search_widgets
			:min_width page_width
			:font (create-font (md-font-name font_doc) base_size)
			:font_bold (create-font (md-font-name font_bold) base_size)
			:font_italic (create-font (md-font-name font_italic) base_size)
			:font_bold_italic (create-font (md-font-name font_bold_italic) base_size)
			:font_terminal (create-font (md-font-name font_terminal) base_size)
			:font_symbol (create-font (md-font-name font_symbol) bullet_size))
		(defq lines (list))
		(each (lambda (line)
			(task-slice)
			(cond
				((eql line "")
					(flush-lines this lines search_widgets))
				((starts-with line "---")
					(flush-lines this lines search_widgets)
					(under-line this 1)
					(blank-line this))
				((defq indent (some (# (if (starts-with %0 line) (!)))
						'("# " "## " "### " "#### " "##### " "###### ")))
					(flush-lines this lines search_widgets)
					(defq h_text (slice line (+ indent 2) -1))
					(paragraph this (parse-line h_text (get :link_event this)) "" "" search_widgets (- 4 (* indent 2)) (- 34 (* indent 4))))
				((push lines line)))) text_lines)
		(flush-lines this lines search_widgets)
		this)

	(defmethod :populate_lines (lines)
		; (. md :populate_lines lines) -> md
		(def this :text_lines (cat lines))
		(. this :render))

	(defmethod :populate_buffer (buffer)
		; (. md :populate_buffer buffer) -> md
		(. this :populate_lines (. buffer :get_buffer_lines)))

	(defmethod :populate_stream (stream)
		; (. md :populate_stream stream) -> md
		(defq text_lines (list))
		(lower :text_lines)
		(lines! (# (push text_lines (trim-end %0 "\r")) :nil) stream)
		(. this :render))
	)

;module
(export-symbols '(md-link-file))
(export-classes '(Md Link))
(env-pop)