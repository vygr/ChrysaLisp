;;;;;;;;;;;
; md widget
;;;;;;;;;;;

(import "lib/consts/colors.inc")
(import "././flow/lisp.inc")
(import "././grid/lisp.inc")
(import "././text/lisp.inc")
(import "././backdrop/lisp.inc")
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
	+md_symbol_font "fonts/Symbol.ctf")

(defun md-font-name (f)
	(cond
		((str? f) f)
		(f (first (font-info f)))
		(:t +md_doc_font)))

(defun parse-text (line)
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

(defun parse-line (line)
	(bind '(tagged_text quoted) (parse-text line))
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

(defun create-word (word state search_widgets flow)
	(. flow :add_child (defq text (Text)))
	(push search_widgets text)
	(def text :text (sym (cat word " ")))
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

(defun under-line (this size)
	(when (and size (> size 0))
		(raise :zoom)
		(def (defq backdrop (Backdrop)) :color +argb_black :min_height (max 1 (md-scale size zoom)))
		(. this :add_child backdrop)))

(defun format-words (this container words indent style max_width search_widgets &optional underline font_size is_header)
	(defq cnt 0 state 0 indent_width :nil flow (line-flow this container font_size))
	(if is_header (setq state (logior state +state_bold)))
	(unless (and (eql indent "") (eql style ""))
		(defq text (Text))
		(if (find "*" style)
			(progn
				(defq indent_text (Text) style_text (Text))
				(def indent_text :text (cat indent indent))
				(def text :text (cat (num-to-utf8 0xe979) " ") :font (get :font_symbol flow))
				(def style_text :text (slice style (bskip +char_class_white_space style -1) -1))
				(.-> flow (:add_child indent_text) (:add_child text) (:add_child style_text)))
			(progn
				(def text :text (cat indent indent style))
				(. flow :add_child text)))
		(setq cnt (setq indent_width (first (. flow :pref_size)))))
	(each (lambda (word)
		(if (= (defq next_state (word-state word state)) -1)
			(progn
				(defq text (create-word word state search_widgets flow))
				(bind '(w &) (. text :pref_size))
				(when (> (++ cnt w) max_width)
					(setq flow (line-flow this container font_size))
					(when indent_width
						(def (defq indent_tab (Backdrop)) :min_width indent_width)
						(. flow :add_child indent_tab)
						(setq w (+ w indent_width)))
					(. flow :add_child text)
					(setq cnt w)))
			(setq state next_state))) words)
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
	(filter (const nempty?) (map (const trim) (split line "|"))))

(defun parse-align (cell)
	(defq c (trim cell))
	(cond
		((and (starts-with ":" c) (ends-with ":" c))
			(bit-mask +flow_flag_down +flow_flag_align_hcenter))
		((ends-with ":" c)
			(bit-mask +flow_flag_down +flow_flag_align_hright))
		(:t +flow_down)))

(defun render-row (this cells q_stack num_cols cell_width alignments search_widgets is_header)
	(def (defq row_grid (Grid))
		:grid_width num_cols :flow_flags +flow_down_fill)
	(. this :add_child row_grid)
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
			(raise :page_width
				(cell_width (/ page_width num_cols)
				q_stack (if (nempty? quoted) (reverse quoted))))
			(when (and has_sep (nempty? headers))
				(render-row this headers q_stack num_cols cell_width alignments search_widgets :t)
				(under-line this 1))
			(each (lambda (row)
				(render-row this row q_stack num_cols cell_width alignments search_widgets :nil))
				data_rows)
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
		(bind '(tagged_text quoted) (parse-text (join lines "\n")))
		(cond
			((try-render-table this tagged_text quoted search_widgets))
			(:t (defq words (restore-quotes (split tagged_text +char_class_white_space) quoted))
				(when (nempty? words)
					(paragraph this words indent style search_widgets))))
		(clear lines)))

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
					(paragraph this (parse-line h_text) "" "" search_widgets (- 4 (* indent 2)) (- 34 (* indent 4))))
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
		(lines! (# (push text_lines (trim-end %0 "\r"))) stream)
		(. this :render))
	)

;module
(export-classes '(Md))
(env-pop)