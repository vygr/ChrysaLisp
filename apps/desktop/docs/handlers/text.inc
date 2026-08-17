;module
(env-push)

(bits +state 0
	(bit code bold italic bold_italic highlight strike))

(defun parse-quotes (line)
	(defq quoted (list))
	(when (nempty? (defq idxs (nums) x 0 match (matches line "\q[^\q]+\q|`[^`]+`")))
		(each (lambda (((x1 x2)))
			(push quoted (split (cond
				((eql (elem-get line x1) "`")
					;alternate src1 (line) and src2 (seq2 indices 0 to -1)
					(push idxs x x1 0 -1) (slice line (inc x1) (dec x2)))
				(:t ;alternate src1 (line) and src2 (seq2 indices 4 to 9)
					(push idxs x x1 4 9) (slice line x1 x2)))))
			(setq x x2)) match)
		;one allocation, atomic scatter copy!
		(setq line (splice line " <c> <q> </c> " (push idxs x -1))))
	(list line quoted))

(defun parse-styles ((line quoted))
	;the formatter Ladder
	(defq idxs (nums) words (split (reduce! (lambda (line pattern tag c)
		(cond
			((nempty? (defq match (matches line pattern)))
				(defq idxs (clear idxs) x 0)
				(each (lambda (((x1 x2)))
					; src1: keep leading text (x to x1)
					; src2: insert open tag (0 to 5)
					; src1: keep inner text (x1+c to x2-c)
					; src2: insert close tag (4 to -1)
					(push idxs x x1 0 5 (+ x1 c) (- x2 c) 4 -1)
					(setq x x2)) match)
				;one allocation, atomic scatter copy!
				(splice line (cat " <" tag "> </" tag "> ") (push idxs x -1)))
			(line)))
		'(("~~[^~]+~~" "==[^=]+==" "\\*\\*\\*[^*]+\\*\\*\\*" "\\*\\*[^*]+\\*\\*" "\\*[^*]+\\*")
		"shxbi" (2 2 3 2 1)) line) +char_class_white_space))
	;splice quoted sections back in
	(cond
		((nempty? quoted)
			(reach (# (if (eql %0 "<q>") (elem-set words (!) (pop quoted)))) words)
			(flatten words))
		(words)))

(defun parse-line (line)
	(parse-styles (parse-quotes line)))

(defun word-state (word)
	(case word
		("<c>" (setq state (logior state +state_code)))
		("</c>" (setq state (logand state (lognot +state_code))))
		("<b>" (setq state (logior state +state_bold)))
		("</b>" (setq state (logand state (lognot +state_bold))))
		("<i>" (setq state (logior state +state_italic)))
		("</i>" (setq state (logand state (lognot +state_italic))))
		("<x>" (setq state (logior state +state_bold_italic)))
		("</x>" (setq state (logand state (lognot +state_bold_italic))))
		("<h>" (setq state (logior state +state_highlight)))
		("</h>" (setq state (logand state (lognot +state_highlight))))
		("<s>" (setq state (logior state +state_strike)))
		("</s>" (setq state (logand state (lognot +state_strike))))
		(:t -1)))

(defun create-word (word state)
	(. flow :add_child (defq text (Text)))
	(push *search_widgets* text)
	(def text :text (sym (cat word " ")))
	;font changes
	(if (bits? state +state_italic) (def text :font (get :font_italic flow)))
	(if (bits? state +state_bold) (def text :font (get :font_bold flow)))
	(if (bits? state +state_bold_italic) (def text :font (get :font_bold_italic flow)))
	;color overrides
	(if (bits? state +state_code) (def text :ink_color +argb_blue :font (get :font_term flow)))
	(if (bits? state +state_strike) (def text :ink_color +argb_grey8))
	(if (bits? state +state_highlight) (def text :color +argb_yellow :ink_color +argb_black))
	text)

(defun line-flow (container &optional size)
	(def (defq flow (Flow)) :flow_flags +flow_right)
	(. container :add_child flow)
	(when size
		(def flow :font (create-font +doc_font (page-scale size))
			:font_bold (create-font +doc_font_bold (page-scale size))
			:font_italic (create-font +doc_font_italic (page-scale size))
			:font_bold_italic (create-font +doc_font_bold_italic (page-scale size))
			:font_bullet (create-font +bullet_font (page-scale size))
			:font_term (create-font +term_font (page-scale size))))
	flow)

(defun blank-line ()
	(def (defq text (Text)) :text "")
	(. page :add_child text))

(defun under-line (size)
	(when (and size (> size 0))
		(def (defq backdrop (Backdrop)) :color +argb_black :min_height (max 1 (page-scale size)))
		(. page :add_child backdrop)))

(defun format-words (container words indent style max_width &optional underline font_size is_header)
	(defq cnt 0 state 0 indent_width :nil flow (line-flow container font_size))
	(if is_header (setq state (logior state +state_bold)))
	(unless (and (eql indent "") (eql style ""))
		;needs an indent or bullet style
		(defq text (Text))
		(if (find "*" style)
			(progn
				(defq indent_text (Text) style_text (Text))
				(def indent_text :text (cat indent indent))
				(def text :text (cat (num-to-utf8 0xe979) " ") :font (get :font_bullet flow))
				(def style_text :text (slice style (bskip +char_class_white_space style -1) -1))
				(.-> flow (:add_child indent_text) (:add_child text) (:add_child style_text)))
			(progn
				(def text :text (cat indent indent style))
				(. flow :add_child text)))
		(setq cnt (setq indent_width (first (. flow :pref_size)))))
	(each (lambda (word)
		(when (= (word-state word) -1)
			(defq text (create-word word state))
			(bind '(w &) (. text :pref_size))
			(when (> (++ cnt w) max_width)
				(setq flow (line-flow container font_size))
				(when indent_width
					(def (defq indent_tab (Backdrop)) :min_width indent_width)
					(. flow :add_child indent_tab)
					(setq w (+ w indent_width)))
				(. flow :add_child text)
				(setq cnt w)))) words)
	(if underline (under-line underline)))

(defun paragraph (words indent style &optional underline font_size)
	(format-words page words indent style (get :min_width page) underline font_size)
	(blank-line))

(defun separator-cell? (cell)
	(defq trimmed (trim cell " :"))
	(and (nempty? trimmed) (every (# (eql %0 "-")) trimmed)))

(defun table-separator? (cells)
	(and (nempty? cells) (every (const separator-cell?) cells)))

(defun split-table-row (line)
	(bind '(qline quoted) (parse-quotes line))
	(defq cells (filter (const nempty?) (map (const trim) (split qline "|"))))
	(list cells quoted))

(defun render-row (page (cells quoted) num_cols cell_width is_header)
	(def (defq row_grid (Grid))
		:grid_width num_cols :flow_flags +flow_down_fill)
	(. page :add_child row_grid)
	(defq q_idx 0)
	(for 0 num_cols
		(defq cell_text (if (< (!) (length cells)) (elem-get cells (!)) "")
			cell_flow (Flow) cell_q_cnt (length (substr cell_text "<q>"))
			cell_quoted (slice quoted q_idx (+ q_idx cell_q_cnt)))
		(setq q_idx (+ q_idx cell_q_cnt))
		(def cell_flow :flow_flags +flow_down)
		(. row_grid :add_child cell_flow)
		(format-words cell_flow (parse-styles (list cell_text cell_quoted)) "" "" cell_width :nil :nil is_header))
	row_grid)

(defun render-grid (page rows num_cols cell_width is_header)
	(def (defq grid (Grid)) :grid_width num_cols :flow_flags +flow_down_fill)
	(. page :add_child grid)
	(each (lambda (row) (render-row grid row num_cols cell_width is_header)) rows)
	grid)

(defun render-table (page lines)
	(defq raw_rows (filter (# (nempty? (first %0))) (map (const split-table-row) lines))
		has_sep :nil num_cols 0 rows (list) headers (list))
	(each! (lambda ((cells quoted))
		(cond
			((table-separator? cells)
				(setq has_sep :t)
				(when (nempty? rows)
					(setq headers (pop rows))))
			(:t (push rows (list cells quoted))
				(setq num_cols (max num_cols (length cells))))))
		(list raw_rows))
	(when (> num_cols 1)
		(defq page_width (get :min_width page)
			cell_width (/ page_width num_cols))
		(when (and has_sep (nempty? headers))
			(render-row page headers num_cols cell_width :t)
			(under-line 1))
		(each (lambda (row)
			(render-row page row num_cols cell_width :nil))
			rows)
		(blank-line)))

(defun table-lines? (lines)
	(defq rows (map (const split-table-row) lines))
	(or (some (# (table-separator? (first %0))) rows)
		(and (> (length lines) 1)
			(every (# (> (length (first %0)) 1)) rows))))

(defun handler (state page line)
	; (handler state page line) -> state
	(defq lines '())
	(cond
		((match? line "^\\s*```")
			;section start
			(if (eql (setq state (sym (cat ":" (slice line (rfind "`" line) -1)))) :) :code state))
		(:t ;body text
			(cond
				((eql line "")
					;body paragraph or table end
					(when (nempty? lines)
						(cond
							((table-lines? lines)
								(render-table page lines))
							(:t (defq line (first lines) style ""
									indent (slice line 0 (bskip +char_class_white_space line 0))
									match (matches line "^\\s*((\\p\\.)+\\s+|[*-]\\s+)"))
								(when (nempty? match)
									(bind '(((& cx2) (sx1 sx2) &ignore)) match)
									(setq style (slice line sx1 sx2) line (slice line cx2 -1)))
								(setq line (join (elem-set lines 0 line) " "))
								(when (nempty? (defq words (parse-line line)))
									(paragraph words indent style))))
						(clear lines)))
				((starts-with line "---")
					(under-line 1)
					(blank-line))
				((defq indent (some (# (if (starts-with %0 line) (!)))
						'("# " "## " "### " "#### ")))
					(setq line (slice line (+ indent 2) -1))
					(paragraph (parse-line line) "" "" (- 4 (* indent 2)) (- 34 (* indent 4))))
				((push lines line)))
			state)))

;module
(export-symbols '(handler))
(env-pop)