;module
(env-push)

(bits +state 0
	(bit code bold italic bold_italic highlight strike))

(defun parse-line (line)
	;pre-process code blocks and quotes
	(when (nempty? (defq quoted (list) idxs (nums) x 0
			match (matches line "\q[^\q]+\q|`[^`]+`")))
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
	;the formatter Ladder
	(defq words (split (reduce! (lambda (line pattern tag c)
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
		"shxbi" (2 2 3 2 1)) line)))
	;splice quoted sections back in
	(cond
		((nempty? quoted)
			(reach (# (if (eql %0 "<q>") (elem-set words (!) (pop quoted)))) words)
			(flatten words))
		(words)))

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
	(def text :text (. *page_words* :intern (cat word " ")))
	;font changes
	(if (bits? state +state_italic) (def text :font (get :font_italic flow)))
	(if (bits? state +state_bold) (def text :font (get :font_bold flow)))
	(if (bits? state +state_bold_italic) (def text :font (get :font_bold_italic flow)))
	;color overrides
	(if (bits? state +state_code) (def text :ink_color +argb_blue :font (get :font_term flow)))
	(if (bits? state +state_strike) (def text :ink_color +argb_grey8))
	(if (bits? state +state_highlight) (def text :color +argb_yellow :ink_color +argb_black))
	text)

(defun line-flow (&optional size)
	(def (defq flow (Flow)) :flow_flags +flow_right)
	(. page :add_child flow)
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

(defun paragraph (words indent style &optional underline font_size)
	(defq cnt 0 state 0 page_width (get :min_width page)
		indent_width :nil flow (line-flow font_size))
	(unless (and (eql indent "") (eql style ""))
		;needs an indent or bullet style
		(defq text (Text))
		(if (find "*" style)
			(def text :text (cat indent indent (num-to-utf8 0xe979) " ") :font (get :font_bullet flow))
			(def text :text (cat indent indent style)))
		(. flow :add_child text)
		(setq cnt (setq indent_width (first (. text :pref_size)))))
	(each (lambda (word)
		(when (= (word-state word) -1)
			(defq text (create-word word state))
			(bind '(w &) (. text :pref_size))
			(when (> (++ cnt w) page_width)
				(setq flow (line-flow font_size))
				(when indent_width
					(def (defq indent_tab (Backdrop)) :min_width indent_width)
					(. flow :add_child indent_tab)
					(setq w (+ w indent_width)))
				(. flow :add_child text)
				(setq cnt w)))) words)
	(under-line underline)
	(blank-line))

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
					;body paragraph end
					(when (nempty? lines)
						(defq line (first lines) style ""
							indent (slice line 0 (bskip +char_class_space line 0))
							match (matches line "^\\s*((\\p\\.)+\\s+|[*-]\\s+)"))
						(when (nempty? match)
							(bind '(((& cx2) (sx1 sx2) &ignore)) match)
							(setq style (slice line sx1 sx2) line (slice line cx2 -1)))
						(setq line (join (elem-set lines 0 line) " "))
						(when (nempty? (defq words (parse-line line)))
							(paragraph words indent style))
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
