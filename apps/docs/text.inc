;module
(env-push)

(bits +state 0
	(bit code bold italic))

(defun parse-style (line pattern tag c)
	(cond
		((nempty? (defq match (matches line pattern)))
			(defq seqs (list) x 0)
			(each (lambda (((x1 x2)))
				(push seqs (slice line x x1) " <" tag "> "
					(slice line (+ x1 c) (- x2 c)) " </" tag "> ")
				(setq x x2)) match)
			(apply (const cat) (push seqs (slice line x -1))))
		(line)))

(defun parse-line (line)
	(split (reduce! (const parse-style)
		'(("`[^`]+`" "\*\*[^*]+\*\*" "\*[^*]+\*") "cbi" (1 2 1)) line) +char_class_space))

(defun word-state (word)
	(case word
		("<c>" (setq state (logior state +state_code)))
		("</c>" (setq state (logand state (lognot +state_code))))
		("<b>" (setq state (logior state +state_bold)))
		("</b>" (setq state (logand state (lognot +state_bold))))
		("<i>" (setq state (logior state +state_italic)))
		("</i>" (setq state (logand state (lognot +state_italic))))
		(:t -1)))

(defun create-word (word state)
	(. flow :add_child (defq text (Text)))
	(push *search_widgets* text)
	(def text :text (intern-word (cat word " ")))
	(if (/= 0 (logand state +state_italic)) (def text :font *doc_font_italic*))
	(if (/= 0 (logand state +state_bold)) (def text :font *doc_font_bold*))
	(when (/= 0 (logand state +state_code)) (def text :ink_color +argb_blue :font *doc_term_font*))
	text)

(defun line-flow (&optional size)
	(def (defq flow (Flow)) :flow_flags +flow_right)
	(if size (def flow :font (create-font +doc_font size)))
	(. page :add_child flow)
	flow)

(defun heading (words size)
	(defq size (page-scale size) cnt 0 state 0
		page_width (get :min_width page) flow (line-flow size))
	(each (lambda (word) (cond
		((/= (word-state word) -1))
		(:t (defq text (create-word word state))
			(bind '(w _) (. text :pref_size))
			(when (> (setq cnt (+ cnt w)) page_width)
				(setq flow (line-flow size))
				(. flow :add_child text)
				(setq cnt w))))) words))

(defun paragraph (words indent)
	(defq cnt 0 state 0 page_width (get :min_width page) flow (line-flow))
	(when (> indent 0)
		;needs a bullet and indent
		(def (defq text (Text))
			:text (pad (cat (num-to-utf8 0xe95f) "    ") (* 24 (dec indent)))
			:font *doc_symbol_font*)
		(. flow :add_child text)
		(setq cnt (defq indent_width (first (. text :pref_size)))))
	(each (lambda (word) (cond
		((/= (word-state word) -1))
		(:t	(defq text (create-word word state))
			(bind '(w _) (. text :pref_size))
			(when (> (setq cnt (+ cnt w)) page_width)
				(setq flow (line-flow))
				(when (> indent 0)
					(def (defq indent_tab (Backdrop)) :min_width indent_width)
					(. flow :add_child indent_tab)
					(setq w (+ w indent_width)))
				(. flow :add_child text)
				(setq cnt w))))) words))

(defun blank-line ()
	(def (defq text (Text)) :text "")
	(. page :add_child text))

(defun under-line (size)
	(def (defq backdrop (Backdrop)) :color +argb_black :min_height (max 1 (page-scale size)))
	(. page :add_child backdrop))

(defun handler (state page line)
	; (handler state page line) -> state
	(defq lines '())
	(cond
		((starts-with "```" line)
			;section start
			(if (eql (setq state (sym (cat ":" (slice line 3 -1)))) :) :code state))
		(:t ;body text
			(cond
				((eql line "")
					;body paragraph end
					(when (nempty? lines)
						(defq line (first lines) indent 0)
						(cond
							((starts-with "* " line)
								(setq line (slice line 2 -1) indent 1))
							((starts-with "    * " line)
								(setq line (slice line 6 -1) indent 2))
							((starts-with "        * " line)
								(setq line (slice line 10 -1) indent 3))
							((starts-with "            * " line)
								(setq line (slice line 14 -1) indent 4))
							((starts-with "                * " line)
								(setq line (slice line 18 -1) indent 5)))
						(defq line (apply (const cat) (join (elem-set lines 0 line) '(" "))))
						(defq words (parse-line line))
						(when (nempty? words)
							(paragraph words indent)
							(blank-line)
							(clear lines))))
				((starts-with "#### " line)
					(heading (parse-line (slice line 5 -1)) 22)
					(blank-line))
				((starts-with "### " line)
					(heading (parse-line (slice line 4 -1)) 26)
					(blank-line))
				((starts-with "## " line)
					(heading (parse-line (slice line 3 -1)) 30)
					(under-line 2)
					(blank-line))
				((starts-with "# " line)
					(heading (parse-line (slice line 2 -1)) 34)
					(under-line 4)
					(blank-line))
				(:t (push lines line)))
			state)))

;module
(export-symbols '(handler))
(env-pop)
