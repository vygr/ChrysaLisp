;module
(env-push)

(defun line-words (line)
	(map (# (cat %0 " ")) (split line +char_class_space)))

(defun line-flow (&optional size)
	(def (defq flow (Flow)) :flow_flags +flow_right)
	(if size (def flow :font (create-font +doc_font size)))
	(. page :add_child flow)
	flow)

(defun heading (words size)
	(defq size (page-scale size) cnt 0 state :nil
		page_width (get :min_width page) flow (line-flow size))
	(each (lambda (word)
		(. flow :add_child (defq text (Text)))
		(push *search_widgets* text)
		(if (starts-with "`" word)
			(setq state :t word (rest word)))
		(if state (def text :ink_color +argb_blue))
		(if (find "`" word)
			(setq state :nil word (apply (const cat) (split word "`"))))
		(def text :text (intern-word word))
		(bind '(w _) (. text :pref_size))
		(when (> (setq cnt (+ cnt w)) page_width)
			(setq flow (line-flow size))
			(. flow :add_child text)
			(setq cnt w))) words))

(defun paragraph (words &optional size)
	(defq cnt 0 state :nil index 0 page_width (get :min_width page)
		flow (line-flow size))
	(when (and (> (length words) 0)
			(eql (first words) "* "))
		;needs a bullet and indent
		(def (defq text (Text))
			:text (cat (num-to-utf8 0xe95f) "    ")
			:font (create-font "fonts/Entypo.ctf" (page-scale 10)))
		(. flow :add_child text)
		(setq index 1 cnt (defq indent_width (first (. text :pref_size)))))
	(each! (lambda (word)
		(. flow :add_child (defq text (Text)))
		(push *search_widgets* text)
		(if (starts-with "`" word)
			(setq state :t word (rest word)))
		(if state (def text :ink_color +argb_blue))
		(if (find "`" word)
			(setq state :nil word (apply (const cat) (split word "`"))))
		(def text :text (intern-word word))
		(bind '(w _) (. text :pref_size))
		(when (> (setq cnt (+ cnt w)) page_width)
			(setq flow (line-flow size))
			(when (> index 0)
				;indent
				(def (defq indent (Backdrop)) :min_width indent_width)
				(. flow :add_child indent)
				(setq w (+ w indent_width)))
			(. flow :add_child text)
			(setq cnt w))) (list words) index))

(defun blank-line ()
	(def (defq text (Text)) :text "")
	(. page :add_child text))

(defun under-line (size)
	(def (defq backdrop (Backdrop)) :color +argb_black :min_height (max 1 (page-scale size)))
	(. page :add_child backdrop))

(defun handler (state page line)
	; (handler state page line) -> state
	(defq words '())
	(cond
		((starts-with "```" line)
			;section start
			(if (eql (setq state (sym (cat ":" (slice line 3 -1)))) :) :code state))
		(:t ;body text
			(cond
				((eql line "")
					;body paragraph end
					(when (> (length words) 0)
						(paragraph words)
						(blank-line)
						(clear words)))
				((starts-with "#### " line)
					(heading (line-words (slice line 5 -1)) 22)
					(blank-line))
				((starts-with "### " line)
					(heading (line-words (slice line 4 -1)) 26)
					(blank-line))
				((starts-with "## " line)
					(heading (line-words (slice line 3 -1)) 30)
					(under-line 2)
					(blank-line))
				((starts-with "# " line)
					(heading (line-words (slice line 2 -1)) 34)
					(under-line 4)
					(blank-line))
				(:t (each (lambda (word)
						(push words word)) (line-words line))))
			state)))

;module
(export-symbols '(handler))
(env-pop)
