;text section handler

;module
(env-push)

(defun words (line)
	(map (# (cat %0 " ")) (split line " ")))

(defun heading (line size)
	(def (defq flow (Flow))
		:flow_flags +flow_right_fill
		:font (create-font "fonts/OpenSans-Regular.ctf" size))
	(each (lambda (word)
		(def (defq text (Text)) :text word)
		(. flow :add_child text)) (words line))
	(. page :add_child flow)
	(def (defq text (Text)) :text "")
	(. page :add_child text))

(defun body (lines)
	(defq body_words '() cnt 0 state nil index 0 page_width (get :min_width page))
	(each (lambda (line)
		(each (lambda (word)
			(push body_words word)) (words line))) lines)
	(def (defq flow (Flow)) :flow_flags +flow_right_fill)
	(. page :add_child flow)
	(when (and (> (length body_words) 0)
			(eql (elem 0 body_words) "* "))
		;needs a bullet
		(def (defq text (Text))
			:text (cat " " (num-to-utf8 0xe979) " ") 
			:font (create-font "fonts/Entypo.ctf" 24))
		(. flow :add_child text)
		(setq index 1 cnt (elem 0 (. text :pref_size))))
	(each! index -1 (lambda (word)
		(. flow :add_child (defq text (Text)))
		(if (starts-with "`" word)
			(setq state t word (slice 1 -1 word)))
		(if state (def text :ink_color +argb_blue))
		(if (find "`" word)
			(setq state nil word (apply cat (split word "`"))))
		(def text :text word)
		(bind '(w _) (. text :pref_size))
		(when (> (setq cnt (+ cnt w)) page_width)
			(setq cnt w flow (Flow))
			(def flow :flow_flags +flow_right_fill)
			(. page :add_child flow))
		(. flow :add_child text)) (list body_words))
	(def (defq text (Text)) :text "")
	(. page :add_child text)
	(clear body_words))

(defun handler (state page line)
	; (handler state page line) -> state
	(defq lines '())
	(cond
		((starts-with "```" line)
			;section start
			(if (eql (setq state (sym (cat ":" (slice 3 -1 line)))) :) :lisp state))
		(t	;body text
			(cond
				((eql line "")
					;body paragraph end
					(when (> (length lines) 0)
						(body lines)
						(clear lines)))
				((starts-with "#### " line)
					(heading (slice 5 -1 line) 22))
				((starts-with "### " line)
					(heading (slice 4 -1 line) 26))
				((starts-with "## " line)
					(heading (slice 3 -1 line) 30))
				((starts-with "# " line)
					(heading (slice 2 -1 line) 34))
				(t	(push lines line)))
			state)))

;module
(export (penv)
	handler)
(env-pop)
