;module
(env-push)

(defun flush-md (page lines)
	(when (nempty? lines)
		(def (defq md (Md (cat lines)))
			:zoom (get :zoom *window*)
			:page_width (get :min_width page)
			:font_doc +doc_font
			:font_bold +doc_font_bold
			:font_italic +doc_font_italic
			:font_bold_italic +doc_font_bold_italic
			:font_term +term_font
			:font_bullet +bullet_font)
		(. md :render)
		(. page :add_child md)
		(clear lines)))

(defun handler (state page line)
	; (handler state page line) -> state
	(defq lines '())
	(cond
		((match? line "^\\s*```")
			(flush-md page lines)
			;section start
			(if (eql (setq state (sym (cat ":" (slice line (rfind "`" line) -1)))) :) :code state))
		(:t (push lines line)
			state)))

;module
(export-symbols '(handler))
(env-pop)