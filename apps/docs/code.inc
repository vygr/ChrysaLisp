;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(defq lines '())
	(cond
		((match? line "^\s*```")
			(def (defq vdu (Vdu))
				:font (get :font_term_small page)
				:vdu_width (max-length lines)
				:vdu_height (length lines)
				:color 0
				:ink_color +argb_blue)
			(bind '(w h) (. vdu :pref_size))
			(.-> vdu
				(:change 0 0 w h)
				(:load lines 0 0 0 1000))
			(def (defq text (Text)) :text "")
			(.-> page (:add_child vdu) (:add_child text))
			(clear lines)
			:text)
		(:t (push lines (expand line 4))
			state)))

;module
(export-symbols '(handler))
(env-pop)
