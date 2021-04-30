;vdu section handler

;module
(env-push)

(defun handler (state page line)
	; (handler state page line) -> state
	(defq lines '())
	(cond
		((starts-with "```" line)
			(def (defq vdu (Vdu))
				:font *env_terminal_font*
				:vdu_width 80
				:vdu_height (+ (length lines) 2)
				:color 0
				:ink_color +argb_black)
			(bind '(w h) (. vdu :pref_size))
			(. vdu :change 0 0 w h)
			(. coloriser :set_state :text)
			(. vdu :load (map (# (. coloriser :colorise %0)) (cat '("") lines '(""))) 0 0 0 1000)
			(def (defq backdrop (Backdrop))
				:color +argb_grey1
				:min_width w
				:min_height h)
			(. backdrop :add_child vdu)
			(. page :add_child backdrop)
			(clear lines)
			(def (defq text (Text)) :text "")
			(. page :add_child text)
			:text)
		(t	(push lines (. coloriser :expand_tabs line 4))
			state)))

;module
(export (penv)
	handler)
(env-pop)
