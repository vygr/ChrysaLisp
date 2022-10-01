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
			(. syntax :set_state :text)
			(.-> vdu
				(:change 0 0 w h)
				(:load (map (# (. syntax :colorise %0)) (cat '("") lines '(""))) 0 0 0 1000))
			(def (defq backdrop (Backdrop))
				:color +argb_grey1
				:min_width w
				:min_height h)
			(def (defq text (Text)) :text "")
			(. backdrop :add_child vdu)
			(.-> page (:add_child backdrop) (:add_child text))
			(clear lines)
			:text)
		(:t  (push lines (expand line 4))
			state)))

;module
(export-symbols handler)
(env-pop)
