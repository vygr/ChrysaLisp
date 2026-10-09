(report-header "Theme: which symbol font a desktop has, kept, and swapped in a window that is open")

(import "usr/env.inc")
(import "gui/lisp.inc")

;the themes, and the one a user has
(assert-true "there are themes, and the first is Regular" (eql (first (first *themes*)) "Regular"))
(assert-true "every theme's fonts are there"
	(every (lambda ((name symbols tiny)) (and (pii-fstat symbols) (pii-fstat tiny))) *themes*))
(assert-list-eq "the fonts of a theme" '("fonts/Symbols-Light.ctf" "fonts/Symbols.ctf") (theme-files "Light"))
(assert-list-eq "of one that is not, the first's" (theme-files "Regular") (theme-files "Nope"))
(defq th_home "tests/scratch/theme/")
(if (pii-fstat (cat th_home "theme")) (pii-remove (cat th_home "theme")))
(assert-eq "a user who never chose has the first" "Regular" (theme-current th_home))
(theme-save th_home "Sharp")
(assert-eq "one that is chosen is kept" "Sharp" (theme-current th_home))
(save "Nonsense" (cat th_home "theme"))
(assert-eq "a file that names no theme is the first" "Regular" (theme-current th_home))
(pii-remove (cat th_home "theme"))

;a window, with a bar of symbols, a label in a font of its own, and one in
;the font of text. It is not on a screen, a swap is of what it holds
(defq th_own (create-font "fonts/Symbols.ctf" 30))
(ui-window th_window ()
	(ui-title-bar th_title "Theme" (+sym_close) 0)
	(ui-tool-bar th_bar ()
		(ui-buttons (+sym_undo +sym_redo) 1))
	(ui-label th_mine (:text "x" :font th_own))
	(ui-label th_text (:text "words" :font *env_body_font*)))
(defq th_regular (theme-files "Regular") th_bold (theme-files "Bold")
	th_was *env_symbol_font* th_body *env_body_font*)
;the test has the theme of whoever runs it, start from one that is known
(. th_window :theme "Regular")
(assert-true "the bar has the symbol font of the theme"
	(eql (get :font th_bar) (create-font (first th_regular) 28)))
(. th_window :theme "Bold")
(assert-true "the theme is changed, and the bar has the new one's, the same size"
	(eql (get :font th_bar) (create-font (first th_bold) 28)))
(assert-true "the buttons of the title are a size of their own, and have that"
	(eql (get :font (penv th_title)) (create-font (first th_bold) 22)))
(assert-true "the task's own name for the font is the new one, for what is made next"
	(eql *env_symbol_font* (create-font (first th_bold) 28)))
(assert-true "a font the app made for itself is left" (eql (get :font th_mine) th_own))
(assert-true "and the font of text is" (eql (get :font th_text) th_body))
(. th_window :theme "Bold")
(assert-true "the same theme again changes nothing" (eql (get :font th_bar) (create-font (first th_bold) 28)))
(. th_window :theme "Sharp")
(assert-true "and another, straight from that" (eql (get :font th_bar) (create-font "fonts/Symbols-Sharp.ctf" 28)))
;as an event, as the GUI sends it to a window
(. th_window :event (cat (setf-> (str-alloc +ev_msg_theme_size)
	(+ev_msg_type +ev_type_theme) (+ev_msg_target_id (. th_window :get_id))) "Light"))
(assert-true "the event does it" (eql (get :font th_bar) (create-font "fonts/Symbols-Light.ctf" 28)))
(assert-true "the smallest symbols of the light theme are the regular font"
	(eql *env_tiny_symbol_font* (create-font "fonts/Symbols.ctf" 10)))
