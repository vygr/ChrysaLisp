(report-header "Theme: which symbol font a desktop has, kept, and swapped in a window that is open")

(import "usr/env.inc")
(import "gui/lisp.inc")

;the themes, and the one a user has
(assert-true "there are themes, and the first is Light" (eql (first (first *themes*)) "Light"))
(assert-true "every theme's font is there"
	(every (lambda ((name symbols)) (pii-fstat symbols)) *themes*))
(assert-eq "the font of a theme" "fonts/Symbols-Light.ctf" (theme-file "Light"))
(assert-eq "of one that is not, the first's" (theme-file "Light") (theme-file "Nope"))
(defq th_home "tests/scratch/theme/")
(if (pii-fstat (cat th_home "theme")) (pii-remove (cat th_home "theme")))
(assert-eq "a user who never chose has the first" "Light" (theme-current th_home))
(theme-save th_home "Sharp")
(assert-eq "one that is chosen is kept" "Sharp" (theme-current th_home))
(save "Nonsense" (cat th_home "theme"))
(assert-eq "a file that names no theme is the first" "Light" (theme-current th_home))
(pii-remove (cat th_home "theme"))

;a window, with a bar of symbols, a label in a font of its own, and one in
;the font of text. It is not on a screen, a swap is of what it holds
(defq th_own (create-font "fonts/Symbols.ctf" 30))
(ui-window th_window ()
	(ui-title-bar th_title "Theme" (+sym_close) 0)
	(ui-tool-bar th_bar ()
		(ui-buttons (+sym_undo +sym_redo) 1))
	(ui-label th_mine (:text "x" :font th_own))
	(ui-label th_text (:text "words" :font :font_body)))
(defq th_regular (theme-file "Regular") th_bold (theme-file "Bold")
	th_was (get :font_symbol *env*) th_body (get :font_body *env*))
;the test has the theme of whoever runs it, start from one that is known
(. th_window :theme "Regular")
(assert-true "the bar has the symbol font of the theme"
	(eql (get :font th_bar) (create-font th_regular 28)))
(. th_window :theme "Bold")
(assert-true "the theme is changed, and the bar has the new one's, the same size"
	(eql (get :font th_bar) (create-font th_bold 28)))
(assert-true "the buttons of the title are a size of their own, and have that"
	(eql (get :font (penv th_title)) (create-font th_bold 22)))
(assert-true "the task's own name for the font is the new one, for what is made next"
	(eql (get :font_symbol *env*) (create-font th_bold 28)))
(assert-true "a font the app made for itself is left" (eql (get :font th_mine) th_own))
(assert-true "and the font of text is" (eql (get :font th_text) th_body))
(. th_window :theme "Bold")
(assert-true "the same theme again changes nothing" (eql (get :font th_bar) (create-font th_bold 28)))
(. th_window :theme "Sharp")
(assert-true "and another, straight from that" (eql (get :font th_bar) (create-font "fonts/Symbols-Sharp.ctf" 28)))
;as an event, as the GUI sends it to a window
(. th_window :event (cat (setf-> (str-alloc +ev_msg_theme_size)
	(+ev_msg_type +ev_type_action) (+ev_msg_target_id +event_theme)
	(+ev_msg_action_source_id (. th_window :get_id))) "Light"))
(assert-true "the event does it" (eql (get :font th_bar) (create-font "fonts/Symbols-Light.ctf" 28)))
(assert-true "the smallest symbols are the bold font, whatever the theme"
	(eql (get :font_tiny_symbol *env*) (create-font "fonts/Symbols-Bold.ctf" 10)))

;a composite says what a role is for those in it. A button names :border_button,
;its title bar says that is :border_title_buttons, and a button of a bar finds
;that from where it is, one beside it what the user has. A value no other has, so
;it is seen where it went, and as a new one is had when it is found again
(defq th_b_was (get :border_button *env*) th_t_was (get :border_title_buttons *env*))
(defun th-borders (button title)
	;the user's, in *env* for what is made, on the root for what finds it
	(def *env* :border_button button :border_title_buttons title)
	(def (env-root) :border_button button :border_title_buttons title))
(th-borders 2 7)
(ui-window th_w ()
	(ui-title-bar th_tb "x" (+sym_close) +event_close)
	(ui-button th_plain (:text "y")))
(defq th_in_bar (ui-find th_w :type "Button"))
(assert-eq "a button of a title bar has what the bar says it is" 7 (get :border th_in_bar))
(assert-eq "and one beside it what the user has" 2 (get :border th_plain))
(assert-eq "the bar has no border of its own to hand down" :nil (def? :border (penv th_in_bar)))
(th-borders 3 5)
(view-resolve th_w)
(assert-eq "changed, it is found again from where it is, the bar first" 5 (get :border th_in_bar))
(assert-eq "and the one beside it" 3 (get :border th_plain))
;a default erased, it finds what is above it
(ui-window th_x ()
	(ui-flow th_f (:border 4)
		(ui-button th_e (:text "z" :border :erase))))
(assert-eq "a default erased, it finds what is above it" 4 (get :border th_e))
(th-borders th_b_was th_t_was)
