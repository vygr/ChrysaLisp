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

;a new theme tells the app, as a window sized by hand does: what is in the window
;may have a new size, and a vdu has to count its columns and rows again
(defq th_told 0)
(defclass Th-window () (Window)
	(defmethod :emit ()
		(setq th_told (inc th_told))
		this))
(ui-root th_tw (Th-window) (:font :font_window :ink :ink_window :paper :paper_window
		:border :border_window :shadow :shadow_window)
	(ui-flow _ (:flow_flags +flow_down_fill)))
(window-attach th_tw)
(. th_tw :theme "Light")
(assert-eq "a new theme tells the app" 1 th_told)

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
;a value given by hand is the view's own, a new theme or user leaves it,
;and given its variant again it finds it again
(def th_plain :border 9)
(th-borders 4 6)
(view-resolve th_w)
(assert-eq "a value given by hand stays" 9 (get :border th_plain))
(assert-eq "one beside it not given one finds it again" 6 (get :border th_in_bar))
(th-borders 8 6)
(view-resolve th_w)
(assert-eq "and stays as the user changes" 9 (get :border th_plain))
(view-variant th_plain :border :border_button)
(th-borders 1 6)
(view-resolve th_w)
(assert-eq "given its variant again, it is the user's" 1 (get :border th_plain))
;the forms: each says what it is, a bar one of them or each on or off, a
;sheet as a scroll is, and a view given nothing, or a default erased, has no more
(ui-window th_f_w ()
	(ui-radio-bar th_radio ("a" "b"))
	(ui-toggle-bar th_toggle ("a" "b"))
	(ui-sheet th_sheet (:min_width 10)
		(ui-flow th_in_sheet ()))
	(ui-flow th_bare ()))
(assert-eq "a radio bar is one of them" :nil (get :mode th_radio))
(assert-eq "a toggle bar is each on or off" :t (get :mode th_toggle))
(assert-eq "the two have the one look" (get :paper th_radio) (get :paper th_toggle))
(assert-true "a sheet is a Sheet" (Sheet? th_sheet))
(assert-eq "with a scroll's paper" (get :paper_slider *env*) (def? :paper th_sheet))
(assert-eq "and what it was given" 10 (def? :min_width th_sheet))
(assert-true "what is in it is" (find th_in_sheet (. th_sheet :children)))
(assert-eq "a form given nothing sets nothing" :nil (def? :flow_flags th_bare))
;a look a widget needs is the user's or a form's, it has no default of its own.
;With nothing above it to find one, a Text says so, it does not take up no room
(defq th_alone (Text))
(def th_alone :text "words")
(assert-error "a Text with no font and nothing above it is a fault" (. th_alone :pref_size))
(defq th_quiet (Text))
(assert-list-eq "and one with no text has nothing to measure" '(0 0) (. th_quiet :pref_size))
;a default erased, it finds what is above it
(ui-window th_x ()
	(ui-flow th_f (:border 4)
		(ui-button th_e (:text "z" :border :erase))))
(assert-eq "a default erased, it finds what is above it" 4 (get :border th_e))
(th-borders th_b_was th_t_was)

;a font an app makes for itself, by its zoom, is the app's. A new theme leaves it
(bind '(th_ef th_es) (font-info (get :font_editor *env*)))
(ui-root th_zw (Window) (:font :font_window :ink :ink_window :paper :paper_window
		:border :border_window :shadow :shadow_window)
	(ui-flow _ (:flow_flags +flow_down_fill)
		(ui-element th_zv (Vdu) (:vdu_width 8 :vdu_height 2 :ink +argb_white
			:font (get :font_editor *env*)))))
(window-attach th_zw)
(def th_zv :font (defq th_zf (create-font th_ef (+ th_es 4))))
(. th_zw :theme "Bold")
(assert-eq "a new theme leaves an app's own font" th_zf (get :font th_zv))
(. th_zw :theme "Light")
(assert-eq "and the next" th_zf (get :font th_zv))
