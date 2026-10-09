(report-header "GUI with no desktop: a view is laid out, marked as changed and asked for its glyphs on a node that has no GUI")

(import "usr/env.inc")
(import "gui/lisp.inc")

;A node with no GUI has views too: a test loads an app, a command lays out
;a page. Two things of a view stood on the GUI being there, and a node fell
;over, with no error, for want of them. The heap that the regions of views
;are cut from was made by the GUI as it started: it is made with the first
;view of a node, gui/view/class.vp. And a font made a texture of each glyph
;by a call to the driver: with no driver the pixels are left with no
;texture, gui/pixmap/class.vp.
;
;These are run where the tests are, a node that may or may not have a GUI.
;Each is right either way, what is tested is that it comes back

(assert-true "a font asked for the texture of a glyph gives one, or none where there is no GUI"
	(progn (font-sym-texture *env_editor_font* 'A) :t))

;a Vdu, the block of docs/gui/widgets.md that a node with no GUI fell over at
(ui-window nd_window (:min_width 0 :min_height 0)
	(ui-flow _ (:flow_flags +flow_stack_fill)
		(ui-vdu nd_vdu (:vdu_width 16 :vdu_height 4 :ink_color +argb_green :font *env_editor_font*))
		(ui-backdrop _ (:color +argb_grey1 :style :plain))))
(bind '(nd_w nd_h) (. nd_vdu :pref_size))
(assert-true "a Vdu of 16 by 4 says how big it wants to be" (and (> nd_w 16) (> nd_h 4)))
(assert-list-eq "which is its characters by the size of one" (list nd_w nd_h)
	(progn (bind '(cw ch) (. nd_vdu :char_size)) (list (* 16 cw) (* 4 ch))))
(. nd_vdu :change 0 0 nd_w nd_h)
(assert-true "given that size, it is loaded with lines and marked as changed"
	(Vdu? (. nd_vdu :load '("This is line 1." "This is line 2." "This is line 3." "This is line 4.") 0 0 0 0)))

;a window laid out, and every view of it marked as changed, moved and marked again
(bind '(nd_w nd_h) (. nd_window :pref_size))
(. nd_window :change 0 0 nd_w nd_h)
(assert-true "a window is laid out at the size it wants" (and (> nd_w 0) (> nd_h 0)))
(assert-true "it is marked as changed, all of it" (View? (. nd_window :dirty)))
(assert-true "and moved, which is what was there and what is there now"
	(View? (. nd_window :change_dirty 10 20 nd_w nd_h)))
(assert-true "each view of it has the place it was given"
	(every (# (bind '(w h) (. %0 :get_size)) (and (>= w 0) (>= h 0))) (. nd_window :flatten)))
;many views, made and let go, their regions go back to the heap
(times 200
	(ui-window nd_many () (ui-flow _ () (ui-button _ (:text "a")) (ui-button _ (:text "b"))))
	(bind '(w h) (. nd_many :pref_size))
	(.-> nd_many (:change 0 0 w h) :dirty))
(assert-true "two hundred windows made, laid out, marked and let go" :t)
