(report-header "GUI with no desktop: a view is laid out, marked as changed and asked for its glyphs on a node that has no GUI")

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/debug/loops.inc")

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
	(progn (font-sym-texture (get :font_editor *env*) 'A) :t))

;a Vdu, the block of docs/gui/widgets.md that a node with no GUI fell over at
(ui-window nd_window (:min_width 0 :min_height 0)
	(ui-flow _ (:flow_flags +flow_stack_fill)
		(ui-vdu nd_vdu (:vdu_width 16 :vdu_height 4 :ink +argb_green :font :font_editor))
		(ui-backdrop _ (:paper +argb_grey1 :style :plain))))
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

;a text field whose text is set to a shorter one, and is then clicked past the end of what it showed
(ui-window nd_form () (ui-textfield nd_field (:clear_text "1024x768" :min_width 120)))
(bind '(w h) (. nd_form :pref_size))
(. nd_form :change 0 0 w h)
(. nd_field :set_text "557x415")
(defq nd_click (setf-> (str-alloc +ev_msg_mouse_size) (+ev_msg_type +ev_type_mouse) (+ev_msg_mouse_rx 110) (+ev_msg_mouse_ry 8) (+ev_msg_mouse_buttons 1)))
(assert-eq "a text field given a shorter text, and clicked where the end of the longer was, does not throw" :ok
	(catch (progn (. nd_field :mouse_down nd_click) (. nd_field :mouse_move nd_click) (. nd_field :mouse_up nd_click) :ok) :threw))
(assert-true "and its cursor is in the text it has" (<= (get :cursor nd_field) 7))

;a window says how much of itself is solid each time it is laid out. One with
;no shadow is all solid, and has the flag that says so. Given a shadow again
;it is not, what is behind its shadow shows, and the flag has to go: it was
;left set, and a window that had been to no shadow and back left the old
;picture in its edge
(defun nd-solid? (window) (/= 0 (logand (. window :get_flags) +view_flag_opaque)))
(ui-window nd_win2 (:min_width 0 :min_height 0 :paper 0xffc0c0c0)
	(ui-backdrop _ (:paper +argb_grey1 :style :plain :min_width 100 :min_height 60)))
(bind '(nd_w nd_h) (. nd_win2 :pref_size))
(. nd_win2 :change 0 0 nd_w nd_h)
(defq nd_shadow (get :shadow nd_win2))
(assert-true "a window has a shadow" (> nd_shadow 0))
(assert-true "with it, it is not all solid" (not (nd-solid? nd_win2)))
(def nd_win2 :shadow 0)
(. nd_win2 :layout)
(assert-true "with none, it is" (nd-solid? nd_win2))
(def nd_win2 :shadow nd_shadow)
(. nd_win2 :layout)
(assert-true "with its shadow back it is not all solid again" (not (nd-solid? nd_win2)))
(def nd_win2 :shadow 0)
(. nd_win2 :layout)
(def nd_win2 :paper 0x80c0c0c0)
(. nd_win2 :layout)
(assert-true "nor is one of a colour that can be seen through, shadow or none" (not (nd-solid? nd_win2)))

;a property of a view that holds itself or a view above it is a loop: the
;tree is never let go of, canvases and all, and stays in the node's memory
(defq nd_top (View) nd_mid (View) nd_low (View))
(.-> nd_top (:add_child nd_mid))
(. nd_mid :add_child nd_low)
(def nd_low :n 3 :sibling (View))
(assert-eq "a tree whose properties hold nothing above them has no loops" 0 (length (view-loops nd_top)))
(def nd_low :scroll nd_top :list (list 1 nd_mid))
(assert-eq "one that holds a view above it is a loop, by its key, and one in a list by the list and where in it"
	(str (list (list :nil 1) (list :t :scroll)))
	(str (sort (map (lambda ((v k)) (list (eql v nd_low) k)) (view-loops nd_top)) (lambda (a b) (cmp (str (second a)) (str (second b)))))))
