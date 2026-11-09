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
(assert-list-eq "configured, its chars are the size it is asked for, or none are drawn"
	'(16 4) (getf-> nd_vdu (+vdu_width 0) (+vdu_height 0)))
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

;an app with no dispatch-action of its own has its actions called, and one that
;fails is told of and the app goes on
(defq nd_acted 0 *event_map* (scatter (Fmap)
		7001 (lambda () (setq nd_acted (inc nd_acted)))
		7002 (lambda () (setq nd_acted (inc nd_acted)) (throw "this action fails, as it is meant to" 0)))
	nd_event (# (setf-> (str-alloc +ev_msg_mouse_size) (+ev_msg_type +ev_type_action) (+ev_msg_target_id %0))))
(assert-eq "an action is called, and is an event dealt with" :t (. nd_form :dispatch (nd_event 7001)))
(assert-eq "it was called" 1 nd_acted)
(assert-eq "one that fails is dealt with too" :t (. nd_form :dispatch (nd_event 7002)))
(assert-eq "it was called, and here we are" 2 nd_acted)
(assert-eq "no action of that id is not" :nil (. nd_form :dispatch (nd_event 7003)))

;a view says the least it may be and what it would like. A window opens at what is
;wanted, and may be dragged down to the least
(ui-window nd_ww (:min_width 10)
	(ui-backdrop nd_wv (:min_width 32 :min_height 32 :want_width 400 :want_height 300))
	(ui-vdu nd_wd (:vdu_width 8 :vdu_height 2 :want_width 20)))
(bind '(nd_pw nd_ph) (. nd_ww :pref_size))
(bind '(nd_ww2 nd_wh) (. nd_ww :want_size))
(assert-true "what is wanted is more than the least" (and (> nd_ww2 nd_pw) (> nd_wh nd_ph)))
(assert-eq "the least a view may be is as it was" 32 (get :min_width nd_wv))
(assert-eq "one that had no least has none" :nil (def? :min_width nd_wd))
(assert-list-eq "the least is measured as before" (list nd_pw nd_ph) (. nd_ww :pref_size))
(. nd_ww :want_size)
(assert-list-eq "and measuring what is wanted leaves it so" (list nd_pw nd_ph) (. nd_ww :get_constraint))
(bind '(nd_cw nd_ch) (. nd_wd :char_size))
(def nd_wv :want_width 0 :want_height 0)
(def nd_wd :want_width 40)
(bind '(nd_a _) (. nd_ww :want_size))
(def nd_wd :want_width 41)
(bind '(nd_b _) (. nd_ww :want_size))
(assert-eq "a vdu wants characters, not pixels" nd_cw (- nd_b nd_a))

;a canvas has no paper, whatever is above it, as it is made by a form or not
(ui-window nd_cw (:paper +argb_white) (ui-canvas nd_c 8 8 1))
(assert-eq "a canvas has no paper" 0 (get :paper nd_c))

;a label given a new text fits it to it, the same text is nothing to do
(defq nd_laid 0)
(defclass Nd-label () (Label)
	(defmethod :layout ()
		(setq nd_laid (inc nd_laid))
		(.super this :layout)))
(ui-window nd_said () (ui-element nd_label (Nd-label) (:text "short" :min_width 300
			:flow_flags (bit-mask +flow_flag_right +flow_flag_align_vcenter))))
(bind '(w h) (. nd_said :pref_size))
(. nd_said :change 0 0 w h)
(bind '(nd_w _) (. (get :label_text nd_label) :get_size))
(setq nd_laid 0)
(assert-eq "a label's new text gives the label back" nd_label (. nd_label :set_text "a good deal longer than it was"))
(assert-eq "it has the text" "a good deal longer than it was" (get :text nd_label))
(assert-eq "it is laid out for it" 1 nd_laid)
(bind '(nd_w2 _) (. (get :label_text nd_label) :get_size))
(assert-true "and the text has the room" (> nd_w2 nd_w))
(assert-eq "the same text gives the label back" nd_label (. nd_label :set_text "a good deal longer than it was"))
(assert-eq "and is not laid out again" 1 nd_laid)

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

;a view taken out of its parent comes back with the reference the parent
;had, and one taken out again, in no parent, does nothing: it is held as
;many times after as before. It was let go of each time, and a canvas and
;its texture were freed twice when a scroll took out a child its app had
(defq nd_parent (View) nd_child (View))
(. nd_parent :add_child nd_child)
(defq nd_held (getf nd_child +obj_count 0))
(. nd_child :sub)
(assert-eq "taken out, held one less, by this alone" (dec nd_held) (getf nd_child +obj_count 0))
(times 3 (. nd_child :sub))
(assert-eq "and taken out again and again, held the same" (dec nd_held) (getf nd_child +obj_count 0))
(undef (env) 'nd_parent 'nd_child 'nd_held)
;a scroll sizes what it scrolls and is sized by it, as it is given: of a
;percentage of its child, the least it may be on a way it has no slider,
;what it would like on a way it has one
(defq nd_sv (Scroll +scroll_flag_vertical) nd_sc (Backdrop))
(def nd_sv :paper 0)
(def nd_sc :min_width 300 :min_height 900 :paper 0)
(def nd_sv :fit_width 100 :fit_height 50)
(. nd_sv :add_child nd_sc)
(assert-list-eq "given its child, a scroll up and down is at least as wide as it, and would like half its height"
	'(300 450) (list (def? :min_width nd_sv) (def? :want_height nd_sv)))
(assert-list-eq "and has no least height, nor wants a width" '(:nil :nil) (list (def? :min_height nd_sv) (def? :want_width nd_sv)))
(assert-list-eq "what it scrolls is its own size" '(300 900) (. nd_sc :get_size))
(defq nd_sb (Scroll +scroll_flag_both) nd_sbc (Backdrop))
(def nd_sb :paper 0)
(def nd_sbc :min_width 400 :min_height 100 :paper 0)
(def nd_sb :fit_width 75)
(. nd_sb :add_child nd_sbc)
(assert-list-eq "a scroll both ways would like three quarters of its child's width, and is not held to it"
	'(300 :nil) (list (def? :want_width nd_sb) (def? :min_width nd_sb)))
(def nd_sb :fit_min :t)
(. nd_sb :fit)
(assert-eq "unless told its fit is the least it may be, as one that does not fill its flow is" 300 (def? :min_width nd_sb))
;what it scrolls fills it, at least the view less its slider, and the
;scroll is sized by what it scrolls alone, not by what it fills
(defq nd_sf (Scroll +scroll_flag_vertical) nd_sfc (Backdrop))
(def nd_sf :paper 0)
(def nd_sfc :min_width 100 :min_height 50 :paper 0)
(def nd_sf :child_fill +scroll_flag_horizontal :fit_width 100)
(. nd_sf :add_child nd_sfc)
(. nd_sf :change 0 0 400 300)
(bind '(nd_vw _) (. nd_sf :view_size))
(assert-true "the view of a scroll is it less its slider" (< 0 nd_vw 400))
(assert-list-eq "what fills it is as wide as the view and its own height" (list nd_vw 50) (. nd_sfc :get_size))
(assert-eq "and the scroll's least width is still its child's own" 100 (def? :min_width nd_sf))
(def nd_sfc :min_width 1000)
(. nd_sf :place)
(assert-list-eq "scrolled, nothing is sized" (list nd_vw 50) (. nd_sfc :get_size))
(. nd_sf :layout)
(assert-list-eq "laid out, what is wider than the view is its own width" '(1000 50) (. nd_sfc :get_size))
(undef (env) 'nd_sv 'nd_sc 'nd_sb 'nd_sbc 'nd_sf 'nd_sfc 'nd_vw)
