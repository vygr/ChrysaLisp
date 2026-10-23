;the vdu, its line numbers and sliders, of a text app's *edit*: the Editor's,
;the Viewer's and Hexview's. The app gives +margin and +refresh_mode first

(defun refresh-display ()
	;load the vdu widgets with the text, selection and line numbers
	(defq buffer (. *edit* :get_buffer))
	(bind '(cx cy ax ay) (. *edit* :get_cursor))
	(bind '(sx sy) (. *edit* :get_scroll))
	(defq lines (clear '()) start_line sy
		end_line (inc (min
			(second (. buffer :get_size))
			(+ start_line (get :vdu_height *edit*)))))
	(while (< (setq start_line (inc start_line)) end_line)
		(push lines (pad (str start_line) (const (dec +vdu_line_width)) "    ")))
	(. buffer :find (. *find_text* :get_text) *whole_words* *regexp* *ignore_case*)
	;the masks made again, the edit and all in it to be drawn again
	(.-> *edit* :underlay :dirty)
	(. *vdu_lines* :load lines 0 0 -1 -1)
	(. buffer :vdu_load (. *edit* :get_vdu_text) sx sy)
	;update status bar
	(each (# (def (. %0 :dirty) :text (str %1)))
		(list *cx* *cy* *sw* *sh* *fc*)
		(list (inc cx) (inc cy) (abs (- cx ax)) (abs (- cy ay)) (find-count))))

(defun refresh-sliders ()
	;set slider values for current file
	(defq meta (.-> *meta_map* (:find :files) (:find (str *current_file*))))
	(bind '(sx sy buffer) (gather meta :sx :sy :buffer))
	(bind '(w h) (. buffer :get_size))
	(bind '(vw vh) (.-> *edit* :get_vdu_text :vdu_size))
	(defq smaxx (max 0 (- w vw -1)) smaxy (max 0 (- h vh -1))
		sx (clamp sx 0 smaxx) sy (clamp sy 0 smaxy))
	(def (. *xslider* :dirty) :maximum smaxx :portion vw :value sx)
	(def (. *yslider* :dirty) :maximum smaxy :portion vh :value sy)
	(scatter meta :sx sx :sy sy)
	(. *edit* :set_scroll sx sy))

(defun refresh ()
	(when (<= (last *refresh_mode*) +refresh_mode_visible)
		;refresh display and ensure cursor is visible
		(defq meta (.-> *meta_map* (:find :files) (:find (str *current_file*))))
		(bind '(sx sy buffer) (gather meta :sx :sy :buffer))
		(bind '(cx cy &ignore) (. buffer :get_cursor))
		(bind '(w h) (.-> *edit* :get_vdu_text :vdu_size))
		(if (< (- cx +margin) sx) (setq sx (- cx +margin)))
		(if (< (- cy +margin) sy) (setq sy (- cy +margin)))
		(if (>= (+ cx +margin) (+ sx w)) (setq sx (- (+ cx +margin) w -1)))
		(if (>= (+ cy +margin) (+ sy h)) (setq sy (- (+ cy +margin) h -1)))
		(scatter meta :sx sx :sy sy)
		(refresh-sliders) (refresh-display)))

(defun window-resize ()
	;layout the window and size the vdu to fit
	(bind '(w h) (. *edit* :max_size))
	(set *edit* :vdu_width w :vdu_height h)
	(set *vdu_lines* :vdu_height h)
	(. *edit* :constrain)
	(. *vdu_lines* :constrain)
	(refresh-sliders) (refresh-display))

(defun vdu-resize (w h)
	;size the vdu and layout the window to fit
	(set *edit* :vdu_width w :vdu_height h :min_width w :min_height h)
	(set *vdu_lines* :vdu_height h :min_height h)
	(bind '(x y w h) (apply view-fit
		(cat (. *window* :get_pos) (. *window* :pref_size))))
	(set *edit* :min_width +vdu_min_width :min_height +vdu_min_height)
	(set *vdu_lines* :min_height +vdu_min_height)
	(. *window* :change_dirty x y w h)
	(window-resize))
