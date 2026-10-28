;;;;;;;;;;;;;;;;
; sheet widget
;;;;;;;;;;;;;;;;

(import "././scroll/lisp.inc")

(defclass Sheet () (Scroll +scroll_flag_vertical)
	; (Sheet) -> sheet
	;a scroll up and down that, each time it is laid out, a new size of
	;the window say, first makes what it scrolls fit it: the rows, :rows
	;if it is set, else what it holds, the width of it less its slider and
	;as long as they are, and what it holds, if the rows are in that, as
	;long as the rows or the sheet, whichever is longer

	(defmethod :fit ()
		; (. sheet :fit) -> sheet
		(when (defq child (get :child this))
			(bind '(w h) (. this :get_size))
			(bind '(sw &ignore) (. (get :vslider this) :get_constraint))
			(defq w (max 0 (- w sw)) rows (ifn (get :rows this) child)
				rh (second (. rows :pref_size)))
			(. rows :change_dirty 0 0 w rh :t)
			(unless (eql rows child) (. child :change_dirty 0 0 w (max rh h))))
		this)

	(defmethod :layout ()
		; (. sheet :layout) -> sheet
		(. this :fit)
		(.super this :layout))
	)
