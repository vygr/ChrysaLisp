;the first lines of a text, as they are written, cut to the width: any
;file with no 0 bytes in its start, so asked only after all those for a
;kind
(file-previewer
	(# (/= (age %0) 0))
	(lambda (file most_w most_h)
		(when (defq lines (file-lines file 24))
			(defq font (create-font "fonts/Hack-Regular.ctf" 10) view (Flow))
			(def view :flow_flags +flow_down_fill :min_width most_w)
			(each (lambda (line)
					;a tab as the editor has it, to the next of each 4
					(defq out "")
					(each (# (setq out (if (eql %0 (ascii-char 9))
						(cat out (pad "" (- 4 (% (length out) 4))))
						(cat out %0)))) line)
					;cut to what fits, its glyphs are not all of a width
					(defq n (min (length out) 160))
					(while (and (> n 0) (> (first (font-glyph-bounds font (slice out 0 n))) most_w))
						(setq n (dec n)))
					(def (defq label (Label)) :text (slice out 0 n) :font font
						:ink +argb_black :paper +argb_white)
					(. view :add_child label))
				lines)
			view))
	:t)
