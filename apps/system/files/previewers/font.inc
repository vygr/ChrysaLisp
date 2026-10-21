;a font, a sheet of its first glyphs, eight to a row as the Fonts app has
;them, at a size that fits the width. A font of the host's, (create-font)
;reads a file, so not an object of the store
(file-previewer
	(lambda (file) (and (ends-with ".ctf" file) (not (name-in-store file))))
	(lambda (file most_w most_h)
		(when (defq font (create-font file (max 8 (/ most_w 11))))
			(defq ranges (reverse (font-glyph-ranges font)) codes (list) sheet (Grid))
			(while (and (< (length codes) 48) (defq s (pop ranges) e (pop ranges)))
				(setq s (max s 33))
				(while (and (<= s e) (< (length codes) 48))
					(push codes s)
					(setq s (inc s))))
			(when (nempty? codes)
				(each (lambda (c)
						(def (defq label (Label)) :text (num-to-utf8 c) :font font :border 0
							:ink +argb_black :paper +argb_white :flow_flags +flow_flag_align_hcenter)
						(. sheet :add_child label))
					codes)
				(def sheet :grid_width 8 :grid_height (/ (+ (length codes) 7) 8) :paper +argb_white)
				(def sheet :min_width most_w)
				sheet))))
