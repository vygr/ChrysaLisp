;a markdown document, its first lines set out as Docs would
(file-previewer
	(# (ends-with ".md" %0))
	(lambda (file most_w most_h)
		(when (defq lines (file-lines file 16))
			;on white paper, as a page is, not the grey of the side it is in
			(def (defq md (Md)) :page_width most_w :zoom 1.0 :base_font_size 11 :paper +argb_white :ink +argb_black)
			(. md :populate_lines lines)
			md)))
