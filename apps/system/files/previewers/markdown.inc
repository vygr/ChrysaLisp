;a markdown document, its first lines set out as Docs would
(file-previewer
	(# (ends-with ".md" %0))
	(lambda (file most_w most_h)
		(when (defq lines (file-lines file 16))
			;on white paper, as a page is, not the grey of the side it is in: a
			;flow draws no paper, so a backdrop behind it does, as Todo's lists
			(def (defq md (Md)) :page_width most_w :zoom 1.0 :base_font_size 11 :ink +argb_black)
			(def (defq page (Flow)) :flow_flags +flow_stack_fill)
			(def (defq paper (Backdrop)) :paper +argb_white)
			(.-> page (:add_child md) (:add_child paper))
			(. md :populate_lines lines)
			page)))
