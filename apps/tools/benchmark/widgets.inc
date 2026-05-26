(enums +event 0
	(enum close))

(defq +scale_size 3)

(ui-window *window* ()
	(ui-title-bar *title* "Benchmark -> 0.000000 seconds" (0xea19) +event_close)
	(ui-grid *charts* (:grid_width 1)
		(ui-hchart *mean_chart* "Mean Time (us)" +scale_size (:units 1 :color +argb_yellow))
		(ui-hchart *best_chart* "Best Time (us)" +scale_size (:units 1 :color +argb_green))
		(ui-hchart *worst_chart* "Worst Time (us)" +scale_size (:units 1 :color +argb_red))))
