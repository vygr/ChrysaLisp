(import "././login/env.inc")
(import "gui/lisp.inc")
(import "lib/plot/plot.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Plot Demo Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enums +select 0
	(enum main timer))

(enums +event 0
	(enum close))

(defq +plot_width 800 +plot_height 600
	  +rate (/ 1000000 15))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Generate Sample Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-line-data ()
	; Generate sine wave data
	(map (lambda (x)
		(defq xv (/ x 10.0))
		(list xv (sin xv)))
		(range 0 63)))

(defun generate-scatter-data ()
	; Generate random scatter data
	(map (lambda (_)
		(list (+ 1.0 (* 5.0 (random))) (+ 1.0 (* 5.0 (random)))))
		(range 0 50)))

(defun generate-bar-data ()
	; Generate bar chart data
	(map (lambda (x)
		(list x (* 10.0 (+ 5.0 (* 5.0 (sin (/ x 2.0)))))))
		(range 0 10)))

(defun generate-area-data ()
	; Generate area plot data
	(map (lambda (x)
		(defq xv (/ x 5.0))
		(list xv (+ 5.0 (* 3.0 (sin xv)) (* 2.0 (cos (* xv 0.5))))))
		(range 0 31)))

(defun generate-pie-data ()
	; Generate pie chart data
	(list
		(list "Red" 30.0)
		(list "Green" 20.0)
		(list "Blue" 25.0)
		(list "Yellow" 15.0)
		(list "Magenta" 10.0)))

(defun generate-histogram-data ()
	; Generate random data for histogram
	(map (lambda (_)
		; Generate normally distributed random values
		(+ 5.0 (* 2.0 (- (+ (random) (random) (random) (random)) 2.0))))
		(range 0 200)))

(defun generate-hbar-data ()
	; Generate horizontal bar chart data
	(list
		(list 25.0 1.0)
		(list 45.0 2.0)
		(list 30.0 3.0)
		(list 60.0 4.0)
		(list 15.0 5.0)))

(defun generate-error-bar-data ()
	; Generate data with error bars
	(map (lambda (x)
		(defq xv (/ x 5.0)
			  y (+ 5.0 (* 3.0 (sin xv)))
			  err (* 0.5 (+ 1.0 (random))))
		(list xv y err))
		(range 0 20)))

(defun generate-box-plot-data ()
	; Generate data for box plots
	(list
		(list 1.0 (map (lambda (_) (+ 5.0 (* 2.0 (- (+ (random) (random)) 1.0)))) (range 0 50)))
		(list 2.0 (map (lambda (_) (+ 7.0 (* 1.5 (- (+ (random) (random)) 1.0)))) (range 0 50)))
		(list 3.0 (map (lambda (_) (+ 6.0 (* 2.5 (- (+ (random) (random)) 1.0)))) (range 0 50)))
		(list 4.0 (map (lambda (_) (+ 8.0 (* 1.0 (- (+ (random) (random)) 1.0)))) (range 0 50)))))

(defun generate-stacked-bar-data ()
	; Generate stacked bar chart data
	(list
		(list 1.0 (list 10.0 15.0 8.0))
		(list 2.0 (list 12.0 18.0 10.0))
		(list 3.0 (list 8.0 20.0 12.0))
		(list 4.0 (list 15.0 14.0 9.0))
		(list 5.0 (list 11.0 17.0 11.0))))

(defun generate-heatmap-data ()
	; Generate 2D heatmap data
	(map (lambda (row)
		(map (lambda (col)
			(+ (* 10.0 (sin (/ row 2.0))) (* 5.0 (cos (/ col 2.0)))))
			(range 0 20)))
		(range 0 15)))

(defun generate-candlestick-data ()
	; Generate candlestick chart data
	(defq data (list) price 100.0)
	(each (lambda (i)
		(defq open price
			  change (* 5.0 (- (random) 0.5))
			  close (+ open change)
			  high (+ (max open close) (* 2.0 (random)))
			  low (- (min open close) (* 2.0 (random))))
		(push data (list i open high low close))
		(setq price close))
		(range 0 30))
	data)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Create Plots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun create-line-plot ()
	; Create a line plot
	(defq plot (Plot +plot_width +plot_height "Sine Wave - Line Plot"))
	(def plot :x_label "X" :y_label "sin(X)")

	; Add multiple series
	(plot-add-series plot :line (generate-line-data) "sin(x)")
	(plot-add-series plot :line
		(map (lambda (x)
			(defq xv (/ x 10.0))
			(list xv (* 0.8 (cos xv))))
			(range 0 63))
		"cos(x)")

	(plot-render plot)
	plot)

(defun create-scatter-plot ()
	; Create a scatter plot
	(defq plot (Plot +plot_width +plot_height "Random Data - Scatter Plot"))
	(def plot :x_label "X" :y_label "Y")

	(plot-add-series plot :scatter (generate-scatter-data) "Random Points")

	(plot-render plot)
	plot)

(defun create-bar-plot ()
	; Create a bar chart
	(defq plot (Plot +plot_width +plot_height "Sample Values - Bar Chart"))
	(def plot :x_label "Category" :y_label "Value")

	(plot-add-series plot :bar (generate-bar-data) "Values")

	(plot-render plot)
	plot)

(defun create-area-plot ()
	; Create an area plot
	(defq plot (Plot +plot_width +plot_height "Complex Wave - Area Plot"))
	(def plot :x_label "X" :y_label "Y")

	(plot-add-series plot :area (generate-area-data) "Wave")

	(plot-render plot)
	plot)

(defun create-pie-chart ()
	; Create a pie chart
	(defq plot (Plot +plot_width +plot_height "Distribution - Pie Chart"))

	(plot-add-series plot :pie (generate-pie-data))

	(plot-render plot)
	plot)

(defun create-multi-series-plot ()
	; Create a plot with multiple series
	(defq plot (Plot +plot_width +plot_height "Multi-Series Plot"))
	(def plot :x_label "Time" :y_label "Value")

	; Add line series
	(plot-add-series plot :line
		(map (lambda (x)
			(defq xv (/ x 10.0))
			(list xv (* 2.0 (sin xv))))
			(range 0 31))
		"Signal A")

	; Add area series
	(plot-add-series plot :area
		(map (lambda (x)
			(defq xv (/ x 10.0))
			(list xv (+ 1.0 (cos xv))))
			(range 0 31))
		"Signal B")

	(plot-render plot)
	plot)

(defun create-histogram-plot ()
	; Create a histogram
	(defq values (generate-histogram-data))
	(plot-histogram values 20 +plot_width +plot_height "Normal Distribution - Histogram")
	)

(defun create-hbar-plot ()
	; Create a horizontal bar chart
	(defq plot (Plot +plot_width +plot_height "Performance Metrics - Horizontal Bars"))
	(def plot :x_label "Score" :y_label "Category")

	(plot-add-series plot :hbar (generate-hbar-data) "Scores")

	(plot-render plot)
	plot)

(defun create-markers-plot ()
	; Create scatter plot with different marker shapes
	(defq plot (Plot +plot_width +plot_height "Marker Shapes Demo"))
	(def plot :x_label "X" :y_label "Y" :show_legend :t)

	; Different marker shapes at different positions
	(defq shapes (list :circle :square :triangle :diamond :cross :plus))

	(each (lambda (i)
		(defq shape (elem i shapes)
			  y_offset (* i 2.0)
			  data (map (lambda (x)
				  (list (+ 1.0 (* 0.5 x)) (+ y_offset 5.0 (* 0.3 (sin (* x 0.5))))))
				  (range 0 10)))
		; Create series with specific marker shape
		(defq series_opts (env :color (elem (% i (length (get :colors plot))) (get :colors plot))
							   :marker shape
							   :size 5
							   :label (str shape)))
		; Manually add to series
		(push (get :series plot) (list :scatter data series_opts))
		(push (get :legend_items plot) (list (str shape) (get :color series_opts))))
		(range 0 (length shapes)))

	(plot-render plot)
	plot)

(defun create-error-bars-plot ()
	; Create a plot with error bars
	(defq plot (Plot +plot_width +plot_height "Measurements with Error Bars"))
	(def plot :x_label "X" :y_label "Value")

	(defq data (generate-error-bar-data))
	(defq series_opts (env :color +argb_cyan :error_bars :t :label "Measurements"))
	(push (get :series plot) (list :scatter data series_opts))
	(push (get :legend_items plot) (list "Measurements" +argb_cyan))

	(plot-render plot)
	plot)

(defun create-box-plot ()
	; Create a box plot
	(defq plot (Plot +plot_width +plot_height "Distribution Comparison - Box Plot"))
	(def plot :x_label "Category" :y_label "Value")

	(plot-add-series plot :box (generate-box-plot-data) "Distributions")

	(plot-render plot)
	plot)

(defun create-stacked-bar-plot ()
	; Create a stacked bar chart
	(defq plot (Plot +plot_width +plot_height "Components - Stacked Bars"))
	(def plot :x_label "Category" :y_label "Total Value")

	(defq series_opts (env :color +argb_cyan :label "Stacked"))
	(push (get :series plot) (list :stacked_bar (generate-stacked-bar-data) series_opts))

	(plot-render plot)
	plot)

(defun create-heatmap-plot ()
	; Create a heatmap
	(defq plot (Plot +plot_width +plot_height "2D Data - Heatmap"))
	(def plot :show_grid :nil :auto_scale :nil)

	(defq series_opts (env :colormap :hot :label "Intensity"))
	(push (get :series plot) (list :heatmap (generate-heatmap-data) series_opts))

	(plot-render plot)
	plot)

(defun create-candlestick-plot ()
	; Create a candlestick chart
	(defq plot (Plot +plot_width +plot_height "Stock Price - Candlestick"))
	(def plot :x_label "Time" :y_label "Price")

	(plot-add-series plot :candlestick (generate-candlestick-data) "Price")

	(plot-render plot)
	plot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq *current_plot* 0
	  *plots* (list))

; Create all plots
(push *plots* (create-line-plot))
(push *plots* (create-scatter-plot))
(push *plots* (create-markers-plot))
(push *plots* (create-error-bars-plot))
(push *plots* (create-bar-plot))
(push *plots* (create-hbar-plot))
(push *plots* (create-stacked-bar-plot))
(push *plots* (create-area-plot))
(push *plots* (create-pie-chart))
(push *plots* (create-histogram-plot))
(push *plots* (create-box-plot))
(push *plots* (create-heatmap-plot))
(push *plots* (create-candlestick-plot))
(push *plots* (create-multi-series-plot))

; Create UI window
(ui-window *window* ()
	(ui-title-bar *title* "Plot Demo - Line Plot" (0xea19) +event_close)
	(Component-connect (elem 0 *plots*)))

; Show first plot
(defq *current_canvas* (get :canvas (elem *current_plot* *plots*)))
(. *window* :add_child *current_canvas*)

; Plot titles
(defq *plot_titles*
	(list "Plot Demo - Line Plot"
		  "Plot Demo - Scatter Plot"
		  "Plot Demo - Marker Shapes"
		  "Plot Demo - Error Bars"
		  "Plot Demo - Bar Chart"
		  "Plot Demo - Horizontal Bars"
		  "Plot Demo - Stacked Bars"
		  "Plot Demo - Area Plot"
		  "Plot Demo - Pie Chart"
		  "Plot Demo - Histogram"
		  "Plot Demo - Box Plot"
		  "Plot Demo - Heatmap"
		  "Plot Demo - Candlestick"
		  "Plot Demo - Multi-Series"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Event Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun next-plot ()
	; Switch to next plot
	(. *window* :remove_child *current_canvas*)
	(setq *current_plot* (% (inc *current_plot*) (length *plots*)))
	(setq *current_canvas* (get :canvas (elem *current_plot* *plots*)))
	(. *window* :add_child *current_canvas*)
	(def *title* :text (elem *current_plot* *plot_titles*))
	(. *title* :layout))

(defun main ()
	; Main event loop
	(defq select (alloc-select +select_size))
	(defq timer_id (mail-timer-start +rate 0))

	(while :t
		(defq msg (mail-read (elem (mail-select select
			(elem-set +select_main (. *window* :get_mbox_id))
			(elem-set +select_timer timer_id)) (mail-poll select))))

		(cond
			((= (getf msg +ev_msg_target_id) timer_id)
				; Timer tick - could animate here
				(mail-timeout select +rate))
			((= (getf msg +ev_msg_action) +event_close)
				; Close window
				(break))
			((and (= (getf msg +ev_msg_type) +ev_type_key)
				  (= (getf msg +ev_msg_action) +ev_type_key_down))
				; Handle key press
				(defq key (getf msg +ev_msg_keycode))
				(cond
					((or (= key +char_space) (= key +char_n) (= key +char_right))
						; Next plot
						(next-plot))
					((= key +char_s)
						; Save current plot
						(defq filename (cat "plot_" (str *current_plot*) ".cpm"))
						(plot-save (elem *current_plot* *plots*) filename)
						(print "Saved: " filename))
					((= key +char_e)
						; Export to SVG
						(defq filename (cat "plot_" (str *current_plot*) ".svg"))
						(plot-export-svg (elem *current_plot* *plots*) filename)
						(print "Exported: " filename))))))

	; Cleanup
	(mail-timer-stop timer_id)
	(free-select select))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Entry Point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print)
(print "=== ChrysaLisp Plot Demo ===")
(print)
(print "Controls:")
(print "  SPACE/N/→ : Next plot")
(print "  S         : Save plot as CPM")
(print "  E         : Export plot as SVG")
(print)

(main)
