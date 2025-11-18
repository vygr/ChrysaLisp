;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChrysaLisp Plotting Library
; Inspired by vgplot and Oz
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "gui/path/lisp.inc")
(import "lib/consts/colors.inc")
(import "lib/math/vector.inc")
(import "lib/plot/svg.inc")

;;;;;;;;;;;;;;;;
; Plot Constants
;;;;;;;;;;;;;;;;

(defq
	+plot_margin_left 80
	+plot_margin_right 40
	+plot_margin_top 40
	+plot_margin_bottom 60
	+plot_tick_size 8
	+plot_label_offset 10
	+plot_grid_color (logior 0x40000000 +argb_grey8)
	+plot_axis_color +argb_white
	+plot_text_color +argb_white
	+plot_title_height 40
	+plot_legend_width 150
	+plot_legend_item_height 25)

;;;;;;;;;;;;;;;;
; Helper Functions
;;;;;;;;;;;;;;;;

(defun plot-lerp (a b t)
	; (plot-lerp a b t) -> value
	; Linear interpolation
	(+ a (* (- b a) t)))

(defun plot-map-range (val in_min in_max out_min out_max)
	; (plot-map-range val in_min in_max out_min out_max) -> value
	; Map value from one range to another
	(+ out_min (* (- val in_min) (/ (- out_max out_min) (- in_max in_min)))))

(defun plot-auto-ticks (min_val max_val num_ticks)
	; (plot-auto-ticks min_val max_val num_ticks) -> (tick_values ...)
	; Generate nice tick values
	(defq range (- max_val min_val))
	(defq step (/ range (dec num_ticks)))
	(map (lambda (_) (+ min_val (* step _))) (range 0 num_ticks)))

(defun plot-format-number (n)
	; (plot-format-number n) -> str
	; Format number for display
	(cond
		((= n (floor n)) (str (floor n)))
		((< (abs n) 0.01) (str n))
		(:t (str (floor (* n 100)) "." (% (floor (* n 100)) 100)))))

(defun plot-data-bounds (data)
	; (plot-data-bounds data) -> (min_x max_x min_y max_y)
	; Find bounds of data points
	(cond
		((empty? data) '(0.0 1.0 0.0 1.0))
		(:t
			(defq min_x (first (first data))
				  max_x (first (first data))
				  min_y (second (first data))
				  max_y (second (first data)))
			(each (lambda (pt)
				(setq min_x (min min_x (first pt))
					  max_x (max max_x (first pt))
					  min_y (min min_y (second pt))
					  max_y (max max_y (second pt)))) data)
			(list min_x max_x min_y max_y))))

;;;;;;;;;;;;;;;;
; Plot Class
;;;;;;;;;;;;;;;;

(defun Plot (width height &optional title)
	; (Plot width height [title]) -> plot
	; Create a new plot
	(defq plot (penv
		:width width
		:height height
		:title (opt title "Plot")
		:canvas (Canvas width height 1)
		:x_label "X"
		:y_label "Y"
		:x_min 0.0
		:x_max 1.0
		:y_min 0.0
		:y_max 1.0
		:auto_scale :t
		:show_grid :t
		:show_legend :t
		:series (list)
		:colors (list +argb_cyan +argb_yellow +argb_magenta +argb_green +argb_red +argb_orange +argb_steel)
		:color_index 0
		:legend_items (list)
		:x_ticks 5
		:y_ticks 5))

	; Set up canvas
	(.-> (get :canvas plot)
		(:set_canvas_flags +canvas_flag_antialias)
		(:fill +argb_black))

	plot)

(defun plot-get-color (plot)
	; (plot-get-color plot) -> color
	; Get next color for series
	(defq colors (get :colors plot)
		  idx (get :color_index plot))
	(def plot :color_index (% (inc idx) (length colors)))
	(elem idx colors))

(defun plot-get-plot-area (plot)
	; (plot-get-plot-area plot) -> (x y w h)
	; Get the plotting area coordinates
	(defq left +plot_margin_left
		  right (- (get :width plot) +plot_margin_right)
		  top (+ +plot_margin_top (if (get :title plot) +plot_title_height 0))
		  bottom (- (get :height plot) +plot_margin_bottom))
	(when (get :show_legend plot)
		(setq right (- right +plot_legend_width)))
	(list left top (- right left) (- bottom top)))

(defun plot-to-screen (plot x y)
	; (plot-to-screen plot x y) -> (screen_x screen_y)
	; Transform data coordinates to screen coordinates
	(bind '(px py pw ph) (plot-get-plot-area plot))
	(defq sx (plot-map-range x
				(get :x_min plot) (get :x_max plot)
				px (+ px pw))
		  sy (plot-map-range y
				(get :y_min plot) (get :y_max plot)
				(+ py ph) py))
	(list sx sy))

(defun plot-draw-axes (plot)
	; (plot-draw-axes plot) -> plot
	; Draw axes, ticks, and grid
	(bind '(px py pw ph) (plot-get-plot-area plot))
	(defq canvas (get :canvas plot)
		  x_min (get :x_min plot)
		  x_max (get :x_max plot)
		  y_min (get :y_min plot)
		  y_max (get :y_max plot))

	; Draw grid
	(when (get :show_grid plot)
		(. canvas :set_color +plot_grid_color)
		; Vertical grid lines
		(each (lambda (i)
			(defq x (+ px (* i (/ pw (dec (get :x_ticks plot))))))
			(. canvas :fbox x py 1 ph)) (range 0 (get :x_ticks plot)))
		; Horizontal grid lines
		(each (lambda (i)
			(defq y (+ py (* i (/ ph (dec (get :y_ticks plot))))))
			(. canvas :fbox px y pw 1)) (range 0 (get :y_ticks plot))))

	; Draw axes
	(. canvas :set_color +plot_axis_color)
	(. canvas :fbox px (+ py ph) pw 2)  ; X-axis
	(. canvas :fbox px py 2 ph)  ; Y-axis

	plot)

(defun plot-draw-title (plot)
	; (plot-draw-title plot) -> plot
	; Draw plot title
	; Note: Text rendering would require font support
	; For now, this is a placeholder
	plot)

(defun plot-draw-legend (plot)
	; (plot-draw-legend plot) -> plot
	; Draw legend
	(when (and (get :show_legend plot) (> (length (get :legend_items plot)) 0))
		(bind '(px py pw ph) (plot-get-plot-area plot))
		(defq canvas (get :canvas plot)
			  lx (+ px pw +plot_label_offset)
			  ly py
			  items (get :legend_items plot))

		; Draw legend background
		(. canvas :set_color (logior 0x80000000 +argb_grey2))
		(. canvas :fbox lx ly +plot_legend_width
			(* (length items) +plot_legend_item_height))

		; Draw legend items
		(each (lambda (item)
			(bind '(label color) item)
			(defq iy (+ ly (* (inc (!)) +plot_legend_item_height)))
			; Color box
			(. canvas :set_color color)
			(. canvas :fbox (+ lx 10) (- iy 15) 20 15)
			) items))
	plot)

(defun plot-render (plot)
	; (plot-render plot) -> plot
	; Render the complete plot
	(defq canvas (get :canvas plot))

	; Clear canvas
	(. canvas :fill +argb_black)

	; Draw components
	(plot-draw-axes plot)
	(plot-draw-title plot)

	; Draw all series
	(each (lambda (series)
		(bind '(type data opts) series)
		(case type
			(:line (plot-draw-line plot data opts))
			(:scatter (plot-draw-scatter plot data opts))
			(:bar (plot-draw-bar plot data opts))
			(:area (plot-draw-area plot data opts))
			(:pie (plot-draw-pie plot data opts))))
		(get :series plot))

	(plot-draw-legend plot)

	(. canvas :swap 0)
	plot)

;;;;;;;;;;;;;;;;
; Series Management
;;;;;;;;;;;;;;;;

(defun plot-add-series (plot type data &optional label)
	; (plot-add-series plot type data [label]) -> plot
	; Add a data series to the plot
	(defq color (plot-get-color plot)
		  label (opt label (cat (str type) " " (str (inc (length (get :series plot)))))))

	; Auto-scale if needed
	(when (get :auto_scale plot)
		(bind '(min_x max_x min_y max_y) (plot-data-bounds data))
		(when (empty? (get :series plot))
			(def plot :x_min min_x :x_max max_x :y_min min_y :y_max max_y))
		; Expand bounds if needed
		(def plot
			:x_min (min (get :x_min plot) min_x)
			:x_max (max (get :x_max plot) max_x)
			:y_min (min (get :y_min plot) min_y)
			:y_max (max (get :y_max plot) max_y))
		; Add 5% padding
		(defq x_range (- (get :x_max plot) (get :x_min plot))
			  y_range (- (get :y_max plot) (get :y_min plot)))
		(def plot
			:x_min (- (get :x_min plot) (* x_range 0.05))
			:x_max (+ (get :x_max plot) (* x_range 0.05))
			:y_min (- (get :y_min plot) (* y_range 0.05))
			:y_max (+ (get :y_max plot) (* y_range 0.05))))

	; Add series
	(push (get :series plot) (list type data (env :color color :label label)))
	(push (get :legend_items plot) (list label color))

	plot)

;;;;;;;;;;;;;;;;
; Plot Types
;;;;;;;;;;;;;;;;

(defun plot-draw-line (plot data opts)
	; (plot-draw-line plot data opts) -> plot
	; Draw a line plot
	(defq canvas (get :canvas plot)
		  color (get :color opts)
		  points (path))

	; Convert data points to screen coordinates
	(each (lambda (pt)
		(bind '(x y) pt)
		(bind '(sx sy) (plot-to-screen plot x y))
		(push points sx sy)) data)

	; Stroke the path
	(when (> (length data) 1)
		(. canvas :set_color color)
		(defq paths (path-stroke-polyline points 2.0 +join_round +cap_round +cap_round))
		(. canvas :fpoly 0.0 0.0 +winding_odd_even (list paths)))

	plot)

(defun plot-draw-scatter (plot data opts)
	; (plot-draw-scatter plot data opts) -> plot
	; Draw a scatter plot
	(defq canvas (get :canvas plot)
		  color (get :color opts)
		  size (opt (def? :size opts) 4))

	(. canvas :set_color color)

	; Draw each point
	(each (lambda (pt)
		(bind '(x y) pt)
		(bind '(sx sy) (plot-to-screen plot x y))
		(. canvas :fbox (- sx size) (- sy size) (* 2 size) (* 2 size)))
		data)

	plot)

(defun plot-draw-bar (plot data opts)
	; (plot-draw-bar plot data opts) -> plot
	; Draw a bar chart
	(defq canvas (get :canvas plot)
		  color (get :color opts)
		  n (length data))

	(. canvas :set_color color)

	; Calculate bar width
	(bind '(px py pw ph) (plot-get-plot-area plot))
	(defq bar_width (/ pw n 1.2))

	; Draw each bar
	(each (lambda (pt)
		(bind '(x y) pt)
		(bind '(sx sy) (plot-to-screen plot x y))
		(bind '(sx0 sy0) (plot-to-screen plot x (get :y_min plot)))
		(defq bar_height (- sy0 sy))
		(. canvas :fbox (- sx (/ bar_width 2)) sy bar_width bar_height))
		data)

	plot)

(defun plot-draw-area (plot data opts)
	; (plot-draw-area plot data opts) -> plot
	; Draw an area plot
	(defq canvas (get :canvas plot)
		  color (logior 0x80000000 (logand (get :color opts) 0xffffff))
		  points (path))

	; Convert data points to screen coordinates
	(each (lambda (pt)
		(bind '(x y) pt)
		(bind '(sx sy) (plot-to-screen plot x y))
		(push points sx sy)) data)

	; Close the path to the baseline
	(when (> (length data) 1)
		(bind '(sx0 sy0) (plot-to-screen plot (first (first data)) (get :y_min plot)))
		(bind '(sx1 sy1) (plot-to-screen plot (first (elem -1 data)) (get :y_min plot)))
		(push points sx1 sy1 sx0 sy0)

		; Fill the area
		(. canvas :set_color color)
		(. canvas :fpoly 0.0 0.0 +winding_odd_even (list points)))

	plot)

(defun plot-draw-pie (plot data opts)
	; (plot-draw-pie plot data opts) -> plot
	; Draw a pie chart
	; Data format: ((label value) ...)
	(defq canvas (get :canvas plot)
		  colors (get :colors plot))

	; Calculate total and center
	(defq total (reduce + (map (lambda (item) (second item)) data) 0)
		  cx (/ (get :width plot) 2)
		  cy (/ (get :height plot) 2)
		  radius (* (min cx cy) 0.6)
		  start_angle 0.0)

	; Clear legend items for pie chart
	(def plot :legend_items (list))

	; Draw each slice
	(each (lambda (item)
		(bind '(label value) item)
		(defq angle (* +fp_2pi (/ value total))
			  end_angle (+ start_angle angle)
			  color (elem (% (inc (!)) (length colors)) colors))

		; Create wedge path
		(defq wedge (path cx cy))
		(path-gen-arc cx cy start_angle angle radius wedge)
		(push wedge cx cy)

		; Fill wedge
		(. canvas :set_color color)
		(. canvas :fpoly 0.0 0.0 +winding_odd_even (list wedge))

		; Add to legend
		(push (get :legend_items plot) (list label color))

		(setq start_angle end_angle))
		data)

	plot)

;;;;;;;;;;;;;;;;
; Quick Plot Functions
;;;;;;;;;;;;;;;;

(defun plot-line (data &optional width height title)
	; (plot-line data [width height title]) -> plot
	; Quick line plot
	(defq width (opt width 800)
		  height (opt height 600)
		  title (opt title "Line Plot")
		  plot (Plot width height title))
	(plot-add-series plot :line data)
	(plot-render plot)
	plot)

(defun plot-scatter (data &optional width height title)
	; (plot-scatter data [width height title]) -> plot
	; Quick scatter plot
	(defq width (opt width 800)
		  height (opt height 600)
		  title (opt title "Scatter Plot")
		  plot (Plot width height title))
	(plot-add-series plot :scatter data)
	(plot-render plot)
	plot)

(defun plot-bar (data &optional width height title)
	; (plot-bar data [width height title]) -> plot
	; Quick bar chart
	(defq width (opt width 800)
		  height (opt height 600)
		  title (opt title "Bar Chart")
		  plot (Plot width height title))
	(plot-add-series plot :bar data)
	(plot-render plot)
	plot)

(defun plot-area (data &optional width height title)
	; (plot-area data [width height title]) -> plot
	; Quick area plot
	(defq width (opt width 800)
		  height (opt height 600)
		  title (opt title "Area Plot")
		  plot (Plot width height title))
	(plot-add-series plot :area data)
	(plot-render plot)
	plot)

(defun plot-pie (data &optional width height title)
	; (plot-pie data [width height title]) -> plot
	; Quick pie chart
	; Data format: ((label value) ...)
	(defq width (opt width 800)
		  height (opt height 600)
		  title (opt title "Pie Chart")
		  plot (Plot width height title))
	(def plot :show_grid :nil :auto_scale :nil)
	(plot-add-series plot :pie data)
	(plot-render plot)
	plot)

;;;;;;;;;;;;;;;;
; Export Functions
;;;;;;;;;;;;;;;;

(defun plot-save (plot filename)
	; (plot-save plot filename) -> :t | :nil
	; Save plot to file
	(. (get :canvas plot) :save filename +load_flag_shared))
