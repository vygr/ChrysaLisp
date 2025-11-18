;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChrysaLisp Plotting Library
; Inspired by vgplot and Oz
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "gui/path/lisp.inc")
(import "gui/font/lisp.inc")
(import "lib/consts/colors.inc")
(import "lib/math/vector.inc")
(import "lib/plot/svg.inc")
(import "lib/plot/data.inc")

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
	+plot_legend_item_height 25
	+plot_font_title (create-font "fonts/OpenSans-Bold.ctf" 24)
	+plot_font_label (create-font "fonts/OpenSans-Regular.ctf" 16)
	+plot_font_tick (create-font "fonts/OpenSans-Regular.ctf" 12)
	+plot_font_legend (create-font "fonts/OpenSans-Regular.ctf" 14))

;;;;;;;;;;;;;;;;
; Helper Functions
;;;;;;;;;;;;;;;;

(defun plot-color-gradient (value min_val max_val colormap)
	; (plot-color-gradient value min_val max_val colormap) -> argb
	; Map value to color using gradient
	; colormap: :viridis, :hot, :cool, :jet
	(defq t_val (cond
		((= min_val max_val) 0.5)
		(:t (/ (- value min_val) (- max_val min_val)))))

	(case colormap
		(:hot
			; Black -> Red -> Yellow -> White
			(cond
				((< t_val 0.33)
					(defq t1 (* t_val 3.0))
					(logior 0xff000000 (<< (floor (* t1 255)) 16)))
				((< t_val 0.66)
					(defq t1 (* (- t_val 0.33) 3.0))
					(logior 0xff000000 0xff0000 (<< (floor (* t1 255)) 8)))
				(:t
					(defq t1 (* (- t_val 0.66) 3.0))
					(logior 0xff000000 0xffff00 (floor (* t1 255))))))
		(:cool
			; Cyan -> Magenta
			(defq r (floor (* t_val 255))
				  b (floor (* (- 1.0 t_val) 255)))
			(logior 0xff000000 (<< r 16) 0xff00 b))
		(:jet
			; Blue -> Cyan -> Yellow -> Red
			(cond
				((< t_val 0.25)
					(defq t1 (* t_val 4.0))
					(logior 0xff000000 (<< (floor (* t1 255)) 8) 0xff))
				((< t_val 0.5)
					(defq t1 (* (- t_val 0.25) 4.0))
					(logior 0xff000000 (<< (floor (* t1 255)) 16) 0xff00 (floor (* (- 1.0 t1) 255))))
				((< t_val 0.75)
					(defq t1 (* (- t_val 0.5) 4.0))
					(logior 0xff000000 0xff0000 (<< (floor (* (- 1.0 t1) 255)) 8)))
				(:t
					(defq t1 (* (- t_val 0.75) 4.0))
					(logior 0xff000000 (<< (floor (* (- 1.0 t1) 255)) 16) 0x800000))))
		(:t
			; Default: grayscale
			(defq gray (floor (* t_val 255)))
			(logior 0xff000000 (<< gray 16) (<< gray 8) gray))))

(defun plot-draw-marker (canvas shape x y size color)
	; (plot-draw-marker canvas shape x y size color) -> canvas
	; Draw a marker at position
	; Shapes: :circle :square :triangle :diamond :cross :plus
	(. canvas :set_color color)
	(case shape
		(:circle
			; Draw filled circle using ellipse path
			(defq circle_path (path))
			(path-gen-ellipse x y size size circle_path)
			(. canvas :fpoly 0.0 0.0 +winding_odd_even (list circle_path)))
		(:square
			(. canvas :fbox (- x size) (- y size) (* 2 size) (* 2 size)))
		(:triangle
			; Upward pointing triangle
			(defq tri_path (path
				x (- y size)
				(- x size) (+ y size)
				(+ x size) (+ y size)))
			(. canvas :fpoly 0.0 0.0 +winding_odd_even (list tri_path)))
		(:diamond
			(defq diamond_path (path
				x (- y size)
				(+ x size) y
				x (+ y size)
				(- x size) y))
			(. canvas :fpoly 0.0 0.0 +winding_odd_even (list diamond_path)))
		(:cross
			; X-shaped cross
			(defq s2 (/ size 1.4))
			(. canvas :fbox (- x 1) (- y s2) 2 (* 2 s2))
			(. canvas :fbox (- x s2) (- y 1) (* 2 s2) 2))
		(:plus
			; + shaped plus
			(. canvas :fbox (- x 1) (- y size) 2 (* 2 size))
			(. canvas :fbox (- x size) (- y 1) (* 2 size) 2))
		(:t
			; Default: square
			(. canvas :fbox (- x size) (- y size) (* 2 size) (* 2 size))))
	canvas)

(defun plot-draw-text (canvas font text x y color &optional scale align)
	; (plot-draw-text canvas font text x y color [scale align]) -> canvas
	; Draw text on canvas at position
	; align: :left :center :right (default :left)
	(defq scale (setd scale 1.0)
		  align (setd align :left)
		  paths (font-glyph-paths font text)
		  bounds (font-glyph-bounds font text))

	; Calculate alignment offset
	(defq text_width (* scale (first bounds))
		  x_offset (cond
			((eql align :center) (- (/ text_width 2)))
			((eql align :right) (- text_width))
			(:t 0)))

	; Transform and render paths
	(defq transformed_paths
		(map (lambda (path)
			(path-transform (fixeds scale 0.0 (+ x x_offset) 0.0 scale y) path (path)))
			paths))

	(. canvas :set_color color)
	(. canvas :fpoly 0.0 0.0 +winding_odd_even transformed_paths)
	canvas)

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

(defun plot-histogram-bins (values num_bins)
	; (plot-histogram-bins values num_bins) -> ((bin_center count) ...)
	; Bin data for histogram
	(cond
		((or (empty? values) (<= num_bins 0)) (list))
		(:t
			(defq min_val (apply min values)
				  max_val (apply max values)
				  range (- max_val min_val)
				  bin_width (/ range num_bins)
				  bins (map (lambda (_) 0) (range 0 num_bins)))

			; Count values in each bin
			(each (lambda (val)
				(defq bin_idx (min (dec num_bins) (floor (/ (- val min_val) bin_width))))
				(elem-set bins bin_idx (inc (elem-get bins bin_idx))))
				values)

			; Create bin data (center, count)
			(map (lambda (i)
				(defq bin_center (+ min_val (* bin_width (+ i 0.5))))
				(list bin_center (elem-get bins i)))
				(range 0 num_bins)))))

;;;;;;;;;;;;;;;;
; Plot Class
;;;;;;;;;;;;;;;;

(defun Plot (width height &optional title)
	; (Plot width height [title]) -> plot
	; Create a new plot
	(defq plot (penv
		:width width
		:height height
		:title (setd title "Plot")
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
	(elem-get colors idx))

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
		  y_max (get :y_max plot)
		  x_ticks (get :x_ticks plot)
		  y_ticks (get :y_ticks plot))

	; Draw grid
	(when (get :show_grid plot)
		(. canvas :set_color +plot_grid_color)
		; Vertical grid lines
		(each (lambda (i)
			(defq x (+ px (* i (/ pw (dec x_ticks)))))
			(. canvas :fbox x py 1 ph)) (range 0 x_ticks))
		; Horizontal grid lines
		(each (lambda (i)
			(defq y (+ py (* i (/ ph (dec y_ticks)))))
			(. canvas :fbox px y pw 1)) (range 0 y_ticks)))

	; Draw axes
	(. canvas :set_color +plot_axis_color)
	(. canvas :fbox px (+ py ph) pw 2)  ; X-axis
	(. canvas :fbox px py 2 ph)  ; Y-axis

	; Draw tick marks and labels
	; X-axis ticks
	(each (lambda (i)
		(defq x (+ px (* i (/ pw (dec x_ticks))))
			  value (+ x_min (* i (/ (- x_max x_min) (dec x_ticks)))))
		; Tick mark
		(. canvas :set_color +plot_axis_color)
		(. canvas :fbox x (+ py ph) 1 +plot_tick_size)
		; Tick label
		(plot-draw-text canvas +plot_font_tick (plot-format-number value)
			x (+ py ph +plot_tick_size 15) +plot_text_color 1.0 :center))
		(range 0 x_ticks))

	; Y-axis ticks
	(each (lambda (i)
		(defq y (+ py (* i (/ ph (dec y_ticks))))
			  value (- y_max (* i (/ (- y_max y_min) (dec y_ticks)))))
		; Tick mark
		(. canvas :set_color +plot_axis_color)
		(. canvas :fbox (- px +plot_tick_size) y +plot_tick_size 1)
		; Tick label
		(plot-draw-text canvas +plot_font_tick (plot-format-number value)
			(- px +plot_tick_size 5) (+ y 4) +plot_text_color 1.0 :right))
		(range 0 y_ticks))

	; Draw axis labels
	(when (def? :x_label plot)
		(plot-draw-text canvas +plot_font_label (get :x_label plot)
			(+ px (/ pw 2)) (- (get :height plot) 10) +plot_text_color 1.0 :center))
	(when (def? :y_label plot)
		; Y-axis label (rotated would be ideal, but for now just place it on the side)
		(plot-draw-text canvas +plot_font_label (get :y_label plot)
			10 (+ py (/ ph 2)) +plot_text_color 1.0 :left))

	plot)

(defun plot-draw-title (plot)
	; (plot-draw-title plot) -> plot
	; Draw plot title
	(when (get :title plot)
		(defq canvas (get :canvas plot)
			  title (get :title plot))
		(plot-draw-text canvas +plot_font_title title
			(/ (get :width plot) 2) 30 +plot_text_color 1.0 :center))
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
			; Label text
			(plot-draw-text canvas +plot_font_legend label
				(+ lx 35) (- iy 5) +plot_text_color 1.0 :left))
			items))
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
			(:hbar (plot-draw-hbar plot data opts))
			(:area (plot-draw-area plot data opts))
			(:pie (plot-draw-pie plot data opts))
			(:histogram (plot-draw-histogram plot data opts))
			(:box (plot-draw-box plot data opts))
			(:stacked_bar (plot-draw-stacked-bar plot data opts))
			(:heatmap (plot-draw-heatmap plot data opts))
			(:candlestick (plot-draw-candlestick plot data opts))))
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
		  label (setd label (cat (str type) " " (str (inc (length (get :series plot)))))))

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

	; Draw error bars if present
	(when (def? :error_bars opts)
		(plot-draw-error-bars plot data opts))

	plot)

(defun plot-draw-error-bars (plot data opts)
	; (plot-draw-error-bars plot data opts) -> plot
	; Draw error bars for data points
	; Data format: ((x y [y_err]) ...) or ((x y [y_err_low y_err_high]) ...)
	(defq canvas (get :canvas plot)
		  color (setd (get :color opts) +argb_white)
		  cap_width 6)

	(. canvas :set_color color)

	(each (lambda (pt)
		(cond
			((= (length pt) 3)
				; Symmetric error bars
				(bind '(x y y_err) pt)
				(bind '(sx sy) (plot-to-screen plot x y))
				(bind '(sx_lo sy_lo) (plot-to-screen plot x (- y y_err)))
				(bind '(sx_hi sy_hi) (plot-to-screen plot x (+ y y_err)))
				; Vertical line
				(. canvas :fbox sx sy_lo 1 (- sy_hi sy_lo))
				; Top cap
				(. canvas :fbox (- sx cap_width) sy_hi (* 2 cap_width) 1)
				; Bottom cap
				(. canvas :fbox (- sx cap_width) sy_lo (* 2 cap_width) 1))
			((= (length pt) 4)
				; Asymmetric error bars
				(bind '(x y y_err_low y_err_high) pt)
				(bind '(sx sy) (plot-to-screen plot x y))
				(bind '(sx_lo sy_lo) (plot-to-screen plot x (- y y_err_low)))
				(bind '(sx_hi sy_hi) (plot-to-screen plot x (+ y y_err_high)))
				; Vertical line
				(. canvas :fbox sx sy_lo 1 (- sy_hi sy_lo))
				; Top cap
				(. canvas :fbox (- sx cap_width) sy_hi (* 2 cap_width) 1)
				; Bottom cap
				(. canvas :fbox (- sx cap_width) sy_lo (* 2 cap_width) 1))))
		data)

	plot)

(defun plot-draw-scatter (plot data opts)
	; (plot-draw-scatter plot data opts) -> plot
	; Draw a scatter plot
	(defq canvas (get :canvas plot)
		  color (get :color opts)
		  size (setd (def? :size opts) 4)
		  marker (setd (def? :marker opts) :circle))

	; Draw error bars first (so markers appear on top)
	(when (def? :error_bars opts)
		(plot-draw-error-bars plot data opts))

	; Draw each point
	(each (lambda (pt)
		(bind '(x y) pt)
		(bind '(sx sy) (plot-to-screen plot x y))
		(plot-draw-marker canvas marker sx sy size color))
		data)

	plot)

(defun plot-draw-bar (plot data opts)
	; (plot-draw-bar plot data opts) -> plot
	; Draw a vertical bar chart
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

(defun plot-draw-hbar (plot data opts)
	; (plot-draw-hbar plot data opts) -> plot
	; Draw a horizontal bar chart
	(defq canvas (get :canvas plot)
		  color (get :color opts)
		  n (length data))

	(. canvas :set_color color)

	; Calculate bar height
	(bind '(px py pw ph) (plot-get-plot-area plot))
	(defq bar_height (/ ph n 1.2))

	; Draw each bar
	(each (lambda (pt)
		(bind '(x y) pt)
		(bind '(sx sy) (plot-to-screen plot x y))
		(bind '(sx0 sy0) (plot-to-screen plot (get :x_min plot) y))
		(defq bar_width (- sx sx0))
		(. canvas :fbox sx0 (- sy (/ bar_height 2)) bar_width bar_height))
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
		(bind '(sx1 sy1) (plot-to-screen plot (first (elem-get data -1)) (get :y_min plot)))
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
	(defq total (reduce (lambda (acc x) (+ acc x)) (map (lambda (item) (second item)) data) 0.0)
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
			  color (elem-get colors (% (inc (!)) (length colors))))

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

(defun plot-draw-histogram (plot data opts)
	; (plot-draw-histogram plot data opts) -> plot
	; Draw a histogram
	; Data is already binned as ((bin_center count) ...)
	(defq canvas (get :canvas plot)
		  color (get :color opts)
		  n (length data))

	(. canvas :set_color color)

	; Calculate bar width
	(bind '(px py pw ph) (plot-get-plot-area plot))
	(defq bar_width (/ pw n 1.1))

	; Draw each bar
	(each (lambda (pt)
		(bind '(x y) pt)
		(bind '(sx sy) (plot-to-screen plot x y))
		(bind '(sx0 sy0) (plot-to-screen plot x (get :y_min plot)))
		(defq bar_height (- sy0 sy))
		(. canvas :fbox (- sx (/ bar_width 2)) sy bar_width bar_height))
		data)

	plot)

(defun plot-draw-box (plot data opts)
	; (plot-draw-box plot data opts) -> plot
	; Draw box plots
	; Data format: ((x_pos values) ...) where values is list of data points
	(defq canvas (get :canvas plot)
		  color (get :color opts)
		  box_width 40)

	(. canvas :set_color color)

	(each (lambda (item)
		(bind '(x_pos values) item)
		(bind '(min_val q1 median q3 max_val outliers) (plot-box-stats values))

		; Convert to screen coordinates
		(bind '(sx sy_min) (plot-to-screen plot x_pos min_val))
		(bind '(_ sy_q1) (plot-to-screen plot x_pos q1))
		(bind '(_ sy_median) (plot-to-screen plot x_pos median))
		(bind '(_ sy_q3) (plot-to-screen plot x_pos q3))
		(bind '(_ sy_max) (plot-to-screen plot x_pos max_val))

		; Draw whiskers (lines from box to min/max)
		(. canvas :fbox (- sx 1) sy_min 2 (- sy_q1 sy_min))  ; Lower whisker
		(. canvas :fbox (- sx 1) sy_q3 2 (- sy_max sy_q3))   ; Upper whisker

		; Draw whisker caps
		(. canvas :fbox (- sx 10) sy_min 20 2)
		(. canvas :fbox (- sx 10) sy_max 20 2)

		; Draw box (from Q1 to Q3)
		(defq box_height (- sy_q3 sy_q1))
		(. canvas :fbox (- sx (/ box_width 2)) sy_q1 box_width box_height)

		; Draw median line (darker/contrasting)
		(. canvas :set_color +argb_white)
		(. canvas :fbox (- sx (/ box_width 2)) sy_median box_width 3)
		(. canvas :set_color color)

		; Draw outliers as small circles
		(each (lambda (outlier)
			(bind '(_ sy_out) (plot-to-screen plot x_pos outlier))
			(plot-draw-marker canvas :circle sx sy_out 3 color))
			outliers))
		data)

	plot)

(defun plot-draw-stacked-bar (plot data opts)
	; (plot-draw-stacked-bar plot data opts) -> plot
	; Draw stacked bar chart
	; Data format: ((x_pos (val1 val2 val3 ...)) ...)
	(defq canvas (get :canvas plot)
		  colors (get :colors plot)
		  n (length data))

	; Calculate bar width
	(bind '(px py pw ph) (plot-get-plot-area plot))
	(defq bar_width (/ pw n 1.2))

	; Draw each stacked bar
	(each (lambda (item)
		(bind '(x values) item)
		(defq cumulative 0.0)

		; Draw each segment
		(each (lambda (val)
			(defq color (elem-get colors (% (inc (!)) (length colors))))
			(bind '(sx sy) (plot-to-screen plot x (+ cumulative val)))
			(bind '(sx0 sy0) (plot-to-screen plot x cumulative))
			(defq segment_height (- sy0 sy))

			(. canvas :set_color color)
			(. canvas :fbox (- sx (/ bar_width 2)) sy bar_width segment_height)

			(setq cumulative (+ cumulative val)))
			values))
		data)

	plot)

(defun plot-draw-heatmap (plot data opts)
	; (plot-draw-heatmap plot data opts) -> plot
	; Draw a heatmap
	; Data format: 2D list ((row1_vals...) (row2_vals...) ...)
	(defq canvas (get :canvas plot)
		  colormap (setd (def? :colormap opts) :hot)
		  rows (length data)
		  cols (length (first data)))

	(cond
		((and (> rows 0) (> cols 0))
			; Find min/max values
			(defq all_vals (reduce (lambda (acc x) (cat acc x)) data (list)))
			(defq min_val (apply min all_vals)
				  max_val (apply max all_vals))

			; Calculate cell size
			(bind '(px py pw ph) (plot-get-plot-area plot))
			(defq cell_width (/ pw cols)
				  cell_height (/ ph rows))

			; Draw each cell
			(each (lambda (row_idx)
				(defq row (elem-get data row_idx))
				(each (lambda (col_idx)
					(defq val (elem-get row col_idx)
						  color (plot-color-gradient val min_val max_val colormap)
						  x (+ px (* col_idx cell_width))
						  y (+ py (* row_idx cell_height)))
					(. canvas :set_color color)
					(. canvas :fbox x y cell_width cell_height))
					(range 0 cols)))
				(range 0 rows))))

	plot)

(defun plot-draw-candlestick (plot data opts)
	; (plot-draw-candlestick plot data opts) -> plot
	; Draw candlestick chart
	; Data format: ((x open high low close) ...)
	(defq canvas (get :canvas plot)
		  color_up (setd (def? :color_up opts) +argb_green)
		  color_down (setd (def? :color_down opts) +argb_red)
		  candle_width 10)

	; Draw each candlestick
	(each (lambda (item)
		(bind '(x open high low close) item)
		(defq color (if (>= close open) color_up color_down)
			  body_top (max open close)
			  body_bottom (min open close))

		; Convert to screen coordinates
		(bind '(sx sy_high) (plot-to-screen plot x high))
		(bind '(_ sy_low) (plot-to-screen plot x low))
		(bind '(_ sy_top) (plot-to-screen plot x body_top))
		(bind '(_ sy_bottom) (plot-to-screen plot x body_bottom))

		; Draw wick (high to low)
		(. canvas :set_color color)
		(. canvas :fbox (- sx 1) sy_high 2 (- sy_low sy_high))

		; Draw candle body
		(defq body_height (- sy_bottom sy_top))
		(. canvas :fbox (- sx candle_width) sy_top (* 2 candle_width) body_height))
		data)

	plot)

;;;;;;;;;;;;;;;;
; Statistical Annotations
;;;;;;;;;;;;;;;;

(defun plot-add-mean-line (plot data)
	; (plot-add-mean-line plot data) -> plot
	; Add horizontal line at mean value
	(defq y_values (map second data)
		  mean_val (plot-mean y_values)
		  x_min (get :x_min plot)
		  x_max (get :x_max plot))

	; Add as line series
	(plot-add-series plot :line
		(list (list x_min mean_val) (list x_max mean_val))
		"Mean")
	plot)

(defun plot-add-median-line (plot data)
	; (plot-add-median-line plot data) -> plot
	; Add horizontal line at median value
	(defq y_values (map second data)
		  median_val (plot-median y_values)
		  x_min (get :x_min plot)
		  x_max (get :x_max plot))

	; Add as line series
	(plot-add-series plot :line
		(list (list x_min median_val) (list x_max median_val))
		"Median")
	plot)

;;;;;;;;;;;;;;;;
; Quick Plot Functions
;;;;;;;;;;;;;;;;

(defun plot-line (data &optional width height title)
	; (plot-line data [width height title]) -> plot
	; Quick line plot
	(defq width (setd width 800)
		  height (setd height 600)
		  title (setd title "Line Plot")
		  plot (Plot width height title))
	(plot-add-series plot :line data)
	(plot-render plot)
	plot)

(defun plot-scatter (data &optional width height title)
	; (plot-scatter data [width height title]) -> plot
	; Quick scatter plot
	(defq width (setd width 800)
		  height (setd height 600)
		  title (setd title "Scatter Plot")
		  plot (Plot width height title))
	(plot-add-series plot :scatter data)
	(plot-render plot)
	plot)

(defun plot-bar (data &optional width height title)
	; (plot-bar data [width height title]) -> plot
	; Quick bar chart
	(defq width (setd width 800)
		  height (setd height 600)
		  title (setd title "Bar Chart")
		  plot (Plot width height title))
	(plot-add-series plot :bar data)
	(plot-render plot)
	plot)

(defun plot-area (data &optional width height title)
	; (plot-area data [width height title]) -> plot
	; Quick area plot
	(defq width (setd width 800)
		  height (setd height 600)
		  title (setd title "Area Plot")
		  plot (Plot width height title))
	(plot-add-series plot :area data)
	(plot-render plot)
	plot)

(defun plot-pie (data &optional width height title)
	; (plot-pie data [width height title]) -> plot
	; Quick pie chart
	; Data format: ((label value) ...)
	(defq width (setd width 800)
		  height (setd height 600)
		  title (setd title "Pie Chart")
		  plot (Plot width height title))
	(def plot :show_grid :nil :auto_scale :nil)
	(plot-add-series plot :pie data)
	(plot-render plot)
	plot)

(defun plot-histogram (values num_bins &optional width height title)
	; (plot-histogram values num_bins [width height title]) -> plot
	; Quick histogram from raw values
	(defq width (setd width 800)
		  height (setd height 600)
		  title (setd title "Histogram")
		  plot (Plot width height title)
		  binned_data (plot-histogram-bins values num_bins))
	(def plot :x_label "Value" :y_label "Frequency")
	(plot-add-series plot :histogram binned_data "Distribution")
	(plot-render plot)
	plot)

;;;;;;;;;;;;;;;;
; Export Functions
;;;;;;;;;;;;;;;;

(defun plot-save (plot filename)
	; (plot-save plot filename) -> :t | :nil
	; Save plot to file
	(. (get :canvas plot) :save filename +load_flag_shared))
