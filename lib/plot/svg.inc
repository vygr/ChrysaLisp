;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SVG Export for Plot Library
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/streams/string-stream.inc")

;;;;;;;;;;;;;;;;
; SVG Helper Functions
;;;;;;;;;;;;;;;;

(defun svg-color-to-hex (argb)
	; (svg-color-to-hex argb) -> hex_str
	; Convert ARGB color to SVG hex format
	(defq r (logand (>> argb 16) 0xff)
		  g (logand (>> argb 8) 0xff)
		  b (logand argb 0xff)
		  a (logand (>> argb 24) 0xff))
	(cat "#" (pad (str r 16) 2 "0") (pad (str g 16) 2 "0") (pad (str b 16) 2 "0")))

(defun svg-color-opacity (argb)
	; (svg-color-opacity argb) -> opacity_str
	; Get opacity from ARGB color
	(defq a (logand (>> argb 24) 0xff))
	(str (/ a 255.0)))

(defun svg-path-to-string (points &optional closed)
	; (svg-path-to-string points [closed]) -> svg_path_str
	; Convert path points to SVG path string
	(defq result (list))
	(when (> (length points) 0)
		(push result (cat "M " (str (elem 0 points)) " " (str (elem 1 points))))
		(each-rev (lambda (i)
			(when (and (> i 0) (even? i))
				(push result (cat "L " (str (elem i points)) " " (str (elem (inc i) points))))))
			(range 0 (length points)))
		(when closed
			(push result "Z")))
	(apply cat (interleave result " ")))

;;;;;;;;;;;;;;;;
; SVG Export Functions
;;;;;;;;;;;;;;;;

(defun plot-export-svg (plot filename)
	; (plot-export-svg plot filename) -> :t | :nil
	; Export plot as SVG file
	(defq width (get :width plot)
		  height (get :height plot)
		  svg (list))

	; SVG header
	(push svg (cat "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"))
	(push svg (cat "<svg xmlns=\"http://www.w3.org/2000/svg\" "
				   "width=\"" (str width) "\" "
				   "height=\"" (str height) "\" "
				   "viewBox=\"0 0 " (str width) " " (str height) "\">\n"))

	; Background
	(push svg (cat "  <rect x=\"0\" y=\"0\" "
				   "width=\"" (str width) "\" "
				   "height=\"" (str height) "\" "
				   "fill=\"#000000\"/>\n"))

	; Add title if present
	(when (get :title plot)
		(push svg (cat "  <text x=\"" (str (/ width 2)) "\" y=\"30\" "
					   "fill=\"#ffffff\" "
					   "font-size=\"20\" "
					   "font-family=\"Arial\" "
					   "text-anchor=\"middle\">"
					   (get :title plot)
					   "</text>\n")))

	; Draw grid if enabled
	(when (get :show_grid plot)
		(bind '(px py pw ph) (plot-get-plot-area plot))
		(defq grid_color (svg-color-to-hex +plot_grid_color)
			  grid_opacity (svg-color-opacity +plot_grid_color))

		; Vertical grid lines
		(each (lambda (i)
			(defq x (+ px (* i (/ pw (dec (get :x_ticks plot))))))
			(push svg (cat "  <line x1=\"" (str x) "\" y1=\"" (str py) "\" "
						   "x2=\"" (str x) "\" y2=\"" (str (+ py ph)) "\" "
						   "stroke=\"" grid_color "\" "
						   "stroke-opacity=\"" grid_opacity "\" "
						   "stroke-width=\"1\"/>\n")))
			(range 0 (get :x_ticks plot)))

		; Horizontal grid lines
		(each (lambda (i)
			(defq y (+ py (* i (/ ph (dec (get :y_ticks plot))))))
			(push svg (cat "  <line x1=\"" (str px) "\" y1=\"" (str y) "\" "
						   "x2=\"" (str (+ px pw)) "\" y2=\"" (str y) "\" "
						   "stroke=\"" grid_color "\" "
						   "stroke-opacity=\"" grid_opacity "\" "
						   "stroke-width=\"1\"/>\n")))
			(range 0 (get :y_ticks plot))))

	; Draw axes
	(bind '(px py pw ph) (plot-get-plot-area plot))
	(defq axis_color (svg-color-to-hex +plot_axis_color))

	; X-axis
	(push svg (cat "  <line x1=\"" (str px) "\" y1=\"" (str (+ py ph)) "\" "
				   "x2=\"" (str (+ px pw)) "\" y2=\"" (str (+ py ph)) "\" "
				   "stroke=\"" axis_color "\" "
				   "stroke-width=\"2\"/>\n"))

	; Y-axis
	(push svg (cat "  <line x1=\"" (str px) "\" y1=\"" (str py) "\" "
				   "x2=\"" (str px) "\" y2=\"" (str (+ py ph)) "\" "
				   "stroke=\"" axis_color "\" "
				   "stroke-width=\"2\"/>\n"))

	; Draw all series
	(each (lambda (series)
		(bind '(type data opts) series)
		(case type
			(:line (push svg (svg-export-line plot data opts)))
			(:scatter (push svg (svg-export-scatter plot data opts)))
			(:bar (push svg (svg-export-bar plot data opts)))
			(:area (push svg (svg-export-area plot data opts)))
			(:pie (push svg (svg-export-pie plot data opts)))))
		(get :series plot))

	; Draw legend
	(when (and (get :show_legend plot) (> (length (get :legend_items plot)) 0))
		(bind '(px py pw ph) (plot-get-plot-area plot))
		(defq lx (+ px pw +plot_label_offset)
			  ly py
			  items (get :legend_items plot))

		; Legend background
		(push svg (cat "  <rect x=\"" (str lx) "\" y=\"" (str ly) "\" "
					   "width=\"" (str +plot_legend_width) "\" "
					   "height=\"" (str (* (length items) +plot_legend_item_height)) "\" "
					   "fill=\"#202020\" "
					   "fill-opacity=\"0.5\"/>\n"))

		; Legend items
		(each (lambda (item)
			(bind '(label color) item)
			(defq iy (+ ly (* (inc (!)) +plot_legend_item_height))
				  color_hex (svg-color-to-hex color))
			; Color box
			(push svg (cat "  <rect x=\"" (str (+ lx 10)) "\" y=\"" (str (- iy 15)) "\" "
						   "width=\"20\" height=\"15\" "
						   "fill=\"" color_hex "\"/>\n"))
			; Label text (would need proper text rendering)
			(push svg (cat "  <text x=\"" (str (+ lx 35)) "\" y=\"" (str (- iy 5)) "\" "
						   "fill=\"#ffffff\" "
						   "font-size=\"12\" "
						   "font-family=\"Arial\">"
						   label
						   "</text>\n")))
			items))

	; SVG footer
	(push svg "</svg>\n")

	; Write to file
	(catch
		(with-open-stream (s (file-stream-out filename))
			(write-line s (apply cat svg)))
		:t))

;;;;;;;;;;;;;;;;
; Series Export Functions
;;;;;;;;;;;;;;;;

(defun svg-export-line (plot data opts)
	; (svg-export-line plot data opts) -> svg_str
	(defq color (svg-color-to-hex (get :color opts))
		  points (path))

	; Convert data points to screen coordinates
	(each (lambda (pt)
		(bind '(x y) pt)
		(bind '(sx sy) (plot-to-screen plot x y))
		(push points sx sy)) data)

	; Build SVG polyline
	(defq point_str (apply cat
		(map (lambda (i)
			(cat (str (elem i points)) "," (str (elem (inc i) points)) " "))
			(filter even? (range 0 (length points))))))

	(cat "  <polyline points=\"" point_str "\" "
		 "fill=\"none\" "
		 "stroke=\"" color "\" "
		 "stroke-width=\"2\" "
		 "stroke-linejoin=\"round\" "
		 "stroke-linecap=\"round\"/>\n"))

(defun svg-export-scatter (plot data opts)
	; (svg-export-scatter plot data opts) -> svg_str
	(defq color (svg-color-to-hex (get :color opts))
		  size (opt (def? :size opts) 4)
		  result (list))

	; Draw each point as a circle
	(each (lambda (pt)
		(bind '(x y) pt)
		(bind '(sx sy) (plot-to-screen plot x y))
		(push result (cat "  <circle cx=\"" (str sx) "\" cy=\"" (str sy) "\" "
						  "r=\"" (str size) "\" "
						  "fill=\"" color "\"/>\n")))
		data)

	(apply cat result))

(defun svg-export-bar (plot data opts)
	; (svg-export-bar plot data opts) -> svg_str
	(defq color (svg-color-to-hex (get :color opts))
		  n (length data)
		  result (list))

	; Calculate bar width
	(bind '(px py pw ph) (plot-get-plot-area plot))
	(defq bar_width (/ pw n 1.2))

	; Draw each bar
	(each (lambda (pt)
		(bind '(x y) pt)
		(bind '(sx sy) (plot-to-screen plot x y))
		(bind '(sx0 sy0) (plot-to-screen plot x (get :y_min plot)))
		(defq bar_height (- sy0 sy)
			  bar_x (- sx (/ bar_width 2)))
		(push result (cat "  <rect x=\"" (str bar_x) "\" y=\"" (str sy) "\" "
						  "width=\"" (str bar_width) "\" "
						  "height=\"" (str bar_height) "\" "
						  "fill=\"" color "\"/>\n")))
		data)

	(apply cat result))

(defun svg-export-area (plot data opts)
	; (svg-export-area plot data opts) -> svg_str
	(defq color (get :color opts)
		  color_hex (svg-color-to-hex color)
		  opacity (svg-color-opacity color)
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

		; Build SVG polygon
		(defq point_str (apply cat
			(map (lambda (i)
				(cat (str (elem i points)) "," (str (elem (inc i) points)) " "))
				(filter even? (range 0 (length points))))))

		(cat "  <polygon points=\"" point_str "\" "
			 "fill=\"" color_hex "\" "
			 "fill-opacity=\"" opacity "\"/>\n")))

(defun svg-export-pie (plot data opts)
	; (svg-export-pie plot data opts) -> svg_str
	; Draw a pie chart in SVG
	(defq colors (get :colors plot)
		  result (list))

	; Calculate total and center
	(defq total (reduce + (map (lambda (item) (second item)) data) 0)
		  cx (/ (get :width plot) 2)
		  cy (/ (get :height plot) 2)
		  radius (* (min cx cy) 0.6)
		  start_angle 0.0)

	; Draw each slice
	(each (lambda (item)
		(bind '(label value) item)
		(defq angle (* +fp_2pi (/ value total))
			  end_angle (+ start_angle angle)
			  color (svg-color-to-hex (elem (% (inc (!)) (length colors)) colors)))

		; Calculate arc points
		(defq x1 (+ cx (* radius (cos start_angle)))
			  y1 (+ cy (* radius (sin start_angle)))
			  x2 (+ cx (* radius (cos end_angle)))
			  y2 (+ cy (* radius (sin end_angle)))
			  large_arc (if (> angle +fp_pi) 1 0))

		; SVG path for wedge
		(push result (cat "  <path d=\"M " (str cx) " " (str cy)
						  " L " (str x1) " " (str y1)
						  " A " (str radius) " " (str radius) " 0 " (str large_arc) " 1 " (str x2) " " (str y2)
						  " Z\" "
						  "fill=\"" color "\"/>\n"))

		(setq start_angle end_angle))
		data)

	(apply cat result))
