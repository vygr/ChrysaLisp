# ChrysaLisp Plotting Library

A comprehensive plotting and charting library for ChrysaLisp, inspired by vgplot (Common Lisp) and Oz (Clojure).

## Features

- **Multiple Plot Types**:
  - Line plots (with error bars)
  - Scatter plots (with 6 marker shapes and error bars)
  - Bar charts (vertical, horizontal, and stacked)
  - Area plots
  - Pie charts
  - Histograms
  - Box plots (with quartiles and outliers)
  - Heatmaps (with color gradients)
  - Candlestick charts (for financial data)
  - Multi-series plots

- **Text Rendering**: Proper axis labels, tick labels, titles, and legend text using ChrysaLisp fonts

- **Interactive Canvas**: Built on ChrysaLisp's GUI Canvas with antialiasing support

- **Marker Shapes**: Circle, square, triangle, diamond, cross, and plus markers for scatter plots

- **Data Processing**: Built-in helper functions for:
  - Statistical analysis (mean, median, std dev)
  - Data smoothing and normalization
  - Linear regression and curve fitting
  - Histogram binning
  - Peak detection

- **Customizable**:
  - Configurable colors
  - Grid and axis display
  - Legend support
  - Auto-scaling or manual axis ranges
  - Multiple marker shapes and sizes

- **Export Capabilities**:
  - Save as CPM (ChrysaLisp Pixmap)
  - Export to SVG

## Installation

The plotting library is located in `lib/plot/`. To use it in your application:

```lisp
(import "lib/plot/plot.inc")
```

## Quick Start

### Line Plot

```lisp
; Create data points
(defq data (map (lambda (x)
    (defq xv (/ x 10.0))
    (list xv (sin xv)))
    (range 0 63)))

; Create and render plot
(defq plot (plot-line data 800 600 "Sine Wave"))
```

### Scatter Plot

```lisp
; Create random data
(defq data (map (lambda (_)
    (list (random) (random)))
    (range 0 50)))

; Create plot
(defq plot (plot-scatter data 800 600 "Random Points"))
```

### Bar Chart

```lisp
; Create data
(defq data (list
    (list 1.0 10.0)
    (list 2.0 15.0)
    (list 3.0 7.0)
    (list 4.0 20.0)))

; Create plot
(defq plot (plot-bar data 800 600 "Bar Chart"))
```

### Area Plot

```lisp
; Create data
(defq data (map (lambda (x)
    (defq xv (/ x 5.0))
    (list xv (+ 5.0 (* 3.0 (sin xv)))))
    (range 0 31)))

; Create plot
(defq plot (plot-area data 800 600 "Area Plot"))
```

### Pie Chart

```lisp
; Create labeled data
(defq data (list
    (list "Category A" 30.0)
    (list "Category B" 20.0)
    (list "Category C" 25.0)
    (list "Category D" 15.0)
    (list "Category E" 10.0)))

; Create plot
(defq plot (plot-pie data 800 600 "Distribution"))
```

### Histogram

```lisp
; Generate random data
(defq values (map (lambda (_) (+ 5.0 (* 2.0 (- (random) 0.5))))
    (range 0 100)))

; Create histogram with 20 bins
(defq plot (plot-histogram values 20 800 600 "Data Distribution"))
```

### Horizontal Bar Chart

```lisp
; Create data (value, category pairs)
(defq data (list
    (list 25.0 1.0)
    (list 45.0 2.0)
    (list 30.0 3.0)
    (list 60.0 4.0)))

; Create plot
(defq plot (Plot 800 600 "Performance Metrics"))
(def plot :x_label "Score" :y_label "Category")
(plot-add-series plot :hbar data "Scores")
(plot-render plot)
```

### Scatter Plot with Marker Shapes

```lisp
; Create data
(defq data (map (lambda (x)
    (list x (+ 5.0 (* 0.5 (sin x)))))
    (range 0 20)))

; Create plot with diamond markers
(defq plot (Plot 800 600 "Scatter with Markers"))
(defq series_opts (env :color +argb_cyan :marker :diamond :size 6 :label "Data"))
(push (get :series plot) (list :scatter data series_opts))
(push (get :legend_items plot) (list "Data" +argb_cyan))
(plot-render plot)
```

Available marker shapes: `:circle`, `:square`, `:triangle`, `:diamond`, `:cross`, `:plus`

### Error Bars

```lisp
; Data with symmetric error bars: ((x y error) ...)
(defq data (list
    (list 1.0 5.0 0.5)
    (list 2.0 7.0 0.7)
    (list 3.0 6.0 0.4)
    (list 4.0 8.0 0.6)))

; Create plot with error bars
(defq plot (Plot 800 600 "Measurements with Uncertainty"))
(defq series_opts (env :color +argb_cyan :error_bars :t))
(push (get :series plot) (list :scatter data series_opts))
(plot-render plot)
```

### Box Plot

```lisp
; Data format: ((x_position values_list) ...)
(defq data (list
    (list 1.0 '(4.5 5.0 5.5 6.0 7.0 5.2 5.8))
    (list 2.0 '(6.0 7.0 7.5 8.0 6.5 7.2 7.8))
    (list 3.0 '(5.0 6.0 6.5 7.5 8.0 6.2 15.0))))  ; 15.0 is an outlier

; Create box plot
(defq plot (Plot 800 600 "Distribution Comparison"))
(def plot :x_label "Group" :y_label "Value")
(plot-add-series plot :box data "Distributions")
(plot-render plot)
```

### Stacked Bar Chart

```lisp
; Data format: ((x_position (val1 val2 val3...)) ...)
(defq data (list
    (list 1.0 (list 10.0 15.0 8.0))
    (list 2.0 (list 12.0 18.0 10.0))
    (list 3.0 (list 8.0 20.0 12.0))))

; Create stacked bar chart
(defq plot (Plot 800 600 "Components Over Time"))
(defq series_opts (env :label "Stacked"))
(push (get :series plot) (list :stacked_bar data series_opts))
(plot-render plot)
```

### Heatmap

```lisp
; Data format: 2D list ((row1...) (row2...) ...)
(defq data (list
    (list 1.0 2.0 3.0 4.0 5.0)
    (list 2.0 4.0 6.0 8.0 10.0)
    (list 3.0 6.0 9.0 12.0 15.0)))

; Create heatmap with color gradient
(defq plot (Plot 800 600 "2D Data Heatmap"))
(def plot :show_grid :nil)
(defq series_opts (env :colormap :hot))  ; Options: :hot :cool :jet :grayscale
(push (get :series plot) (list :heatmap data series_opts))
(plot-render plot)
```

### Candlestick Chart

```lisp
; Data format: ((x open high low close) ...)
(defq data (list
    (list 1.0 100.0 105.0 98.0 103.0)
    (list 2.0 103.0 108.0 102.0 104.0)
    (list 3.0 104.0 106.0 99.0 101.0)))

; Create candlestick chart
(defq plot (Plot 800 600 "Stock Price"))
(def plot :x_label "Time" :y_label "Price")
(plot-add-series plot :candlestick data "Price")
(plot-render plot)
```

## Advanced Usage

### Custom Plot with Multiple Series

```lisp
; Create a custom plot
(defq plot (Plot 800 600 "Multi-Series Plot"))

; Configure plot
(def plot
    :x_label "Time"
    :y_label "Value"
    :x_min 0.0
    :x_max 10.0
    :y_min -2.0
    :y_max 2.0
    :show_grid :t
    :show_legend :t)

; Add first series
(plot-add-series plot :line
    (map (lambda (x)
        (defq xv (/ x 10.0))
        (list xv (sin xv)))
        (range 0 100))
    "sin(x)")

; Add second series
(plot-add-series plot :line
    (map (lambda (x)
        (defq xv (/ x 10.0))
        (list xv (cos xv)))
        (range 0 100))
    "cos(x)")

; Render the plot
(plot-render plot)
```

### Saving and Exporting

```lisp
; Save as CPM (ChrysaLisp Pixmap)
(plot-save plot "my_plot.cpm")

; Export to SVG
(import "lib/plot/svg.inc")
(plot-export-svg plot "my_plot.svg")
```

### Manual Axis Scaling

```lisp
; Create plot
(defq plot (Plot 800 600 "Custom Scale"))

; Disable auto-scaling
(def plot :auto_scale :nil)

; Set manual ranges
(def plot
    :x_min 0.0
    :x_max 100.0
    :y_min -50.0
    :y_max 50.0)

; Add data and render
(plot-add-series plot :line data)
(plot-render plot)
```

### Customizing Colors

```lisp
; Create plot
(defq plot (Plot 800 600 "Custom Colors"))

; Set custom color palette
(def plot :colors (list
    +argb_cyan
    +argb_yellow
    +argb_magenta
    +argb_green
    +argb_red
    +argb_orange))

; Add series (will use colors in order)
(plot-add-series plot :line data1)
(plot-add-series plot :line data2)

(plot-render plot)
```

### Hiding Grid or Legend

```lisp
; Create plot
(defq plot (Plot 800 600 "Clean Plot"))

; Configure display options
(def plot
    :show_grid :nil
    :show_legend :nil)

(plot-add-series plot :line data)
(plot-render plot)
```

## API Reference

### Core Functions

#### `(Plot width height [title])`
Create a new plot object.
- `width`: Width in pixels
- `height`: Height in pixels
- `title`: Optional plot title
- Returns: Plot object

#### `(plot-add-series plot type data [label])`
Add a data series to the plot.
- `plot`: Plot object
- `type`: Type of plot (`:line`, `:scatter`, `:bar`, `:area`, `:pie`)
- `data`: Data points (format depends on type)
- `label`: Optional series label for legend
- Returns: Plot object

#### `(plot-render plot)`
Render the complete plot.
- `plot`: Plot object
- Returns: Plot object

#### `(plot-save plot filename)`
Save plot to file as CPM format.
- `plot`: Plot object
- `filename`: Output filename
- Returns: `:t` on success, `:nil` on failure

### Quick Plot Functions

#### `(plot-line data [width height title])`
Create and render a line plot.

#### `(plot-scatter data [width height title])`
Create and render a scatter plot.

#### `(plot-bar data [width height title])`
Create and render a bar chart.

#### `(plot-area data [width height title])`
Create and render an area plot.

#### `(plot-pie data [width height title])`
Create and render a pie chart.

### Data Formats

**Line, Scatter, Bar, Area Plots:**
```lisp
((x1 y1) (x2 y2) (x3 y3) ...)
```

**Pie Charts:**
```lisp
((label1 value1) (label2 value2) ...)
```

### Plot Properties

Set properties using `def`:

- `:width`, `:height`: Plot dimensions
- `:title`: Plot title
- `:x_label`, `:y_label`: Axis labels
- `:x_min`, `:x_max`, `:y_min`, `:y_max`: Axis ranges
- `:auto_scale`: Enable/disable auto-scaling (`:t` or `:nil`)
- `:show_grid`: Show/hide grid (`:t` or `:nil`)
- `:show_legend`: Show/hide legend (`:t` or `:nil`)
- `:colors`: List of colors for series
- `:x_ticks`, `:y_ticks`: Number of tick marks

### SVG Export

Import the SVG module:
```lisp
(import "lib/plot/svg.inc")
```

Then use:
```lisp
(plot-export-svg plot "filename.svg")
```

### Statistical Annotations

Add statistical reference lines to plots:

```lisp
; Create scatter plot
(defq data (generate-some-data))
(defq plot (Plot 800 600 "Data with Statistics"))
(plot-add-series plot :scatter data "Data")

; Add mean line
(plot-add-mean-line plot data)

; Add median line
(plot-add-median-line plot data)

(plot-render plot)
```

### Color Gradients for Heatmaps

Available colormaps:
- `:hot` - Black → Red → Yellow → White
- `:cool` - Cyan → Magenta
- `:jet` - Blue → Cyan → Yellow → Red
- `:grayscale` - Black → White (default)

### Data Processing Helpers

The library includes many data processing functions in `lib/plot/data.inc`:

#### Statistical Functions

```lisp
; Calculate statistics
(defq values '(1.0 2.0 3.0 4.0 5.0))
(plot-mean values)      ; -> 3.0
(plot-median values)    ; -> 3.0
(plot-std-dev values)   ; -> standard deviation
```

#### Data Transformation

```lisp
; Smooth data with moving average
(defq smooth_data (plot-smooth-data data 5))

; Normalize to [0, 1] range
(defq normalized (plot-normalize values))

; Resample to fewer points
(defq resampled (plot-resample data 50))
```

#### Data Generation

```lisp
; Generate linearly spaced values
(defq xs (plot-linspace 0.0 10.0 100))

; Generate function data
(defq sine_data (plot-function sin 0.0 +fp_2pi 100))

; Combine x and y lists
(defq data (plot-combine-xy xs ys))

; Add noise to data
(defq noisy_data (plot-add-noise clean_data 0.1))
```

#### Statistical Analysis

```lisp
; Find peaks in data
(defq peaks (plot-find-peaks data 5.0))

; Linear regression
(defq fit (plot-fit-linear data))  ; -> (slope intercept)
(defq fit_line (plot-apply-linear-fit data fit))

; Plot original data and fit line
(defq plot (Plot 800 600 "Linear Fit"))
(plot-add-series plot :scatter data "Data")
(plot-add-series plot :line fit_line "Fit")
(plot-render plot)
```

## Demo Application

A comprehensive demo application is available in `apps/plots/`. Run it to see examples of all plot types:

```bash
./run.sh apps/plots/app.lisp
```

### Demo Controls

- **SPACE/N/→**: Cycle through different plot types
- **S**: Save current plot as CPM
- **E**: Export current plot as SVG

## Architecture

The plotting library leverages ChrysaLisp's existing capabilities:

- **Canvas**: Hardware-accelerated drawing with antialiasing
- **Path System**: Bezier curves, arcs, and polygon rendering
- **Event System**: Interactive plots with mouse/keyboard input
- **Streams**: File I/O for saving and exporting

### File Structure

```
lib/plot/
├── plot.inc      - Main plotting library
├── svg.inc       - SVG export functionality
├── data.inc      - Data processing helpers
└── README.md     - This file

apps/plots/
└── app.lisp      - Demo application
```

## Performance

The plotting library is optimized for ChrysaLisp's performance characteristics:

- **O(1) Function Calls**: Uses pre-bound methods
- **Vectorized Operations**: Leverages fixeds arrays for paths
- **Hardware Acceleration**: Canvas rendering uses native code
- **Memory Efficient**: Minimal allocations, reference counting

## Examples

See `apps/plots/app.lisp` for complete working examples of **14 different plot types**:

- Line plots (single and multi-series)
- Scatter plots with various marker shapes
- Scatter plots with error bars
- Vertical bar charts
- Horizontal bar charts
- Stacked bar charts
- Area plots with filled regions
- Pie charts with labeled segments
- Histograms with automatic binning
- Box plots with quartiles and outliers
- Heatmaps with color gradients
- Candlestick charts for financial data
- Statistical annotations (mean/median lines)
- Mixed plot types on the same axes

## Future Enhancements

Possible extensions (contributions welcome):

- Logarithmic scales
- 3D surface plots
- Contour plots
- Violin plots
- Polar coordinate plots
- Interactive zooming and panning with mouse
- Real-time streaming data updates
- Date/time axis formatting
- Multiple subplots in grid layout
- Rotated Y-axis labels
- Network/graph visualization
- Radar/spider charts
- Waterfall charts
- Sankey diagrams

## Credits

Inspired by:
- **vgplot** (Common Lisp) - Simple, elegant plotting
- **Oz** (Clojure) - Declarative visualization

Built on ChrysaLisp's powerful Canvas and Path primitives.

## License

Part of the ChrysaLisp project. See main repository for license information.
