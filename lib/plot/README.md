# ChrysaLisp Plotting Library

A comprehensive plotting and charting library for ChrysaLisp, inspired by vgplot (Common Lisp) and Oz (Clojure).

## Features

- **Multiple Plot Types**:
  - Line plots
  - Scatter plots
  - Bar charts
  - Area plots
  - Pie charts
  - Multi-series plots

- **Interactive Canvas**: Built on ChrysaLisp's GUI Canvas with antialiasing support

- **Customizable**:
  - Configurable colors
  - Grid and axis display
  - Legend support
  - Auto-scaling or manual axis ranges

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

See `apps/plots/app.lisp` for complete working examples of:

- Single and multi-series line plots
- Scatter plots with random data
- Bar charts with categorical data
- Area plots with filled regions
- Pie charts with labeled segments
- Mixed plot types on the same axes

## Future Enhancements

Possible extensions (contributions welcome):

- Text rendering for axis labels and titles
- Logarithmic scales
- 3D plots
- Contour plots
- Heatmaps
- Error bars
- Interactive zooming and panning
- Real-time data updates
- Histogram plots
- Box plots

## Credits

Inspired by:
- **vgplot** (Common Lisp) - Simple, elegant plotting
- **Oz** (Clojure) - Declarative visualization

Built on ChrysaLisp's powerful Canvas and Path primitives.

## License

Part of the ChrysaLisp project. See main repository for license information.
