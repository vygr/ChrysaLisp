# Testing Guide for ChrysaLisp Plotting Library

This document outlines the testing procedures required to verify the plotting library meets the standards specified in `CONTRIBUTIONS.md`.

## Overview

The plotting library consists of:
- `lib/plot/plot.inc` - Main plotting engine (~900 lines)
- `lib/plot/svg.inc` - SVG export functionality (~260 lines)
- `lib/plot/data.inc` - Data processing helpers (~234 lines)
- `apps/plots/app.lisp` - Comprehensive demo application (~350 lines)

Total: ~1,750 lines of new code implementing 14 different plot types.

## Testing Requirements

Per `CONTRIBUTIONS.md`, all contributions must meet these requirements:

### 1. Functionality Verification

The plotting library must be verified through the GUI demo application.

#### Running the Demo Application

```bash
./run.sh apps/plots/app.lisp
```

#### Expected Behavior

The demo should display a window titled "Plot Demo - Line Plot" showing a sine wave with proper:
- Axis labels ("X" and "sin(X)")
- Grid lines
- Legend showing "sin(x)" and "cos(x)"
- Smooth antialiased lines
- Numeric tick labels on both axes

#### Interactive Controls

Test all keyboard controls:

| Key | Action | Expected Result |
|-----|--------|-----------------|
| SPACE | Next plot | Window cycles to next plot type, title updates |
| N | Next plot | Same as SPACE |
| → | Next plot | Same as SPACE |
| S | Save CPM | Saves current plot as `plot_N.cpm`, prints filename |
| E | Export SVG | Exports current plot as `plot_N.svg`, prints filename |

#### All 14 Plot Types to Verify

Cycle through all plots and verify each renders correctly:

1. **Line Plot** - Two smooth curves (sin and cos)
2. **Scatter Plot** - Random points with circle markers
3. **Marker Shapes** - Six different marker shapes (circle, square, triangle, diamond, cross, plus)
4. **Error Bars** - Scatter plot with vertical error bars
5. **Bar Chart** - Vertical bars with varying heights
6. **Horizontal Bars** - Horizontal bars extending from Y-axis
7. **Stacked Bars** - Vertical bars with 3 colored segments each
8. **Area Plot** - Filled area under a curve
9. **Pie Chart** - Circular chart with 5 colored segments and labels
10. **Histogram** - Distribution bars showing normal distribution
11. **Box Plot** - 4 box plots showing quartiles and outliers
12. **Heatmap** - 2D color gradient grid (hot colormap)
13. **Candlestick** - Financial chart with open/high/low/close bars
14. **Multi-Series** - Combined line and area plots

#### Visual Quality Checklist

For each plot type, verify:

- [ ] Title displays correctly at top
- [ ] Axis labels are readable and properly positioned
- [ ] Tick labels show appropriate numeric values
- [ ] Grid lines (if enabled) are visible and evenly spaced
- [ ] Legend (if present) shows correct colors and labels
- [ ] Colors are distinct and visually appealing
- [ ] No clipping or overflow issues
- [ ] Antialiasing produces smooth curves and shapes
- [ ] Text is readable and properly aligned

### 2. Build Verification

The plotting library must not break the deterministic build system.

#### Standard Build Test

From within ChrysaLisp TUI or GUI Terminal:

```bash
make it
```

**Expected Result:**
- All files compile successfully
- No warnings or errors
- Boot images generate correctly
- Documentation builds successfully

#### Installation Verification

After successful `make it`:

```bash
make snapshot
make install
```

**Expected Result:**
- `snapshot.zip` contains all new plotting files
- `make install` completes without errors
- System boots successfully from snapshot
- Demo application runs correctly

#### Binary Reproducibility

The plotting library must build reproducibly:

```bash
# First build
make it
make snapshot
make install

# Copy obj/ directory
cp -r obj/ obj.first/

# Second build
make it

# Compare
diff -r obj/ obj.first/
```

**Expected Result:**
- No differences between builds
- All files in `obj/` are bit-for-bit identical

### 3. Emulator Mode Testing

Test in VP64 emulated mode:

```bash
./run_tui.sh -e
```

Then within the emulated environment:

```bash
make it
make snapshot
make install
```

**Expected Result:**
- Same results as native mode (only slower)
- Binary reproducibility maintained
- Demo application runs correctly

### 4. Platform Testing

If changes affect Canvas, Path, or font rendering (which this library uses), test on all supported platforms:

- Linux (x86_64)
- macOS (x86_64, ARM64)
- Windows (x86_64)
- BSD variants

**Expected Result:**
- Demo app runs identically on all platforms
- SVG export produces identical files
- No platform-specific rendering issues

## Feature-Specific Testing

### Text Rendering

Test proper text rendering using fonts:

```lisp
(import "lib/plot/plot.inc")

; Create simple plot with text
(defq plot (Plot 800 600 "Text Rendering Test"))
(def plot :x_label "X Axis Label" :y_label "Y Axis Label")
(plot-add-series plot :line '((0 0) (1 1)) "Test Data")
(plot-render plot)
```

**Verify:**
- [ ] Title appears at top center
- [ ] X-axis label appears at bottom center
- [ ] Y-axis label appears at left (rotated 90°)
- [ ] Tick labels show numeric values
- [ ] Legend shows "Test Data" with correct color
- [ ] All text is crisp and readable

### Error Bars

Test symmetric and asymmetric error bars:

```lisp
; Symmetric errors: ((x y error) ...)
(defq data1 '((1.0 5.0 0.5) (2.0 7.0 0.7) (3.0 6.0 0.4)))

; Asymmetric errors: ((x y error_low error_high) ...)
(defq data2 '((1.0 5.0 0.3 0.7) (2.0 7.0 0.5 0.9) (3.0 6.0 0.2 0.6)))

(defq plot (Plot 800 600 "Error Bars Test"))
(defq opts (env :error_bars :t))
(push (get :series plot) (list :scatter data1 opts))
(plot-render plot)
```

**Verify:**
- [ ] Vertical error bars extend symmetrically from points
- [ ] Error bar caps (horizontal lines) are visible
- [ ] Error bars match marker positions

### Box Plots

Test quartile calculation and outlier detection:

```lisp
(defq data '((1.0 (4.5 5.0 5.5 6.0 7.0 5.2 5.8))
             (2.0 (6.0 7.0 7.5 8.0 6.5 7.2 7.8))
             (3.0 (5.0 6.0 6.5 7.5 8.0 6.2 15.0))))  ; 15.0 is outlier

(defq plot (Plot 800 600 "Box Plot Test"))
(plot-add-series plot :box data)
(plot-render plot)
```

**Verify:**
- [ ] Box shows Q1, median, Q3
- [ ] Whiskers extend to min/max within 1.5×IQR
- [ ] Outliers plotted as individual points (e.g., 15.0 in third box)
- [ ] Median line is clearly visible

### Heatmaps

Test all four colormaps:

```lisp
(defq data '((1.0 2.0 3.0) (4.0 5.0 6.0) (7.0 8.0 9.0)))

; Test each colormap
(each (lambda (cmap)
    (defq plot (Plot 800 600 (cat "Heatmap - " (str cmap))))
    (def plot :show_grid :nil)
    (defq opts (env :colormap cmap))
    (push (get :series plot) (list :heatmap data opts))
    (plot-render plot))
    '(:hot :cool :jet :grayscale))
```

**Verify:**
- [ ] `:hot` - Black → Red → Yellow → White gradient
- [ ] `:cool` - Cyan → Magenta gradient
- [ ] `:jet` - Blue → Cyan → Yellow → Red gradient
- [ ] `:grayscale` - Black → White gradient
- [ ] Color transitions are smooth
- [ ] Values map correctly (low=dark, high=bright for most)

### SVG Export

Test SVG generation for all plot types:

```bash
# In demo app, press 'E' for each plot type
# Verify 14 SVG files are created: plot_0.svg through plot_13.svg
```

**Verify each SVG file:**
- [ ] Opens in web browser
- [ ] Renders correctly (matches Canvas version)
- [ ] Text is selectable and searchable
- [ ] Colors match original plot
- [ ] Scales properly when browser window resized
- [ ] File size is reasonable (typically 10-50 KB)

### Data Processing Functions

Test statistical functions:

```lisp
(import "lib/plot/data.inc")

; Test mean
(plot-mean '(1.0 2.0 3.0 4.0 5.0))  ; Should be 3.0

; Test median
(plot-median '(1.0 2.0 3.0 4.0 5.0))  ; Should be 3.0
(plot-median '(1.0 2.0 3.0 4.0))      ; Should be 2.5

; Test standard deviation
(plot-std-dev '(2.0 4.0 4.0 4.0 5.0 5.0 7.0 9.0))  ; Should be ~2.0

; Test linear regression
(plot-fit-linear '((0 0) (1 2) (2 4) (3 6)))  ; Should be ~(2.0 0.0)

; Test normalization
(plot-normalize '(0.0 5.0 10.0))  ; Should be (0.0 0.5 1.0)
```

**Verify:**
- [ ] Results are mathematically correct
- [ ] Functions handle edge cases (empty lists, single values)
- [ ] No crashes or errors

## Performance Testing

While not required by CONTRIBUTIONS.md, performance should be reasonable:

### Rendering Speed

Create a large dataset and measure render time:

```lisp
; Generate 1000 data points
(defq data (map (lambda (x)
    (defq xv (/ x 100.0))
    (list xv (sin xv)))
    (range 0 1000)))

(defq plot (Plot 800 600 "Performance Test"))
(plot-add-series plot :line data)

; Time the render
(defq start (time-stamp))
(plot-render plot)
(defq elapsed (- (time-stamp) start))
(print "Render time: " elapsed " microseconds")
```

**Expected:**
- Renders in under 100ms on modern hardware
- No visible lag or stutter
- Memory usage is reasonable

### Memory Leaks

Run demo application and cycle through all plots multiple times:

```bash
# Watch memory usage
# Cycle through all 14 plots 10+ times
# Memory should stabilize, not grow indefinitely
```

**Expected:**
- Memory usage stable after initial allocation
- No continuous growth indicating leaks
- Reference counting works correctly

## Regression Testing

After any changes to the plotting library, verify:

### Existing Functionality

- [ ] All 14 plot types still render correctly
- [ ] Demo app still runs without errors
- [ ] SVG export still works for all types
- [ ] No visual regressions (colors, layout, alignment)

### Backward Compatibility

- [ ] Existing plot code (from README examples) still works
- [ ] API remains consistent
- [ ] Optional parameters work as before
- [ ] Default values haven't changed

## Known Limitations

Document any known issues or limitations:

1. **Y-axis label rotation**: Currently not rotated 90° (displays horizontally)
2. **Interactive features**: No zooming or panning (not in initial scope)
3. **Date/time axes**: Not yet implemented
4. **Logarithmic scales**: Not yet implemented
5. **3D plots**: Not in scope for this version

## Test Result Documentation

When submitting a PR, include:

1. **Platform tested**: OS and architecture
2. **Build verification**: Output of `make it`
3. **Binary reproducibility**: Confirmation of identical builds
4. **Visual verification**: Screenshots of all 14 plot types
5. **SVG samples**: Sample SVG exports
6. **Performance numbers**: Render times for large datasets

## Automated Testing (Future)

Potential areas for automated tests:

- Unit tests for statistical functions
- Coordinate transformation tests
- Color gradient function tests
- Data format validation tests
- SVG XML structure validation

These could be implemented in `lib/plot/test.inc` in the future.

## Conclusion

This testing guide ensures the plotting library meets ChrysaLisp's high standards for quality, reproducibility, and cross-platform compatibility. All tests should pass before submitting a PR to the main repository.
