# Fractal Explorer

An interactive, high-performance fractal visualization application for ChrysaLisp featuring distributed rendering, multiple fractal types, and beautiful color schemes.

## Features

### Multiple Fractal Types

1. **Julia Set** - Classic Julia set fractals with adjustable parameters
2. **Burning Ship** - The dramatic "Burning Ship" fractal
3. **Newton** - Newton fractal showing convergence basins for z³-1=0
4. **Tricorn** (Mandelbar) - The conjugate Mandelbrot set
5. **Phoenix** - Phoenix fractal with adjustable parameters
6. **Mandelbrot^3** - Third-power Mandelbrot set
7. **Mandelbrot^4** - Fourth-power Mandelbrot set
8. **Mandelbox** - 2D slice of the fascinating Mandelbox fractal
9. **Lyapunov** - Lyapunov fractal visualization

### Color Schemes

Choose from 10 beautiful color palettes:
- **Classic** - Traditional iteration-based coloring
- **Rainbow** - Vibrant rainbow gradient
- **Fire** - Warm reds, oranges, and yellows
- **Ice** - Cool blues and cyans
- **Psychedelic** - Sinusoidal color cycling
- **Ocean** - Deep blues and greens
- **Sunset** - Warm sunset tones
- **Electric** - Bright electric colors
- **Forest** - Natural greens
- **Copper** - Metallic copper tones

### Interactive Controls

#### Mouse Controls
- **Left Click**: Zoom in and re-center at click point
- **Right Click**: Zoom out and re-center at click point
- **Middle Click**: Reset to default view

#### GUI Controls
- **Fractal Type Selector**: Switch between different fractal types
- **Color Scheme Selector**: Change color palettes
- **Parameter Sliders**: Adjust fractal parameters (Julia real/imaginary, Phoenix parameters, etc.)
- **Reset View Button**: Reset zoom and position to defaults
- **Zoom In/Out Buttons**: Manual zoom controls

### Technical Features

- **Distributed Rendering**: Utilizes ChrysaLisp's task farm for parallel computation across CPU cores
- **29-bit Fixed Point Math**: High-precision calculations for deep zooms
- **Real-time Updates**: Interactive parameter adjustment with immediate feedback
- **Scalable Architecture**: Automatically uses all available CPU cores

## Usage

Launch the application:
```
apps/fractalexplorer/app.lisp
```

### Exploring Fractals

1. Select a fractal type from the dropdown menu
2. Choose a color scheme
3. Click on interesting areas to zoom in
4. Use parameter sliders to modify fractal behavior (for Julia and Phoenix sets)
5. Right-click to zoom out
6. Press "Reset View" to return to the default viewpoint

### Tips

- **Julia Set**: Adjust the Real and Imaginary parameters to explore different Julia sets. Classic values include:
  - c = -0.4 + 0.6i (default)
  - c = -0.8 + 0.156i
  - c = -0.7269 + 0.1889i

- **Newton Fractal**: Look for the three-fold symmetry showing convergence to the three roots of z³-1=0

- **Burning Ship**: The "ship" shape appears in the negative y region - try centering at (-0.5, -0.5)

- **Mandelbox**: Adjust the Scale parameter to see different structural features

## Implementation Details

The application demonstrates several ChrysaLisp concepts:

- **Distributed Computing**: Uses the Farm pattern to distribute rendering across worker tasks
- **Fixed-Point Arithmetic**: Custom 29-bit fixed-point math for fractal calculations
- **GUI Services**: Rich interactive UI with sliders, selectors, and canvas rendering
- **JIT Compilation**: Native code generation for performance-critical functions
- **Task Slicing**: Cooperative multitasking during rendering

## Architecture

```
app.lisp       - Main application, GUI, and event handling
child.lisp     - Distributed worker for fractal computation
app.inc        - Shared definitions, math utilities, and color schemes
```

Each scanline of the fractal is rendered as an independent job, distributed across worker processes that can run on different CPU cores or even different nodes in a ChrysaLisp network.

## Performance

The application can render an 800x800 pixel fractal in real-time, scaling performance with the number of available CPU cores. The distributed architecture allows for:

- Automatic load balancing
- Fault tolerance (failed workers are automatically restarted)
- Efficient use of multi-core systems
- Progressive rendering with immediate visual feedback

## Future Enhancements

Potential additions:
- Animation mode (parameter sweeps)
- Export to image files
- L-system fractals (different rendering approach)
- IFS fractals (Sierpinski, Barnsley Fern)
- Orbit traps for advanced coloring
- Continuous coloring algorithms
- Deeper zoom capabilities with arbitrary precision

---

*Created as a demonstration of ChrysaLisp's capabilities for interactive, distributed computation and rich GUI applications.*
