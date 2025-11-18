# OpenGL Port for ChrysaLisp

This document describes the OpenGL integration into ChrysaLisp, providing 3D graphics capabilities to the system.

## Architecture Overview

The OpenGL port follows ChrysaLisp's standard Platform Independent Interface (PII) pattern, consisting of four layers:

### 1. C++ Host Layer (`src/host/gl_host.cpp`)
- Implements OpenGL function wrappers in C++
- Manages OpenGL state, textures, buffers, and resources
- Provides a function table exported to the VP layer
- Uses handle-based resource management (similar to audio_sdl.cpp)

### 2. VP Class Definitions (`sys/opengl/class.inc`)
- Defines the `host_gl` class with virtual method declarations
- Includes helper function `host-gl-call` for ABI dispatch
- Defines OpenGL constants (GL_TRIANGLES, GL_COLOR_BUFFER_BIT, etc.)

### 3. VP Method Implementations (`sys/opengl/class.vp`)
- Implements VP wrapper methods that bridge to C++ functions
- Handles register management and ABI calling conventions
- Examples: `gl_init`, `gl_clear`, `gl_vertex3f`, etc.

### 4. Lisp Interface (`sys/opengl/lisp.vp`, `sys/opengl/lisp.inc`)
- Provides Lisp-level functions for OpenGL operations
- Handles type checking and error reporting
- Lisp macros for convenient OpenGL calls

## OpenGL Functions Implemented

### Initialization
- `gl-init` - Initialize OpenGL state
- `gl-deinit` - Clean up OpenGL resources

### Viewport and Clearing
- `gl-viewport (x y width height)` - Set viewport dimensions
- `gl-clear (mask)` - Clear buffers (color, depth, stencil)
- `gl-clear-color (r g b a)` - Set clear color
- `gl-clear-depth (depth)` - Set clear depth value

### State Management
- `gl-enable (cap)` - Enable OpenGL capability
- `gl-disable (cap)` - Disable OpenGL capability

### Matrix Operations
- `gl-matrix-mode (mode)` - Select matrix mode
- `gl-load-identity` - Load identity matrix
- `gl-push-matrix` - Push matrix onto stack
- `gl-pop-matrix` - Pop matrix from stack

### Transformations
- `gl-translate-f (x y z)` - Apply translation
- `gl-rotate-f (angle x y z)` - Apply rotation
- `gl-scale-f (x y z)` - Apply scaling

### Projections
- `gl-ortho (left right bottom top near far)` - Orthographic projection
- `gl-frustum (left right bottom top near far)` - Frustum projection
- `gl-perspective (fovy aspect znear zfar)` - Perspective projection (GLU)

### Drawing Primitives
- `gl-begin (mode)` - Begin primitive drawing
- `gl-end` - End primitive drawing
- `gl-vertex2f (x y)` - 2D vertex
- `gl-vertex3f (x y z)` - 3D vertex
- `gl-vertex4f (x y z w)` - 4D vertex
- `gl-color3f (r g b)` - Set color (RGB)
- `gl-color4f (r g b a)` - Set color (RGBA)
- `gl-normal3f (nx ny nz)` - Set normal vector
- `gl-tex-coord2f (s t)` - Set texture coordinates

### Texture Management
- `gl-gen-texture` - Generate texture handle
- `gl-delete-texture (handle)` - Delete texture
- `gl-bind-texture (target handle)` - Bind texture
- `gl-tex-image-2d (...)` - Upload texture data
- `gl-tex-parameter-i (target pname param)` - Set texture parameter

### Buffer Management (VBOs)
- `gl-gen-buffer` - Generate buffer handle
- `gl-delete-buffer (handle)` - Delete buffer
- `gl-bind-buffer (target handle)` - Bind buffer
- `gl-buffer-data (target size data usage)` - Upload buffer data

### Vertex Arrays
- `gl-vertex-pointer (size type stride pointer)` - Set vertex array
- `gl-color-pointer (size type stride pointer)` - Set color array
- `gl-normal-pointer (type stride pointer)` - Set normal array
- `gl-tex-coord-pointer (size type stride pointer)` - Set texcoord array
- `gl-enable-client-state (array)` - Enable client state
- `gl-disable-client-state (array)` - Disable client state

### Drawing Arrays
- `gl-draw-arrays (mode first count)` - Draw from arrays
- `gl-draw-elements (mode count type indices)` - Draw with indices

### Lighting
- `gl-light-fv (light pname params)` - Set light parameters
- `gl-material-fv (face pname params)` - Set material properties
- `gl-shade-model (mode)` - Set shading model

### Blending
- `gl-blend-func (sfactor dfactor)` - Set blend function

### Utility
- `gl-flush` - Flush OpenGL commands
- `gl-finish` - Wait for all commands to complete
- `gl-get-error` - Get last error code

## OpenGL Constants

All standard OpenGL constants are defined in `sys/opengl/class.inc`:

```lisp
; Primitives
GL_POINTS, GL_LINES, GL_LINE_LOOP, GL_LINE_STRIP
GL_TRIANGLES, GL_TRIANGLE_STRIP, GL_TRIANGLE_FAN
GL_QUADS, GL_QUAD_STRIP, GL_POLYGON

; Matrix Modes
GL_MODELVIEW, GL_PROJECTION, GL_TEXTURE

; Buffer Bits
GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT, GL_STENCIL_BUFFER_BIT

; Capabilities
GL_DEPTH_TEST, GL_CULL_FACE, GL_LIGHTING, GL_TEXTURE_2D, GL_BLEND

; Texture Parameters
GL_TEXTURE_MAG_FILTER, GL_TEXTURE_MIN_FILTER
GL_TEXTURE_WRAP_S, GL_TEXTURE_WRAP_T
GL_NEAREST, GL_LINEAR, GL_CLAMP, GL_REPEAT

; Pixel Formats
GL_RGB, GL_RGBA

; And many more...
```

## System Integration

### Statics Structure
The OpenGL function table is stored in the system statics:
- `sys_load_host_gl_funcs` - Pointer to OpenGL function table

### Bootstrap Integration
- Modified `sys/load/class.vp` to accept GL function table as 5th parameter
- Modified `sys/statics/class.inc` to include `sys_load_host_gl_funcs` pointer
- Modified `main.cpp` and `vp64.cpp` to pass OpenGL function table

### Build System
- Added `HOST_GL := 1` to Makefile
- Added `-lGL -lGLU` linker flags for GUI builds
- Added `-D_HOST_GL=$(HOST_GL)` compiler flag

## Usage Example

```lisp
(include "sys/opengl/class.inc")
(include "sys/opengl/lisp.inc")

(defun draw-triangle ()
	; Initialize OpenGL
	(call 'host_gl :gl_init (list))

	; Set viewport
	(call 'host_gl :gl_viewport (list 0 0 800 600))

	; Clear screen
	(call 'host_gl :gl_clear (list GL_COLOR_BUFFER_BIT))

	; Draw triangle
	(call 'host_gl :gl_begin (list GL_TRIANGLES))
	(call 'host_gl :gl_color3f (list 1.0 0.0 0.0))  ; Red
	(call 'host_gl :gl_vertex3f (list 0.0 1.0 0.0))
	(call 'host_gl :gl_color3f (list 0.0 1.0 0.0))  ; Green
	(call 'host_gl :gl_vertex3f (list -1.0 -1.0 0.0))
	(call 'host_gl :gl_color3f (list 0.0 0.0 1.0))  ; Blue
	(call 'host_gl :gl_vertex3f (list 1.0 -1.0 0.0))
	(call 'host_gl :gl_end (list))

	; Flush
	(call 'host_gl :gl_flush (list)))
```

## Demo Application

A demo application is provided in `apps/opengl_demo/app.lisp` that demonstrates:
- Basic OpenGL initialization
- Drawing colored triangles
- Clearing buffers
- Viewport management

## Notes on SDL Integration

The OpenGL context is created when SDL initializes a window with the `SDL_WINDOW_OPENGL` flag (already present in `gui_sdl.cpp`). This means:
- OpenGL is automatically available when GUI is initialized
- No additional context creation is needed
- OpenGL rendering is compatible with SDL's rendering pipeline

## Platform Requirements

- **Linux**: Install `mesa-common-dev` and `libglu1-mesa-dev`
- **macOS**: OpenGL is built-in (framework)
- **Windows**: OpenGL32.lib and GLU32.lib (usually included)

## Future Enhancements

Potential areas for expansion:
1. **Shaders**: GLSL shader support (glCreateShader, glCompileShader, etc.)
2. **Modern OpenGL**: VAOs, FBOs, UBOs
3. **Extensions**: Query and use OpenGL extensions
4. **Error Handling**: More robust error checking and reporting
5. **Lisp Macros**: Higher-level drawing abstractions
6. **3D Primitives**: Sphere, cube, cylinder helpers
7. **Camera**: Camera class for view transformations

## Implementation Details

### Resource Management
- Textures and buffers use handle-based management
- Handles are 32-bit integers managed by C++ maps
- Automatic cleanup on deletion

### ABI Integration
- Uses `abi-call-table` for C function dispatch
- Float parameters passed as integers (reinterpreted in C++)
- Follows ChrysaLisp's standard calling convention

### Error Handling
- `gl-get-error` returns OpenGL error codes
- Lisp functions include type checking
- C++ functions log errors to stderr

## Files Modified/Created

### New Files:
- `src/host/gl_host.cpp` - C++ OpenGL wrapper
- `sys/opengl/class.inc` - Class definitions and constants
- `sys/opengl/class.vp` - VP method implementations
- `sys/opengl/lisp.inc` - Lisp macros
- `sys/opengl/lisp.vp` - Lisp method implementations
- `apps/opengl_demo/app.lisp` - Demo application
- `OPENGL_PORT_README.md` - This documentation

### Modified Files:
- `Makefile` - Added OpenGL linker flags and HOST_GL define
- `src/host/main.cpp` - Pass GL function table to VP
- `src/host/vp64.cpp` - Accept GL function table parameter
- `sys/statics/class.inc` - Added `sys_load_host_gl_funcs` pointer
- `sys/load/class.vp` - Accept and store GL function table

## Conclusion

This OpenGL port provides ChrysaLisp with comprehensive 3D graphics capabilities while maintaining the system's elegant architecture and design philosophy. The implementation follows established patterns and integrates seamlessly with the existing GUI system.
