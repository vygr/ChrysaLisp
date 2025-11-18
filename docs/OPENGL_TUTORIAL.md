# ChrysaLisp OpenGL Tutorial

## Table of Contents
1. [Introduction](#introduction)
2. [Getting Started](#getting-started)
3. [Basic Rendering](#basic-rendering)
4. [Modern OpenGL with Shaders](#modern-opengl-with-shaders)
5. [GUI Integration](#gui-integration)
6. [Advanced Topics](#advanced-topics)
7. [Best Practices](#best-practices)

---

## Introduction

ChrysaLisp now includes comprehensive OpenGL support, enabling you to create 3D graphics applications. This tutorial will guide you through both immediate mode OpenGL (for simplicity) and modern OpenGL (for performance).

### What You'll Learn
- Drawing 2D and 3D primitives
- Using textures and materials
- Writing and using GLSL shaders
- Integrating OpenGL with ChrysaLisp's GUI system
- Creating interactive 3D applications

### Prerequisites
- ChrysaLisp installed and built with OpenGL support (`HOST_GL=1` in Makefile)
- Basic understanding of Lisp
- Familiarity with 3D graphics concepts (helpful but not required)

---

## Getting Started

### Including OpenGL

Every OpenGL application needs to include the OpenGL system files:

```lisp
(include "sys/opengl/class.inc")
(include "sys/opengl/lisp.inc")
```

### Initializing OpenGL

OpenGL is automatically initialized when you create an SDL GUI window. You can call `gl-init` to ensure it's ready:

```lisp
(defun init-opengl ()
    (call 'host_gl :gl_init (list))
    (print "OpenGL initialized"))
```

### Your First Triangle

Here's a complete minimal example:

```lisp
(include "sys/opengl/class.inc")
(include "sys/opengl/lisp.inc")

(defun draw-triangle ()
    ; Clear the screen
    (call 'host_gl :gl_clear (list (logior GL_COLOR_BUFFER_BIT
                                           GL_DEPTH_BUFFER_BIT)))

    ; Set clear color (dark blue)
    (call 'host_gl :gl_clear_color (list 0.0 0.0 0.3 1.0))

    ; Set viewport
    (call 'host_gl :gl_viewport (list 0 0 800 600))

    ; Draw a triangle
    (call 'host_gl :gl_begin (list GL_TRIANGLES))

    ; Red vertex (top)
    (call 'host_gl :gl_color3f (list 1.0 0.0 0.0))
    (call 'host_gl :gl_vertex3f (list 0.0 0.5 0.0))

    ; Green vertex (bottom left)
    (call 'host_gl :gl_color3f (list 0.0 1.0 0.0))
    (call 'host_gl :gl_vertex3f (list -0.5 -0.5 0.0))

    ; Blue vertex (bottom right)
    (call 'host_gl :gl_color3f (list 0.0 0.0 1.0))
    (call 'host_gl :gl_vertex3f (list 0.5 -0.5 0.0))

    (call 'host_gl :gl_end (list))

    ; Flush commands
    (call 'host_gl :gl_flush (list)))

; Run the drawing function
(draw-triangle)
```

---

## Basic Rendering

### Drawing Different Primitives

OpenGL supports various primitive types:

```lisp
; Points
(call 'host_gl :gl_begin (list GL_POINTS))
(call 'host_gl :gl_vertex3f (list 0.0 0.0 0.0))
(call 'host_gl :gl_end (list))

; Lines
(call 'host_gl :gl_begin (list GL_LINES))
(call 'host_gl :gl_vertex3f (list -1.0 0.0 0.0))
(call 'host_gl :gl_vertex3f (list 1.0 0.0 0.0))
(call 'host_gl :gl_end (list))

; Triangle strip
(call 'host_gl :gl_begin (list GL_TRIANGLE_STRIP))
(call 'host_gl :gl_vertex3f (list -0.5 -0.5 0.0))
(call 'host_gl :gl_vertex3f (list 0.5 -0.5 0.0))
(call 'host_gl :gl_vertex3f (list -0.5 0.5 0.0))
(call 'host_gl :gl_vertex3f (list 0.5 0.5 0.0))
(call 'host_gl :gl_end (list))

; Quads
(call 'host_gl :gl_begin (list GL_QUADS))
(call 'host_gl :gl_vertex3f (list -0.5 -0.5 0.0))
(call 'host_gl :gl_vertex3f (list 0.5 -0.5 0.0))
(call 'host_gl :gl_vertex3f (list 0.5 0.5 0.0))
(call 'host_gl :gl_vertex3f (list -0.5 0.5 0.0))
(call 'host_gl :gl_end (list))
```

### 3D Transformations

Use matrix operations for 3D transformations:

```lisp
(defun setup-3d-view (width height)
    ; Set up projection matrix
    (call 'host_gl :gl_matrix_mode (list GL_PROJECTION))
    (call 'host_gl :gl_load_identity (list))
    (call 'host_gl :gl_perspective (list
        45.0                              ; Field of view
        (/ (float width) (float height))  ; Aspect ratio
        0.1                               ; Near plane
        100.0))                           ; Far plane

    ; Set up model-view matrix
    (call 'host_gl :gl_matrix_mode (list GL_MODELVIEW))
    (call 'host_gl :gl_load_identity (list)))

(defun draw-rotating-cube (angle)
    ; Save matrix state
    (call 'host_gl :gl_push_matrix (list))

    ; Move back from camera
    (call 'host_gl :gl_translate_f (list 0.0 0.0 -5.0))

    ; Rotate
    (call 'host_gl :gl_rotate_f (list angle 1.0 1.0 0.0))

    ; Draw cube (6 faces)
    (draw-cube-faces)

    ; Restore matrix
    (call 'host_gl :gl_pop_matrix (list)))
```

### Texturing

Apply textures to surfaces:

```lisp
(defun load-texture (width height data)
    ; Generate texture handle
    (defq tex-handle (call 'host_gl :gl_gen_texture (list)))

    ; Bind texture
    (call 'host_gl :gl_bind_texture (list GL_TEXTURE_2D tex-handle))

    ; Set texture parameters
    (call 'host_gl :gl_tex_parameter_i (list GL_TEXTURE_2D
                                              GL_TEXTURE_MIN_FILTER
                                              GL_LINEAR))
    (call 'host_gl :gl_tex_parameter_i (list GL_TEXTURE_2D
                                              GL_TEXTURE_MAG_FILTER
                                              GL_LINEAR))

    ; Upload texture data
    (call 'host_gl :gl_tex_image_2d (list
        GL_TEXTURE_2D    ; Target
        0                ; Level
        GL_RGBA          ; Internal format
        width height     ; Dimensions
        0                ; Border
        GL_RGBA          ; Format
        GL_UNSIGNED_BYTE ; Type
        data))           ; Pixel data

    tex-handle)

(defun draw-textured-quad (tex-handle)
    (call 'host_gl :gl_enable (list GL_TEXTURE_2D))
    (call 'host_gl :gl_bind_texture (list GL_TEXTURE_2D tex-handle))

    (call 'host_gl :gl_begin (list GL_QUADS))
    (call 'host_gl :gl_tex_coord2f (list 0.0 0.0))
    (call 'host_gl :gl_vertex3f (list -1.0 -1.0 0.0))
    (call 'host_gl :gl_tex_coord2f (list 1.0 0.0))
    (call 'host_gl :gl_vertex3f (list 1.0 -1.0 0.0))
    (call 'host_gl :gl_tex_coord2f (list 1.0 1.0))
    (call 'host_gl :gl_vertex3f (list 1.0 1.0 0.0))
    (call 'host_gl :gl_tex_coord2f (list 0.0 1.0))
    (call 'host_gl :gl_vertex3f (list -1.0 1.0 0.0))
    (call 'host_gl :gl_end (list))

    (call 'host_gl :gl_disable (list GL_TEXTURE_2D)))
```

---

## Modern OpenGL with Shaders

### Why Use Shaders?

Modern OpenGL with shaders provides:
- **Better Performance**: Programmable pipeline runs on GPU
- **More Control**: Custom vertex and fragment processing
- **Visual Effects**: Post-processing, lighting models, etc.

### Writing Your First Shader

**Vertex Shader** (transforms vertices):
```glsl
#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;

out vec3 vertexColor;

uniform mat4 transform;

void main()
{
    gl_Position = transform * vec4(aPos, 1.0);
    vertexColor = aColor;
}
```

**Fragment Shader** (colors pixels):
```glsl
#version 330 core
in vec3 vertexColor;
out vec4 FragColor;

uniform float time;

void main()
{
    // Animate color based on time
    vec3 color = vertexColor * (0.5 + 0.5 * sin(time));
    FragColor = vec4(color, 1.0);
}
```

### Using Shaders in ChrysaLisp

```lisp
(defun create-shader-program (vertex-source fragment-source)
    ; Create shaders
    (defq vs (call 'host_gl :gl_create_shader (list GL_VERTEX_SHADER)))
    (defq fs (call 'host_gl :gl_create_shader (list GL_FRAGMENT_SHADER)))

    ; Set shader source code
    (call 'host_gl :gl_shader_source (list vs vertex-source))
    (call 'host_gl :gl_shader_source (list fs fragment-source))

    ; Compile shaders
    (if (= (call 'host_gl :gl_compile_shader (list vs)) -1)
        (print "Vertex shader compilation failed!"))
    (if (= (call 'host_gl :gl_compile_shader (list fs)) -1)
        (print "Fragment shader compilation failed!"))

    ; Create program
    (defq program (call 'host_gl :gl_create_program (list)))

    ; Attach shaders
    (call 'host_gl :gl_attach_shader (list program vs))
    (call 'host_gl :gl_attach_shader (list program fs))

    ; Link program
    (if (= (call 'host_gl :gl_link_program (list program)) -1)
        (print "Program linking failed!"))

    ; Clean up shaders (they're linked into the program now)
    (call 'host_gl :gl_delete_shader (list vs))
    (call 'host_gl :gl_delete_shader (list fs))

    program)

(defun use-shader (program time)
    ; Activate shader
    (call 'host_gl :gl_use_program (list program))

    ; Set uniform variables
    (defq time-loc (call 'host_gl :gl_get_uniform_location
                         (list program "time")))
    (call 'host_gl :gl_uniform1f (list time-loc time))

    ; Draw geometry...
    ; ...

    ; Deactivate shader
    (call 'host_gl :gl_use_program (list 0)))
```

### Vertex Array Objects (VAOs)

VAOs store vertex attribute configuration for efficient rendering:

```lisp
(defun create-triangle-vao (vertices)
    ; Create VAO
    (defq vao (call 'host_gl :gl_gen_vertex_array (list)))
    (call 'host_gl :gl_bind_vertex_array (list vao))

    ; Create VBO
    (defq vbo (call 'host_gl :gl_gen_buffer (list)))
    (call 'host_gl :gl_bind_buffer (list GL_ARRAY_BUFFER vbo))
    (call 'host_gl :gl_buffer_data (list
        GL_ARRAY_BUFFER
        (* (length vertices) 4)  ; Size in bytes
        vertices
        GL_STATIC_DRAW))

    ; Configure vertex attributes
    ; Position attribute (location = 0)
    (call 'host_gl :gl_vertex_attrib_pointer (list
        0                ; Attribute location
        3                ; Size (vec3)
        GL_FLOAT         ; Type
        GL_FALSE         ; Normalized
        (* 6 4)          ; Stride (6 floats per vertex)
        0))              ; Offset
    (call 'host_gl :gl_enable_vertex_attrib_array (list 0))

    ; Color attribute (location = 1)
    (call 'host_gl :gl_vertex_attrib_pointer (list
        1                ; Attribute location
        3                ; Size (vec3)
        GL_FLOAT         ; Type
        GL_FALSE         ; Normalized
        (* 6 4)          ; Stride
        (* 3 4)))        ; Offset (after position)
    (call 'host_gl :gl_enable_vertex_attrib_array (list 1))

    ; Unbind
    (call 'host_gl :gl_bind_vertex_array (list 0))

    (list vao vbo))

(defun draw-vao (vao vertex-count)
    (call 'host_gl :gl_bind_vertex_array (list vao))
    (call 'host_gl :gl_draw_arrays (list GL_TRIANGLES 0 vertex-count))
    (call 'host_gl :gl_bind_vertex_array (list 0)))
```

---

## GUI Integration

### Rendering OpenGL in a ChrysaLisp Window

OpenGL automatically renders to the SDL window. Here's how to integrate with the GUI system:

```lisp
(include "sys/opengl/class.inc")
(include "sys/opengl/lisp.inc")
(include "service/gui/class.inc")

(defun create-gl-window ()
    ; GUI initialization creates OpenGL context
    (defq window-width 800)
    (defq window-height 600)

    ; Initialize OpenGL
    (call 'host_gl :gl_init (list))
    (call 'host_gl :gl_viewport (list 0 0 window-width window-height))

    ; Set up your 3D scene
    (setup-3d-scene window-width window-height)

    ; Main loop
    (defq running t)
    (defq angle 0.0)

    (while running
        ; Clear buffers
        (call 'host_gl :gl_clear (list (logior GL_COLOR_BUFFER_BIT
                                               GL_DEPTH_BUFFER_BIT)))

        ; Render 3D scene
        (render-scene angle)

        ; Flush OpenGL commands
        (call 'host_gl :gl_flush (list))

        ; Swap buffers (done by GUI system)
        ; ...

        ; Update animation
        (setq angle (+ angle 0.01))
        (if (> angle 360.0) (setq angle 0.0))))
```

### Mixing 2D GUI and 3D OpenGL

You can combine ChrysaLisp's 2D GUI with 3D OpenGL rendering:

```lisp
(defun hybrid-render ()
    ; 1. Render 3D OpenGL scene
    (render-3d-scene)

    ; 2. Switch to 2D orthographic projection for GUI
    (call 'host_gl :gl_matrix_mode (list GL_PROJECTION))
    (call 'host_gl :gl_push_matrix (list))
    (call 'host_gl :gl_load_identity (list))
    (call 'host_gl :gl_ortho (list 0.0 800.0 600.0 0.0 -1.0 1.0))

    (call 'host_gl :gl_matrix_mode (list GL_MODELVIEW))
    (call 'host_gl :gl_push_matrix (list))
    (call 'host_gl :gl_load_identity (list))

    ; 3. Disable depth test for 2D overlay
    (call 'host_gl :gl_disable (list GL_DEPTH_TEST))

    ; 4. Draw 2D GUI elements with OpenGL
    (draw-2d-gui-elements)

    ; 5. Restore 3D state
    (call 'host_gl :gl_enable (list GL_DEPTH_TEST))
    (call 'host_gl :gl_matrix_mode (list GL_PROJECTION))
    (call 'host_gl :gl_pop_matrix (list))
    (call 'host_gl :gl_matrix_mode (list GL_MODELVIEW))
    (call 'host_gl :gl_pop_matrix (list)))
```

---

## Advanced Topics

### Lighting

```lisp
(defun setup-lighting ()
    ; Enable lighting
    (call 'host_gl :gl_enable (list GL_LIGHTING))
    (call 'host_gl :gl_enable (list GL_LIGHT0))

    ; Light position
    (defq light-pos (list 1.0 1.0 1.0 0.0))
    (call 'host_gl :gl_light_fv (list GL_LIGHT0 GL_POSITION light-pos))

    ; Light colors
    (defq ambient (list 0.2 0.2 0.2 1.0))
    (defq diffuse (list 1.0 1.0 1.0 1.0))
    (defq specular (list 1.0 1.0 1.0 1.0))

    (call 'host_gl :gl_light_fv (list GL_LIGHT0 GL_AMBIENT ambient))
    (call 'host_gl :gl_light_fv (list GL_LIGHT0 GL_DIFFUSE diffuse))
    (call 'host_gl :gl_light_fv (list GL_LIGHT0 GL_SPECULAR specular)))

(defun set-material (ambient diffuse specular shininess)
    (call 'host_gl :gl_material_fv (list GL_FRONT GL_AMBIENT ambient))
    (call 'host_gl :gl_material_fv (list GL_FRONT GL_DIFFUSE diffuse))
    (call 'host_gl :gl_material_fv (list GL_FRONT GL_SPECULAR specular))
    (call 'host_gl :gl_material_fv (list GL_FRONT GL_SHININESS
                                         (list shininess))))
```

### Blending and Transparency

```lisp
(defun enable-transparency ()
    (call 'host_gl :gl_enable (list GL_BLEND))
    (call 'host_gl :gl_blend_func (list GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA))

    ; Draw transparent objects back-to-front
    ; Disable depth writing for transparent objects
    (call 'host_gl :gl_depth_mask (list GL_FALSE))

    ; Draw transparent geometry...

    ; Re-enable depth writing
    (call 'host_gl :gl_depth_mask (list GL_TRUE)))
```

### Performance Optimization

1. **Use VAOs and VBOs** instead of immediate mode
2. **Batch similar draw calls** together
3. **Minimize state changes** (texture binds, shader switches)
4. **Use indexed drawing** with `glDrawElements`
5. **Frustum culling** - don't draw what's not visible

---

## Best Practices

### Error Handling

Always check for OpenGL errors:

```lisp
(defun check-gl-error (operation)
    (defq err (call 'host_gl :gl_get_error (list)))
    (if (/= err GL_NO_ERROR)
        (print (cat "OpenGL error in " operation ": " err))))

; Use after important operations
(call 'host_gl :gl_tex_image_2d (list ...))
(check-gl-error "texture upload")
```

### Resource Management

Clean up resources when done:

```lisp
(defun cleanup-resources (vao vbo tex shader)
    (call 'host_gl :gl_delete_vertex_array (list vao))
    (call 'host_gl :gl_delete_buffer (list vbo))
    (call 'host_gl :gl_delete_texture (list tex))
    (call 'host_gl :gl_delete_program (list shader)))
```

### Code Organization

Structure your code into logical functions:

```lisp
; Initialization
(defun init-scene () ...)

; Resource loading
(defun load-models () ...)
(defun load-textures () ...)
(defun load-shaders () ...)

; Rendering
(defun render-scene () ...)
(defun render-ui () ...)

; Update
(defun update-physics (dt) ...)
(defun update-animation (dt) ...)

; Cleanup
(defun cleanup () ...)
```

---

## Next Steps

1. **Study the demos** in `apps/opengl_demo/`
2. **Read OPENGL_PORT_README.md** for API reference
3. **Experiment** with the examples
4. **Build something** cool!

### Useful Resources

- **OpenGL Reference**: https://www.khronos.org/opengl/
- **GLSL Reference**: https://www.khronos.org/opengl/wiki/OpenGL_Shading_Language
- **ChrysaLisp Docs**: See `docs/ai_digest/` for system internals

---

**Happy Rendering!** 🎨✨
