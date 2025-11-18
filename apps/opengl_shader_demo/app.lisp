;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Modern OpenGL Shader Demo
; Demonstrates VAOs, VBOs, and GLSL Shaders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "sys/opengl/class.inc")
(include "sys/opengl/lisp.inc")

; Vertex Shader Source
(defq vertex-shader-source "
#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;

out vec3 vertexColor;

uniform float time;
uniform mat4 transform;

void main()
{
    // Apply transformation
    vec3 pos = aPos;

    // Animated wave effect
    pos.z += sin(aPos.x * 3.0 + time) * 0.1;

    gl_Position = vec4(pos, 1.0);
    vertexColor = aColor;
}
")

; Fragment Shader Source
(defq fragment-shader-source "
#version 330 core
in vec3 vertexColor;
out vec4 FragColor;

uniform float time;

void main()
{
    // Pulse effect based on time
    float pulse = 0.5 + 0.5 * sin(time * 2.0);
    vec3 color = vertexColor * (0.7 + 0.3 * pulse);
    FragColor = vec4(color, 1.0);
}
")

; Triangle vertex data (position + color interleaved)
; Format: x, y, z, r, g, b
(defq triangle-vertices (list
    ; Vertex 1 (top, red)
    0.0 0.5 0.0 1.0 0.0 0.0
    ; Vertex 2 (bottom-left, green)
    -0.5 -0.5 0.0 0.0 1.0 0.0
    ; Vertex 3 (bottom-right, blue)
    0.5 -0.5 0.0 0.0 0.0 1.0))

(defun create-shader (type source)
    ; Create shader
    (defq shader (call 'host_gl :gl_create_shader (list type)))

    (if (= shader 0)
        (progn
            (print "Failed to create shader")
            (return 0)))

    ; Set source
    (if (/= (call 'host_gl :gl_shader_source (list shader source)) 0)
        (progn
            (print "Failed to set shader source")
            (call 'host_gl :gl_delete_shader (list shader))
            (return 0)))

    ; Compile shader
    (if (/= (call 'host_gl :gl_compile_shader (list shader)) 0)
        (progn
            (print "Shader compilation failed!")
            (call 'host_gl :gl_delete_shader (list shader))
            (return 0)))

    shader)

(defun create-program (vs-source fs-source)
    ; Create shaders
    (defq vs (create-shader GL_VERTEX_SHADER vs-source))
    (defq fs (create-shader GL_FRAGMENT_SHADER fs-source))

    (if (or (= vs 0) (= fs 0))
        (progn
            (print "Failed to create shaders")
            (return 0)))

    ; Create program
    (defq program (call 'host_gl :gl_create_program (list)))

    (if (= program 0)
        (progn
            (print "Failed to create program")
            (return 0)))

    ; Attach shaders
    (call 'host_gl :gl_attach_shader (list program vs))
    (call 'host_gl :gl_attach_shader (list program fs))

    ; Link program
    (if (/= (call 'host_gl :gl_link_program (list program)) 0)
        (progn
            (print "Program linking failed!")
            (call 'host_gl :gl_delete_program (list program))
            (call 'host_gl :gl_delete_shader (list vs))
            (call 'host_gl :gl_delete_shader (list fs))
            (return 0)))

    ; Clean up shaders (they're now in the program)
    (call 'host_gl :gl_delete_shader (list vs))
    (call 'host_gl :gl_delete_shader (list fs))

    program)

(defun create-triangle-vao ()
    ; Generate VAO
    (defq vao (call 'host_gl :gl_gen_vertex_array (list)))
    (if (= vao 0)
        (progn
            (print "Failed to generate VAO")
            (return nil)))

    ; Bind VAO
    (call 'host_gl :gl_bind_vertex_array (list vao))

    ; Generate VBO
    (defq vbo (call 'host_gl :gl_gen_buffer (list)))
    (if (= vbo 0)
        (progn
            (print "Failed to generate VBO")
            (call 'host_gl :gl_delete_vertex_array (list vao))
            (return nil)))

    ; Bind VBO
    (call 'host_gl :gl_bind_buffer (list GL_ARRAY_BUFFER vbo))

    ; Upload vertex data
    (call 'host_gl :gl_buffer_data (list
        GL_ARRAY_BUFFER
        (* (length triangle-vertices) 4)  ; Size in bytes (float = 4 bytes)
        triangle-vertices
        GL_STATIC_DRAW))

    ; Configure vertex attributes
    ; Position attribute (location = 0, 3 floats)
    (call 'host_gl :gl_vertex_attrib_pointer (list
        0                ; Location
        3                ; Size (vec3)
        GL_FLOAT         ; Type
        GL_FALSE         ; Normalized
        (* 6 4)          ; Stride (6 floats * 4 bytes)
        0))              ; Offset
    (call 'host_gl :gl_enable_vertex_attrib_array (list 0))

    ; Color attribute (location = 1, 3 floats)
    (call 'host_gl :gl_vertex_attrib_pointer (list
        1                ; Location
        3                ; Size (vec3)
        GL_FLOAT         ; Type
        GL_FALSE         ; Normalized
        (* 6 4)          ; Stride
        (* 3 4)))        ; Offset (after 3 position floats)
    (call 'host_gl :gl_enable_vertex_attrib_array (list 1))

    ; Unbind
    (call 'host_gl :gl_bind_vertex_array (list 0))
    (call 'host_gl :gl_bind_buffer (list GL_ARRAY_BUFFER 0))

    (list vao vbo))

(defun main ()
    (print "")
    (print "========================================")
    (print "  Modern OpenGL Shader Demo")
    (print "  ChrysaLisp + GLSL Shaders + VAO/VBO")
    (print "========================================")
    (print "")

    ; Initialize OpenGL
    (print "Initializing OpenGL...")
    (call 'host_gl :gl_init (list))

    ; Check for errors
    (defq err (call 'host_gl :gl_get_error (list)))
    (if (/= err GL_NO_ERROR)
        (print (cat "OpenGL error after init: " err)))

    ; Set viewport
    (defq width 800)
    (defq height 600)
    (call 'host_gl :gl_viewport (list 0 0 width height))
    (print "Viewport set")

    ; Set clear color
    (call 'host_gl :gl_clear_color (list 0.1 0.1 0.2 1.0))

    ; Create shader program
    (print "Creating shader program...")
    (defq program (create-program vertex-shader-source fragment-shader-source))

    (if (= program 0)
        (progn
            (print "ERROR: Failed to create shader program!")
            (return nil)))

    (print "Shader program created successfully")

    ; Create VAO with triangle data
    (print "Creating VAO...")
    (defq vao-vbo (create-triangle-vao))

    (if (= vao-vbo nil)
        (progn
            (print "ERROR: Failed to create VAO!")
            (return nil)))

    (defq vao (car vao-vbo))
    (defq vbo (cadr vao-vbo))
    (print "VAO created successfully")

    ; Get uniform locations
    (defq time-loc (call 'host_gl :gl_get_uniform_location (list program "time")))
    (print (cat "Uniform 'time' location: " time-loc))

    ; Main render loop
    (print "")
    (print "Starting render loop...")
    (print "(Running 100 frames for demonstration)")
    (print "")

    (defq running t)
    (defq frame 0)
    (defq time 0.0)

    (while (and running (< frame 100))
        ; Clear buffers
        (call 'host_gl :gl_clear (list (logior GL_COLOR_BUFFER_BIT
                                               GL_DEPTH_BUFFER_BIT)))

        ; Use shader program
        (call 'host_gl :gl_use_program (list program))

        ; Set time uniform
        (if (>= time-loc 0)
            (call 'host_gl :gl_uniform1f (list time-loc time)))

        ; Bind VAO and draw
        (call 'host_gl :gl_bind_vertex_array (list vao))
        (call 'host_gl :gl_draw_arrays (list GL_TRIANGLES 0 3))
        (call 'host_gl :gl_bind_vertex_array (list 0))

        ; Unbind shader
        (call 'host_gl :gl_use_program (list 0))

        ; Flush
        (call 'host_gl :gl_flush (list))

        ; Update time
        (setq time (+ time 0.016))  ; ~60 FPS
        (setq frame (+ frame 1))

        ; Progress indicator
        (if (= (% frame 10) 0)
            (print (cat "Frame " frame "/100"))))

    (print "")
    (print "Render loop complete!")
    (print "")

    ; Cleanup
    (print "Cleaning up resources...")
    (call 'host_gl :gl_delete_vertex_array (list vao))
    (call 'host_gl :gl_delete_buffer (list vbo))
    (call 'host_gl :gl_delete_program (list program))

    (print "")
    (print "========================================")
    (print "  Demo Complete!")
    (print "========================================")
    (print "")
    (print "Modern OpenGL features demonstrated:")
    (print "  ✓ GLSL Vertex Shaders")
    (print "  ✓ GLSL Fragment Shaders")
    (print "  ✓ Shader Uniforms (time)")
    (print "  ✓ Vertex Array Objects (VAO)")
    (print "  ✓ Vertex Buffer Objects (VBO)")
    (print "  ✓ Interleaved vertex attributes")
    (print "  ✓ Animated shader effects")
    (print ""))

; Run the demo
(main)
