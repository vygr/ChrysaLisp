(include "lib/asm/class.inc")

;;;;;;;;;;;;;;;;;;;;
; OpenGL static class
;;;;;;;;;;;;;;;;;;;;

(def-class host_gl :nil
	; Initialization
	(dec-method :init :nil :virtual (abi-args 0) (:r0))
	(dec-method :deinit :nil :virtual (abi-args 0) (:r0))

	; Viewport and clear
	(dec-method :viewport :nil :virtual (abi-args 4) (:r0))
	(dec-method :clear :nil :virtual (abi-args 1) (:r0))
	(dec-method :clear_color :nil :virtual (abi-args 4) (:r0))
	(dec-method :clear_depth :nil :virtual (abi-args 1) (:r0))

	; State management
	(dec-method :enable :nil :virtual (abi-args 1) (:r0))
	(dec-method :disable :nil :virtual (abi-args 1) (:r0))

	; Matrix operations
	(dec-method :matrix_mode :nil :virtual (abi-args 1) (:r0))
	(dec-method :load_identity :nil :virtual (abi-args 0) (:r0))
	(dec-method :push_matrix :nil :virtual (abi-args 0) (:r0))
	(dec-method :pop_matrix :nil :virtual (abi-args 0) (:r0))
	(dec-method :load_matrix_f :nil :virtual (abi-args 1) (:r0))
	(dec-method :mult_matrix_f :nil :virtual (abi-args 1) (:r0))

	; Transformations
	(dec-method :translate_f :nil :virtual (abi-args 3) (:r0))
	(dec-method :rotate_f :nil :virtual (abi-args 4) (:r0))
	(dec-method :scale_f :nil :virtual (abi-args 3) (:r0))

	; Projections
	(dec-method :ortho :nil :virtual (abi-args 6) (:r0))
	(dec-method :frustum :nil :virtual (abi-args 6) (:r0))
	(dec-method :perspective :nil :virtual (abi-args 4) (:r0))

	; Drawing primitives
	(dec-method :begin :nil :virtual (abi-args 1) (:r0))
	(dec-method :end :nil :virtual (abi-args 0) (:r0))
	(dec-method :vertex2f :nil :virtual (abi-args 2) (:r0))
	(dec-method :vertex3f :nil :virtual (abi-args 3) (:r0))
	(dec-method :vertex4f :nil :virtual (abi-args 4) (:r0))
	(dec-method :color3f :nil :virtual (abi-args 3) (:r0))
	(dec-method :color4f :nil :virtual (abi-args 4) (:r0))
	(dec-method :normal3f :nil :virtual (abi-args 3) (:r0))
	(dec-method :tex_coord2f :nil :virtual (abi-args 2) (:r0))

	; Texture management
	(dec-method :gen_texture :nil :virtual (abi-args 0) (:r0))
	(dec-method :delete_texture :nil :virtual (abi-args 1) (:r0))
	(dec-method :bind_texture :nil :virtual (abi-args 2) (:r0))
	(dec-method :tex_image_2d :nil :virtual (abi-args 9) (:r0))
	(dec-method :tex_parameter_i :nil :virtual (abi-args 3) (:r0))

	; Buffer management
	(dec-method :gen_buffer :nil :virtual (abi-args 0) (:r0))
	(dec-method :delete_buffer :nil :virtual (abi-args 1) (:r0))
	(dec-method :bind_buffer :nil :virtual (abi-args 2) (:r0))
	(dec-method :buffer_data :nil :virtual (abi-args 4) (:r0))

	; Vertex arrays
	(dec-method :vertex_pointer :nil :virtual (abi-args 4) (:r0))
	(dec-method :color_pointer :nil :virtual (abi-args 4) (:r0))
	(dec-method :normal_pointer :nil :virtual (abi-args 3) (:r0))
	(dec-method :tex_coord_pointer :nil :virtual (abi-args 4) (:r0))
	(dec-method :enable_client_state :nil :virtual (abi-args 1) (:r0))
	(dec-method :disable_client_state :nil :virtual (abi-args 1) (:r0))

	; Drawing arrays
	(dec-method :draw_arrays :nil :virtual (abi-args 3) (:r0))
	(dec-method :draw_elements :nil :virtual (abi-args 4) (:r0))

	; Lighting
	(dec-method :light_fv :nil :virtual (abi-args 3) (:r0))
	(dec-method :material_fv :nil :virtual (abi-args 3) (:r0))
	(dec-method :shade_model :nil :virtual (abi-args 1) (:r0))

	; Blending
	(dec-method :blend_func :nil :virtual (abi-args 2) (:r0))

	; Utility
	(dec-method :flush :nil :virtual (abi-args 0) (:r0))
	(dec-method :finish :nil :virtual (abi-args 0) (:r0))
	(dec-method :get_error :nil :virtual (abi-args 0) (:r0))

	; Modern OpenGL - Shaders
	(dec-method :create_shader :nil :virtual (abi-args 1) (:r0))
	(dec-method :delete_shader :nil :virtual (abi-args 1) (:r0))
	(dec-method :shader_source :nil :virtual (abi-args 2) (:r0))
	(dec-method :compile_shader :nil :virtual (abi-args 1) (:r0))

	; Modern OpenGL - Programs
	(dec-method :create_program :nil :virtual (abi-args 0) (:r0))
	(dec-method :delete_program :nil :virtual (abi-args 1) (:r0))
	(dec-method :attach_shader :nil :virtual (abi-args 2) (:r0))
	(dec-method :link_program :nil :virtual (abi-args 1) (:r0))
	(dec-method :use_program :nil :virtual (abi-args 1) (:r0))
	(dec-method :get_uniform_location :nil :virtual (abi-args 2) (:r0))
	(dec-method :uniform1f :nil :virtual (abi-args 2) (:r0))
	(dec-method :uniform3f :nil :virtual (abi-args 4) (:r0))
	(dec-method :uniform_matrix4fv :nil :virtual (abi-args 2) (:r0))

	; Modern OpenGL - VAOs
	(dec-method :gen_vertex_array :nil :virtual (abi-args 0) (:r0))
	(dec-method :delete_vertex_array :nil :virtual (abi-args 1) (:r0))
	(dec-method :bind_vertex_array :nil :virtual (abi-args 1) (:r0))
	(dec-method :vertex_attrib_pointer :nil :virtual (abi-args 6) (:r0))
	(dec-method :enable_vertex_attrib_array :nil :virtual (abi-args 1) (:r0))
	(dec-method :disable_vertex_attrib_array :nil :virtual (abi-args 1) (:r0))

	; Static wrapper methods for VP code
	(dec-method :gl_init sys/opengl/init :static :nil (:r0))
	(dec-method :gl_deinit sys/opengl/deinit :static :nil (:r0))
	(dec-method :gl_viewport sys/opengl/viewport :static (:r0 :r1 :r2 :r3))
	(dec-method :gl_clear sys/opengl/clear :static (:r0))
	(dec-method :gl_clear_color sys/opengl/clear_color :static (:r0 :r1 :r2 :r3))
	(dec-method :gl_enable sys/opengl/enable :static (:r0))
	(dec-method :gl_disable sys/opengl/disable :static (:r0))
	(dec-method :gl_begin sys/opengl/begin :static (:r0))
	(dec-method :gl_end sys/opengl/end :static)
	(dec-method :gl_vertex3f sys/opengl/vertex3f :static (:r0 :r1 :r2))
	(dec-method :gl_color3f sys/opengl/color3f :static (:r0 :r1 :r2))
	(dec-method :gl_flush sys/opengl/flush :static)
	(dec-method :gl_get_error sys/opengl/get_error :static :nil (:r0))
	(dec-method :gl_draw_arrays sys/opengl/draw_arrays :static (:r0 :r1 :r2))
	(dec-method :gl_gen_buffer sys/opengl/gen_buffer :static :nil (:r0))
	(dec-method :gl_delete_buffer sys/opengl/delete_buffer :static (:r0))
	(dec-method :gl_bind_buffer sys/opengl/bind_buffer :static (:r0 :r1))
	(dec-method :gl_buffer_data sys/opengl/buffer_data :static (:r0 :r1 :r2 :r3))

	; Modern OpenGL - Static wrappers
	(dec-method :gl_create_shader sys/opengl/create_shader :static (:r0) (:r0))
	(dec-method :gl_delete_shader sys/opengl/delete_shader :static (:r0))
	(dec-method :gl_shader_source sys/opengl/shader_source :static (:r0 :r1) (:r0))
	(dec-method :gl_compile_shader sys/opengl/compile_shader :static (:r0) (:r0))
	(dec-method :gl_create_program sys/opengl/create_program :static :nil (:r0))
	(dec-method :gl_delete_program sys/opengl/delete_program :static (:r0))
	(dec-method :gl_attach_shader sys/opengl/attach_shader :static (:r0 :r1))
	(dec-method :gl_link_program sys/opengl/link_program :static (:r0) (:r0))
	(dec-method :gl_use_program sys/opengl/use_program :static (:r0))
	(dec-method :gl_get_uniform_location sys/opengl/get_uniform_location :static (:r0 :r1) (:r0))
	(dec-method :gl_uniform1f sys/opengl/uniform1f :static (:r0 :r1))
	(dec-method :gl_uniform3f sys/opengl/uniform3f :static (:r0 :r1 :r2 :r3))
	(dec-method :gl_uniform_matrix4fv sys/opengl/uniform_matrix4fv :static (:r0 :r1))
	(dec-method :gl_gen_vertex_array sys/opengl/gen_vertex_array :static :nil (:r0))
	(dec-method :gl_delete_vertex_array sys/opengl/delete_vertex_array :static (:r0))
	(dec-method :gl_bind_vertex_array sys/opengl/bind_vertex_array :static (:r0))
	(dec-method :gl_vertex_attrib_pointer sys/opengl/vertex_attrib_pointer :static (:r0 :r1 :r2 :r3 :r4 :r5))
	(dec-method :gl_enable_vertex_attrib_array sys/opengl/enable_vertex_attrib_array :static (:r0))
	(dec-method :gl_disable_vertex_attrib_array sys/opengl/disable_vertex_attrib_array :static (:r0))

	; Lisp interface methods
	(dec-method :lisp_init sys/opengl/lisp_init :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_clear sys/opengl/lisp_clear :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_viewport sys/opengl/lisp_viewport :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_begin sys/opengl/lisp_begin :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_end sys/opengl/lisp_end :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_vertex3f sys/opengl/lisp_vertex3f :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_color3f sys/opengl/lisp_color3f :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_flush sys/opengl/lisp_flush :static (:r0 :r1) (:r0 :r1)))

(defun host-gl-call (_1 _2 &optional _3 _4 _5 _6)
	;_1 = class name (host_gl)
	;_2 = member name (:init, :clear, etc.)
	;_3 = in parameters
	;_4 = out parameters
	;_5 = dispatch reg
	;_6 = temp reg
	(setd _5 :r14 _6 :r13)
	(fn-bind 'sys/statics/statics _5)
	(vp-cpy-ir _5 statics_sys_load_host_gl_funcs _5)
	(abi-call-table _1 _2 _5 _3 _4 _6))

;;;;;;;;;;;;;;;;;;
; OpenGL constants
;;;;;;;;;;;;;;;;;;

; Boolean values
(defcvar GL_FALSE 0)
(defcvar GL_TRUE 1)

; Data types
(defcvar GL_BYTE 0x1400)
(defcvar GL_UNSIGNED_BYTE 0x1401)
(defcvar GL_SHORT 0x1402)
(defcvar GL_UNSIGNED_SHORT 0x1403)
(defcvar GL_INT 0x1404)
(defcvar GL_UNSIGNED_INT 0x1405)
(defcvar GL_FLOAT 0x1406)
(defcvar GL_DOUBLE 0x140A)

; Primitives
(defcvar GL_POINTS 0x0000)
(defcvar GL_LINES 0x0001)
(defcvar GL_LINE_LOOP 0x0002)
(defcvar GL_LINE_STRIP 0x0003)
(defcvar GL_TRIANGLES 0x0004)
(defcvar GL_TRIANGLE_STRIP 0x0005)
(defcvar GL_TRIANGLE_FAN 0x0006)
(defcvar GL_QUADS 0x0007)
(defcvar GL_QUAD_STRIP 0x0008)
(defcvar GL_POLYGON 0x0009)

; Matrix Mode
(defcvar GL_MODELVIEW 0x1700)
(defcvar GL_PROJECTION 0x1701)
(defcvar GL_TEXTURE 0x1702)

; Buffers
(defcvar GL_COLOR_BUFFER_BIT 0x00004000)
(defcvar GL_DEPTH_BUFFER_BIT 0x00000100)
(defcvar GL_STENCIL_BUFFER_BIT 0x00000400)

; Boolean capability names
(defcvar GL_DEPTH_TEST 0x0B71)
(defcvar GL_CULL_FACE 0x0B44)
(defcvar GL_LIGHTING 0x0B50)
(defcvar GL_TEXTURE_2D 0x0DE1)
(defcvar GL_BLEND 0x0BE2)
(defcvar GL_ALPHA_TEST 0x0BC0)

; Texture mapping
(defcvar GL_TEXTURE_ENV 0x2300)
(defcvar GL_TEXTURE_ENV_MODE 0x2200)
(defcvar GL_TEXTURE_MAG_FILTER 0x2800)
(defcvar GL_TEXTURE_MIN_FILTER 0x2801)
(defcvar GL_TEXTURE_WRAP_S 0x2802)
(defcvar GL_TEXTURE_WRAP_T 0x2803)
(defcvar GL_NEAREST 0x2600)
(defcvar GL_LINEAR 0x2601)
(defcvar GL_CLAMP 0x2900)
(defcvar GL_REPEAT 0x2901)

; Pixel formats
(defcvar GL_RGB 0x1907)
(defcvar GL_RGBA 0x1908)

; Shading model
(defcvar GL_FLAT 0x1D00)
(defcvar GL_SMOOTH 0x1D01)

; Blending
(defcvar GL_SRC_ALPHA 0x0302)
(defcvar GL_ONE_MINUS_SRC_ALPHA 0x0303)
(defcvar GL_ONE 1)
(defcvar GL_ZERO 0)

; Vertex Arrays
(defcvar GL_VERTEX_ARRAY 0x8074)
(defcvar GL_NORMAL_ARRAY 0x8075)
(defcvar GL_COLOR_ARRAY 0x8076)
(defcvar GL_TEXTURE_COORD_ARRAY 0x8078)

; Buffer objects
(defcvar GL_ARRAY_BUFFER 0x8892)
(defcvar GL_ELEMENT_ARRAY_BUFFER 0x8893)
(defcvar GL_STATIC_DRAW 0x88E4)
(defcvar GL_DYNAMIC_DRAW 0x88E8)
(defcvar GL_STREAM_DRAW 0x88E0)

; Lighting
(defcvar GL_LIGHT0 0x4000)
(defcvar GL_LIGHT1 0x4001)
(defcvar GL_LIGHT2 0x4002)
(defcvar GL_LIGHT3 0x4003)
(defcvar GL_AMBIENT 0x1200)
(defcvar GL_DIFFUSE 0x1201)
(defcvar GL_SPECULAR 0x1202)
(defcvar GL_POSITION 0x1203)

; Material
(defcvar GL_FRONT 0x0404)
(defcvar GL_BACK 0x0405)
(defcvar GL_FRONT_AND_BACK 0x0408)
(defcvar GL_EMISSION 0x1600)
(defcvar GL_SHININESS 0x1601)

; Errors
(defcvar GL_NO_ERROR 0)
(defcvar GL_INVALID_ENUM 0x0500)
(defcvar GL_INVALID_VALUE 0x0501)
(defcvar GL_INVALID_OPERATION 0x0502)
(defcvar GL_STACK_OVERFLOW 0x0503)
(defcvar GL_STACK_UNDERFLOW 0x0504)
(defcvar GL_OUT_OF_MEMORY 0x0505)

; Modern OpenGL - Shader types
(defcvar GL_VERTEX_SHADER 0x8B31)
(defcvar GL_FRAGMENT_SHADER 0x8B30)
(defcvar GL_GEOMETRY_SHADER 0x8DD9)

; Shader/Program status
(defcvar GL_COMPILE_STATUS 0x8B81)
(defcvar GL_LINK_STATUS 0x8B82)
(defcvar GL_VALIDATE_STATUS 0x8B83)
(defcvar GL_INFO_LOG_LENGTH 0x8B84)
