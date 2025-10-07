;lisp bindings
(ffi "gui/font/lisp_create" create-font)
; (create-font name pixels) -> font
(ffi "gui/font/lisp_info" font-info)
; (font-info font) -> (name pixels)
(ffi "gui/font/lisp_glyph_paths" font-glyph-paths)
; (font-glyph-paths font str) -> paths
(ffi "gui/font/lisp_glyph_ranges" font-glyph-ranges)
; (font-glyph-ranges font) -> ranges
(ffi "gui/font/lisp_glyph_bounds" font-glyph-bounds)
; (font-glyph-bounds font str) -> bounds
(ffi "gui/font/lisp_texture" font-sym-texture)
; (font-sym-texture font sym) -> texture
