(include "class/obj/class.inc")

(def-class font obj
	(dec-method :vtable gui/font/vtable)
	(dec-method :open gui/font/open :static (:r0 :r1) (:r0))
	(dec-method :create gui/font/create :static (:r0 :r1 :r2) (:r0))
	(dec-method :init gui/font/init :static (:r0 :r1 :r2 :r3 :r4))
	(dec-method :flush gui/font/flush)
	(dec-method :sym_texture gui/font/sym_texture :static (:r0 :r1) (:r0 :r1))
	(dec-method :ascii_textures gui/font/ascii_textures :static (:r0) (:r0 :r1))
	(dec-method :get_metrics gui/font/get_metrics :static (:r0) (:r0 :r1 :r2 :r3))
	(dec-method :glyph_data gui/font/glyph_data :static (:r0 :r1) (:r0 :r1))
	(dec-method :glyph_ranges gui/font/glyph_ranges :static (:r0) (:r0 :r1))
	(dec-method :glyph_info gui/font/glyph_info :static (:r0 :r1) (:r0 :r1))
	(dec-method :glyph_paths gui/font/glyph_paths :static (:r0 :r1 :r2) (:r0 :r1 :r2 :r3))
	(dec-method :glyph_bounds gui/font/glyph_bounds :static (:r0 :r1) (:r0 :r1 :r2))
	(dec-method :deinit gui/font/deinit :final)
	(dec-method :lisp_create gui/font/lisp_create :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_info gui/font/lisp_info :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_glyph_ranges gui/font/lisp_glyph_ranges :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_glyph_paths gui/font/lisp_glyph_paths :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_glyph_bounds gui/font/lisp_glyph_bounds :static (:r0 :r1) (:r0 :r1))
	(dec-method :lisp_texture gui/font/lisp_texture :static (:r0 :r1) (:r0 :r1)))

(def-struct font obj_size
	(ptr data sym_map name chars)
	(uint pixels))

(def-struct font_data 0
	(int ascent descent))

(def-struct font_page 0
	(uint end start))

(def-struct font_path 0
	(uint width len))

(def-struct font_path_element 0
	(uint type))

(def-struct font_line_element font_path_element_size
	(int x y))

(def-struct font_curve_element font_line_element_size
	(int x1 y1 x2 y2))

(defcvar 'font_max_word_cache 1024)
