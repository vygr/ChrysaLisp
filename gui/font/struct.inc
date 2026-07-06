(import "class/obj/struct.inc")

(structure +font +obj_size
	(uint pixels)
	(ptr data sym_map name chars))

(structure +font_data 0
	;all 8.24 format
	(int ascent descent xkern))

(structure +font_page 0
	(uint end start))

(structure +font_path 0
	(uint code)
	(int width)	;8.24 format
	(uint plen klen))

(structure +font_kern 0
	(ushort code)
	(short xkern))	;2.14 format

(structure +font_path_element 0
	(uint type))

(structure +font_line_element +font_path_element_size
	;all 8.24 format
	(int x y))

(structure +font_quad_element +font_line_element_size
	;all 8.24 format
	(int x1 y1))

(structure +font_curve_element +font_quad_element_size
	;all 8.24 format
	(int x2 y2))
