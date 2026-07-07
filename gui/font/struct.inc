(import "class/obj/struct.inc")

(structure +font +obj_size
	(uint pixels)
	(ptr data sym_map name chars))

(structure +font_data 0
	;all 3.13 format
	(short ascent descent xkern)
	(align +int_size))

(structure +font_page 0
	(ushort end start))

(structure +font_path 0
	(ushort code)
	(short width)	;3.13 format
	(uint plen klen))

(structure +font_kern 0
	(ushort code)
	(short xkern))	;3.13 format

(structure +font_path_element 0
	(ushort type))

(structure +font_line_element +font_path_element_size
	;all 3.13 format
	(short x y))

(structure +font_quad_element +font_line_element_size
	;all 3.13 format
	(short x1 y1))

(structure +font_curve_element +font_quad_element_size
	;all 3.13 format
	(short x2 y2))
