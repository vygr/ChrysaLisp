(structure +font +obj_size
	(uint pixels)
	(ptr data sym_map name chars))

(structure +font_data 0
	(int ascent descent))

(structure +font_page 0
	(uint end start))

(structure +font_path 0
	(uint width len))

(structure +font_path_element 0
	(uint type))

(structure +font_line_element +font_path_element_size
	(int x y))

(structure +font_curve_element +font_line_element_size
	(int x1 y1 x2 y2))
