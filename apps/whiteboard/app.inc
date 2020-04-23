;common imports and tuples
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'dlist 0
	(byte 'mask 'rate 'flatten 'commited_canvas 'overlay_canvas 'commited_polygons 'overlay_paths))

(structure 'path 0
	(byte 'mode 'color 'radius 'path))
