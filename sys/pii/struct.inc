(enums +file_open 0
	(enum read write append))

(enums +mmap 0
	(enum data exec shared none))

(structure +stat 0
	(long mtime fsize)
	(ushort mode))
