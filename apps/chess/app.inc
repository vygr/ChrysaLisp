;lf macro
(defmacro LF () (ascii-char 10))

;board square/piece types
(defq +white+ 1 +empty+ 0 +black+ -1)

;job structure
(structure '+job 0
	(netid 'reply+)
	(long 'move_time+)
	(byte 'color+)
	(offset 'board+))
