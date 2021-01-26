;lf macro
(defmacro LF ()
	(ascii-char 10))

;job structure
(structure '+job 0
	(netid 'reply+)
	(long 'move_time+)
	(long 'color+)
	(offset 'board+))
