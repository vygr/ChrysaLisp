;lf macro
(defmacro LF () "\n")

;board square/piece types
(defq +white 1 +empty 0 +black -1)

(enums +job_type 0
	(enum move get_moves))

;job structure
(structure +job 0
	(netid reply)
	(long move_time)
	(byte type color)
	(offset board))

;child reply
(structure +reply 0
	(long seq)
	(byte type)
	(offset data))
