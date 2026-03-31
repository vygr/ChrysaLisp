;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/science/molecule/app.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;message structures

(structure +job 0
	(long key)
	(long atom_key)
	(netid reply)
	(offset file))

(structure +job_reply 0
	(long key))