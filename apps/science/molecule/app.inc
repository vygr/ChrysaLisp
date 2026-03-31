(defq atom_cache_dir "apps/science/molecule/data/cache/")

;message structures
(structure +job 0
	(long key)
	(long atom_key)
	(netid reply)
	(offset file))

(structure +job_reply 0
	(long key))