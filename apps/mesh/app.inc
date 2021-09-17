;message structure

(structure +job 0
	(long key)
	(netid reply)
	(struct name 16)
	(offset command))

(structure +job_reply 0
	(long key num_verts num_norms num_tris)
	(struct name 16)
	(offset data))
