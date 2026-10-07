;message structure

(import "lib/task/jobs.inc")

;a mesh for a child to make, its name and the form that makes it
(structure +mesh +job_size
	(struct name 16)
	(offset command))

;its answer, the mesh
(structure +mesh_reply +job_reply_size
	(uint num_verts num_norms num_tris)
	(struct name 16)
	(offset data))
