(import "lib/task/jobs.inc")

;an atom image for a child to make, its size and the file it goes in.
;The answer is the bare reply, the image is in the file
(structure +atom +job_size
	(long key)
	(offset file))
