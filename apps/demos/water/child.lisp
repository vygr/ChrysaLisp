(import "lib/math/scene.inc")
(import "./chunk.inc")
(import "./route.inc")
(import "./fish.inc")

;a child of Water's farm: it makes a chunk of the reef, chunk.inc, and
;sends back the stones on its floors and its mesh as the shaders want it;
;or a job of one cube, the mesh of a kind of stone, stones.inc, or of
;weed, weeds.inc, the kind; or of two cubes, the mesh of a kind of fish,
;fish.inc,
;and how fine after the job; or a job of no cubes, the line
;of the auto pilot along a tunnel or over the reef, route.inc, the curve
;it is round after the job, and back its places. Its kernels and the
;field's functions are made the first time

(defq kernels :nil funcs :nil)

(defun make-route (key mbox id cave data)
	(unless funcs (setq funcs (route-funcs)))
	(defq pts (partition (push (doubles) data) 3)
		line (shader-verts-str (route-line funcs pts (/= cave 0)))
		reply (setf-> (str-alloc +chunk_reply_size)
			(+job_reply_key key)
			(+chunk_reply_id id)))
	(mail-send mbox (cat (slice reply 0 +chunk_reply_size) line)))

(defun make-chunk (key mbox id x y z cubes)
	(unless kernels (setq kernels (chunk-kernels)))
	(bind '(stones mesh) (chunk-mesh kernels x y z cubes))
	(defq reply (setf-> (str-alloc +chunk_reply_size)
			(+job_reply_key key)
			(+chunk_reply_id id)
			(+chunk_reply_stones (/ (length stones) +stone_size))))
	(mail-send mbox (cat (slice reply 0 +chunk_reply_size) (shader-verts-str stones) (shader-verts-str mesh))))

(defun make-stone (key mbox id kind fine)
	(defq reply (setf-> (str-alloc +chunk_reply_size)
			(+job_reply_key key)
			(+chunk_reply_id id)))
	(mail-send mbox (cat (slice reply 0 +chunk_reply_size)
		(if (< kind +stone_kinds) (stone-mesh kind fine) (weed-mesh (- kind +stone_kinds) fine)))))

(defun make-fish (key mbox id kind)
	(defq reply (setf-> (str-alloc +chunk_reply_size)
			(+job_reply_key key)
			(+chunk_reply_id id)))
	(mail-send mbox (cat (slice reply 0 +chunk_reply_size) (fish-mesh kind))))

(defun main ()
	;a child waits a while for its next chunk, they are wanted now and then
	;as the diver goes on, not all at once
	(job-child 30000000 (lambda (msg mbox)
			(bind '(key reply id x y z cubes) (getf-> msg +job_key +job_reply +chunk_id
				+chunk_x +chunk_y +chunk_z +chunk_cubes))
			(case cubes
				(0 (make-route key reply id x (slice msg +chunk_size -1)))
				(1 (make-stone key reply id x y))
				(2 (make-fish key reply id x))
				(:t (make-chunk key reply id x y z cubes))))))
