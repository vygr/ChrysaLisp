;five members, three objects put at three of them, and a gopher that goes to the blocks

(import "service/storage/scene.inc")

(defun main ()
	(defq sc (scene-begin "Five members of a store, each on a node of this machine. Three keep each block."))
	(when (scene-check sc "five members start and meet" (scene-members sc +scene_ids))
		(defq objects (map (lambda (n) (list (cat "docs/object-" (str n)) (scene-bytes (+ 10 (* n 7)) n))) (range 0 3)))
		(each (lambda ((path data))
			(defq id (elem-get +scene_ids (* (!) 2)))
			(scene-say sc path ", " (/ (length data) 4096) " blocks, put at " id ": "
				(first (storage-put path data :nil (scene-svc sc id)))))
			objects)
		(scene-say sc "They meet.")
		(scene-settle sc)
		(each (lambda ((path data))
			(scene-check sc (cat path " is seen by five of five") (scene-seen? sc path "m-b" 5 5))
			(scene-check sc (cat "every block of " path " is kept by three") (every (# (= %0 3)) (scene-keepers sc path))))
			objects)
		(bind '(path data) (first objects))
		(defq back (storage-walk path 0 +scene_gopher :nil 30 (scene-svc sc "m-e")))
		(scene-say sc "A gopher sent from m-e to " path " to add up its bytes went by way of "
			(join (map (# (first %0)) (ifn (second back) (list))) " "))
		(scene-check sc "it came back with what they add up to" (and back (= (first back) (scene-sum data))))
		(scene-check sc "having been to more than one member"
			(> (length (reduce (lambda (all (who &ignore)) (if (find who all) all (push all who))) (ifn (second back) (list)) (list))) 1)))
	(scene-end sc))
