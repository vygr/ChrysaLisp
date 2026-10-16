;an object pinned at a member, which fetches all of it, and a gopher goes no further

(import "service/storage/scene.inc")

(defun main ()
	(defq sc (scene-begin "An object pinned at one member, which then holds all of it. Unpinned, it lets go."))
	(when (scene-check sc "five members start and meet" (scene-members sc +scene_ids))
		(defq data (scene-bytes 20 "pinned"))
		(storage-put "big" data :nil (scene-svc sc "m-a"))
		(scene-settle sc)
		(scene-say sc "big is pinned at m-b.")
		(storage-pin "big" :nil (scene-svc sc "m-b"))
		(scene-settle sc)
		(scene-check sc "m-b has every block of it" (empty? (storage-pins-missing (scene-svc sc "m-b"))))
		(defq back (storage-walk "big" 0 +scene_gopher :nil 30 (scene-svc sc "m-d")))
		(scene-say sc "A gopher from m-d went by way of " (join (map (# (first %0)) (ifn (second back) (list))) " "))
		(scene-check sc "it came back right" (and back (= (first back) (scene-sum data))))
		(scene-check sc "and stayed at m-b once it got there"
			(some! (# (eql (first %0) "m-b")) (list (ifn (second back) (list :nil))) :t 1 -1))
		(scene-say sc "Unpinned.")
		(storage-pin "big" :t (scene-svc sc "m-b"))
		(scene-settle sc)
		(scene-check sc "every block is back to three keepers" (every (# (= %0 3)) (scene-keepers sc "big"))))
	(scene-end sc))
