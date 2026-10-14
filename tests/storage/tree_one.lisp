;every file of the tree put into one member, a file at a time, and read back

(import "service/storage/scene.inc")

(defun main ()
	(defq sc (scene-big (scene-begin "Every file of this folder put into one member, a file at a time, then read back and compared."))
		files (filter (# (not (or (starts-with "./deps/" %0) (starts-with "./tests/scratch/" %0)))) (files-all "." '(""))))
	(when (scene-check sc "a member with room starts" (scene-members sc '("m-a")))
		(defq svc (scene-svc sc "m-a") put 0 same 0 t0 (pii-time))
		(each (lambda (file)
			(if (eql (first (storage-put-file (cat "tree/" (slice file 2 -1)) file :nil svc)) :ok) (++ put)
				(scene-say sc "  not put " file)))
			files)
		(defq t1 (pii-time))
		(each (lambda (file)
			(if (storage-same-file? (cat "tree/" (slice file 2 -1)) file svc) (++ same)
				(scene-say sc "  not the same " file)))
			files)
		(scene-say sc put " of " (length files) " files put in " (/ (- t1 t0) 1000000) " s, " same
			" read back the same in " (/ (- (pii-time) t1) 1000000) " s")
		(scene-check sc "every file is put" (= put (length files)))
		(scene-check sc "and read back the same" (= same (length files))))
	(scene-end sc))
