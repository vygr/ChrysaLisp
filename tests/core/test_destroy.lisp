(report-header "Objects: one destroyed from inside another, however deep, and the task's stack")

;a list in a list in a list. Let go of, each is destroyed from inside the
;destroying of the one that held it. That was a call further down the
;task's stack for each, and at about 250 deep the task was gone, with
;nothing said. Now one with little stack left is put by, and done by the
;destroy further out that has the room, (:obj :destroy)
(defun ds-nest (depth &optional leaf)
	(defq l (list (ifn leaf "leaf")))
	(times depth (setq l (list l "beside")))
	l)

(each (lambda (depth)
	(defq l (ds-nest depth))
	(setq l :nil)
	(assert-true (cat "a list " (str depth) " deep is let go of, and the task goes on") :t))
	'(10 240 300 5000 100000))

;and it is all let go of: what the system has free is as it was, near enough
(defun ds-used () (second (kernel-stats)))
(defq ds_l (ds-nest 1000) ds_l :nil ds_before (ds-used))
(times 20 (defq ds_l (ds-nest 20000)) (setq ds_l :nil))
(assert-true "twenty of them, 20,000 deep, made and let go of: no more memory is held than before"
	(< (- (ds-used) ds_before) 1000000))

;what is inside is destroyed with it: a stream in the deepest, closed when the outermost goes
(defq ds_path (cat "tests/scratch/destroy_" (str (pii-pid)) ".txt")
	ds_l (ds-nest 3000 (file-stream ds_path +file_open_write)))
(write-blk (first (reduce (lambda (l n) (first l)) (range 0 3000) ds_l)) "written")
(setq ds_l :nil)
(assert-eq "a file stream 3,000 lists deep is flushed and closed when the outermost is let go of" "written" (load ds_path))
(pii-remove ds_path)
