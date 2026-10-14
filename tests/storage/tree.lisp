;every file of the tree put into five members by a farm, each file at the next of them, and read back

(import "service/storage/scene.inc")

(defun main ()
	(defq sc (scene-big (scene-begin "Every file of this folder put into five members by a farm of workers, then read back and compared.")))
	(when (scene-check sc "five members with room start and meet" (scene-members sc +scene_ids))
		(scene-say sc "Putting, at all five at once.")
		(defq said (mail-mbox) farm (open-child "apps/system/storage/tree.lisp" +kn_call_run) result :nil)
		(mail-send farm (join (list (hex-encode said) "." "tree/" (join (map (# (hex-encode (second %0))) (. sc :find :svcs)) " "))
			(ascii-char 10)))
		(while (not result)
			(defq msg (mail-read said))
			(cond
				((starts-with "done " msg) (setq result (map (const str-as-num) (rest (split msg " ")))))
				((find (first (split msg " ")) '("notput" "notread")) (scene-say sc "  " msg))))
		(bind '(put same count bytes put_ms read_ms) result)
		(scene-say sc put " of " count " files put in " (/ put_ms 1000) "." (/ (% put_ms 1000) 100) " s, "
			same " read back the same, " (/ bytes 1048576) " MB, in " (/ read_ms 1000) "." (/ (% read_ms 1000) 100) " s")
		(scene-check sc "every file is put" (= put count))
		(scene-check sc "and read back the same" (= same count)))
	(scene-end sc))
