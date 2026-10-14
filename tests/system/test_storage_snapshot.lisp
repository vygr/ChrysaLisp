(report-header "System: Storage, a snapshot over five members: taken at one, things changed, restored from another")

(import "service/storage/scene.inc")

(defq ss_said (list) ss_result (scene-run "snapshot" (lambda (line) (if line (push ss_said line))) :nil 120))
(assert-true "the snapshot scene ran to its end" ss_result)
(when ss_result
	(assert-eq (cat "no check of it failed, " (join (filter (# (starts-with "  FAIL" %0)) ss_said) "; ")) 0 (second ss_result))
	(assert-true "its checks held" (> (first ss_result) 8)))
