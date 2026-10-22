(report-header "System: Storage, a snapshot over five members: taken at one, things changed, restored from another")

(import "service/storage/scene.inc")

;4s on a quiet Pi, 66s and more under a full run's load; one stuck is still
;given up on when nothing has been said for a minute
(defq ss_said (list) ss_result (scene-run "snapshot" (lambda (line) (if line (push ss_said line)))))
(unless ss_result (print (last ss_said)))
(assert-true "the snapshot scene ran to its end" ss_result)
(when ss_result
	(assert-eq (cat "no check of it failed, " (join (filter (# (starts-with "  FAIL" %0)) ss_said) "; ")) 0 (second ss_result))
	(assert-true "its checks held" (> (first ss_result) 8)))
