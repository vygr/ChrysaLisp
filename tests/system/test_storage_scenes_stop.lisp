(report-header "System: Storage, the stop scene, run as storage -t runs it, and recorded")

(import "service/storage/scene.inc")

;a scene of its own module: with split it came near what a batch may take
;on a Raspberry Pi
(defq ss_said (list) ss_recorded (list)
	ss_result (scene-run "stop" (lambda (line) (push ss_said line)) (lambda (line) (if line (push ss_recorded line))) 120))
(assert-true "stop: it ran to the end" ss_result)
(when ss_result
	(assert-eq (cat "stop: no check of it failed, " (join (filter (# (starts-with "  FAIL" %0)) ss_said) "; ")) 0 (second ss_result))
	(assert-true "stop: its checks held" (> (first ss_result) 0))
	(assert-true "stop: what its members did was recorded" (> (length ss_recorded) 10)))
