(report-header "System: Storage, the scenes of tests/storage/, run as storage -t runs them, and recorded")

(import "service/storage/scene.inc")

(defq sc_names (map (const first) (scene-list)))
(assert-true "the scenes are listed, each with what it is for"
	(and (find "split" sc_names) (find "stop" sc_names) (every (# (nempty? (second %0))) (scene-list))))
(assert-eq "a scene that is not there is not run" :nil (scene-run "no such scene" (lambda (line) line)))

;the quick ones: each holds all its checks, and what its members did is recorded
(each (lambda (name)
	(defq said (list) recorded (list)
		result (scene-run name (lambda (line) (push said line)) (lambda (line) (if line (push recorded line))) 120))
	(assert-true (cat name ": it ran to the end") result)
	(when result
		(assert-eq (cat name ": no check of it failed, " (join (filter (# (starts-with "  FAIL" %0)) said) "; ")) 0 (second result))
		(assert-true (cat name ": its checks held") (> (first result) 0))
		(assert-true (cat name ": what its members did was recorded") (> (length recorded) 10))))
	;stop is in test_storage_scenes_stop: the two together come near what a
	;batch may take on a Raspberry Pi
	'("split"))
