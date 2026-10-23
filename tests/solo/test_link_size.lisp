(report-header "Link: shared memory of another size, a node of another build, is given up on")

;the second end of a shared memory link waits for the file the first made
;to be its size. One of a build whose link is another size never makes it
;so: a file at the link's name, of a few bytes, stands for it. A solo
;module, the node it is on does nothing else while it waits. On Windows a
;link is a mapping, not a file, and there is nothing to stand for it
(unless (eql (os) 'Windows)
	(defq ls_name (cat "lsize-" (str (pii-pid))) ls_file (cat "/tmp/" ls_name))
	(save "short" ls_file)
	(defq ls_t0 (pii-time) ls_link (node-link ls_name))
	(while (mail-validate ls_link) (task-sleep 100000))
	(assert-true "the link is given up on, not waited for for ever" (not (mail-validate ls_link)))
	(assert-true "in the 5 seconds the host gives it"
		(< (- (pii-time) ls_t0) (task-timeout 8)))
	(pii-remove ls_file)
	(undef (env) 'ls_name 'ls_file 'ls_t0 'ls_link))
