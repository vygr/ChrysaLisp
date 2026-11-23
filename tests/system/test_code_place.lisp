(report-header "Code place: no instruction after data off a place of 4, in any function of obj/vp/")

;Code that comes after data, the names of states, a table, is on the place
;the data's bytes leave it, and no translator moves it. An arm64, a RISC-V
;or a LoongArch instruction off a place of 4 faults as it runs, a VP64 one
;off a place of 2 is off the VP's rule: lib/xml/parse's error path was 3
;past, after 43 bytes of strings, and a wrong argument to it took an M4's
;node down, Illegal instruction, 2026-11-23. The place is known from the
;end of the code before, a place of 4, on by the data's bytes. Data after
;its function's last instruction is not looked at, nothing runs there
(defq cp_found (list) cp_funcs 0 cp_data (Fmap 11) cp_meta (Fset 11))
(each (# (. cp_data :insert (first %0) (second %0))) '((byte 1) (short 2) (int 4) (long 8)))
(each (# (. cp_meta :insert %0)) '(label nop trash))
(each (lambda (file)
		(defq stream (file-stream file))
		(while (defq entry (first (read stream)))
			(++ cp_funcs)
			(defq at 0 after_data :nil)
			(each (lambda (op)
					(defq head (if (list? op) (first op) op))
					(cond
						((. cp_meta :find head))
						((defq size (. cp_data :find head))
							(setq at (+ at (* size (dec (length op)))) after_data :t))
						((eql head 'string)
							(setq at (+ at (length (second op))) after_data :t))
						((eql head 'align)
							(defq n (second op))
							(setq at (* n (/ (+ at (dec n)) n))))
						(:t (and after_data (/= 0 (% at 4))
								(push cp_found (cat (first entry) " op " (str (!)) ", " (str (% at 4)) " past")))
							(setq at 0 after_data :nil))))
				(second entry)))
		(task-slice))
	(filter (# (not (starts-with "obj/vp/tests/" %0))) (files-all "obj/vp" '(".vp"))))
(assert-true "there are functions to look at" (> cp_funcs 1000))
(assert-list-eq "no instruction after data off a place of 4" '() cp_found)

(undef (env) 'cp_found 'cp_funcs 'cp_data 'cp_meta)
