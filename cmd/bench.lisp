(import "lib/options/options.inc")
(import "lib/files/files.inc")
(import "tests/bench.inc")

(defq usage `(
(("-h" "--help")
"Usage: bench [options] [path] ...

    options:
        -h --help: this help info.
        -m --match str: only the modules with str in their path.
        -l --list: list the modules, do not run them.
        -r --runs num: the goes each benchmark gets, default 5.
            The best of them is what is shown.
        -s --save file: write what was timed to the file.
        -c --compare file: show each against what a file written
            by -s has for it, and how much it has changed.

    Run the benchmarks, tests/bench/bench_<name>.lisp, or just
    the module paths given, one after another in this task, and
    print what each thing took, in ns.

    They are not tests and (tests) does not run them. They are
    to hold a change to: 'bench -s before.txt', make the
    change and build, 'bench -c before.txt'.

    A time is of this machine, as loaded as it is. Run them on
    a quiet one, with one node, and a change of a few percent
    in a time of a few ns is as likely the machine as the code.")
(("-m" "--match") ,(opt-str 'opt_m))
(("-l" "--list") ,(opt-flag 'opt_l))
(("-r" "--runs") ,(opt-num 'opt_r))
(("-s" "--save") ,(opt-str 'opt_s))
(("-c" "--compare") ,(opt-str 'opt_c))
))

(defun bench-modules (pattern)
	; (bench-modules pattern) -> paths
	(sort (filter (# (and (starts-with "bench_" (slice %0 (rfind "/" %0) -1))
			(or (not pattern) (found? %0 pattern))))
		(files-all "tests/bench" '(".lisp")))))

(defun ns (tenths)
	; (ns tenths) -> str
	(cat (str (/ tenths 10)) "." (str (% tenths 10))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_m :nil opt_l :nil opt_r :nil opt_s :nil opt_c :nil
				args (options stdio usage)))
		(defq modules (if (empty? (defq modules (rest args))) (bench-modules opt_m) modules))
		(cond
			(opt_l (each (const print) modules))
			(:t ;what a file written before has, by module and name
				(defq was (Fmap 31) saved (list) tab (ascii-char 9))
				(and opt_c (defq stream (file-stream opt_c))
					(lines! (lambda (line)
						(when (= (length (defq parts (split line tab))) 3)
							(. was :insert (cat (first parts) tab (second parts)) (str-as-num (third parts))))
						;(lines!) goes on while it is given :nil
						:nil)
						stream))
				(if opt_r (setq *bench_runs* (max 1 opt_r)))
				(each (lambda (module)
					(setq *bench_results* (list))
					(print module)
					(stream-flush (io-stream 'stdout))
					(import module)
					(defq width (+ 2 (max-length (map (const first) *bench_results*))))
					(each (lambda ((name tenths))
						(push saved (cat module tab name tab (str tenths)))
						(defq then (. was :find (cat module tab name)))
						(print "  " (rpad name width)
							(if then (cat (pad (ns then) 7) " -> ") "")
							(pad (ns tenths) 7) " ns"
							(if (and then (> then 0))
								(cat "  " (if (>= tenths then) "+" "-")
									(ns (/ (* (abs (- tenths then)) 1000) then)) "%")
								"")))
						*bench_results*)
					(stream-flush (io-stream 'stdout)))
					modules)
				(and opt_s (nempty? saved)
					(save (apply (const cat) (map (# (cat %0 (ascii-char 10))) saved)) opt_s))))))
