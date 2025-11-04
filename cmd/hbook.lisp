(import "lib/options/options.inc")
(import "lib/task/cmd.inc")
(import "lib/streams/huffman.inc")

(defq usage `(
(("-h" "--help")
"Usage: hbook [options] [path] ...

	options:
		-h --help: this help info.
		-j --jobs num: max jobs per batch, default 1.
		-t --tbits num: bit size for data tokens, default 8.
		-c --codebook path: codebook filename, default :nil.

	Scan files for Huffman frequency information, creates
	a merged tree of all the information.

	Optionally create and save a codebook for use with
	the static huffman library.

	If no paths given on command line
	then will take paths from stdin.")
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-t" "--tbits") ,(opt-num 'opt_t))
(("-c" "--codebook") ,(opt-str 'opt_c))
))

;do the work on a file
(defun work (file)
	(defq freqs (huffman-build-freq-map (file-stream file) opt_t))
	(. freqs :each (lambda (k v) (. freq_map :update k (# (if %0 (+ %0 v) v))))))

;merge child work
(defun merge-work ((job result))
	(defq freqs (tree-load (string-stream result)))
	(. freqs :each (lambda (k v) (. freq_map :update k (# (if %0 (+ %0 v) v))))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_c :nil opt_j 1 opt_t 8 args (options stdio usage)))
		(defq freq_map (Fmap 101))
		;from args ?
		(if (empty? (defq jobs (rest args)))
			;no, so from stdin
			(lines! (# (push jobs %0)) (io-stream 'stdin)))
		(if (<= (length jobs) opt_j)
			;do the work when batch size ok !
			(each (const work) jobs)
			;do the jobs out there, by calling myself !
			(each (const merge-work)
				(pipe-farm (map (# (str (first args)
						" -j " opt_j
						" -t " opt_t
						" " (slice (str %0) 1 -2)))
					(partition jobs opt_j)))))
		;write codebook if requested
		(if opt_c (huffman-write-codebook
			(file-stream opt_c +file_open_write) opt_t freq_map))
		;output results
		(tree-save (io-stream 'stdout) freq_map)))
