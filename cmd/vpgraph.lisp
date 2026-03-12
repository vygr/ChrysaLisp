(import "lib/options/options.inc")
(import "lib/files/files.inc")

(defq usage `(
(("-h" "--help")
"Usage: vpgraph [options] [path] ...

	options:
		-h --help: this help info.

	Scan .vp files and create a VP function/method call graph.
	If no paths are given on the command line, it will read
	paths from stdin.")
))

(defun strip-colon (s)
	(if (starts-with ":" s) (slice s 1 -1) s))

; comprehensive list of direct calls, jumps, indirect jumps, and abi host calls
(defq +all_calls ''("call" "jump" "s-call" "s-jump" "d-call" "d-jump"
	"v-call" "v-jump" "f-call" "f-jmp" "r-call" "r-jump"
	"l-call" "vp-call" "vp-jmp" "vp-call-p" "vp-jmp-p"
	"vp-call-r" "vp-call-i" "vp-call-abi"
	"vp-jmp-r" "vp-jmp-i"
	"host-os-call" "host-gui-call" "host-audio-call"
	"abi-call-table"))

(defun build-graph (files)
	(defq graph (Fmap 101) current_caller :nil)
	(each (lambda (file)
		(if (defq stream (file-stream file))
			(lines! (lambda (line)
				; Strip comments to prevent false parsing
				(defq comment_pos (find ";" line))
				(if comment_pos
					(setq line (slice line 0 comment_pos)))
				; Break line down into space/bracket/quote separated tokens
				(defq tokens (split line (const (char-class " '()\q\r\t"))))
				(when (nempty? tokens)
					(defq type (first tokens))
					(cond
						((eql type "def-method")
							(when (>= (length tokens) 3)
								(setq current_caller (cat (strip-colon (second tokens)) "/" (strip-colon (third tokens))))
								(unless (. graph :find current_caller)
									(. graph :insert current_caller (Fset 11)))))
						((eql type "def-func")
							(when (>= (length tokens) 2)
								(setq current_caller (second tokens))
								; Resolve programmatic paths injected into 'def-func'
								(if (and (eql current_caller "path-to-absolute") (>= (length tokens) 3))
									(setq current_caller (path-to-absolute (third tokens) file)))
								(unless (. graph :find current_caller)
									(. graph :insert current_caller (Fset 11)))))
						((eql type "def-func-end")
							(setq current_caller :nil))
						((find type +all_calls)
							(when (and current_caller (>= (length tokens) 2))
								(if (starts-with ":" (second tokens))
									(when (>= (length tokens) 3)
										(defq callee (cat (strip-colon (second tokens)) "/" (strip-colon (third tokens))))
										(. (. graph :find current_caller) :insert callee))
									(progn
										(defq callee (second tokens))
										; De-reference variables mapped under the '$' identifier syntax
										(if (eql callee "$")
											(if (>= (length tokens) 3)
												(setq callee (third tokens))))
										(. (. graph :find current_caller) :insert callee)))))
						((find "/" type)
							; Handle fast inline macro inclusions (eg: class/obj/ref) 
							(when current_caller
								(. (. graph :find current_caller) :insert type)))
					))) stream))) files)
	graph)

(defun print-graph (graph)
	(defq callers (list))
	(. graph :each (lambda (caller callees)
		(push callers (list caller callees))))
	(sort callers (lambda (a b) (cmp (first a) (first b))))
	(each (lambda (entry)
		(bind '(caller callees) entry)
		(print caller)
		(defq callee_list (list))
		(. callees :each (lambda (callee) (push callee_list callee)))
		(sort callee_list)
		(each (lambda (callee)
			(print "  -> " callee)) callee_list)) callers))

(defun main ()
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(defq files (rest args))
		(if (empty? files)
			; Pipe lines recursively into the files list
			(lines! (lambda (line) (push files line)) (io-stream 'stdin)))
		; filtering of only .vp VP Assembler files
		(setq files (filter (lambda (f) (ends-with ".vp" f)) files))
		(when (nempty? files)
			(defq graph (build-graph files))
			(print-graph graph))))