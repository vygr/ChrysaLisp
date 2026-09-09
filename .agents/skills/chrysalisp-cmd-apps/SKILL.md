---
name: chrysalisp-cmd-apps
display-name: ChrysaLisp CMD Apps
description: Use when writing or modifying ChrysaLisp command-line apps (cmd/*.lisp) — options, stdin/stdout, pipes, and pipe-farm parallelism.
---

# ChrysaLisp CMD App Skill

A ChrysaLisp command app is a short-lived task in `cmd/` named
`<name>.lisp`. It takes options and paths, does work on them, and writes
results to stdout. The general ChrysaLisp disciplines (see the
`chrysalisp` skill, `LLM.md`, and `docs/ai_digest/`) apply on top of these
app-specific patterns.

## How CMD Apps Run

CMD apps run inside the TUI or Terminal app (`apps/tui/`), which uses
the `Pipe` class from `lib/task/pipe.inc`:

*	You type bare command names — no `cmd/` prefix, no `.lisp`
	extension. Each element of the command line resolves to
	`cmd/<name>.lisp` automatically, with its remaining words passed as
	arguments.

*	The kernel launches each task and runs its `main` function (via
	`class/lisp/run.vp`), so the app's entry point is `(defun main ())`.

*	Elements are chained into a pipeline; each element's stdout feeds
	the next element's stdin, all via message-passing streams.

*	**Task distribution binders** control where each stage is placed in
	the cluster (see `LLM.md` and `docs/ai_digest/task_pipelines.md`):

	*	`|` (distribution): launches the next task with
		`+kn_call_child`, triggering emergent load balancing. The search
		starts at the previous task's node, so stages land near their
		data source.

	*	`!` (pinning): launches the next task with `+kn_call_open`,
		pinned to the exact same node as the previous task. Use it for
		communication-intensive stages that should share local memory.

*	Example:

	files obj/vp/ | grep -v apps/ | grep -v /create | grep -v /type
	| trace -l

## Canonical Structure

Copy `cmd/template.lisp` as the starting point:

	(import "lib/options/options.inc")
	(import "lib/task/cmd.inc")

	(defq usage `(
	(("-h" "--help")
	"Usage: template [options] [path] ...

	    options:
	        -h --help: this help info.
	        -j --jobs num: max jobs per batch, default 1.

	    If no paths given on command line
	    then will take paths from stdin.")
	(("-j" "--jobs") ,(opt-num 'opt_j))
	))

	;do the work on a file
	(defun work (file)
		(print "Work on file: " file))

	(defun main ()
		;initialize pipe details and command args, abort on error
		(when (and
				(defq stdio (create-stdio))
				(defq opt_j 1 args (options stdio usage)))
			;from args ?
			(if (empty? (defq jobs (rest args)))
				;no, so from stdin
				(lines! (# (push jobs %0)) (io-stream 'stdin)))
			(if (<= (length jobs) opt_j)
				;do the work when batch size ok !
				(each (const work) jobs)
				;do the jobs out there, by calling myself !
				(each (lambda ((job result)) (prin result))
					(pipe-farm (map (# (str (first args)
							" -j " opt_j
							" " (slice (str %0) 1 -2)))
						(partition jobs opt_j)))))))

## Key Patterns

*	**Options:** The `usage` form is a quasiquote: the first element
	pairs `("-h" "--help")` with the help text; each following element
	is `(short long) ,(handler 'var)` where handler is `opt-flag`,
	`opt-num`, or `opt-str`. Initialize every option variable with its
	default before `(options stdio usage)` runs — typically in the same
	`defq` — because options only overwrites what was given on the
	command line.

*	**Args and stdin:** `(options stdio usage)` returns the remaining
	args; `args`[0] is the program name, so jobs are `(rest args)`.
	When no paths are given, read them from stdin line by line.

*	**Self-invocation for parallelism:** When the job batch exceeds
	`opt_j`, farm it out by calling *yourself*: build each child's
	command line from `(first args)` (the program name) plus your flags
	plus the job paths, and run them with `(pipe-farm (partition jobs
	opt_j))`. Results come back as `((job result) ...)`; print each
	result with `prin`.

*	**Quoting:** Job paths arrive quoted; strip the surrounding quotes
	with `(slice (str %0) 1 -2)` when rebuilding command lines. Encode
	tricky arguments (e.g., patterns) with `hex-encode` and decode them
	on the child side (see `cmd/grep.lisp`).

*	**Cooperative scheduling:** Call `(task-slice)` periodically in
	long loops (e.g., per line of a large file) so the scheduler can
	run other tasks.

*	**Output:** Write results to stdout with `print`/`prin`; that is
	all the pipe needs.

## Examples

*	`cmd/template.lisp`: minimal starting point.

*	`cmd/wc.lisp`: multiple flags, default-all behavior, single-file
	fast path.

*	`cmd/grep.lisp`: a positional pattern argument, many modes,
	hex-encoded pattern passing, and both file-mode (farmed) and
	stream-mode (stdin) paths.
