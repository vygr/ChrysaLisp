;; ========================================================================
;; LITPROG Performance Benchmarks
;; ========================================================================
;;
;; Comprehensive performance testing for LITPROG Phase 4
;;
;; Tests:
;; - Parse performance (small, medium, large files)
;; - Tangle performance
;; - Chunk expansion performance
;; - String operations performance
;; - Full pipeline performance
;;
;; Usage:
;;   (import "litprog_benchmark.lisp")
;;   (run-all-benchmarks)
;;

;; ========================================================================
;; Benchmark Framework
;; ========================================================================

(defq *benchmark-results* (list))

(defun benchmark (name fn &optional iterations)
	"Run a benchmark and record results"
	(defq iters (or iterations 1))
	(defq start-time (time))

	(defq i 0)
	(while (< i iters)
		(fn)
		(setq i (inc i)))

	(defq end-time (time))
	(defq elapsed (- end-time start-time))
	(defq avg-time (/ elapsed iters))

	(defq result (list
		:name name
		:iterations iters
		:total-time elapsed
		:avg-time avg-time))

	(push *benchmark-results* result)

	(print (cat "  " name ": " avg-time "ms (avg of " iters " runs)"))
	result)

(defun print-benchmark-header ()
	"Print benchmark header"
	(print "")
	(print "╔════════════════════════════════════════════════════════════════╗")
	(print "║                                                                ║")
	(print "║         LITPROG PERFORMANCE BENCHMARKS                        ║")
	(print "║                                                                ║")
	(print "╚════════════════════════════════════════════════════════════════╝")
	(print ""))

(defun print-benchmark-section (title)
	"Print benchmark section header"
	(print "")
	(print (cat "## " title))
	(print ""))

(defun print-benchmark-summary ()
	"Print summary of all benchmark results"
	(print "")
	(print "╔════════════════════════════════════════════════════════════════╗")
	(print "║  Benchmark Summary                                            ║")
	(print "╚════════════════════════════════════════════════════════════════╝")
	(print "")

	(defq total-tests (length *benchmark-results*))
	(print (cat "Total benchmarks run: " total-tests))
	(print "")

	(print "Results:")
	(each! *benchmark-results*
		(lambda (result)
			(defq name (elem result 1))
			(defq avg (elem result 7))
			(print (cat "  • " name ": " avg "ms"))))

	(print ""))

;; ========================================================================
;; Test Data Generators
;; ========================================================================

(defun generate-small-literate-file ()
	"Generate a small literate file (10 chunks, ~50 lines)"
	(defq content "# Small Test File\n\n")

	(defq i 0)
	(while (< i 10)
		(setq content (cat content "<<chunk-" i ">>=\n"))
		(setq content (cat content "; Code for chunk " i "\n"))
		(setq content (cat content "(defq var-" i " " i ")\n"))
		(setq content (cat content "(print \"Chunk " i "\")\n"))
		(setq content (cat content "@\n\n"))
		(setq i (inc i)))

	content)

(defun generate-medium-literate-file ()
	"Generate a medium literate file (100 chunks, ~500 lines)"
	(defq content "# Medium Test File\n\n")

	(defq i 0)
	(while (< i 100)
		(setq content (cat content "<<chunk-" i ">>=\n"))
		(setq content (cat content "; Code for chunk " i "\n"))
		(setq content (cat content "(defq var-" i " " i ")\n"))
		(setq content (cat content "(print \"Chunk " i "\")\n"))

		; Add some references to other chunks
		(if (> i 0)
			(setq content (cat content "<<chunk-" (dec i) ">>\n")))

		(setq content (cat content "@\n\n"))
		(setq i (inc i)))

	content)

(defun generate-large-literate-file ()
	"Generate a large literate file (500 chunks, ~2500 lines)"
	(defq content "# Large Test File\n\n")

	(defq i 0)
	(while (< i 500)
		(setq content (cat content "<<chunk-" i ">>=\n"))
		(setq content (cat content "; Code for chunk " i "\n"))
		(setq content (cat content "(defq var-" i " " i ")\n"))
		(setq content (cat content "(print \"Chunk " i "\")\n"))

		; Add some references to other chunks
		(if (> i 0)
			(setq content (cat content "<<chunk-" (dec i) ">>\n")))

		(setq content (cat content "@\n\n"))
		(setq i (inc i)))

	content)

(defun generate-deeply-nested-file ()
	"Generate a file with deep chunk nesting"
	(defq content "# Deeply Nested Test File\n\n")

	; Create a chain of 20 nested chunks
	(defq i 0)
	(while (< i 20)
		(setq content (cat content "<<nested-" i ">>=\n"))
		(setq content (cat content "; Level " i "\n"))

		(if (< i 19)
			(setq content (cat content "<<nested-" (inc i) ">>\n")))

		(setq content (cat content "@\n\n"))
		(setq i (inc i)))

	content)

;; ========================================================================
;; String Operation Benchmarks
;; ========================================================================

(defun benchmark-string-operations ()
	"Benchmark basic string operations"
	(print-benchmark-section "String Operations")

	; Test string trimming
	(benchmark "trim-string (small)"
		(lambda ()
			(trim-string "   hello world   "))
		1000)

	; Test string concatenation
	(benchmark "cat (small strings)"
		(lambda ()
			(cat "hello" " " "world"))
		1000)

	; Test string splitting
	(benchmark "split-lines (small)"
		(lambda ()
			(split-lines "line1\nline2\nline3\nline4\nline5"))
		1000)

	; Test string slicing
	(benchmark "slice (small)"
		(lambda ()
			(slice "hello world" 0 5))
		1000))

;; ========================================================================
;; Parse Benchmarks
;; ========================================================================

(defun benchmark-parsing ()
	"Benchmark file parsing"
	(print-benchmark-section "Parse Performance")

	; Small file
	(defq small-content (generate-small-literate-file))
	(benchmark "parse small file (10 chunks)"
		(lambda ()
			(parse-literate-content small-content))
		100)

	; Medium file
	(defq medium-content (generate-medium-literate-file))
	(benchmark "parse medium file (100 chunks)"
		(lambda ()
			(parse-literate-content medium-content))
		10)

	; Large file
	(defq large-content (generate-large-literate-file))
	(benchmark "parse large file (500 chunks)"
		(lambda ()
			(parse-literate-content large-content))
		1))

;; ========================================================================
;; Chunk Expansion Benchmarks
;; ========================================================================

(defun benchmark-chunk-expansion ()
	"Benchmark chunk expansion"
	(print-benchmark-section "Chunk Expansion Performance")

	; Simple expansion (no nesting)
	(defq ctx (create-context))
	(add-chunk-to-context ctx "simple" "; Simple code\n(print \"hello\")\n" 1)

	(benchmark "expand simple chunk"
		(lambda ()
			(expand-chunk-refs ctx "simple"))
		1000)

	; Nested expansion (deep recursion)
	(defq nested-content (generate-deeply-nested-file))
	(defq nested-ctx (parse-literate-content nested-content))

	(benchmark "expand deeply nested chunk (20 levels)"
		(lambda ()
			(expand-chunk-refs nested-ctx "nested-0"))
		10))

;; ========================================================================
;; Tangle Benchmarks
;; ========================================================================

(defun benchmark-tangle ()
	"Benchmark tangle operations"
	(print-benchmark-section "Tangle Performance")

	; Small file
	(defq small-content (generate-small-literate-file))
	(defq small-ctx (parse-literate-content small-content))

	(benchmark "tangle small file (10 chunks)"
		(lambda ()
			(tangle-all-chunks small-ctx))
		100)

	; Medium file
	(defq medium-content (generate-medium-literate-file))
	(defq medium-ctx (parse-literate-content medium-content))

	(benchmark "tangle medium file (100 chunks)"
		(lambda ()
			(tangle-all-chunks medium-ctx))
		10)

	; Large file
	(defq large-content (generate-large-literate-file))
	(defq large-ctx (parse-literate-content large-content))

	(benchmark "tangle large file (500 chunks)"
		(lambda ()
			(tangle-all-chunks large-ctx))
		1))

;; ========================================================================
;; Full Pipeline Benchmarks
;; ========================================================================

(defun benchmark-full-pipeline ()
	"Benchmark complete parse→tangle pipeline"
	(print-benchmark-section "Full Pipeline Performance")

	; Small file: parse + tangle
	(defq small-content (generate-small-literate-file))
	(benchmark "full pipeline: small (10 chunks)"
		(lambda ()
			(defq ctx (parse-literate-content small-content))
			(tangle-all-chunks ctx))
		100)

	; Medium file: parse + tangle
	(defq medium-content (generate-medium-literate-file))
	(benchmark "full pipeline: medium (100 chunks)"
		(lambda ()
			(defq ctx (parse-literate-content medium-content))
			(tangle-all-chunks ctx))
		10)

	; Large file: parse + tangle
	(defq large-content (generate-large-literate-file))
	(benchmark "full pipeline: large (500 chunks)"
		(lambda ()
			(defq ctx (parse-literate-content large-content))
			(tangle-all-chunks ctx))
		1))

;; ========================================================================
;; Real-World Example Benchmarks
;; ========================================================================

(defun benchmark-real-examples ()
	"Benchmark actual example files"
	(print-benchmark-section "Real Example Files")

	; test_simple.lit
	(if (file-exists? "test_simple.lit")
		(benchmark "test_simple.lit (real file)"
			(lambda ()
				(parse-literate-file "test_simple.lit"))
			100))

	; real_world_string_utils.lit
	(if (file-exists? "examples/real_world_string_utils.lit")
		(benchmark "real_world_string_utils.lit (real file)"
			(lambda ()
				(parse-literate-file "examples/real_world_string_utils.lit"))
			10)))

;; ========================================================================
;; Scalability Analysis
;; ========================================================================

(defun analyze-scalability ()
	"Analyze how performance scales with input size"
	(print-benchmark-section "Scalability Analysis")

	(print "Testing parse performance vs chunk count:")
	(print "")

	; Test different sizes
	(defq sizes (list 10 25 50 100 250 500))

	(each! sizes
		(lambda (size)
			(defq content (cat "# Scalability Test\n\n"))
			(defq i 0)
			(while (< i size)
				(setq content (cat content "<<chunk-" i ">>=\n"))
				(setq content (cat content "(defq x " i ")\n"))
				(setq content (cat content "@\n\n"))
				(setq i (inc i)))

			(defq start (time))
			(parse-literate-content content)
			(defq elapsed (- (time) start))

			(print (cat "  " size " chunks: " elapsed "ms"))))

	(print ""))

;; ========================================================================
;; Main Benchmark Runner
;; ========================================================================

(defun run-all-benchmarks ()
	"Run all performance benchmarks"
	(print-benchmark-header)

	(print "Starting comprehensive performance benchmarks...")
	(print "This may take a few moments...")
	(print "")

	; Run all benchmark suites
	(benchmark-string-operations)
	(benchmark-parsing)
	(benchmark-chunk-expansion)
	(benchmark-tangle)
	(benchmark-full-pipeline)
	(benchmark-real-examples)
	(analyze-scalability)

	; Print summary
	(print-benchmark-summary)

	(print "✓ All benchmarks complete!")
	(print ""))

;; ========================================================================
;; Helper Functions
;; ========================================================================

(defun parse-literate-content (content)
	"Parse literate content from string (for benchmarking)"
	; This simulates parse-literate-file but works with string content
	(defq ctx (create-context))
	(defq lines (split-lines content))
	(defq i 0)
	(defq in-chunk nil)
	(defq current-chunk-name nil)
	(defq chunk-code (list))

	(while (< i (length lines))
		(defq line (elem lines i))

		; Check for chunk definition
		(if (starts-with-str line "<<")
			(progn
				; Save previous chunk if exists
				(if current-chunk-name
					(add-chunk-to-context ctx current-chunk-name
						(join-strings chunk-code "\n") i))

				; Start new chunk
				(defq end-pos (find-str line ">>="))
				(if end-pos
					(progn
						(setq current-chunk-name (slice line 2 end-pos))
						(setq chunk-code (list))
						(setq in-chunk t)))))

		; Check for chunk end
		(if (starts-with-str line "@")
			(progn
				; Save chunk
				(if current-chunk-name
					(add-chunk-to-context ctx current-chunk-name
						(join-strings chunk-code "\n") i))
				(setq in-chunk nil)
				(setq current-chunk-name nil)
				(setq chunk-code (list))))

		; Accumulate code
		(if (and in-chunk
				(not (starts-with-str line "<<"))
				(not (starts-with-str line "@")))
			(push chunk-code line))

		(setq i (inc i)))

	ctx)

(defun file-exists? (filename)
	"Check if file exists (simplified)"
	; In real implementation, would check actual file system
	; For now, return t for test files we know exist
	(or (eql filename "test_simple.lit")
		(eql filename "examples/real_world_string_utils.lit")))

(defun find-str (haystack needle)
	"Find position of needle in haystack"
	; Simplified implementation
	; Real implementation would search string
	(defq i 0)
	(defq found nil)
	(defq h-len (length haystack))
	(defq n-len (length needle))

	(while (and (< i (- h-len n-len)) (not found))
		(defq match t)
		(defq j 0)
		(while (and (< j n-len) match)
			(if (not (eql (char haystack (+ i j)) (char needle j)))
				(setq match nil))
			(setq j (inc j)))
		(if match
			(setq found i))
		(setq i (inc i)))

	found)

(defun join-strings (lst sep)
	"Join list of strings with separator"
	(if (empty? lst)
		""
		(reduce (lambda (acc item)
			(if (eql acc "")
				item
				(cat acc sep item)))
			lst "")))

(defun empty? (lst)
	"Check if list is empty"
	(eql (length lst) 0))

;; ========================================================================
;; Expected Performance Targets
;; ========================================================================

(defun print-performance-targets ()
	"Print expected performance targets for reference"
	(print "")
	(print "╔════════════════════════════════════════════════════════════════╗")
	(print "║  Expected Performance Targets (Phase 4)                      ║")
	(print "╚════════════════════════════════════════════════════════════════╝")
	(print "")
	(print "Target performance for LITPROG core operations:")
	(print "")
	(print "Parse Performance:")
	(print "  • Small (10 chunks):      < 20ms")
	(print "  • Medium (100 chunks):    < 100ms")
	(print "  • Large (500 chunks):     < 500ms")
	(print "")
	(print "Tangle Performance:")
	(print "  • Small (10 chunks):      < 10ms")
	(print "  • Medium (100 chunks):    < 60ms")
	(print "  • Large (500 chunks):     < 300ms")
	(print "")
	(print "Full Pipeline:")
	(print "  • Small (10 chunks):      < 30ms")
	(print "  • Medium (100 chunks):    < 160ms")
	(print "  • Large (500 chunks):     < 800ms")
	(print "")
	(print "Scalability:")
	(print "  • Should be O(n) where n = file size")
	(print "  • Memory usage O(n) for chunk storage")
	(print ""))

;; ========================================================================
;; Export
;; ========================================================================

(export
	run-all-benchmarks
	benchmark-string-operations
	benchmark-parsing
	benchmark-chunk-expansion
	benchmark-tangle
	benchmark-full-pipeline
	print-performance-targets)

(print "LITPROG Benchmark Suite loaded!")
(print "Run (run-all-benchmarks) to execute all performance tests")
(print "Run (print-performance-targets) to see expected performance")
