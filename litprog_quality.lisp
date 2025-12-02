;; ========================================================================
;; LITPROG QUALITY - Code Quality Analysis (Phase 3)
;; ========================================================================
;;
;; Analyze literate programs for code quality, complexity, and style
;; Features:
;; - Complexity metrics (cyclomatic, cognitive)
;; - Code style checking
;; - Documentation coverage
;; - Chunk size analysis
;; - Dependency complexity
;; - Quality scoring
;; - Best practices validation
;;
;; Usage:
;;   (import "litprog_quality.lisp")
;;   (analyze-quality ctx)
;;   (check-best-practices ctx)
;;   (generate-quality-report ctx "report.html")
;;

(import "litprog.lisp")
(import "litprog_enhanced.lisp")

;; ========================================================================
;; Code Metrics
;; ========================================================================

(defun analyze-chunk-complexity (chunk)
  "Calculate complexity metrics for a chunk"
  (defq metrics (env))
  (defq code (. chunk :code))
  (defq lines (split-lines code))

  ; Lines of code
  (. metrics :loc (length lines))

  ; Non-comment lines
  (defq non-comment-lines 0)
  (each! lines
    (lambda (line)
      (if (and (> (length (trim line)) 0)
               (not (starts-with? (trim line) ";")))
        (setq non-comment-lines (inc non-comment-lines)))))
  (. metrics :ncloc non-comment-lines)

  ; Cyclomatic complexity (simplified)
  (defq complexity 1) ; Base complexity
  (each! lines
    (lambda (line)
      ; Count decision points
      (if (or (find line "if")
              (find line "while")
              (find line "cond")
              (find line "case"))
        (setq complexity (inc complexity)))))
  (. metrics :cyclomatic complexity)

  ; Cognitive complexity (nested structures)
  (defq nesting 0)
  (defq cognitive 0)
  (each! lines
    (lambda (line)
      (if (or (find line "(if")
              (find line "(while")
              (find line "(lambda"))
        (progn
          (setq nesting (inc nesting))
          (setq cognitive (+ cognitive nesting))))
      (if (find line ")")
        (setq nesting (max 0 (dec nesting))))))
  (. metrics :cognitive cognitive)

  ; Function count
  (defq func-count 0)
  (each! lines
    (lambda (line)
      (if (find line "(defun")
        (setq func-count (inc func-count)))))
  (. metrics :functions func-count)

  metrics)

(defun analyze-quality (ctx)
  "Perform comprehensive quality analysis"
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print "║  Code Quality Analysis                                        ║")
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")

  (defq total-loc 0)
  (defq total-ncloc 0)
  (defq total-complexity 0)
  (defq total-cognitive 0)
  (defq chunk-count 0)

  (defq complexity-by-chunk (env))

  (each! (env-keys (. ctx :chunks))
    (lambda (chunk-name)
      (defq chunk (. (. ctx :chunks) chunk-name))
      (defq metrics (analyze-chunk-complexity chunk))

      (. complexity-by-chunk chunk-name metrics)

      (setq total-loc (+ total-loc (. metrics :loc)))
      (setq total-ncloc (+ total-ncloc (. metrics :ncloc)))
      (setq total-complexity (+ total-complexity (. metrics :cyclomatic)))
      (setq total-cognitive (+ total-cognitive (. metrics :cognitive)))
      (setq chunk-count (inc chunk-count))))

  ; Display results
  (print "Overall Metrics:")
  (print (cat "  Total LOC:              " total-loc))
  (print (cat "  Non-comment LOC:        " total-ncloc))
  (print (cat "  Avg LOC per chunk:      " (/ total-loc (max chunk-count 1))))
  (print (cat "  Total cyclomatic:       " total-complexity))
  (print (cat "  Avg cyclomatic:         " (/ total-complexity (max chunk-count 1))))
  (print (cat "  Total cognitive:        " total-cognitive))
  (print (cat "  Avg cognitive:          " (/ total-cognitive (max chunk-count 1))))
  (print "")

  ; Find most complex chunks
  (print "Most Complex Chunks:")
  (defq sorted-chunks (sort-by-complexity complexity-by-chunk))
  (defq shown 0)
  (each! sorted-chunks
    (lambda (entry)
      (if (< shown 5)
        (progn
          (defq chunk-name (car entry))
          (defq metrics (cdr entry))
          (print (cat "  " chunk-name ": complexity=" (. metrics :cyclomatic)
                     ", cognitive=" (. metrics :cognitive)))
          (setq shown (inc shown))))))
  (print "")

  ; Quality score (0-100)
  (defq quality-score (calculate-quality-score ctx complexity-by-chunk))
  (print (cat "Quality Score: " quality-score "/100"))
  (print "")

  (if (>= quality-score 80)
    (print "✓ Excellent code quality!")
    (if (>= quality-score 60)
      (print "⚠ Good code quality, room for improvement")
      (print "❌ Code quality needs attention")))
  (print ""))

(defun calculate-quality-score (ctx metrics)
  "Calculate overall quality score (0-100)"
  (defq score 100)

  ; Penalize high complexity
  (each! (env-keys metrics)
    (lambda (chunk-name)
      (defq m (. metrics chunk-name))
      (if (> (. m :cyclomatic) 10)
        (setq score (- score 5)))
      (if (> (. m :cognitive) 15)
        (setq score (- score 5)))
      (if (> (. m :loc) 100)
        (setq score (- score 3)))))

  ; Reward good documentation
  (defq doc-lines 0)
  (each! (. ctx :order)
    (lambda (item)
      (if (. item :doc)
        (setq doc-lines (+ doc-lines (length (split-lines (. item :doc))))))))

  (defq code-lines 0)
  (each! (env-keys (. ctx :chunks))
    (lambda (chunk-name)
      (defq chunk (. (. ctx :chunks) chunk-name))
      (setq code-lines (+ code-lines (length (split-lines (. chunk :code)))))))

  (defq doc-ratio (/ doc-lines (max code-lines 1)))
  (if (> doc-ratio 0.5)
    (setq score (+ score 5)))

  (max 0 (min 100 score)))

(defun sort-by-complexity (metrics)
  "Sort chunks by complexity (descending)"
  ; Simplified sort - returns list of (name . metrics) pairs
  (defq pairs (list))
  (each! (env-keys metrics)
    (lambda (name)
      (push pairs (cons name (. metrics name)))))

  ; Simple bubble sort
  (defq n (length pairs))
  (defq i 0)
  (while (< i n)
    (defq j 0)
    (while (< j (- n i 1))
      (defq pair1 (get pairs j))
      (defq pair2 (get pairs (inc j)))
      (if (< (. (cdr pair1) :cyclomatic)
             (. (cdr pair2) :cyclomatic))
        (progn
          (set-at pairs j pair2)
          (set-at pairs (inc j) pair1)))
      (setq j (inc j)))
    (setq i (inc i)))

  pairs)

;; ========================================================================
;; Best Practices Validation
;; ========================================================================

(defun check-best-practices (ctx)
  "Check adherence to literate programming best practices"
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print "║  Best Practices Check                                         ║")
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")

  (defq issues (list))

  ; Check 1: Chunk size
  (each! (env-keys (. ctx :chunks))
    (lambda (chunk-name)
      (defq chunk (. (. ctx :chunks) chunk-name))
      (defq lines (length (split-lines (. chunk :code))))
      (if (> lines 100)
        (push issues (list
          :severity "warning"
          :message (cat "Chunk '" chunk-name "' is very large (" lines " lines). Consider splitting."))))))

  ; Check 2: Documentation ratio
  (defq doc-lines 0)
  (defq code-lines 0)
  (each! (. ctx :order)
    (lambda (item)
      (if (. item :doc)
        (setq doc-lines (+ doc-lines (length (split-lines (. item :doc))))))
      (if (. item :chunk)
        (defq chunk (. item :chunk))
        (setq code-lines (+ code-lines (length (split-lines (. chunk :code))))))))

  (defq doc-ratio (/ doc-lines (max code-lines 1)))
  (if (< doc-ratio 0.3)
    (push issues (list
      :severity "warning"
      :message (cat "Low documentation ratio (" (to-int (* doc-ratio 100)) "%). Aim for at least 30%."))))

  ; Check 3: Chunk naming
  (each! (env-keys (. ctx :chunks))
    (lambda (chunk-name)
      (if (or (< (length chunk-name) 3)
              (find chunk-name "test")
              (find chunk-name "temp"))
        (push issues (list
          :severity "info"
          :message (cat "Chunk '" chunk-name "' has unclear name. Use descriptive names."))))))

  ; Check 4: Unused chunks
  (defq deps (analyze-chunk-dependencies ctx))
  (defq all-refs (list))
  (each! (env-keys deps)
    (lambda (name)
      (each! (. deps name)
        (lambda (ref)
          (if (not (find all-refs ref))
            (push all-refs ref))))))

  (each! (env-keys (. ctx :chunks))
    (lambda (chunk-name)
      (if (and (not (find all-refs chunk-name))
               (not (. (. ctx :files) chunk-name)))
        (push issues (list
          :severity "warning"
          :message (cat "Chunk '" chunk-name "' is not referenced or tangled. Consider removing."))))))

  ; Display issues
  (if (= (length issues) 0)
    (print "✓ No issues found! Excellent work!")
    (progn
      (defq errors 0 warnings 0 infos 0)
      (each! issues
        (lambda (issue)
          (cond
            ((= (. issue :severity) "error")
             (setq errors (inc errors))
             (print (cat "❌ ERROR: " (. issue :message))))
            ((= (. issue :severity) "warning")
             (setq warnings (inc warnings))
             (print (cat "⚠️  WARNING: " (. issue :message))))
            (t
             (setq infos (inc infos))
             (print (cat "ℹ️  INFO: " (. issue :message)))))))

      (print "")
      (print (cat "Total: " errors " errors, " warnings " warnings, " infos " infos"))))
  (print ""))

;; ========================================================================
;; Documentation Coverage
;; ========================================================================

(defun analyze-doc-coverage (ctx)
  "Analyze documentation coverage and quality"
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print "║  Documentation Coverage Analysis                              ║")
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")

  ; Chunks with no surrounding documentation
  (defq undocumented (list))
  (defq i 0)
  (defq prev-was-doc nil)

  (each! (. ctx :order)
    (lambda (item)
      (if (. item :chunk)
        (if (not prev-was-doc)
          (push undocumented (. (. item :chunk) :name)))
        (setq prev-was-doc nil))
      (if (. item :doc)
        (setq prev-was-doc t))))

  (print (cat "Chunks without documentation: " (length undocumented)))
  (if (> (length undocumented) 0)
    (progn
      (print "")
      (each! undocumented
        (lambda (name)
          (print (cat "  • " name))))))
  (print "")

  ; Documentation quality heuristics
  (defq doc-blocks (list))
  (each! (. ctx :order)
    (lambda (item)
      (if (. item :doc)
        (push doc-blocks (. item :doc)))))

  (defq avg-doc-length (/ (reduce
    (lambda (sum doc)
      (+ sum (length doc)))
    doc-blocks 0) (max (length doc-blocks) 1)))

  (print (cat "Average documentation block length: " avg-doc-length " chars"))
  (print "")

  (if (< avg-doc-length 100)
    (print "⚠️  Documentation blocks are quite short. Add more detail.")
    (if (> avg-doc-length 500)
      (print "✓ Good documentation length")
      (print "ℹ️  Documentation is adequate")))
  (print ""))

;; ========================================================================
;; Quality Report Generation
;; ========================================================================

(defun generate-quality-report (ctx output-file)
  "Generate comprehensive quality report as HTML"
  (print (cat "Generating quality report: " output-file))

  (defq html "")

  ; Header
  (setq html (cat html (quality-report-header)))

  ; Executive summary
  (setq html (cat html "<div class=\"report-container\">\n"))
  (setq html (cat html "<h1>📊 Code Quality Report</h1>\n"))

  ; Quality score
  (defq metrics (env))
  (each! (env-keys (. ctx :chunks))
    (lambda (chunk-name)
      (defq chunk (. (. ctx :chunks) chunk-name))
      (. metrics chunk-name (analyze-chunk-complexity chunk))))

  (defq quality-score (calculate-quality-score ctx metrics))

  (setq html (cat html "<div class=\"quality-score\">\n"))
  (setq html (cat html "  <h2>Quality Score</h2>\n"))
  (setq html (cat html "  <div class=\"score-circle\">" quality-score "</div>\n"))
  (setq html (cat html "  <p>/100</p>\n"))
  (setq html (cat html "</div>\n"))

  ; Metrics table
  (setq html (cat html "<h2>Complexity Metrics</h2>\n"))
  (setq html (cat html "<table class=\"metrics-table\">\n"))
  (setq html (cat html "<tr><th>Chunk</th><th>LOC</th><th>Cyclomatic</th><th>Cognitive</th></tr>\n"))

  (each! (env-keys metrics)
    (lambda (chunk-name)
      (defq m (. metrics chunk-name))
      (setq html (cat html "<tr>\n"))
      (setq html (cat html "  <td>" (escape-html chunk-name) "</td>\n"))
      (setq html (cat html "  <td>" (. m :loc) "</td>\n"))
      (setq html (cat html "  <td>" (. m :cyclomatic) "</td>\n"))
      (setq html (cat html "  <td>" (. m :cognitive) "</td>\n"))
      (setq html (cat html "</tr>\n"))))

  (setq html (cat html "</table>\n"))

  (setq html (cat html "</div>\n"))
  (setq html (cat html "</body>\n</html>\n"))

  (save-file output-file html)
  (print "✓ Quality report generated!"))

(defun quality-report-header ()
  "HTML header for quality reports"
  (cat
    "<!DOCTYPE html>\n"
    "<html>\n"
    "<head>\n"
    "  <meta charset=\"UTF-8\">\n"
    "  <title>Code Quality Report</title>\n"
    "  <style>\n"
    "    body { font-family: sans-serif; background: #f5f5f5; margin: 0; padding: 20px; }\n"
    "    .report-container { max-width: 1000px; margin: 0 auto; background: white; padding: 40px; border-radius: 8px; }\n"
    "    .quality-score { text-align: center; margin: 40px 0; }\n"
    "    .score-circle { font-size: 72px; font-weight: bold; color: #4CAF50; }\n"
    "    .metrics-table { width: 100%; border-collapse: collapse; margin: 20px 0; }\n"
    "    .metrics-table th, .metrics-table td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }\n"
    "    .metrics-table th { background: #f5f5f5; font-weight: bold; }\n"
    "    .metrics-table tr:hover { background: #f9f9f9; }\n"
    "  </style>\n"
    "</head>\n"
    "<body>\n"))

;; ========================================================================
;; Helper Functions
;; ========================================================================

(defun to-int (x)
  "Convert to integer"
  (if (>= x 0)
    (floor x)
    (ceiling x)))

;; ========================================================================
;; Export
;; ========================================================================

(export
  analyze-chunk-complexity
  analyze-quality
  check-best-practices
  analyze-doc-coverage
  generate-quality-report)

(print "LITPROG Code Quality Analysis loaded! (Phase 3)")
(print "Features:")
(print "  • (analyze-quality ctx) - Comprehensive quality analysis")
(print "  • (check-best-practices ctx) - Best practices validation")
(print "  • (analyze-doc-coverage ctx) - Documentation coverage")
(print "  • (generate-quality-report ctx \"report.html\") - HTML report")
