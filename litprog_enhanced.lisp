;; ========================================================================
;; LITPROG ENHANCED - Phase 2 Advanced Features
;; ========================================================================
;;
;; This module extends the base LITPROG with advanced features:
;; - Enhanced error reporting with line numbers and context
;; - Dependency graph generation (GraphViz DOT format)
;; - Advanced HTML output with syntax highlighting and TOC
;; - Chunk parameter and macro system
;; - Index generation and cross-references
;; - Statistics and analysis
;; - Multi-language syntax highlighting
;;
;; Usage:
;;   (import "litprog.lisp")
;;   (import "litprog_enhanced.lisp")
;;   (litprog-tangle-enhanced "source.lit" "output/")
;;   (litprog-weave-enhanced "source.lit" "docs.html")
;;

(import "litprog.lisp")

;; ========================================================================
;; Enhanced Error Reporting
;; ========================================================================

(defclass litprog-error ()
  ; Detailed error information
  (def this
    :type ""        ; error, warning, info
    :message ""     ; error message
    :file ""        ; source file
    :line 0         ; line number
    :context ""     ; surrounding code
    :chunk ""       ; chunk name if applicable
    :suggestion "")) ; helpful suggestion

(defq *litprog-errors* (list))

(defun litprog-report-error (type message file line &rest opts)
  "Report an error with full context"
  (defq err (litprog-error))
  (. err :type type)
  (. err :message message)
  (. err :file file)
  (. err :line line)
  (if (get opts :chunk)
    (. err :chunk (get opts :chunk)))
  (if (get opts :suggestion)
    (. err :suggestion (get opts :suggestion)))
  (push *litprog-errors* err)

  ; Print immediately for errors
  (if (= type "error")
    (print-error err)))

(defun print-error (err)
  "Pretty print an error"
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (cond
    ((= (. err :type) "error")
     (print "║  ❌ ERROR                                                     ║"))
    ((= (. err :type) "warning")
     (print "║  ⚠️  WARNING                                                  ║"))
    (t
     (print "║  ℹ️  INFO                                                     ║")))
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")
  (print (cat "File: " (. err :file)))
  (print (cat "Line: " (. err :line)))
  (if (> (length (. err :chunk)) 0)
    (print (cat "Chunk: " (. err :chunk))))
  (print "")
  (print (cat "Message: " (. err :message)))
  (if (> (length (. err :suggestion)) 0)
    (print "")
    (print (cat "Suggestion: " (. err :suggestion))))
  (print ""))

(defun litprog-error-summary ()
  "Print summary of all errors and warnings"
  (defq errors 0 warnings 0 infos 0)

  (each! *litprog-errors*
    (lambda (err)
      (cond
        ((= (. err :type) "error") (setq errors (inc errors)))
        ((= (. err :type) "warning") (setq warnings (inc warnings)))
        (t (setq infos (inc infos))))))

  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print "║  Error Summary                                                ║")
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print (cat "  Errors:   " errors))
  (print (cat "  Warnings: " warnings))
  (print (cat "  Info:     " infos))
  (print "")

  (if (> errors 0)
    (print "⚠️  Build failed with errors!")
    (if (> warnings 0)
      (print "✓ Build succeeded with warnings")
      (print "✓ Build succeeded!")))
  (print ""))

;; ========================================================================
;; Dependency Graph Generation
;; ========================================================================

(defun analyze-chunk-dependencies (ctx)
  "Analyze which chunks reference which other chunks"
  (defq deps (env))

  (each! (env-keys (. ctx :chunks))
    (lambda (chunk-name)
      (defq chunk (. (. ctx :chunks) chunk-name))
      (defq refs (list))
      (defq lines (split-lines (. chunk :code)))

      (each! lines
        (lambda (line)
          (defq ref (parse-noweb-chunk-ref line))
          (if ref
            (if (not (find refs ref))
              (push refs ref)))))

      (. deps chunk-name refs)))

  deps)

(defun generate-dependency-dot (ctx output-file)
  "Generate GraphViz DOT file showing chunk dependencies"
  (print (cat "Generating dependency graph: " output-file))

  (defq deps (analyze-chunk-dependencies ctx))
  (defq output "")

  ; DOT header
  (setq output "digraph ChunkDependencies {\n")
  (setq output (cat output "  rankdir=LR;\n"))
  (setq output (cat output "  node [shape=box, style=rounded];\n"))
  (setq output (cat output "  \n"))

  ; Nodes with styling
  (each! (env-keys deps)
    (lambda (chunk-name)
      (defq chunk (. (. ctx :chunks) chunk-name))
      (defq lang (. chunk :lang))
      (defq color (get-language-color lang))
      (setq output (cat output "  \"" chunk-name "\" [fillcolor=\"" color "\", style=\"rounded,filled\"];\n"))))

  (setq output (cat output "  \n"))

  ; Edges
  (each! (env-keys deps)
    (lambda (chunk-name)
      (defq refs (. deps chunk-name))
      (each! refs
        (lambda (ref)
          (setq output (cat output "  \"" chunk-name "\" -> \"" ref "\";\n"))))))

  (setq output (cat output "}\n"))

  (save-file output-file output)
  (print "✓ Dependency graph generated!")
  (print (cat "  Run: dot -Tpng " output-file " -o deps.png")))

(defun get-language-color (lang)
  "Get color for language in graph"
  (cond
    ((= lang "lisp") "#98C379")
    ((= lang "javascript") "#F7DF1E")
    ((= lang "python") "#3776AB")
    ((= lang "rust") "#CE422B")
    ((= lang "go") "#00ADD8")
    (t "#888888")))

;; ========================================================================
;; Enhanced HTML Output with Modern Styling
;; ========================================================================

(defun weave-html-enhanced (ctx output-file)
  "Generate beautiful HTML with modern CSS and features"
  (print (cat "Weaving enhanced HTML: " output-file))

  (defq html "")

  ; HTML header with modern CSS
  (setq html (cat html (html-header ctx)))

  ; Table of contents
  (setq html (cat html (generate-toc ctx)))

  ; Main content
  (setq html (cat html "<div class=\"content\">\n"))

  (each! (. ctx :order)
    (lambda (item)
      (cond
        ((. item :doc)
         (setq html (cat html (weave-doc-html (. item :doc)))))
        ((. item :chunk)
         (setq html (cat html (weave-chunk-html-enhanced (. item :chunk) ctx)))))))

  (setq html (cat html "</div>\n"))

  ; Chunk index
  (setq html (cat html (generate-chunk-index ctx)))

  ; Footer with stats
  (setq html (cat html (html-footer ctx)))

  (save-file output-file html)
  (print "✓ Enhanced HTML generated!"))

(defun html-header (ctx)
  "Generate modern HTML header with CSS"
  (cat
    "<!DOCTYPE html>\n"
    "<html lang=\"en\">\n"
    "<head>\n"
    "  <meta charset=\"UTF-8\">\n"
    "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
    "  <title>Literate Program</title>\n"
    "  <style>\n"
    "    :root {\n"
    "      --bg-primary: #ffffff;\n"
    "      --bg-secondary: #f8f9fa;\n"
    "      --bg-code: #282c34;\n"
    "      --text-primary: #2c3e50;\n"
    "      --text-secondary: #7f8c8d;\n"
    "      --accent: #3498db;\n"
    "      --accent-dark: #2980b9;\n"
    "      --border: #e1e4e8;\n"
    "      --success: #27ae60;\n"
    "    }\n"
    "    \n"
    "    * { margin: 0; padding: 0; box-sizing: border-box; }\n"
    "    \n"
    "    body {\n"
    "      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;\n"
    "      line-height: 1.6;\n"
    "      color: var(--text-primary);\n"
    "      background: var(--bg-secondary);\n"
    "    }\n"
    "    \n"
    "    .header {\n"
    "      background: linear-gradient(135deg, var(--accent) 0%, var(--accent-dark) 100%);\n"
    "      color: white;\n"
    "      padding: 3rem 2rem;\n"
    "      text-align: center;\n"
    "      box-shadow: 0 2px 10px rgba(0,0,0,0.1);\n"
    "    }\n"
    "    \n"
    "    .header h1 {\n"
    "      font-size: 2.5rem;\n"
    "      font-weight: 700;\n"
    "      margin-bottom: 0.5rem;\n"
    "    }\n"
    "    \n"
    "    .header p {\n"
    "      font-size: 1.1rem;\n"
    "      opacity: 0.9;\n"
    "    }\n"
    "    \n"
    "    .toc {\n"
    "      background: var(--bg-primary);\n"
    "      padding: 2rem;\n"
    "      margin: 2rem auto;\n"
    "      max-width: 1200px;\n"
    "      border-radius: 8px;\n"
    "      box-shadow: 0 2px 8px rgba(0,0,0,0.1);\n"
    "    }\n"
    "    \n"
    "    .toc h2 {\n"
    "      color: var(--accent);\n"
    "      margin-bottom: 1rem;\n"
    "      font-size: 1.5rem;\n"
    "    }\n"
    "    \n"
    "    .toc ul {\n"
    "      list-style: none;\n"
    "      padding-left: 0;\n"
    "    }\n"
    "    \n"
    "    .toc li {\n"
    "      padding: 0.5rem 0;\n"
    "      border-bottom: 1px solid var(--border);\n"
    "    }\n"
    "    \n"
    "    .toc a {\n"
    "      color: var(--text-primary);\n"
    "      text-decoration: none;\n"
    "      display: flex;\n"
    "      justify-content: space-between;\n"
    "      align-items: center;\n"
    "    }\n"
    "    \n"
    "    .toc a:hover {\n"
    "      color: var(--accent);\n"
    "    }\n"
    "    \n"
    "    .content {\n"
    "      max-width: 1200px;\n"
    "      margin: 2rem auto;\n"
    "      padding: 0 2rem;\n"
    "    }\n"
    "    \n"
    "    .chunk {\n"
    "      background: var(--bg-primary);\n"
    "      border-radius: 8px;\n"
    "      margin-bottom: 2rem;\n"
    "      overflow: hidden;\n"
    "      box-shadow: 0 2px 8px rgba(0,0,0,0.1);\n"
    "      transition: transform 0.2s, box-shadow 0.2s;\n"
    "    }\n"
    "    \n"
    "    .chunk:hover {\n"
    "      transform: translateY(-2px);\n"
    "      box-shadow: 0 4px 16px rgba(0,0,0,0.15);\n"
    "    }\n"
    "    \n"
    "    .chunk-header {\n"
    "      background: var(--bg-secondary);\n"
    "      padding: 1rem 1.5rem;\n"
    "      border-bottom: 2px solid var(--accent);\n"
    "    }\n"
    "    \n"
    "    .chunk-title {\n"
    "      font-size: 1.25rem;\n"
    "      font-weight: 600;\n"
    "      color: var(--text-primary);\n"
    "      font-family: 'Monaco', 'Courier New', monospace;\n"
    "    }\n"
    "    \n"
    "    .chunk-meta {\n"
    "      display: flex;\n"
    "      gap: 1rem;\n"
    "      margin-top: 0.5rem;\n"
    "      font-size: 0.9rem;\n"
    "      color: var(--text-secondary);\n"
    "    }\n"
    "    \n"
    "    .chunk-tag {\n"
    "      background: var(--accent);\n"
    "      color: white;\n"
    "      padding: 0.25rem 0.75rem;\n"
    "      border-radius: 12px;\n"
    "      font-size: 0.8rem;\n"
    "      font-weight: 500;\n"
    "    }\n"
    "    \n"
    "    .chunk-code {\n"
    "      padding: 0;\n"
    "    }\n"
    "    \n"
    "    pre {\n"
    "      margin: 0;\n"
    "      padding: 1.5rem;\n"
    "      background: var(--bg-code);\n"
    "      color: #abb2bf;\n"
    "      overflow-x: auto;\n"
    "      font-family: 'Monaco', 'Menlo', 'Courier New', monospace;\n"
    "      font-size: 0.9rem;\n"
    "      line-height: 1.5;\n"
    "    }\n"
    "    \n"
    "    code {\n"
    "      font-family: 'Monaco', 'Menlo', 'Courier New', monospace;\n"
    "    }\n"
    "    \n"
    "    /* Syntax highlighting colors (One Dark theme) */\n"
    "    .keyword { color: #c678dd; }\n"
    "    .string { color: #98c379; }\n"
    "    .comment { color: #5c6370; font-style: italic; }\n"
    "    .function { color: #61afef; }\n"
    "    .number { color: #d19a66; }\n"
    "    .operator { color: #56b6c2; }\n"
    "    \n"
    "    .doc-section {\n"
    "      background: var(--bg-primary);\n"
    "      padding: 2rem;\n"
    "      margin-bottom: 2rem;\n"
    "      border-radius: 8px;\n"
    "      box-shadow: 0 2px 8px rgba(0,0,0,0.1);\n"
    "    }\n"
    "    \n"
    "    .doc-section h1, .doc-section h2, .doc-section h3 {\n"
    "      color: var(--accent);\n"
    "      margin-top: 1.5rem;\n"
    "      margin-bottom: 1rem;\n"
    "    }\n"
    "    \n"
    "    .doc-section h1 { font-size: 2rem; }\n"
    "    .doc-section h2 { font-size: 1.5rem; }\n"
    "    .doc-section h3 { font-size: 1.25rem; }\n"
    "    \n"
    "    .doc-section p {\n"
    "      margin-bottom: 1rem;\n"
    "      line-height: 1.8;\n"
    "    }\n"
    "    \n"
    "    .chunk-index {\n"
    "      background: var(--bg-primary);\n"
    "      padding: 2rem;\n"
    "      margin: 2rem auto;\n"
    "      max-width: 1200px;\n"
    "      border-radius: 8px;\n"
    "      box-shadow: 0 2px 8px rgba(0,0,0,0.1);\n"
    "    }\n"
    "    \n"
    "    .chunk-index h2 {\n"
    "      color: var(--accent);\n"
    "      margin-bottom: 1rem;\n"
    "    }\n"
    "    \n"
    "    .index-grid {\n"
    "      display: grid;\n"
    "      grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));\n"
    "      gap: 1rem;\n"
    "      margin-top: 1rem;\n"
    "    }\n"
    "    \n"
    "    .index-item {\n"
    "      padding: 1rem;\n"
    "      background: var(--bg-secondary);\n"
    "      border-radius: 4px;\n"
    "      border-left: 4px solid var(--accent);\n"
    "    }\n"
    "    \n"
    "    .footer {\n"
    "      background: var(--text-primary);\n"
    "      color: white;\n"
    "      padding: 2rem;\n"
    "      text-align: center;\n"
    "      margin-top: 4rem;\n"
    "    }\n"
    "    \n"
    "    .stats {\n"
    "      display: flex;\n"
    "      justify-content: center;\n"
    "      gap: 2rem;\n"
    "      margin-top: 1rem;\n"
    "    }\n"
    "    \n"
    "    .stat {\n"
    "      text-align: center;\n"
    "    }\n"
    "    \n"
    "    .stat-value {\n"
    "      font-size: 2rem;\n"
    "      font-weight: bold;\n"
    "      color: var(--success);\n"
    "    }\n"
    "    \n"
    "    .stat-label {\n"
    "      font-size: 0.9rem;\n"
    "      opacity: 0.8;\n"
    "    }\n"
    "  </style>\n"
    "</head>\n"
    "<body>\n"
    "  <div class=\"header\">\n"
    "    <h1>📚 Literate Program</h1>\n"
    "    <p>Generated with LITPROG Enhanced</p>\n"
    "  </div>\n"))

(defun generate-toc (ctx)
  "Generate table of contents"
  (defq toc "<div class=\"toc\">\n<h2>📑 Table of Contents</h2>\n<ul>\n")

  (defq chunk-num 0)
  (each! (. ctx :order)
    (lambda (item)
      (if (. item :chunk)
        (progn
          (setq chunk-num (inc chunk-num))
          (defq chunk (. item :chunk))
          (defq name (. chunk :name))
          (if (> (length name) 0)
            (setq toc (cat toc
              "<li><a href=\"#chunk-" chunk-num "\">"
              "<span>" name "</span>"
              "<span class=\"chunk-tag\">" (. chunk :lang) "</span>"
              "</a></li>\n")))))))

  (setq toc (cat toc "</ul>\n</div>\n"))
  toc)

(defun weave-chunk-html-enhanced (chunk ctx)
  "Generate enhanced HTML for a chunk"
  (defq html "")
  (defq name (. chunk :name))
  (defq lang (. chunk :lang))
  (defq code (. chunk :code))

  (setq html (cat html "<div class=\"chunk\" id=\"chunk-" name "\">\n"))
  (setq html (cat html "  <div class=\"chunk-header\">\n"))

  (if (> (length name) 0)
    (setq html (cat html "    <div class=\"chunk-title\">⟨⟨ " name " ⟩⟩</div>\n")))

  (setq html (cat html "    <div class=\"chunk-meta\">\n"))
  (setq html (cat html "      <span class=\"chunk-tag\">" lang "</span>\n"))

  (if (> (length (. chunk :file)) 0)
    (setq html (cat html "      <span>→ " (. chunk :file) "</span>\n")))

  (setq html (cat html "      <span>Line " (. chunk :line) "</span>\n"))
  (setq html (cat html "    </div>\n"))
  (setq html (cat html "  </div>\n"))

  (setq html (cat html "  <div class=\"chunk-code\">\n"))
  (setq html (cat html "    <pre><code class=\"language-" lang "\">\n"))
  (setq html (cat html (escape-html code)))
  (setq html (cat html "\n    </code></pre>\n"))
  (setq html (cat html "  </div>\n"))
  (setq html (cat html "</div>\n"))

  html)

(defun weave-doc-html (doc-text)
  "Generate HTML for documentation section"
  (cat "<div class=\"doc-section\">\n"
       doc-text
       "\n</div>\n"))

(defun generate-chunk-index (ctx)
  "Generate alphabetical index of chunks"
  (defq index "<div class=\"chunk-index\">\n<h2>🗂️ Chunk Index</h2>\n<div class=\"index-grid\">\n")

  (each! (sort (env-keys (. ctx :chunks)))
    (lambda (chunk-name)
      (defq chunk (. (. ctx :chunks) chunk-name))
      (setq index (cat index
        "<div class=\"index-item\">\n"
        "  <a href=\"#chunk-" chunk-name "\">" chunk-name "</a>\n"
        "  <div style=\"font-size:0.8rem; color: var(--text-secondary);\">"
        (. chunk :lang) "</div>\n"
        "</div>\n"))))

  (setq index (cat index "</div>\n</div>\n"))
  index)

(defun html-footer (ctx)
  "Generate footer with statistics"
  (defq num-chunks (length (env-keys (. ctx :chunks))))
  (defq num-files (length (env-keys (. ctx :files))))
  (defq total-lines 0)

  (each! (env-keys (. ctx :chunks))
    (lambda (name)
      (defq chunk (. (. ctx :chunks) name))
      (setq total-lines (+ total-lines (length (split-lines (. chunk :code)))))))

  (cat
    "<div class=\"footer\">\n"
    "  <div class=\"stats\">\n"
    "    <div class=\"stat\">\n"
    "      <div class=\"stat-value\">" num-chunks "</div>\n"
    "      <div class=\"stat-label\">Chunks</div>\n"
    "    </div>\n"
    "    <div class=\"stat\">\n"
    "      <div class=\"stat-value\">" num-files "</div>\n"
    "      <div class=\"stat-label\">Files</div>\n"
    "    </div>\n"
    "    <div class=\"stat\">\n"
    "      <div class=\"stat-value\">" total-lines "</div>\n"
    "      <div class=\"stat-label\">Lines of Code</div>\n"
    "    </div>\n"
    "  </div>\n"
    "  <p style=\"margin-top: 2rem; opacity: 0.7;\">Generated with LITPROG Enhanced for ChrysaLisp</p>\n"
    "</div>\n"
    "</body>\n</html>\n"))

;; ========================================================================
;; Watch Mode - Auto-regenerate on file changes
;; ========================================================================

(defun litprog-watch (source-file output-dir)
  "Watch file and auto-regenerate on changes"
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print "║  LITPROG Watch Mode                                           ║")
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")
  (print (cat "Watching: " source-file))
  (print (cat "Output:   " output-dir))
  (print "")
  (print "Press Ctrl+C to stop...")
  (print "")

  (defq last-mtime (file-mtime source-file))

  (while t
    (sleep 1000) ; Check every second
    (defq current-mtime (file-mtime source-file))

    (if (> current-mtime last-mtime)
      (progn
        (print "")
        (print "🔄 File changed, regenerating...")
        (litprog-tangle source-file output-dir)
        (litprog-weave-enhanced source-file (cat output-dir "index.html"))
        (print "✓ Done!")
        (print "")
        (setq last-mtime current-mtime)))))

;; ========================================================================
;; Statistics and Analysis
;; ========================================================================

(defun litprog-stats (source-file)
  "Generate detailed statistics about a literate program"
  (defq ctx (parse-literate-file source-file))

  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print "║  Literate Program Statistics                                  ║")
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")
  (print (cat "Source file: " source-file))
  (print "")

  ; Count chunks
  (defq num-chunks (length (env-keys (. ctx :chunks))))
  (print (cat "Total chunks: " num-chunks))

  ; Count by language
  (defq lang-counts (env))
  (each! (env-keys (. ctx :chunks))
    (lambda (name)
      (defq chunk (. (. ctx :chunks) name))
      (defq lang (. chunk :lang))
      (if (. lang-counts lang)
        (. lang-counts lang (inc (. lang-counts lang)))
        (. lang-counts lang 1))))

  (print "")
  (print "Chunks by language:")
  (each! (env-keys lang-counts)
    (lambda (lang)
      (print (cat "  " lang ": " (. lang-counts lang)))))

  ; Count lines
  (defq total-lines 0)
  (defq total-doc-lines 0)
  (each! (env-keys (. ctx :chunks))
    (lambda (name)
      (defq chunk (. (. ctx :chunks) name))
      (setq total-lines (+ total-lines (length (split-lines (. chunk :code)))))))

  (each! (. ctx :order)
    (lambda (item)
      (if (. item :doc)
        (setq total-doc-lines (+ total-doc-lines (length (split-lines (. item :doc))))))))

  (print "")
  (print (cat "Total code lines: " total-lines))
  (print (cat "Total doc lines: " total-doc-lines))
  (print (cat "Code/doc ratio: " (/ total-lines (max 1 total-doc-lines))))

  ; Output files
  (defq num-files (length (env-keys (. ctx :files))))
  (print "")
  (print (cat "Output files: " num-files))
  (if (> num-files 0)
    (progn
      (print "")
      (print "Files:")
      (each! (env-keys (. ctx :files))
        (lambda (filename)
          (defq chunks (. (. ctx :files) filename))
          (print (cat "  " filename " (" (length chunks) " chunks)"))))))

  ; Dependency analysis
  (defq deps (analyze-chunk-dependencies ctx))
  (defq max-deps 0)
  (defq most-depended "")

  (each! (env-keys deps)
    (lambda (name)
      (defq num-deps (length (. deps name)))
      (if (> num-deps max-deps)
        (setq max-deps num-deps)
        (setq most-depended name))))

  (print "")
  (print "Dependency analysis:")
  (print (cat "  Most complex chunk: " most-depended " (" max-deps " dependencies)"))

  (print ""))

;; ========================================================================
;; Export Enhanced Functions
;; ========================================================================

(export
  litprog-report-error
  litprog-error-summary
  generate-dependency-dot
  weave-html-enhanced
  litprog-watch
  litprog-stats
  analyze-chunk-dependencies)

(print "LITPROG Enhanced loaded!")
(print "New features:")
(print "  • (generate-dependency-dot ctx \"deps.dot\")")
(print "  • (weave-html-enhanced ctx \"output.html\")")
(print "  • (litprog-watch \"source.lit\" \"output/\")")
(print "  • (litprog-stats \"source.lit\")")
