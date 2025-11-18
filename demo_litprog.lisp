#!/usr/bin/env chrysalisp
;; ============================================================
;; LITPROG Demo Script
;; Demonstrates all features of the literate programming tool
;; ============================================================

(import "litprog.lisp")

(defun print-banner (text)
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print (cat "║  " text (repeat-str " " (- 60 (length text))) "║"))
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print ""))

(defun repeat-str (s n)
  (if (<= n 0)
    ""
    (cat s (repeat-str s (dec n)))))

(defun ensure-dir (dir)
  "Ensure directory exists"
  (print (cat "Creating directory: " dir))
  ; Note: In real ChrysaLisp, use appropriate system calls
  ; This is a placeholder
  t)

(defun demo-hello-world ()
  "Demonstrate the Hello World example (noweb style)"
  (print-banner "Demo 1: Hello World (Noweb Style)")

  (print "This example demonstrates:")
  (print "  • Classic noweb syntax (<<chunk>>= and <<chunk>>)")
  (print "  • Chunk composition")
  (print "  • ASCII art headers")
  (print "  • Basic literate programming concepts")
  (print "")

  ; Tangle
  (print "Step 1: Tangling code...")
  (litprog-tangle "examples/hello_literate.lit" "output/")
  (print "")

  ; Weave to different formats
  (print "Step 2: Weaving documentation to Markdown...")
  (litprog-weave "examples/hello_literate.lit" "output/hello_docs.md" :format :markdown)

  (print "Step 3: Weaving documentation to HTML...")
  (litprog-weave "examples/hello_literate.lit" "output/hello_docs.html" :format :html)

  (print "")
  (print "Generated files:")
  (print "  • output/hello.lisp (executable code)")
  (print "  • output/hello_docs.md (Markdown documentation)")
  (print "  • output/hello_docs.html (HTML documentation)")
  (print "")
  (print "Press Enter to continue...")
  (read-line))

(defun demo-fibonacci ()
  "Demonstrate the Fibonacci example (org-mode style)"
  (print-banner "Demo 2: Fibonacci Calculator (Org-Mode Style)")

  (print "This example demonstrates:")
  (print "  • Org-mode syntax (#+BEGIN_SRC / #+END_SRC)")
  (print "  • Multiple algorithm implementations")
  (print "  • Performance analysis")
  (print "  • Mathematical explanations")
  (print "")

  ; Tangle
  (print "Step 1: Tangling code...")
  (litprog-tangle "examples/fibonacci_orgmode.lit" "output/")
  (print "")

  ; Weave
  (print "Step 2: Weaving documentation to HTML...")
  (litprog-weave "examples/fibonacci_orgmode.lit" "output/fibonacci_docs.html" :format :html)

  (print "Step 3: Weaving documentation to LaTeX...")
  (litprog-weave "examples/fibonacci_orgmode.lit" "output/fibonacci_docs.tex" :format :latex)

  (print "")
  (print "Generated files:")
  (print "  • output/fibonacci.lisp (three implementations)")
  (print "  • output/fibonacci_docs.html (HTML with benchmarks)")
  (print "  • output/fibonacci_docs.tex (LaTeX for academic papers)")
  (print "")
  (print "Press Enter to continue...")
  (read-line))

(defun demo-web-server ()
  "Demonstrate the Web Server example (markdown fence style)"
  (print-banner "Demo 3: Web Server (Markdown Fence Style)")

  (print "This example demonstrates:")
  (print "  • Markdown code fence syntax (```lang {#chunk})")
  (print "  • Multiple output files")
  (print "  • Modular architecture")
  (print "  • Modern, GitHub-friendly syntax")
  (print "")

  ; Tangle
  (print "Step 1: Tangling code to multiple files...")
  (litprog-tangle "examples/web_server_markdown.lit" "output/")
  (print "")

  ; Weave
  (print "Step 2: Weaving documentation to HTML...")
  (litprog-weave "examples/web_server_markdown.lit" "output/webserver_docs.html" :format :html)

  (print "")
  (print "Generated files:")
  (print "  • output/server.lisp (main server)")
  (print "  • output/http-parser.lisp (HTTP protocol)")
  (print "  • output/router.lisp (request routing)")
  (print "  • output/webserver_docs.html (comprehensive docs)")
  (print "")
  (print "Press Enter to continue...")
  (read-line))

(defun demo-advanced ()
  "Demonstrate the advanced mixed-styles example"
  (print-banner "Demo 4: Advanced Pipeline (Mixed Styles)")

  (print "This example demonstrates:")
  (print "  • Mixing all three syntaxes in one document!")
  (print "  • Complex multi-file generation")
  (print "  • Data processing pipeline")
  (print "  • Production-ready architecture")
  (print "")

  ; Tangle
  (print "Step 1: Tangling to multiple files...")
  (litprog-tangle "examples/advanced_mixed_styles.lit" "output/")
  (print "")

  ; Weave to all formats
  (print "Step 2: Weaving to Markdown...")
  (litprog-weave "examples/advanced_mixed_styles.lit" "output/pipeline.md" :format :markdown)

  (print "Step 3: Weaving to HTML...")
  (litprog-weave "examples/advanced_mixed_styles.lit" "output/pipeline.html" :format :html)

  (print "Step 4: Weaving to LaTeX...")
  (litprog-weave "examples/advanced_mixed_styles.lit" "output/pipeline.tex" :format :latex)

  (print "")
  (print "Generated files:")
  (print "  Code files:")
  (print "    • output/pipeline.lisp")
  (print "    • output/data-reader.lisp")
  (print "    • output/report-generator.lisp")
  (print "  Documentation:")
  (print "    • output/pipeline.md (GitHub-ready)")
  (print "    • output/pipeline.html (web publishing)")
  (print "    • output/pipeline.tex (academic papers)")
  (print ""))

(defun demo-all ()
  "Run all demonstrations"
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print "║                                                                ║")
  (print "║       LITPROG - Literate Programming Tool Demo                ║")
  (print "║                                                                ║")
  (print "║  This demo will showcase all features of the literate         ║")
  (print "║  programming tool through four comprehensive examples.        ║")
  (print "║                                                                ║")
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")

  ; Ensure output directory exists
  (ensure-dir "output/")

  ; Run demos
  (demo-hello-world)
  (demo-fibonacci)
  (demo-web-server)
  (demo-advanced)

  ; Final summary
  (print-banner "Demo Complete!")
  (print "All examples have been tangled and woven!")
  (print "")
  (print "What you've seen:")
  (print "  ✓ Three different literate programming syntaxes")
  (print "  ✓ Tangle: Extract executable code from documentation")
  (print "  ✓ Weave: Generate beautiful docs from code")
  (print "  ✓ Multiple output formats (Markdown, HTML, LaTeX)")
  (print "  ✓ Chunk composition and code reuse")
  (print "  ✓ Multi-file project generation")
  (print "")
  (print "Next steps:")
  (print "  1. Browse the generated files in output/")
  (print "  2. Open the HTML docs in a browser")
  (print "  3. Read LITPROG_README.md for complete documentation")
  (print "  4. Create your own literate programs!")
  (print "")
  (print "Happy Literate Programming! 📚✨")
  (print ""))

(defun show-menu ()
  "Show interactive menu"
  (print "")
  (print "LITPROG Demo Menu:")
  (print "  1. Hello World Example (Noweb)")
  (print "  2. Fibonacci Calculator (Org-mode)")
  (print "  3. Web Server (Markdown)")
  (print "  4. Advanced Pipeline (Mixed)")
  (print "  5. Run All Demos")
  (print "  6. Show Help")
  (print "  0. Exit")
  (print "")
  (print "Enter choice: "))

(defun interactive-demo ()
  "Interactive demo with menu"
  (defq running t)
  (while running
    (show-menu)
    (defq choice (read-line))

    (cond
      ((= choice "1") (demo-hello-world))
      ((= choice "2") (demo-fibonacci))
      ((= choice "3") (demo-web-server))
      ((= choice "4") (demo-advanced))
      ((= choice "5") (demo-all))
      ((= choice "6") (litprog-help))
      ((= choice "0")
       (print "Goodbye!")
       (setq running nil))
      (t
       (print "Invalid choice. Please try again.")))))

;; Main entry point
(defun main ()
  (defq args (get-args))

  (if (or (= (length args) 0)
          (= (get args 0) "--interactive"))
    ; Interactive mode
    (interactive-demo)

    ; Direct mode
    (cond
      ((= (get args 0) "--all")
       (demo-all))
      ((= (get args 0) "--help")
       (litprog-help))
      (t
       (print "Usage:")
       (print "  demo_litprog.lisp [--interactive|--all|--help]")
       (print "")
       (print "Options:")
       (print "  --interactive  Show interactive menu (default)")
       (print "  --all          Run all demos")
       (print "  --help         Show LITPROG help")))))

;; Stub functions for demo purposes
(defun read-line ()
  ; In real ChrysaLisp, use proper input
  "")

(defun get-args ()
  ; In real ChrysaLisp, get command line args
  (list))

;; Run if executed directly
(print "LITPROG Demo loaded!")
(print "Call (demo-all) to run all demonstrations.")
(print "Or call individual functions: (demo-hello-world), (demo-fibonacci), etc.")
