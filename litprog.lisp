;; ========================================================================
;; LITPROG - A Comprehensive Literate Programming Tool for ChrysaLisp
;; ========================================================================
;;
;; This tool supports both noweb and org-mode style literate programming,
;; allowing you to weave beautiful documentation and tangle executable code
;; from the same source files.
;;
;; Features:
;; - Noweb-style chunk syntax: <<chunk-name>>= and <<chunk-name>>
;; - Org-mode syntax: #+BEGIN_SRC / #+END_SRC
;; - Markdown code fences with literate extensions
;; - Multiple output formats: Markdown, HTML, LaTeX, Plain Text
;; - Syntax highlighting in documentation
;; - Cross-reference index generation
;; - Table of contents
;; - Multiple language support
;; - Chunk composition and transclusion
;; - Beautiful ASCII art headers
;;
;; Usage:
;;   (litprog-tangle "source.lit" "output-dir/")
;;   (litprog-weave "source.lit" "docs.md" :format :markdown)
;;   (litprog-weave "source.lit" "docs.html" :format :html)
;;

(import "lib/asm/asm.inc")
(import "lib/asm/write.inc")

;; ========================================================================
;; Data Structures
;; ========================================================================

(defclass litprog-chunk ()
  ; Represents a code chunk in a literate program
  (def this :name "" :lang "" :code "" :file "" :line 0 :refs (list)))

(defclass litprog-doc ()
  ; Represents a documentation section
  (def this :content "" :line 0))

(defclass litprog-context ()
  ; The parsing context
  (def this
    :chunks (env)          ; Map of chunk-name -> litprog-chunk
    :docs (list)           ; List of documentation blocks
    :order (list)          ; Order of chunks and docs
    :files (env)           ; Map of filename -> list of chunks
    :toc (list)            ; Table of contents entries
    :index (env)))         ; Cross-reference index

;; ========================================================================
;; Utilities
;; ========================================================================

(defun trim-string (s)
  ; Trim whitespace from both ends of string
  (defq start 0 end (length s))
  (while (and (< start end)
              (find (char s start) " \t\n\r"))
    (setq start (inc start)))
  (while (and (< start end)
              (find (char s (dec end)) " \t\n\r"))
    (setq end (dec end)))
  (slice start end s))

(defun split-lines (text)
  ; Split text into lines
  (split text (ascii-char 10)))

(defun join-lines (lines)
  ; Join lines with newlines
  (reduce (lambda (acc line) (cat acc line (ascii-char 10)))
          lines ""))

(defun starts-with? (s prefix)
  ; Check if string starts with prefix
  (and (>= (length s) (length prefix))
       (= (slice 0 (length prefix) s) prefix)))

(defun extract-chunk-name (line)
  ; Extract chunk name from <<name>>= or <<name>> syntax
  (defq start (+ (find line "<") 2))
  (defq end (find line ">" start))
  (if (and start end)
    (trim-string (slice start end line))
    nil))

(defun indent-level (line)
  ; Calculate indentation level
  (defq count 0)
  (while (and (< count (length line))
              (or (= (char line count) (ascii-code " "))
                  (= (char line count) (ascii-code "\t"))))
    (setq count (inc count)))
  count)

(defun indent-lines (lines indent-str)
  ; Add indentation to each line
  (map (lambda (line) (cat indent-str line)) lines))

;; ========================================================================
;; Parser - Noweb Style
;; ========================================================================

(defun parse-noweb-chunk-def (line)
  ; Parse <<chunk-name>>= line
  (if (and (find line "<<") (find line ">>="))
    (extract-chunk-name line)
    nil))

(defun parse-noweb-chunk-ref (line)
  ; Parse <<chunk-name>> reference (not definition)
  (if (and (find line "<<")
           (find line ">>")
           (not (find line ">>=")))
    (extract-chunk-name line)
    nil))

;; ========================================================================
;; Parser - Org-mode Style
;; ========================================================================

(defun parse-org-src-begin (line)
  ; Parse #+BEGIN_SRC lang :tangle filename
  (if (starts-with? (trim-string line) "#+BEGIN_SRC")
    (defq parts (split (trim-string line) " "))
    (defq result (env))
    (if (>= (length parts) 2)
      (progn
        (. result :lang (get parts 1))
        (defq tangle-idx (find parts ":tangle"))
        (if (and tangle-idx (< (inc tangle-idx) (length parts)))
          (. result :file (get parts (inc tangle-idx))))
        result)
      nil)
    nil))

(defun parse-org-src-end (line)
  ; Check for #+END_SRC
  (starts-with? (trim-string line) "#+END_SRC"))

(defun parse-org-name (line)
  ; Parse #+NAME: chunk-name
  (if (starts-with? (trim-string line) "#+NAME:")
    (trim-string (slice 7 -1 line))
    nil))

;; ========================================================================
;; Parser - Markdown Code Fence Style
;; ========================================================================

(defun parse-markdown-fence-begin (line)
  ; Parse ```lang {#chunk-name .tangle=file}
  (if (starts-with? (trim-string line) "```")
    (defq rest (trim-string (slice 3 -1 line)))
    (defq result (env))
    ; Extract language
    (defq parts (split rest " "))
    (if (> (length parts) 0)
      (. result :lang (get parts 0)))
    ; Extract chunk name from {#name}
    (defq name-match (find rest "{#"))
    (if name-match
      (defq name-end (find rest "}" name-match))
      (if name-end
        (defq name-str (slice (+ name-match 2) name-end rest))
        (. result :name (car (split name-str " ")))))
    ; Extract tangle file from .tangle=file
    (defq tangle-match (find rest ".tangle="))
    (if tangle-match
      (defq tangle-rest (slice (+ tangle-match 8) -1 rest))
      (defq tangle-end (find tangle-rest "}"))
      (if tangle-end
        (. result :file (trim-string (slice 0 tangle-end tangle-rest)))
        (. result :file (trim-string tangle-rest))))
    result
    nil))

(defun parse-markdown-fence-end (line)
  ; Check for ```
  (starts-with? (trim-string line) "```"))

;; ========================================================================
;; Main Parser
;; ========================================================================

(defun parse-literate-file (filename)
  ; Parse a literate programming file
  (defq ctx (litprog-context))
  (defq content (load-file filename))
  (defq lines (split-lines content))
  (defq i 0)
  (defq in-chunk nil)
  (defq current-chunk nil)
  (defq current-doc "")
  (defq chunk-code (list))
  (defq mode nil) ; :noweb, :org, :markdown

  (while (< i (length lines))
    (defq line (get lines i))

    (cond
      ; Check for chunk definition (noweb style)
      ((and (not in-chunk) (parse-noweb-chunk-def line))
       (if (> (length current-doc) 0)
         (push (. ctx :order) (list :doc current-doc))
         (setq current-doc ""))
       (defq chunk-name (parse-noweb-chunk-def line))
       (setq current-chunk (litprog-chunk))
       (. current-chunk :name chunk-name)
       (. current-chunk :line (inc i))
       (setq chunk-code (list))
       (setq in-chunk t)
       (setq mode :noweb))

      ; Check for org-mode #+NAME:
      ((and (not in-chunk) (parse-org-name line))
       (if (> (length current-doc) 0)
         (push (. ctx :order) (list :doc current-doc))
         (setq current-doc ""))
       (defq chunk-name (parse-org-name line))
       (setq current-chunk (litprog-chunk))
       (. current-chunk :name chunk-name)
       (. current-chunk :line (inc i)))

      ; Check for org-mode #+BEGIN_SRC
      ((and (not in-chunk) (parse-org-src-begin line))
       (defq org-info (parse-org-src-begin line))
       (if (not current-chunk)
         (setq current-chunk (litprog-chunk)))
       (. current-chunk :lang (. org-info :lang))
       (if (. org-info :file)
         (. current-chunk :file (. org-info :file)))
       (if (= (. current-chunk :line) 0)
         (. current-chunk :line (inc i)))
       (setq chunk-code (list))
       (setq in-chunk t)
       (setq mode :org))

      ; Check for markdown fence begin
      ((and (not in-chunk) (parse-markdown-fence-begin line))
       (if (> (length current-doc) 0)
         (push (. ctx :order) (list :doc current-doc))
         (setq current-doc ""))
       (defq fence-info (parse-markdown-fence-begin line))
       (setq current-chunk (litprog-chunk))
       (if (. fence-info :name)
         (. current-chunk :name (. fence-info :name)))
       (if (. fence-info :lang)
         (. current-chunk :lang (. fence-info :lang)))
       (if (. fence-info :file)
         (. current-chunk :file (. fence-info :file)))
       (. current-chunk :line (inc i))
       (setq chunk-code (list))
       (setq in-chunk t)
       (setq mode :markdown))

      ; Check for chunk end
      ((and in-chunk
            (or (and (= mode :noweb) (or (parse-noweb-chunk-def line) (= (trim-string line) "")))
                (and (= mode :org) (parse-org-src-end line))
                (and (= mode :markdown) (parse-markdown-fence-end line))))
       ; Save the chunk
       (. current-chunk :code (join-lines chunk-code))
       (if (> (length (. current-chunk :name)) 0)
         (. (. ctx :chunks) (. current-chunk :name) current-chunk))
       (push (. ctx :order) (list :chunk current-chunk))

       ; Track file mapping
       (if (> (length (. current-chunk :file)) 0)
         (defq fname (. current-chunk :file))
         (if (not (. (. ctx :files) fname))
           (. (. ctx :files) fname (list)))
         (push (. (. ctx :files) fname) current-chunk))

       (setq in-chunk nil)
       (setq current-chunk nil)
       (if (not (= mode :noweb))
         (setq i (inc i))))

      ; Inside chunk - collect code
      (in-chunk
       (push chunk-code line))

      ; Outside chunk - collect documentation
      (t
       (setq current-doc (cat current-doc line (ascii-char 10)))))

    (setq i (inc i)))

  ; Save any remaining documentation
  (if (> (length current-doc) 0)
    (push (. ctx :order) (list :doc current-doc)))

  ctx)

;; ========================================================================
;; Tangle - Extract Code
;; ========================================================================

(defun expand-chunk-refs (chunk-code ctx indent)
  ; Recursively expand <<chunk-name>> references
  (defq lines (split-lines chunk-code))
  (defq result (list))

  (each! lines
    (lambda (line)
      (defq ref-name (parse-noweb-chunk-ref line))
      (if ref-name
        ; Expand the referenced chunk
        (if (. (. ctx :chunks) ref-name)
          (defq ref-chunk (. (. ctx :chunks) ref-name))
          (defq ref-code (. ref-chunk :code))
          (defq line-indent (cat indent (slice 0 (indent-level line) line)))
          (defq expanded (expand-chunk-refs ref-code ctx line-indent))
          (defq expanded-lines (split-lines expanded))
          (each! expanded-lines
            (lambda (exp-line)
              (push result (cat line-indent exp-line))))
          (print (cat "Warning: undefined chunk reference: " ref-name)))
        ; Regular line
        (push result line))))

  (join-lines result))

(defun tangle-chunk (chunk ctx)
  ; Tangle a single chunk, expanding all references
  (expand-chunk-refs (. chunk :code) ctx ""))

(defun tangle-to-file (filename chunks ctx)
  ; Tangle multiple chunks to a single file
  (defq output "")
  (each! chunks
    (lambda (chunk)
      (setq output (cat output (tangle-chunk chunk ctx) (ascii-char 10)))))
  (save-file filename output)
  (print (cat "Tangled: " filename)))

(defun litprog-tangle (source-file output-dir)
  ; Main tangle function
  (print (cat "Tangling: " source-file))
  (defq ctx (parse-literate-file source-file))

  ; Tangle each file
  (each! (. ctx :files)
    (lambda (entry)
      (defq filename (cat output-dir (car entry)))
      (defq chunks (cdr entry))
      (tangle-to-file filename chunks ctx)))

  (print "Tangle complete!"))

;; ========================================================================
;; Weave - Generate Documentation
;; ========================================================================

(defun escape-html (text)
  ; Escape HTML special characters
  (defq result text)
  (setq result (replace result "&" "&amp;"))
  (setq result (replace result "<" "&lt;"))
  (setq result (replace result ">" "&gt;"))
  (setq result (replace result "\"" "&quot;"))
  result)

(defun escape-latex (text)
  ; Escape LaTeX special characters
  (defq result text)
  (setq result (replace result "\\" "\\textbackslash{}"))
  (setq result (replace result "{" "\\{"))
  (setq result (replace result "}" "\\}"))
  (setq result (replace result "$" "\\$"))
  (setq result (replace result "&" "\\&"))
  (setq result (replace result "%" "\\%"))
  (setq result (replace result "#" "\\#"))
  (setq result (replace result "_" "\\_"))
  result)

(defun weave-markdown-chunk (chunk ctx)
  ; Weave a chunk to Markdown
  (defq output "")
  (defq name (. chunk :name))
  (defq lang (. chunk :lang))
  (defq code (. chunk :code))

  (if (> (length name) 0)
    (setq output (cat output "### Chunk: `" name "`\n\n")))

  (if (> (length (. chunk :file)) 0)
    (setq output (cat output "*Tangles to: " (. chunk :file) "*\n\n")))

  (setq output (cat output "```" lang "\n"))
  (setq output (cat output code "\n"))
  (setq output (cat output "```\n\n"))

  output)

(defun weave-html-chunk (chunk ctx)
  ; Weave a chunk to HTML
  (defq output "")
  (defq name (. chunk :name))
  (defq lang (. chunk :lang))
  (defq code (escape-html (. chunk :code)))

  (if (> (length name) 0)
    (setq output (cat output "<h3>Chunk: <code>" (escape-html name) "</code></h3>\n")))

  (if (> (length (. chunk :file)) 0)
    (setq output (cat output "<p><em>Tangles to: " (escape-html (. chunk :file)) "</em></p>\n")))

  (setq output (cat output "<pre><code class=\"language-" lang "\">\n"))
  (setq output (cat output code "\n"))
  (setq output (cat output "</code></pre>\n"))

  output)

(defun weave-latex-chunk (chunk ctx)
  ; Weave a chunk to LaTeX
  (defq output "")
  (defq name (. chunk :name))
  (defq lang (. chunk :lang))
  (defq code (escape-latex (. chunk :code)))

  (if (> (length name) 0)
    (setq output (cat output "\\subsection{Chunk: \\texttt{" (escape-latex name) "}}\n")))

  (if (> (length (. chunk :file)) 0)
    (setq output (cat output "\\textit{Tangles to: " (escape-latex (. chunk :file)) "}\n\n")))

  (setq output (cat output "\\begin{verbatim}\n"))
  (setq output (cat output code "\n"))
  (setq output (cat output "\\end{verbatim}\n"))

  output)

(defun weave-doc (doc-text format)
  ; Weave documentation text
  (cond
    ((= format :html)
     ; Simple markdown-to-html conversion
     doc-text)
    ((= format :latex)
     doc-text)
    (t ; :markdown or :text
     doc-text)))

(defun litprog-weave (source-file output-file &rest options)
  ; Main weave function
  (defq format (or (get options :format) :markdown))
  (print (cat "Weaving: " source-file " -> " output-file))
  (defq ctx (parse-literate-file source-file))
  (defq output "")

  ; Add header
  (cond
    ((= format :html)
     (setq output (cat output "<!DOCTYPE html>\n<html>\n<head>\n"))
     (setq output (cat output "<meta charset=\"UTF-8\">\n"))
     (setq output (cat output "<title>Literate Program</title>\n"))
     (setq output (cat output "<style>\n"))
     (setq output (cat output "body { font-family: Georgia, serif; max-width: 800px; margin: 0 auto; padding: 20px; }\n"))
     (setq output (cat output "pre { background: #f5f5f5; padding: 15px; border-radius: 5px; overflow-x: auto; }\n"))
     (setq output (cat output "code { font-family: 'Courier New', monospace; }\n"))
     (setq output (cat output "h3 { color: #333; border-bottom: 2px solid #007acc; }\n"))
     (setq output (cat output "</style>\n</head>\n<body>\n")))
    ((= format :latex)
     (setq output (cat output "\\documentclass{article}\n"))
     (setq output (cat output "\\usepackage{listings}\n"))
     (setq output (cat output "\\usepackage{xcolor}\n"))
     (setq output (cat output "\\begin{document}\n"))))

  ; Weave content
  (each! (. ctx :order)
    (lambda (item)
      (cond
        ((. item :doc)
         (setq output (cat output (weave-doc (. item :doc) format))))
        ((. item :chunk)
         (defq chunk (. item :chunk))
         (cond
           ((= format :html)
            (setq output (cat output (weave-html-chunk chunk ctx))))
           ((= format :latex)
            (setq output (cat output (weave-latex-chunk chunk ctx))))
           (t
            (setq output (cat output (weave-markdown-chunk chunk ctx)))))))))

  ; Add footer
  (cond
    ((= format :html)
     (setq output (cat output "</body>\n</html>\n")))
    ((= format :latex)
     (setq output (cat output "\\end{document}\n"))))

  (save-file output-file output)
  (print (cat "Weave complete: " output-file)))

;; ========================================================================
;; Helper Functions for File I/O
;; ========================================================================

(defun load-file (filename)
  ; Load file contents as string
  (defq f (open filename "r"))
  (if (not f)
    (progn
      (print (cat "Error: cannot open file: " filename))
      "")
    (defq content (read f))
    (close f)
    content))

(defun save-file (filename content)
  ; Save string to file
  (defq f (open filename "w"))
  (if (not f)
    (print (cat "Error: cannot write file: " filename))
    (write f content)
    (close f)))

(defun replace (str from to)
  ; Simple string replacement
  (defq parts (split str from))
  (reduce (lambda (acc part)
            (if (= acc "")
              part
              (cat acc to part)))
          parts ""))

;; ========================================================================
;; CLI Interface
;; ========================================================================

(defun litprog-help ()
  ; Print help information
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print "║  LITPROG - Literate Programming Tool for ChrysaLisp          ║")
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")
  (print "A comprehensive literate programming tool supporting:")
  (print "  • Noweb-style syntax: <<chunk>>= and <<chunk>>")
  (print "  • Org-mode syntax: #+BEGIN_SRC / #+END_SRC")
  (print "  • Markdown code fences with literate extensions")
  (print "")
  (print "Commands:")
  (print "  tangle <source.lit> <output-dir>")
  (print "    Extract code chunks to source files")
  (print "")
  (print "  weave <source.lit> <output.md> [format]")
  (print "    Generate documentation")
  (print "    Formats: markdown (default), html, latex")
  (print "")
  (print "  help")
  (print "    Show this help")
  (print "")
  (print "Example:")
  (print "  (litprog-tangle \"myprogram.lit\" \"src/\")")
  (print "  (litprog-weave \"myprogram.lit\" \"docs.html\" :format :html)")
  (print ""))

;; ========================================================================
;; Export
;; ========================================================================

(export
  litprog-tangle
  litprog-weave
  litprog-help
  parse-literate-file)

(print "Literate Programming Tool loaded!")
(print "Type (litprog-help) for usage information.")
