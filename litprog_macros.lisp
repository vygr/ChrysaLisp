;; ========================================================================
;; LITPROG MACROS - Chunk Parameters and Template System
;; ========================================================================
;;
;; Advanced macro and template system for literate programming
;; Allows parameterized chunks for code reuse
;;
;; Example:
;;   <<sort-function(type=int, name=sort_ints)>>=
;;   (defun @name@ (lst)
;;     ; Sort list of @type@
;;     (sort lst))
;;   @
;;
;;   Later: <<sort-function(type=string, name=sort_strings)>>
;;

(import "litprog.lisp")

;; ========================================================================
;; Macro System
;; ========================================================================

(defclass litprog-macro ()
  ; Represents a parameterized chunk
  (def this
    :name ""          ; Base chunk name
    :params (list)    ; List of parameter names
    :template ""      ; Code template with @param@ placeholders
    :instances (env))) ; Map of param-values -> expanded code

(defun parse-chunk-params (chunk-ref)
  "Parse <<chunk-name(param1=value1, param2=value2)>>"
  (defq paren-start (find chunk-ref "("))
  (defq paren-end (find chunk-ref ")"))

  (if (and paren-start paren-end)
    (progn
      (defq base-name (trim (slice 2 paren-start chunk-ref)))
      (defq params-str (slice (inc paren-start) paren-end chunk-ref))
      (defq params (env))

      ; Parse param=value pairs
      (each! (split params-str ",")
        (lambda (pair)
          (defq parts (split (trim pair) "="))
          (if (= (length parts) 2)
            (. params (trim (get parts 0)) (trim (get parts 1))))))

      (list :name base-name :params params))
    nil))

(defun expand-macro-template (template params)
  "Replace @param@ placeholders with actual values"
  (defq result template)

  (each! (env-keys params)
    (lambda (param-name)
      (defq param-value (. params param-name))
      (defq placeholder (cat "@" param-name "@"))
      (setq result (replace-all result placeholder param-value))))

  result)

(defun replace-all (str from to)
  "Replace all occurrences of from with to"
  (defq parts (split str from))
  (join to parts))

(defun process-macro-chunk (chunk ctx)
  "Process a chunk that may contain macro parameters"
  (defq code (. chunk :code))

  ; Scan for @param@ definitions
  (defq params (list))
  (defq param-pattern "@([a-zA-Z0-9_]+)@")

  ; Simple param extraction (in real impl would use regex)
  (defq lines (split-lines code))
  (each! lines
    (lambda (line)
      ; Look for @word@ pattern
      (defq i 0)
      (while (< i (length line))
        (if (= (char line i) (ascii-code "@"))
          (progn
            (defq j (inc i))
            (while (and (< j (length line))
                       (not (= (char line j) (ascii-code "@"))))
              (setq j (inc j)))
            (if (< j (length line))
              (defq param (slice (inc i) j line))
              (if (not (find params param))
                (push params param))))
            (setq i j))
          (setq i (inc i)))))))

  (if (> (length params) 0)
    (progn
      (. chunk :params params)
      (. chunk :is-macro t))
    (. chunk :is-macro nil))

  chunk)

;; ========================================================================
;; Conditional Inclusion
;; ========================================================================

(defun process-conditional (line ctx)
  "Process <<if CONDITION>> ... <<else>> ... <<endif>>"
  ; Simplified - real implementation would support:
  ; <<if platform=linux>>
  ; <<if DEBUG>>
  ; <<ifdef CHUNK_NAME>>
  ; <<ifndef CHUNK_NAME>>

  (cond
    ((starts-with? (trim line) "<<if ")
      (defq condition (extract-condition line))
      (list :type :if :condition condition))

    ((starts-with? (trim line) "<<else>>")
      (list :type :else))

    ((starts-with? (trim line) "<<endif>>")
      (list :type :endif))

    (t nil)))

(defun extract-condition (line)
  "Extract condition from <<if CONDITION>>"
  (defq start (+ (find line "<<if ") 5))
  (defq end (find line ">>" start))
  (if (and start end)
    (trim (slice start end line))
    ""))

(defun evaluate-condition (condition ctx)
  "Evaluate a conditional expression"
  ; Simple variable lookup for now
  ; Could be extended to support:
  ; - platform=linux
  ; - DEBUG
  ; - FEATURE_X
  ; - defined(CHUNK_NAME)

  (. (. ctx :variables) condition))

;; ========================================================================
;; Variable System
;; ========================================================================

(defun litprog-set-var (ctx name value)
  "Set a literate programming variable"
  (if (not (. ctx :variables))
    (. ctx :variables (env)))
  (. (. ctx :variables) name value))

(defun litprog-get-var (ctx name default)
  "Get a literate programming variable"
  (if (. ctx :variables)
    (or (. (. ctx :variables) name) default)
    default))

;; ========================================================================
;; Include External Files
;; ========================================================================

(defun process-include (line ctx)
  "Process <<include filename.lit>>"
  (if (starts-with? (trim line) "<<include ")
    (progn
      (defq start (+ (find line "<<include ") 10))
      (defq end (find line ">>" start))
      (if (and start end)
        (defq filename (trim (slice start end line)))
        (include-literate-file filename ctx)
        nil))
    nil))

(defun include-literate-file (filename ctx)
  "Include and parse another literate file"
  (print (cat "Including: " filename))

  (defq content (load-file filename))
  (if (> (length content) 0)
    (progn
      (defq sub-ctx (parse-literate-file filename))

      ; Merge chunks from included file
      (each! (env-keys (. sub-ctx :chunks))
        (lambda (chunk-name)
          (defq chunk (. (. sub-ctx :chunks) chunk-name))
          (. (. ctx :chunks) chunk-name chunk)))

      t)
    nil))

;; ========================================================================
;; Advanced Chunk Operators
;; ========================================================================

(defun chunk-append (ctx chunk-name code)
  "Append code to an existing chunk (<<chunk>>+=)"
  (if (. (. ctx :chunks) chunk-name)
    (defq chunk (. (. ctx :chunks) chunk-name))
    (. chunk :code (cat (. chunk :code) "\n" code))
    ; Create new chunk if doesn't exist
    (defq new-chunk (litprog-chunk))
    (. new-chunk :name chunk-name)
    (. new-chunk :code code)
    (. (. ctx :chunks) chunk-name new-chunk)))

(defun chunk-prepend (ctx chunk-name code)
  "Prepend code to an existing chunk (<<chunk>>=-)"
  (if (. (. ctx :chunks) chunk-name)
    (defq chunk (. (. ctx :chunks) chunk-name))
    (. chunk :code (cat code "\n" (. chunk :code)))
    ; Create new chunk if doesn't exist
    (defq new-chunk (litprog-chunk))
    (. new-chunk :name chunk-name)
    (. new-chunk :code code)
    (. (. ctx :chunks) chunk-name new-chunk)))

;; ========================================================================
;; Literate Programming Directives
;; ========================================================================

(defun process-directive (line ctx)
  "Process special directives like @line, @file, @date"
  (defq result line)

  ; Current file name
  (setq result (replace-all result "@file@" (or (. ctx :current-file) "unknown")))

  ; Current date/time
  (setq result (replace-all result "@date@" (time)))

  ; Author (from context)
  (setq result (replace-all result "@author@"
    (litprog-get-var ctx "author" "Anonymous")))

  ; Version (from context)
  (setq result (replace-all result "@version@"
    (litprog-get-var ctx "version" "1.0")))

  ; Project name
  (setq result (replace-all result "@project@"
    (litprog-get-var ctx "project" "Project")))

  result)

;; ========================================================================
;; Example Usage
;; ========================================================================

(defun example-macro-usage ()
  "Demonstrate the macro system"
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print "║  LITPROG Macro System Example                                 ║")
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")
  (print "Example literate file with macros:")
  (print "")
  (print "  # Generic Sorting Function")
  (print "")
  (print "  <<sort-template(type, name)>>=")
  (print "  (defun @name@ (lst)")
  (print "    ; Sort list of @type@")
  (print "    (sort lst))")
  (print "  @")
  (print "")
  (print "  Now instantiate for different types:")
  (print "")
  (print "  <<sort-ints>>=")
  (print "  <<sort-template(type=integers, name=sort-ints)>>")
  (print "  @")
  (print "")
  (print "  <<sort-strings>>=")
  (print "  <<sort-template(type=strings, name=sort-strings)>>")
  (print "  @")
  (print "")
  (print "This generates:")
  (print "")
  (print "  (defun sort-ints (lst)")
  (print "    ; Sort list of integers")
  (print "    (sort lst))")
  (print "")
  (print "  (defun sort-strings (lst)")
  (print "    ; Sort list of strings")
  (print "    (sort lst))")
  (print ""))

;; ========================================================================
;; Export
;; ========================================================================

(export
  parse-chunk-params
  expand-macro-template
  process-macro-chunk
  process-conditional
  evaluate-condition
  litprog-set-var
  litprog-get-var
  process-include
  chunk-append
  chunk-prepend
  process-directive
  example-macro-usage)

(print "LITPROG Macros loaded!")
(print "Features:")
(print "  • Parameterized chunks: <<chunk(param=value)>>")
(print "  • Variables: @var@ placeholders")
(print "  • Conditionals: <<if>>, <<else>>, <<endif>>")
(print "  • Includes: <<include file.lit>>")
(print "  • Directives: @file@, @date@, @author@")
(print "Call (example-macro-usage) for examples.")
