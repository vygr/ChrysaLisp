;; ========================================================================
;; LITPROG VCS - Version Control Integration (Phase 3)
;; ========================================================================
;;
;; Integrate literate programming with version control systems (Git)
;; Features:
;; - Chunk-level git diff
;; - Git blame integration
;; - Change tracking
;; - Diff visualization
;; - History browsing
;; - Annotation support
;;
;; Usage:
;;   (import "litprog_vcs.lisp")
;;   (git-diff-chunks "myfile.lit")
;;   (git-blame-chunk ctx "chunk-name")
;;   (generate-changelog ctx "v1.0" "v2.0")
;;

(import "litprog.lisp")

;; ========================================================================
;; Git Diff Integration
;; ========================================================================

(defun git-diff-chunks (filename)
  "Show git diff for literate file, organized by chunks"
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print "║  Git Diff - Chunk View                                        ║")
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")

  ; Get git diff
  (defq diff-output (shell (cat "git diff " filename)))

  (if (= (length diff-output) 0)
    (progn
      (print "No changes detected")
      (print ""))
    (progn
      ; Parse diff and organize by chunks
      (defq parsed-diff (parse-git-diff diff-output))
      (display-chunk-diff parsed-diff))))

(defun parse-git-diff (diff-text)
  "Parse git diff output into structured format"
  (defq lines (split diff-text "\n"))
  (defq chunks (env))
  (defq current-chunk nil)

  (each! lines
    (lambda (line)
      ; Detect chunk boundaries (<<chunk-name>>=)
      (if (find line "<<")
        (defq match (parse-noweb-chunk-def line))
        (if match
          (setq current-chunk match)
          (if (not (. chunks current-chunk))
            (. chunks current-chunk (list)))))

      ; Track changes
      (if current-chunk
        (cond
          ((starts-with? line "+")
           (defq chunk-changes (. chunks current-chunk))
           (push chunk-changes (list :type :added :line line))
           (. chunks current-chunk chunk-changes))

          ((starts-with? line "-")
           (defq chunk-changes (. chunks current-chunk))
           (push chunk-changes (list :type :removed :line line))
           (. chunks current-chunk chunk-changes))))))

  chunks)

(defun display-chunk-diff (chunks)
  "Display diff organized by chunks"
  (each! (env-keys chunks)
    (lambda (chunk-name)
      (defq changes (. chunks chunk-name))
      (if (> (length changes) 0)
        (progn
          (print (cat "Chunk: " chunk-name))
          (print (cat "Changes: " (length changes)))
          (each! changes
            (lambda (change)
              (defq line (. change :line))
              (if (= (. change :type) :added)
                (print (cat "  + " line))
                (print (cat "  - " line)))))
          (print ""))))))

;; ========================================================================
;; Git Blame Integration
;; ========================================================================

(defun git-blame-chunk (ctx chunk-name)
  "Show git blame for a specific chunk"
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print (cat "║  Git Blame: " chunk-name (repeat-str " " (- 48 (length chunk-name))) "║"))
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")

  (defq chunk (. (. ctx :chunks) chunk-name))
  (if (not chunk)
    (print "Chunk not found")
    (progn
      (defq start-line (. chunk :line))
      (defq code-lines (split-lines (. chunk :code)))
      (defq num-lines (length code-lines))

      ; Run git blame
      (defq blame-cmd (cat "git blame -L " start-line "," (+ start-line num-lines) " " (. ctx :filename)))
      (defq blame-output (shell blame-cmd))

      (if (> (length blame-output) 0)
        (print blame-output)
        (print "Unable to get git blame information")))))

;; ========================================================================
;; Change Tracking
;; ========================================================================

(defun track-chunk-changes (ctx chunk-name &rest opts)
  "Track changes to a specific chunk over time"
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print (cat "║  Change History: " chunk-name (repeat-str " " (- 45 (length chunk-name))) "║"))
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")

  ; Get git log for the file
  (defq log-cmd (cat "git log --oneline -- " (. ctx :filename)))
  (defq log-output (shell log-cmd))
  (defq commits (split log-output "\n"))

  (print (cat "Found " (length commits) " commits affecting this file"))
  (print "")

  ; Show recent commits
  (defq max-commits (or (get opts :limit) 10))
  (defq shown 0)

  (each! commits
    (lambda (commit)
      (if (< shown max-commits)
        (progn
          (print commit)
          (setq shown (inc shown))))))

  (print ""))

;; ========================================================================
;; Changelog Generation
;; ========================================================================

(defun generate-changelog (ctx from-tag to-tag)
  "Generate changelog from git history"
  (print "")
  (print "╔════════════════════════════════════════════════════════════════╗")
  (print "║  Changelog Generator                                          ║")
  (print "╚════════════════════════════════════════════════════════════════╝")
  (print "")
  (print (cat "Changes from " from-tag " to " to-tag))
  (print "")

  ; Get commits between tags
  (defq log-cmd (cat "git log --pretty=format:'%h - %s (%an, %ar)' " from-tag ".." to-tag))
  (defq log-output (shell log-cmd))

  (defq changelog "")
  (setq changelog (cat changelog "# Changelog\n\n"))
  (setq changelog (cat changelog "## " to-tag "\n\n"))

  ; Categorize commits
  (defq features (list))
  (defq fixes (list))
  (defq other (list))

  (each! (split log-output "\n")
    (lambda (commit)
      (cond
        ((or (find commit "feat:") (find commit "feature:"))
         (push features commit))
        ((or (find commit "fix:") (find commit "bugfix:"))
         (push fixes commit))
        (t
         (push other commit)))))

  ; Generate changelog
  (if (> (length features) 0)
    (progn
      (setq changelog (cat changelog "### Features\n\n"))
      (each! features
        (lambda (f)
          (setq changelog (cat changelog "- " f "\n"))))))

  (if (> (length fixes) 0)
    (progn
      (setq changelog (cat changelog "\n### Bug Fixes\n\n"))
      (each! fixes
        (lambda (f)
          (setq changelog (cat changelog "- " f "\n"))))))

  (if (> (length other) 0)
    (progn
      (setq changelog (cat changelog "\n### Other Changes\n\n"))
      (each! other
        (lambda (o)
          (setq changelog (cat changelog "- " o "\n"))))))

  ; Save and display
  (save-file "CHANGELOG.md" changelog)
  (print changelog)
  (print "")
  (print "Changelog saved to CHANGELOG.md"))

;; ========================================================================
;; Diff Visualization for HTML
;; ========================================================================

(defun weave-diff-html (ctx old-file new-file output-file)
  "Generate HTML showing differences between two versions"
  (print (cat "Generating diff visualization: " output-file))

  (defq old-ctx (parse-literate-file old-file))
  (defq new-ctx (parse-literate-file new-file))

  (defq html "")

  ; Header
  (setq html (cat html (diff-html-header)))

  ; Compare chunks
  (setq html (cat html "<div class=\"diff-container\">\n"))
  (setq html (cat html "<h1>Literate Program Diff</h1>\n"))
  (setq html (cat html "<p>Comparing: " old-file " → " new-file "</p>\n"))

  ; Find added, removed, and modified chunks
  (defq old-chunks (env-keys (. old-ctx :chunks)))
  (defq new-chunks (env-keys (. new-ctx :chunks)))

  ; Added chunks
  (setq html (cat html "<h2>Added Chunks</h2>\n"))
  (each! new-chunks
    (lambda (name)
      (if (not (find old-chunks name))
        (defq chunk (. (. new-ctx :chunks) name))
        (setq html (cat html (diff-chunk-html chunk "added"))))))

  ; Removed chunks
  (setq html (cat html "<h2>Removed Chunks</h2>\n"))
  (each! old-chunks
    (lambda (name)
      (if (not (find new-chunks name))
        (defq chunk (. (. old-ctx :chunks) name))
        (setq html (cat html (diff-chunk-html chunk "removed"))))))

  ; Modified chunks
  (setq html (cat html "<h2>Modified Chunks</h2>\n"))
  (each! new-chunks
    (lambda (name)
      (if (find old-chunks name)
        (defq old-chunk (. (. old-ctx :chunks) name))
        (defq new-chunk (. (. new-ctx :chunks) name))
        (if (not (= (. old-chunk :code) (. new-chunk :code)))
          (setq html (cat html (diff-chunk-comparison-html old-chunk new-chunk)))))))

  (setq html (cat html "</div>\n"))
  (setq html (cat html "</body>\n</html>\n"))

  (save-file output-file html)
  (print "✓ Diff visualization generated!"))

(defun diff-html-header ()
  "HTML header for diff visualization"
  (cat
    "<!DOCTYPE html>\n"
    "<html>\n"
    "<head>\n"
    "  <meta charset=\"UTF-8\">\n"
    "  <title>Literate Program Diff</title>\n"
    "  <style>\n"
    "    body { font-family: sans-serif; padding: 20px; max-width: 1200px; margin: 0 auto; }\n"
    "    .chunk { border: 1px solid #ddd; border-radius: 4px; margin: 20px 0; }\n"
    "    .chunk-header { background: #f5f5f5; padding: 10px; font-weight: bold; }\n"
    "    .chunk-code { padding: 10px; background: #fafafa; }\n"
    "    pre { margin: 0; white-space: pre-wrap; }\n"
    "    .added { background: #e6ffe6; border-left: 4px solid #4caf50; }\n"
    "    .removed { background: #ffe6e6; border-left: 4px solid #f44336; }\n"
    "    .modified { background: #fff8e1; border-left: 4px solid #ff9800; }\n"
    "    .diff-side-by-side { display: grid; grid-template-columns: 1fr 1fr; gap: 10px; }\n"
    "    .diff-old { background: #ffe6e6; padding: 10px; }\n"
    "    .diff-new { background: #e6ffe6; padding: 10px; }\n"
    "  </style>\n"
    "</head>\n"
    "<body>\n"))

(defun diff-chunk-html (chunk status)
  "Generate HTML for a single chunk diff"
  (cat
    "<div class=\"chunk " status "\">\n"
    "  <div class=\"chunk-header\">" (escape-html (. chunk :name)) " [" status "]</div>\n"
    "  <div class=\"chunk-code\"><pre>" (escape-html (. chunk :code)) "</pre></div>\n"
    "</div>\n"))

(defun diff-chunk-comparison-html (old-chunk new-chunk)
  "Generate side-by-side comparison HTML"
  (cat
    "<div class=\"chunk modified\">\n"
    "  <div class=\"chunk-header\">" (escape-html (. old-chunk :name)) " [modified]</div>\n"
    "  <div class=\"diff-side-by-side\">\n"
    "    <div class=\"diff-old\">\n"
    "      <h4>Old</h4>\n"
    "      <pre>" (escape-html (. old-chunk :code)) "</pre>\n"
    "    </div>\n"
    "    <div class=\"diff-new\">\n"
    "      <h4>New</h4>\n"
    "      <pre>" (escape-html (. new-chunk :code)) "</pre>\n"
    "    </div>\n"
    "  </div>\n"
    "</div>\n"))

;; ========================================================================
;; Annotation Support
;; ========================================================================

(defclass chunk-annotation ()
  ; Represents an annotation on a chunk
  (def this
    :chunk-name ""
    :author ""
    :timestamp 0
    :type ""      ; comment, question, suggestion, review
    :text ""
    :resolved nil))

(defq *annotations* (env))

(defun add-annotation (chunk-name type text &rest opts)
  "Add an annotation to a chunk"
  (defq ann (chunk-annotation))
  (. ann :chunk-name chunk-name)
  (. ann :type type)
  (. ann :text text)
  (. ann :author (or (get opts :author) "Anonymous"))
  (. ann :timestamp (time))
  (. ann :resolved nil)

  (if (not (. *annotations* chunk-name))
    (. *annotations* chunk-name (list)))

  (push (. *annotations* chunk-name) ann)

  (print (cat "Annotation added to chunk: " chunk-name)))

(defun show-annotations (chunk-name)
  "Show all annotations for a chunk"
  (defq anns (. *annotations* chunk-name))

  (if (not anns)
    (print "No annotations for this chunk")
    (progn
      (print (cat "Annotations for chunk: " chunk-name))
      (print "")
      (each! anns
        (lambda (ann)
          (print (cat "[" (. ann :type) "] by " (. ann :author)))
          (print (cat "  " (. ann :text)))
          (if (. ann :resolved)
            (print "  ✓ Resolved"))
          (print ""))))))

;; ========================================================================
;; Export
;; ========================================================================

(export
  git-diff-chunks
  git-blame-chunk
  track-chunk-changes
  generate-changelog
  weave-diff-html
  add-annotation
  show-annotations)

(print "LITPROG Version Control Integration loaded! (Phase 3)")
(print "Features:")
(print "  • (git-diff-chunks \"file.lit\") - Chunk-level diffs")
(print "  • (git-blame-chunk ctx \"chunk\") - See who wrote what")
(print "  • (generate-changelog ctx \"v1\" \"v2\") - Auto changelog")
(print "  • (weave-diff-html ctx \"old.lit\" \"new.lit\" \"diff.html\") - Visual diff")
(print "  • (add-annotation \"chunk\" :comment \"text\") - Add comments")
