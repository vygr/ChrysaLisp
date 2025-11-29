;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Intelligent Save with Diff Analysis
; Preserves whitespace, comments, and imperative code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui_designer/serialize.inc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Diff Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun analyze-line (line)
	; (analyze-line line) -> (:ui | :imperative | :comment | :whitespace | :import)
	(defq trimmed (trim line))
	(cond
		;Empty or whitespace only
		((or (empty? trimmed) (eql trimmed ""))
			:whitespace)
		;Comment
		((starts-with ";" trimmed)
			:comment)
		;Import statement
		((find "(import" trimmed)
			:import)
		;UI macro
		((or (find "(ui-" trimmed)
			(find "(defq" trimmed)
			(find "(def " trimmed)
			(find "(." trimmed)
			(find "(:connect" trimmed))
			;Could be UI or imperative - need context
			(if (or (find "ui-" trimmed) (find ":connect" trimmed))
				:ui
				:imperative))
		;Function definition
		((or (find "(defun" trimmed)
			(find "(defmethod" trimmed)
			(find "(defmacro" trimmed))
			:imperative)
		;Enum
		((or (find "(enums" trimmed) (find "(enum " trimmed))
			:imperative)
		;Everything else - likely imperative
		(:t :imperative)))

(defun extract-comments (source)
	; (extract-comments source) -> ((line-num comment) ...)
	; Extracts all comments with their line numbers
	(defq lines (split source (ascii-char 10))
		comments (list)
		line-num 0)
	(each (lambda (line)
		(when (eql (analyze-line line) :comment)
			(push comments (list line-num line)))
		(++ line-num))
		lines)
	comments)

(defun extract-imperative-in-ui (source)
	; (extract-imperative-in-ui source) -> ((line-num code) ...)
	; Extracts imperative code that appears within the UI section
	(defq lines (split source (ascii-char 10))
		imperative (list)
		in-ui :nil
		line-num 0)
	(each (lambda (line)
		(defq type (analyze-line line))
		;Track when we're in UI section
		(when (and (not in-ui) (or (find "(ui-window" line) (find "(ui-root" line)))
			(setq in-ui :t))
		;In UI section - look for imperative code
		(when (and in-ui (eql type :imperative))
			(push imperative (list line-num line)))
		;Track when UI section ends (heuristic: back to top-level parens)
		(when (and in-ui (not (find "ui-" line)) (eql (first (trim line)) ")"))
			(setq in-ui :nil))
		(++ line-num))
		lines)
	imperative)

(defun extract-whitespace-pattern (source)
	; (extract-whitespace-pattern source) -> pattern
	; Determines indentation style (tabs vs spaces, depth)
	(defq lines (split source (ascii-char 10))
		uses-tabs :nil
		uses-spaces :nil
		indent-size 0)
	(each (lambda (line)
		(when (nempty? line)
			(defq leading "")
			(while (and (< (length leading) (length line))
					(defq ch (elem-get line (length leading)))
					(or (eql ch (ascii-char 9)) (eql ch " ")))
				(setq leading (cat leading ch))
				(if (eql ch (ascii-char 9))
					(setq uses-tabs :t)
					(setq uses-spaces :t)))
			(when (and uses-spaces (> (length leading) indent-size))
				(setq indent-size (length leading)))))
		lines)
	(scatter (Lmap)
		:uses_tabs uses-tabs
		:uses_spaces uses-spaces
		:indent_size (if uses-spaces indent-size 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Intelligent Merging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun merge-with-preservation (original-source new-ui-code tree)
	; (merge-with-preservation original-source new-ui-code tree) -> merged-source
	; Intelligently merges new UI while preserving comments, formatting, imperative code

	;Extract metadata from original
	(defq comments (extract-comments original-source)
		imperative (extract-imperative-in-ui original-source)
		whitespace-pattern (extract-whitespace-pattern original-source))

	(print "Preservation analysis:")
	(print "  Comments in UI: " (str (length comments)))
	(print "  Imperative in UI: " (str (length imperative)))
	(print "  Indent style: " (if (get :uses_tabs whitespace-pattern) "tabs" "spaces"))
	(print "")

	;Find UI section boundaries in original
	(defq lines (split original-source (ascii-char 10))
		ui-start -1
		ui-end -1
		depth 0
		line-num 0)

	(each (lambda (line)
		(when (and (= ui-start -1) (or (find "(ui-window" line) (find "(ui-root" line)))
			(setq ui-start line-num depth 1))
		(when (>= ui-start 0)
			(each (lambda (c)
				(cond
					((eql c "(") (++ depth))
					((eql c ")") (-- depth))))
				(str-to-list line))
			(when (<= depth 0)
				(setq ui-end line-num)))
		(++ line-num))
		lines)

	;Build merged result
	(defq result ""
		line-num 0)

	(each (lambda (line)
		(cond
			;Before UI section - keep as-is
			((< line-num ui-start)
				(setq result (cat result line (ascii-char 10))))

			;At UI start - insert new UI with preserved comments
			((= line-num ui-start)
				;Insert any comments that were before this line
				(each (lambda ((cline-num comment))
					(when (and (>= cline-num ui-start) (< cline-num ui-end))
						(setq result (cat result comment (ascii-char 10)))))
					comments)
				;Insert new UI code
				(setq result (cat result new-ui-code (ascii-char 10)))
				;Insert any imperative code that was in UI section
				(each (lambda ((iline-num code))
					(when (and (> iline-num ui-start) (<= iline-num ui-end))
						(setq result (cat result code (ascii-char 10)))))
					imperative))

			;Between UI start and end - skip (already replaced)
			((and (> line-num ui-start) (< line-num ui-end))
				:nil)

			;After UI section - keep as-is
			(:t (setq result (cat result line (ascii-char 10)))))

		(++ line-num))
		lines)

	result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Diff Report Generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-diff-report (original-source new-source)
	; (generate-diff-report original-source new-source) -> report-string
	(defq orig-lines (split original-source (ascii-char 10))
		new-lines (split new-source (ascii-char 10))
		report ""
		changes (list))

	;Simple line-by-line diff
	(defq max-lines (max (length orig-lines) (length new-lines))
		line-num 0)

	(while (< line-num max-lines)
		(defq orig-line (if (< line-num (length orig-lines))
				(elem-get orig-lines line-num) "")
			new-line (if (< line-num (length new-lines))
				(elem-get new-lines line-num) ""))

		(when (not (eql orig-line new-line))
			(push changes (scatter (Lmap)
				:line line-num
				:type (cond
					((empty? orig-line) :added)
					((empty? new-line) :removed)
					(:t :changed))
				:original orig-line
				:new new-line)))

		(++ line-num))

	;Generate report
	(setq report (cat
		"╔════════════════════════════════════════════════════╗" (ascii-char 10)
		"║  Diff Report                                       ║" (ascii-char 10)
		"╚════════════════════════════════════════════════════╝" (ascii-char 10)
		(ascii-char 10)
		"Changes: " (str (length changes)) (ascii-char 10)
		(ascii-char 10)))

	;Show first 10 changes
	(defq count 0)
	(each (lambda (change)
		(when (< count 10)
			(setq report (cat report
				"Line " (str (get :line change)) ": " (str (get :type change)) (ascii-char 10)))
			(when (nempty? (get :original change))
				(setq report (cat report "  - " (get :original change) (ascii-char 10))))
			(when (nempty? (get :new change))
				(setq report (cat report "  + " (get :new change) (ascii-char 10))))
			(setq report (cat report (ascii-char 10)))
			(++ count)))
		changes)

	(when (> (length changes) 10)
		(setq report (cat report "... and " (str (- (length changes) 10)) " more changes" (ascii-char 10))))

	report)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Enhanced Save Function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun save-with-diff-analysis (tree original-source filepath &optional show-diff)
	; (save-with-diff-analysis tree original-source filepath [show-diff]) -> :t | :nil
	; Saves with intelligent preservation and optional diff report

	(print "")
	(print "╔════════════════════════════════════════════════════╗")
	(print "║  Intelligent Save with Diff Analysis              ║")
	(print "╚════════════════════════════════════════════════════╝")
	(print "")

	;Serialize the tree
	(defq new-ui-code (designer-serialize-tree tree))

	;Intelligent merge
	(defq merged-source (merge-with-preservation original-source new-ui-code tree))

	;Restore imports
	(defq final-source merged-source)
	(setq final-source (apply cat (map (lambda (line)
		(cat (if (find "import \"gui_designer/lisp.inc\"" line)
				"(import \"gui/lisp.inc\")"
				line)
			(ascii-char 10)))
		(split final-source (ascii-char 10)))))

	;Generate diff report if requested
	(when show-diff
		(print (generate-diff-report original-source final-source)))

	;Show preservation summary
	(defq orig-comments (extract-comments original-source)
		new-comments (extract-comments final-source)
		orig-imperative (extract-imperative-in-ui original-source)
		new-imperative (extract-imperative-in-ui final-source))

	(print "Preservation Summary:")
	(print "  Comments: " (str (length orig-comments)) " → " (str (length new-comments))
		(if (= (length orig-comments) (length new-comments)) " ✓" " ⚠"))
	(print "  Imperative code: " (str (length orig-imperative)) " → " (str (length new-imperative))
		(if (= (length orig-imperative) (length new-imperative)) " ✓" " ⚠"))
	(print "")

	;Write to file
	(when (defq stream (file-stream filepath +file_open_write))
		(write stream final-source)
		(print "✓ Saved to: " filepath)
		(print "")
		:t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LLM-Assisted Diff Analysis (Future Enhancement)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun llm-analyze-diff (original new)
	; (llm-analyze-diff original new) -> analysis
	; Future: Send diff to LLM for intelligent analysis
	; LLM could identify:
	; - Lost comments that should be preserved
	; - Formatting changes that should be retained
	; - Imperative code that was incorrectly removed
	; - Suggestions for better merging

	(scatter (Lmap)
		:status "not-implemented"
		:suggestion "Future enhancement: integrate LLM API for diff analysis"
		:would_preserve (list
			"Comments with context"
			"Inline imperative code"
			"Original formatting style"
			"Developer intent")
		:would_suggest (list
			"Where to re-insert lost comments"
			"How to preserve formatting"
			"Which imperative code is UI-related"
			"Conflict resolution strategies")))

(defun save-with-llm-assistance (tree original-source filepath)
	; (save-with-llm-assistance tree original-source filepath) -> :t | :nil
	; Future: Use LLM to analyze and improve the merge

	(print "LLM-Assisted Save (Future Enhancement)")
	(print "")

	;Generate initial merge
	(defq new-ui-code (designer-serialize-tree tree))
	(defq merged-source (merge-with-preservation original-source new-ui-code tree))

	;Analyze with LLM
	(defq analysis (llm-analyze-diff original-source merged-source))

	(print "LLM Analysis:")
	(print "  Status: " (get :status analysis))
	(print "  Would preserve: " (str (get :would_preserve analysis)))
	(print "  Would suggest: " (str (get :would_suggest analysis)))
	(print "")

	;Apply LLM suggestions (when implemented)
	;(setq merged-source (apply-llm-suggestions merged-source analysis))

	;Save
	(save-with-diff-analysis tree original-source filepath :t))
