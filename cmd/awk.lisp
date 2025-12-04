(import "lib/options/options.inc")
(import "lib/text/searching.inc")

(defq usage `(
(("-h" "--help")
"Usage: awk [options] 'program' [path] ...

	options:
		-h --help: this help info.
		-F --field-sep sep: field separator (default whitespace).
		-v --var name=value: set variable.

	program format:
		BEGIN { action }
		pattern { action }
		END { action }
		{ action }

	patterns:
		/regexp/               match regexp
		expression             evaluate expression
		pattern1, pattern2     range (not implemented)

	actions:
		print                  print whole line
		print $1               print field 1
		print $1, $2           print fields
		print \"text\"           print literal text
		variable = value       assignment
		{ statement; ... }     compound statement

	built-in variables:
		NR    current line number
		NF    number of fields in current line
		FS    field separator (input)
		$0    whole line
		$1    first field
		$2    second field, etc.

	examples:
		awk '{print $1}' file.txt
		awk -F: '{print $1, $3}' /etc/passwd
		awk '/error/ {print NR, $0}' log.txt
		awk 'BEGIN {sum=0} {sum+=$1} END {print sum}' data.txt
		awk 'NR > 5 && NR < 10' file.txt

	If no paths given on command line
	then will read from stdin.")
(("-F" "--field-sep") ,(opt-str 'opt_fs))
(("-v" "--var") ,(opt-str 'opt_v))
))

;tokenize awk program into blocks
(defun parse-program (prog_str)
	; (parse-program str) -> ((type pattern action) ...)
	(defq blocks (list) idx 0 len (length prog_str))

	;simple parser - look for BEGIN, END, and pattern { action } blocks
	(while (< idx len)
		;skip whitespace
		(while (and (< idx len) (find (char-at prog_str idx) " \t\n\r"))
			(setq idx (inc idx)))

		(when (< idx len)
			(defq block_type :pattern pattern :nil action :nil)

			;check for BEGIN
			(when (and (>= (- len idx) 5)
					(eql (slice prog_str idx (+ idx 5)) "BEGIN"))
				(setq block_type :begin idx (+ idx 5) pattern :nil))

			;check for END
			(when (and (eql block_type :pattern)
					(>= (- len idx) 3)
					(eql (slice prog_str idx (+ idx 3)) "END"))
				(setq block_type :end idx (+ idx 3) pattern :nil))

			;skip whitespace
			(while (and (< idx len) (find (char-at prog_str idx) " \t\n\r"))
				(setq idx (inc idx)))

			;check for pattern (if not BEGIN/END)
			(when (and (eql block_type :pattern) (< idx len) (not (eql (char-at prog_str idx) "{")))
				(defq pat_start idx)
				;pattern ends at {
				(while (and (< idx len) (not (eql (char-at prog_str idx) "{")))
					(setq idx (inc idx)))
				(setq pattern (trim (slice prog_str pat_start idx))))

			;skip whitespace
			(while (and (< idx len) (find (char-at prog_str idx) " \t\n\r"))
				(setq idx (inc idx)))

			;parse action block
			(when (and (< idx len) (eql (char-at prog_str idx) "{"))
				(setq idx (inc idx))
				(defq action_start idx depth 1)
				;find matching }
				(while (and (< idx len) (> depth 0))
					(defq c (char-at prog_str idx))
					(cond
						((eql c "{") (setq depth (inc depth)))
						((eql c "}") (setq depth (dec depth))))
					(when (> depth 0) (setq idx (inc idx))))
				(setq action (trim (slice prog_str action_start idx)) idx (inc idx)))

			;if we have no action block, look for single statement
			(when (and (not action) (< idx len) (not (eql (char-at prog_str idx) "{")))
				(defq action_start idx)
				;action ends at newline or semicolon
				(while (and (< idx len) (not (find (char-at prog_str idx) "\n\r;")))
					(setq idx (inc idx)))
				(setq action (trim (slice prog_str action_start idx))))

			;add block if we have an action
			(when action
				(push blocks (list block_type pattern action)))))

	blocks)

;split line into fields based on separator
(defun split-fields (line sep_pattern)
	; (split-fields line sep) -> (field ...)
	(if sep_pattern
		;split by pattern
		(split line sep_pattern)
		;split by whitespace (default)
		(filter! (# (not (eql %0 ""))) (split (trim line) " \t"))))

;get field value by number
(defun get-field (fields field_num line)
	; (get-field fields num line) -> str
	(cond
		((= field_num 0) line)
		((and (> field_num 0) (<= field_num (length fields)))
			(elem-get fields (dec field_num)))
		(:t "")))

;evaluate simple expression in awk context
(defun eval-expr (expr_str nr nf fields line vars)
	; (eval-expr str nr nf fields line vars) -> value
	(defq expr (trim expr_str))

	;handle variable references
	(cond
		((eql expr "NR") nr)
		((eql expr "NF") nf)
		((eql expr "$0") line)
		((starts-with "$" expr)
			(defq field_num (num-str (slice expr 1 -1)))
			(if field_num (get-field fields field_num line) expr))
		;check for variable in vars
		((defq val (. vars :find expr)) val)
		;try to parse as number
		((num? (defq n (num-str expr))) n)
		;return as string literal (remove quotes if present)
		((and (starts-with "\"" expr) (ends-with "\"" expr))
			(slice expr 1 -1))
		(:t expr)))

;parse simple comparison/expression
(defun eval-condition (cond_str nr nf fields line vars)
	; (eval-condition str nr nf fields line vars) -> :t | :nil
	(defq cond (trim cond_str))

	;check for pattern match /regexp/
	(when (and (starts-with "/" cond) (ends-with "/" cond))
		(defq pattern (slice cond 1 -1))
		(bind '(search pat meta) (query pattern :nil :t))
		(return (. search :match? line pat meta)))

	;check for comparison operators
	(cond
		((find "==" cond)
			(bind '(left right) (split cond "=="))
			(eql (eval-expr left nr nf fields line vars)
				(eval-expr right nr nf fields line vars)))
		((find "!=" cond)
			(bind '(left right) (split cond "!="))
			(not (eql (eval-expr left nr nf fields line vars)
				(eval-expr right nr nf fields line vars))))
		((find "<=" cond)
			(bind '(left right) (split cond "<="))
			(<= (num-eval (eval-expr left nr nf fields line vars))
				(num-eval (eval-expr right nr nf fields line vars))))
		((find ">=" cond)
			(bind '(left right) (split cond ">="))
			(>= (num-eval (eval-expr left nr nf fields line vars))
				(num-eval (eval-expr right nr nf fields line vars))))
		((find "<" cond)
			(bind '(left right) (split cond "<"))
			(< (num-eval (eval-expr left nr nf fields line vars))
				(num-eval (eval-expr right nr nf fields line vars))))
		((find ">" cond)
			(bind '(left right) (split cond ">"))
			(> (num-eval (eval-expr left nr nf fields line vars))
				(num-eval (eval-expr right nr nf fields line vars))))
		((find "&&" cond)
			(bind '(left right) (split cond "&&"))
			(and (eval-condition left nr nf fields line vars)
				(eval-condition right nr nf fields line vars)))
		((find "||" cond)
			(bind '(left right) (split cond "||"))
			(or (eval-condition left nr nf fields line vars)
				(eval-condition right nr nf fields line vars)))
		(:t
			;treat as simple expression - non-empty/non-zero is true
			(defq val (eval-expr cond nr nf fields line vars))
			(cond
				((num? val) (/= val 0))
				((eql val "") :nil)
				(:t :t)))))

;convert value to number
(defun num-eval (val)
	; (num-eval val) -> num
	(cond
		((num? val) val)
		((num? (defq n (num-str val))) n)
		(:t 0)))

;execute awk action
(defun execute-action (action_str nr nf fields line vars)
	; (execute-action str nr nf fields line vars) -> vars
	(defq action (trim action_str) new_vars vars)

	;split multiple statements by ;
	(defq statements (split action ";"))

	(each (lambda (stmt)
		(setq stmt (trim stmt))
		(when (not (eql stmt ""))
			(cond
				;print statement
				((starts-with "print" stmt)
					(defq print_args (trim (slice stmt 5 -1)))
					(if (eql print_args "")
						;print whole line
						(print line)
						;print specific fields/expressions
						(progn
							(defq parts (split print_args ",")
								output (list))
							(each (lambda (part)
								(push output (str (eval-expr (trim part) nr nf fields line new_vars))))
								parts)
							(print (apply (const cat) (map! (# (cat %0 " ")) output :t 0 -1))
								(elem-get output -1)))))
				;assignment statement var = expr
				((find "=" stmt)
					(bind '(var_name value) (split stmt "="))
					(defq var_name (trim var_name)
						value (eval-expr (trim value) nr nf fields line new_vars))
					(def new_vars var_name value))
				;compound assignment +=
				((find "+=" stmt)
					(bind '(var_name value) (split stmt "+="))
					(defq var_name (trim var_name)
						old_val (. new_vars :find var_name)
						new_val (+ (num-eval (if old_val old_val 0))
							(num-eval (eval-expr (trim value) nr nf fields line new_vars))))
					(def new_vars var_name new_val))))))
		statements)

	new_vars)

;process stream with awk program
(defun awk-stream (stream blocks field_sep)
	; (awk-stream stream blocks sep) -> :nil
	(when stream
		(defq nr 0 vars (env 11))

		;execute BEGIN blocks
		(each (lambda (block)
			(bind '(block_type pattern action) block)
			(when (eql block_type :begin)
				(setq vars (execute-action action 0 0 (list) "" vars))))
			blocks)

		;process each line
		(while (defq line (read-line stream))
			(setq nr (inc nr))
			(defq fields (split-fields line field_sep)
				nf (length fields))

			;execute pattern blocks
			(each (lambda (block)
				(bind '(block_type pattern action) block)
				(when (eql block_type :pattern)
					(defq should_execute
						(if pattern
							(eval-condition pattern nr nf fields line vars)
							:t))
					(when should_execute
						(setq vars (execute-action action nr nf fields line vars)))))
				blocks))

		;execute END blocks
		(each (lambda (block)
			(bind '(block_type pattern action) block)
			(when (eql block_type :end)
				(setq vars (execute-action action nr 0 (list) "" vars))))
			blocks)))

;process file with awk program
(defun awk-file (file blocks field_sep)
	; (awk-file path blocks sep) -> :nil
	(when (defq stream (file-stream file))
		(awk-stream stream blocks field_sep)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_fs :nil opt_v :nil args (options stdio usage)))
		;get program from first arg
		(when (> (length args) 1)
			(defq program (second args) args (erase args 1 2))

			;parse the program
			(when (defq blocks (parse-program program))
				(defq files (rest args))
				(if (empty? files)
					;process stdin
					(awk-stream (io-stream 'stdin) blocks opt_fs)
					;process files
					(each (# (awk-file %0 blocks opt_fs)) files))))))
