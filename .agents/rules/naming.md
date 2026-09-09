# ChrysaLisp Naming Conventions

*	**Functions and Macros:** Use kebab-case with hyphens (`-`), e.g.
	`(mail-send)`, `(elem-get)`, `(defun parse-line ...)`. ONLY callable code
	(functions and macros) may use kebab style.

*	**Variables and Parameters:** Always use snake_case with underscores
	(`_`), e.g. `(defq file_path "...")`, `(defun foo (user_id count_val) ...)`,
	`(bind '(start_idx end_idx) ...)`. NEVER use hyphens in variable,
	argument, or parameter names.

*	**Constants:** Prefixed with `+`, e.g. `+argb_black`, `+event_close`.

*	**Global Variables:** Enclosed in asterisks `*...*`, e.g. `*window*`,
	`*running*`.

*	**Properties and Keywords:** Prefixed with `:`, e.g. `:ink_color`,
	`:select_all`.
