(import "lib/options/options.inc")
(import "lib/text/searching.inc")

(defq usage `(
(("-h" "--help")
"Usage: sed [options] script [path] ...

	options:
		-h --help: this help info.
		-e --expression script: add script to commands.
		-n --quiet: suppress automatic printing.

	script commands:
		s/pattern/replacement/[g]  substitute
		d                          delete line
		p                          print line
		[addr]command              apply to address
		[addr1],[addr2]command     apply to range

	addresses:
		number                     line number (1-based)
		/pattern/                  lines matching pattern
		$                          last line

	examples:
		sed 's/foo/bar/' file.txt
		sed -n '/error/p' log.txt
		sed '1,10d' file.txt
		sed 's/old/new/g' < input.txt

	If no paths given on command line
	then will read from stdin.")
(("-e" "--expression") ,(opt-str 'script))
(("-n" "--quiet") ,(opt-flag 'opt_n))
))

;parse sed address (line number, pattern, or $)
(defun parse-address (addr_str)
	; (parse-address str) -> (:num n) | (:pattern p) | :last | :nil
	(cond
		((eql addr_str "$") :last)
		((starts-with "/" addr_str)
			(if (and (> (length addr_str) 1) (ends-with "/" addr_str))
				(list :pattern (slice addr_str 1 -1))
				:nil))
		((num? (defq n (num-str addr_str))) (list :num n))
		(:t :nil)))

;parse sed command script
(defun parse-script (script_str)
	; (parse-script str) -> (addr1 addr2 cmd arg) | :nil
	(defq addr1 :nil addr2 :nil cmd :nil arg :nil idx 0 len (length script_str))

	;skip whitespace
	(while (and (< idx len) (find (char-at script_str idx) " \t"))
		(setq idx (inc idx)))

	;check for address
	(when (< idx len)
		(defq c (char-at script_str idx))
		(cond
			;line number or $ address
			((or (num? c) (eql c "$"))
				(defq start idx)
				(while (and (< idx len) (not (find (char-at script_str idx) ",; \t")))
					(setq idx (inc idx)))
				(setq addr1 (parse-address (slice script_str start idx))))
			;pattern address
			((eql c "/")
				(defq start idx)
				(setq idx (inc idx))
				(while (and (< idx len) (not (eql (char-at script_str idx) "/")))
					(setq idx (inc idx)))
				(when (< idx len)
					(setq idx (inc idx))
					(setq addr1 (parse-address (slice script_str start idx)))))))

	;check for range
	(when (and addr1 (< idx len) (eql (char-at script_str idx) ","))
		(setq idx (inc idx))
		(defq c (char-at script_str idx))
		(cond
			;line number or $ address
			((or (num? c) (eql c "$"))
				(defq start idx)
				(while (and (< idx len) (not (find (char-at script_str idx) "; \t")))
					(setq idx (inc idx)))
				(setq addr2 (parse-address (slice script_str start idx))))
			;pattern address
			((eql c "/")
				(defq start idx)
				(setq idx (inc idx))
				(while (and (< idx len) (not (eql (char-at script_str idx) "/")))
					(setq idx (inc idx)))
				(when (< idx len)
					(setq idx (inc idx))
					(setq addr2 (parse-address (slice script_str start idx)))))))

	;skip whitespace
	(while (and (< idx len) (find (char-at script_str idx) " \t"))
		(setq idx (inc idx)))

	;get command
	(when (< idx len)
		(setq cmd (char-at script_str idx) idx (inc idx))

		;parse command argument (for 's' command)
		(when (eql cmd "s")
			(if (and (< idx len) (eql (char-at script_str idx) "/"))
				(progn
					(setq idx (inc idx))
					(defq pattern_start idx)
					;find end of pattern
					(while (and (< idx len) (not (eql (char-at script_str idx) "/")))
						(setq idx (inc idx)))
					(when (< idx len)
						(defq pattern (slice script_str pattern_start idx))
						(setq idx (inc idx))
						(defq repl_start idx)
						;find end of replacement
						(while (and (< idx len) (not (eql (char-at script_str idx) "/")))
							(setq idx (inc idx)))
						(when (< idx len)
							(defq replacement (slice script_str repl_start idx))
							(setq idx (inc idx))
							;check for flags
							(defq flags "")
							(while (< idx len)
								(setq flags (cat flags (char-at script_str idx)) idx (inc idx)))
							(setq arg (list pattern replacement flags)))))
				(setq cmd :nil))))

	(if cmd (list addr1 addr2 cmd arg) :nil))

;check if line matches address
(defun matches-address? (addr line_num line_text is_last)
	; (matches-address? addr num text last?) -> :t | :nil
	(cond
		((not addr) :t)
		((eql addr :last) is_last)
		((eql (first addr) :num) (= (second addr) line_num))
		((eql (first addr) :pattern)
			(bind '(search pattern meta) (query (second addr) :nil :t))
			(. search :match? line_text pattern meta))
		(:t :nil)))

;execute sed command on a line
(defun execute-command (cmd_info line_num line_text is_last in_range)
	; (execute-command info num text last? range) -> (print? text new_range)
	(bind '(addr1 addr2 cmd arg) cmd_info)
	(defq should_print :t new_range in_range)

	;determine if we should execute based on address/range
	(defq should_execute :nil)
	(cond
		;range mode
		(addr2
			(cond
				(in_range
					(setq should_execute :t)
					(when (matches-address? addr2 line_num line_text is_last)
						(setq new_range :nil)))
				((matches-address? addr1 line_num line_text is_last)
					(setq should_execute :t new_range :t))))
		;single address
		(addr1
			(setq should_execute (matches-address? addr1 line_num line_text is_last)))
		;no address - execute on all lines
		(:t (setq should_execute :t)))

	;execute command if applicable
	(when should_execute
		(case cmd
			("s"
				;substitute command
				(when arg
					(bind '(pattern replacement flags) arg)
					(bind '(search pat meta) (query pattern :nil :t))
					(defq global (find "g" flags))
					(if global
						;global replace - replace all occurrences
						(while (. search :match? line_text pat meta)
							(defq matches (. search :search line_text pat meta))
							(when (and matches (not (empty? matches)))
								(bind '((start end)) (first matches))
								(setq line_text (cat
									(slice line_text 0 start)
									replacement
									(slice line_text end -1)))))
						;replace first occurrence only
						(when (. search :match? line_text pat meta)
							(defq matches (. search :search line_text pat meta))
							(when (and matches (not (empty? matches)))
								(bind '((start end)) (first matches))
								(setq line_text (cat
									(slice line_text 0 start)
									replacement
									(slice line_text end -1))))))))
			("d"
				;delete command
				(setq should_print :nil))
			("p"
				;print command (always prints, even in -n mode)
				(print line_text))))

	(list should_print line_text new_range))

;process stream with sed script
(defun sed-stream (stream cmd_info suppress_auto)
	; (sed-stream stream cmd suppress) -> :nil
	(when stream
		(defq lines (list) line_num 0 in_range :nil)

		;read all lines to determine last line
		(while (defq line (read-line stream))
			(push lines line))

		(defq total_lines (length lines))

		;process each line
		(each! (lambda (idx line_text)
			(setq line_num (inc idx))
			(defq is_last (= line_num total_lines))
			(bind '(should_print new_text new_range)
				(execute-command cmd_info line_num line_text is_last in_range))
			(setq in_range new_range)
			(when (and should_print (not suppress_auto))
				(print new_text)))
			lines :t 0)))

;process file with sed script
(defun sed-file (file cmd_info suppress_auto)
	; (sed-file path cmd suppress) -> :nil
	(when (defq stream (file-stream file))
		(sed-stream stream cmd_info suppress_auto)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq script "" opt_n :nil args (options stdio usage)))
		;get script from -e option or first arg
		(when (and (eql script "") (> (length args) 1))
			(setq script (second args) args (erase args 1 2)))

		;parse the script
		(when (defq cmd_info (parse-script script))
			(defq files (rest args))
			(if (empty? files)
				;process stdin
				(sed-stream (io-stream 'stdin) cmd_info opt_n)
				;process files
				(each (# (sed-file %0 cmd_info opt_n)) files)))))
