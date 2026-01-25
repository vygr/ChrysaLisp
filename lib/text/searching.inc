;;;;;;;;;;;;;;;;;;;
; pattern searching
;;;;;;;;;;;;;;;;;;;

(import "./charclass.inc")
(import "./substr.inc")
(import "./regexp.inc")

;module
(env-push)

(defq +substr (Substr 11) +regexp (Regexp 11))

(defun found? (_t _p)
	; (found? text substr) -> :t | :nil
	(. +substr :match? _t _p))

(defun substr (_t _p)
	; (substr text substr) -> matches
	(. +substr :search _t _p))

(defun match? (_t _p)
	; (match? text regexp) -> :t | :nil
	(. +regexp :match? _t _p))

(defun matches (_t _p)
	; (matches text regexp) -> matches
	(. +regexp :search _t _p))

(defun query (_p _w _r)
	; (query pattern whole_words regexp) -> (engine meta pattern)
	(if (and (nql _p "") _w (not _r))
		(setq _p (escape _p)))
	(defq _e (if (or _w _r) +regexp +substr)
		_p (if _w (cat "!" _p "!") _p))
	(list _e (. _e :compile _p) _p))

(defun replace-compile (rep_str)
	; (replace-compile rep_str) -> (p_nums z_nums c_map rep_str)
	; memoize the compilation. atomic due to cooperative scheduling.
	(memoize rep_str (progn
		; build the pattern mask vector
		(defq c_map (list) last_idx 0 z_nums (nums)
			p_nums (reduce (lambda (p_nums ((ms me) &ignore))
					; literal pattern part
					(push p_nums 0 0 last_idx ms)
					; capture group hole (0 0 0 0)
					; we record the vector offset and the group number
					; for runtime patching
					(defq group (str-to-num (slice rep_str (inc ms) me)))
					(push c_map (list (length p_nums) group))
					(setq last_idx me)
					(push p_nums 0 0 0 0))
				(matches rep_str "\$(\d+)") (nums)))
		; final literal tail
		(push p_nums 0 0 last_idx -1)
		; build the zero template vector for fast runtime cloning
		(times (length p_nums) (push z_nums 0))
		(list p_nums z_nums c_map rep_str)) 11))

(defun replace-matches (text match_lst compiled)
	; (replace-matches text matches compiled|rep_str) -> text
	(if (str? compiled) (setq compiled (replace-compile compiled)))
	(bind '(p_nums z_nums c_map rep_str) compiled)
	; pre-allocate parts list to avoid resizing
	; max items: 2 per match (intervening text + replacement) + 1 tail
	(defq parts (cap (inc (* 2 (length match_lst))) (list)) last_pos 0)
	(each (lambda (match)
		; match group 0 is always the full match bounds
		(bind '((ms me) &ignore) match)
		; preserve text between matches
		(if (> ms last_pos) (push parts (nums last_pos ms 0 0)))
		; clone the zero template
		(defq t_nums (cat z_nums))
		; patch capture group indices
		(each (lambda ((offset group_idx))
			; SAFETY: only patch if the group actually exists in this match result
			(when (< group_idx (length match))
				(bind '(gs ge) (elem-get match group_idx))
				(elem-set t_nums offset gs)
				(elem-set t_nums (inc offset) ge))) c_map)
		; merge pattern mask and text data and push to list
		(push parts (nums-add p_nums t_nums))
		(setq last_pos me)) match_lst)
	; preserve tail text
	(if (< last_pos (length text)) (push parts (nums last_pos -1)))
	; perform the atomic splice using the concatenated index vectors
	(splice text rep_str (apply (const cat) parts)))

(defun replace-edits (text match_lst compiled)
	; (replace-edits text matches compiled|rep_str) -> ((start end rep_str) ...)
	; returns a list of edit operations compatible with Editor buffers
	(if (str? compiled) (setq compiled (replace-compile compiled)))
	(bind '(p_nums z_nums c_map rep_str) compiled)
	(map (lambda (match)
		(defq t_nums (cat z_nums))
		(each (lambda ((offset group_idx))
			(when (< group_idx (length match))
				(bind '(gs ge) (elem-get match group_idx))
				(elem-set t_nums offset gs)
				(elem-set t_nums (inc offset) ge))) c_map)
		(list (first (first match)) (second (first match))
			(splice text rep_str (nums-add p_nums t_nums)))) match_lst))

(defun replace-regex-edits (text pattern compiled)
	; (replace-regex-edits text pattern compiled|rep_str) -> ((start end rep_str) ...)
	(replace-edits text (matches text pattern) compiled))

(defun replace-str-edits (text pattern compiled)
	; (replace-str-edits text pattern compiled|rep_str) -> ((start end rep_str) ...)
	(replace-edits text (substr text pattern) compiled))

(defun replace-regex (text pattern compiled)
	; (replace-regex text pattern compiled|rep_str) -> text
	(replace-matches text (matches text pattern) compiled))

(defun replace-str (text pattern compiled)
	; (replace-str text pattern compiled|rep_str) -> text
	; substr returns a list of matches, each match is a list containing just ((start end))
	; this works perfectly with $0 in replace-matches
	(replace-matches text (substr text pattern) compiled))

;module
(export-symbols '(found? match? substr matches query replace-compile
	replace-matches replace-edits replace-regex-edits replace-str-edits
	replace-regex replace-str))
(env-pop)