;REXX Interpreter Core
;
;A simplified REXX interpreter with focus on IPC via ADDRESS/PORTS.
;Supports basic REXX syntax and built-in functions.

(import "./ports.inc")

;REXX Interpreter class
(defclass RexxInterpreter () (vars port_manager rc result output control_stack loop_stack loop_control procedures call_stack signal_target)
	; (RexxInterpreter) -> interpreter

	(defmethod :init ()
		(setq vars (Fmap)
			port_manager (PortManager)
			rc 0
			result ""
			output (list)
			control_stack (list)  ; for SELECT/WHEN/OTHERWISE
			loop_stack (list)     ; for DO loops
			loop_control :nil     ; for LEAVE/ITERATE
			procedures (Fmap)     ; procedure definitions
			call_stack (list)     ; call stack for CALL/RETURN
			signal_target :nil)   ; target label for SIGNAL
		;set standard REXX variables
		(. this :setvar "RC" "0")
		(. this :setvar "RESULT" "")
		this)

	(defmethod :setvar (name value)
		; (. interp :setvar name value)
		;set variable (case-insensitive names)
		(. vars :insert (ascii-upper name) (str value)))

	(defmethod :getvar (name &optional default)
		; (. interp :getvar name [default]) -> value
		;get variable (case-insensitive names)
		(defq val (. vars :find (ascii-upper name)))
		(if val val (if default default "")))

	(defmethod :say (text)
		; (. interp :say text)
		;SAY instruction - output text
		(push output text))

	(defmethod :address (&optional port_name)
		; (. interp :address [port_name]) -> old_port
		;ADDRESS instruction - change current port
		(if port_name
			(. port_manager :address port_name)
			(. port_manager :current)))

	(defmethod :send_command (command)
		; (. interp :send_command command) -> (rc result error)
		;send command to current port
		;Special handling for ADDRESS SYSTEM - execute as ChrysaLisp code
		(defq current_addr (. port_manager :current))
		(if (= (ascii-upper current_addr) "SYSTEM")
			;ADDRESS SYSTEM - execute as ChrysaLisp Lisp code
			(. this :execute_system_command command)
			;Other addresses - send via port manager
			(. port_manager :send command)))

	(defmethod :execute_system_command (command)
		; (. interp :execute_system_command command) -> (rc result error)
		;Execute ChrysaLisp Lisp code (ADDRESS SYSTEM)
		(catch
			(progn
				;Evaluate the command as Lisp code
				(defq result (eval (read (str command))))
				;Return success with result converted to string
				(list 0 (str result) ""))
			;On error, return error code
			(list 1 "" (cat "Error executing: " (str _)))))

	(defmethod :get_output ()
		;get accumulated output
		(join output (ascii-char 10)))

	(defmethod :clear_output ()
		;clear accumulated output
		(setq output (list)))

	;Built-in REXX functions

	(defmethod :substr (string start &optional length)
		; (. interp :substr string start [length]) -> substring
		;SUBSTR(string, start [,length]) - extract substring (1-indexed)
		(defq s (str string)
			st (max 0 (dec (int start))))  ; convert to 0-indexed
		(if length
			(slice s st (min (length s) (+ st (int length))))
			(slice s st -1)))

	(defmethod :left (string length &optional pad)
		; (. interp :left string length [pad]) -> string
		;LEFT(string, length [,pad]) - left-justify
		(defq s (str string) l (int length) p (if pad pad " "))
		(if (>= (length s) l)
			(slice s 0 l)
			(cat s (apply cat (map (lambda (_) p) (range (- l (length s))))))))

	(defmethod :right (string length &optional pad)
		; (. interp :right string length [pad]) -> string
		;RIGHT(string, length [,pad]) - right-justify
		(defq s (str string) l (int length) p (if pad pad " "))
		(if (>= (length s) l)
			(slice s (- (length s) l) -1)
			(cat (apply cat (map (lambda (_) p) (range (- l (length s))))) s)))

	(defmethod :length (string)
		; (. interp :length string) -> length
		;LENGTH(string) - string length
		(length (str string)))

	(defmethod :word (string n)
		; (. interp :word string n) -> word
		;WORD(string, n) - extract nth word (1-indexed)
		(defq words (split (str string) " "))
		(if (and (>= n 1) (<= n (length words)))
			(elem-get words (dec n))
			""))

	(defmethod :words (string)
		; (. interp :words string) -> count
		;WORDS(string) - count words
		(length (split (str string) " ")))

	(defmethod :wordpos (phrase string &optional start)
		; (. interp :wordpos phrase string [start]) -> position
		;WORDPOS(phrase, string [,start]) - find word position of phrase in string
		;Returns word position (1-indexed) or 0 if not found
		(defq ph (str phrase)
			s (str string)
			words (split s " ")
			start_idx (if start (dec (int start)) 0))
		(defq idx start_idx)
		(each! start_idx -1 (lambda (word)
			(when (= (ascii-upper word) (ascii-upper ph))
				(return (inc idx)))
			(setq idx (inc idx)))
			words)
		0)

	(defmethod :delword (string n &optional length)
		; (. interp :delword string n [length]) -> string
		;DELWORD(string, n [,length]) - delete words starting at position n
		;n is 1-indexed, length defaults to deleting all remaining words
		(defq s (str string)
			words (split s " ")
			start_pos (dec (int n))
			del_count (if length (int length) (- (length words) start_pos)))
		(when (and (>= start_pos 0) (< start_pos (length words)))
			;build new word list without deleted words
			(defq before (if (> start_pos 0) (slice words 0 start_pos) (list))
				after_pos (+ start_pos del_count)
				after (if (< after_pos (length words)) (slice words after_pos -1) (list)))
			(setq words (append before after)))
		(join words " "))

	(defmethod :subword (string n &optional length)
		; (. interp :subword string n [length]) -> string
		;SUBWORD(string, n [,length]) - extract substring of words
		;n is 1-indexed, length defaults to all remaining words
		(defq s (str string)
			words (split s " ")
			start_pos (dec (int n))
			word_count (if length (int length) (- (length words) start_pos)))
		(if (and (>= start_pos 0) (< start_pos (length words)))
			(progn
				(defq end_pos (min (length words) (+ start_pos word_count))
					result_words (slice words start_pos end_pos))
				(join result_words " "))
			""))

	(defmethod :abbrev (information info &optional min_length)
		; (. interp :abbrev information info [length]) -> 1 or 0
		;ABBREV(information, info [,length]) - test if info is valid abbreviation
		;Returns 1 if info is abbreviation of information (with minimum length)
		(defq full (str information)
			abbr (str info)
			min_len (if min_length (int min_length) (length abbr))
			abbr_len (length abbr)
			full_len (length full))
		(if (and (>= abbr_len min_len)
				(<= abbr_len full_len)
				(= (ascii-upper (slice full 0 abbr_len)) (ascii-upper abbr)))
			1
			0))

	(defmethod :strip (string &optional option char)
		; (. interp :strip string [option] [char]) -> string
		;STRIP(string [,option] [,char]) - strip leading/trailing chars
		(defq s (str string)
			opt (if option (ascii-upper option) "B")
			c (if char char " "))
		(cond
			((= opt "L") (trim-start s c))
			((= opt "T") (trim-end s c))
			(:t (trim s c))))

	(defmethod :upper (string)
		; (. interp :upper string) -> string
		;UPPER(string) - convert to uppercase
		(ascii-upper (str string)))

	(defmethod :lower (string)
		; (. interp :lower string) -> string
		;LOWER(string) - convert to lowercase
		(ascii-lower (str string)))

	(defmethod :reverse (string)
		; (. interp :reverse string) -> string
		;REVERSE(string) - reverse string
		(apply cat (reverse (map (lambda (c) (ascii-char c)) (str string)))))

	(defmethod :pos (needle haystack &optional start)
		; (. interp :pos needle haystack [start]) -> position
		;POS(needle, haystack [,start]) - find position (1-indexed, 0 if not found)
		(defq h (str haystack) n (str needle)
			st (if start (dec (int start)) 0))
		(if (defq p (find n h st))
			(inc p)
			0))

	;Additional string functions (RexxJS-inspired)

	(defmethod :trim_start (string)
		; (. interp :trim_start string) -> string
		;TRIM_START(string) - remove leading whitespace
		(trim-start (str string)))

	(defmethod :trim_end (string)
		; (. interp :trim_end string) -> string
		;TRIM_END(string) - remove trailing whitespace
		(trim-end (str string)))

	(defmethod :proper (string)
		; (. interp :proper string) -> string
		;PROPER(string) - convert to title case (capitalize first letter of each word)
		(defq s (str string)
			words (split s " ")
			result (list))
		(each! 0 -1 (lambda (word)
			(if (> (length word) 0)
				(push result (cat
					(ascii-upper (slice word 0 1))
					(ascii-lower (slice word 1 -1))))
				(push result word)))
			words)
		(join result " "))

	(defmethod :pad_start (string length &optional pad_char)
		; (. interp :pad_start string length [pad_char]) -> string
		;PAD_START(string, length [,pad_char]) - pad beginning to length
		(defq s (str string)
			l (int length)
			p (if pad_char (str pad_char) " ")
			needed (- l (length s)))
		(if (> needed 0)
			(cat (apply cat (map (lambda (_) p) (range needed))) s)
			s))

	(defmethod :pad_end (string length &optional pad_char)
		; (. interp :pad_end string length [pad_char]) -> string
		;PAD_END(string, length [,pad_char]) - pad end to length
		(defq s (str string)
			l (int length)
			p (if pad_char (str pad_char) " ")
			needed (- l (length s)))
		(if (> needed 0)
			(cat s (apply cat (map (lambda (_) p) (range needed))))
			s))

	(defmethod :copies (string count)
		; (. interp :copies string count) -> string
		;COPIES(string, count) - repeat string count times
		(defq s (str string) n (int count))
		(apply cat (map (lambda (_) s) (range n))))

	;Validation functions (RexxJS-inspired)

	(defmethod :is_alpha (string)
		; (. interp :is_alpha string) -> 1 | 0
		;IS_ALPHA(string) - test if contains only letters
		(defq s (str string))
		(if (= (length s) 0)
			0
			(if (every (lambda (ch)
				(or (and (>= ch (ascii-code "a")) (<= ch (ascii-code "z")))
					(and (>= ch (ascii-code "A")) (<= ch (ascii-code "Z")))))
				(map (lambda (c) c) s))
				1
				0)))

	(defmethod :is_numeric (string)
		; (. interp :is_numeric string) -> 1 | 0
		;IS_NUMERIC(string) - test if contains only digits
		(defq s (str string))
		(if (= (length s) 0)
			0
			(if (every (lambda (ch)
				(and (>= ch (ascii-code "0")) (<= ch (ascii-code "9"))))
				(map (lambda (c) c) s))
				1
				0)))

	(defmethod :is_alphanumeric (string)
		; (. interp :is_alphanumeric string) -> 1 | 0
		;IS_ALPHANUMERIC(string) - test if contains only letters and digits
		(defq s (str string))
		(if (= (length s) 0)
			0
			(if (every (lambda (ch)
				(or (and (>= ch (ascii-code "a")) (<= ch (ascii-code "z")))
					(and (>= ch (ascii-code "A")) (<= ch (ascii-code "Z")))
					(and (>= ch (ascii-code "0")) (<= ch (ascii-code "9")))))
				(map (lambda (c) c) s))
				1
				0)))

	(defmethod :is_empty (string)
		; (. interp :is_empty string) -> 1 | 0
		;IS_EMPTY(string) - test if string is empty or only whitespace
		(if (= (length (trim (str string))) 0) 1 0))

	(defmethod :starts_with (string prefix)
		; (. interp :starts_with string prefix) -> 1 | 0
		;STARTS_WITH(string, prefix) - test if string starts with prefix
		(defq s (str string) p (str prefix))
		(if (and (>= (length s) (length p))
				(= (slice s 0 (length p)) p))
			1
			0))

	(defmethod :ends_with (string suffix)
		; (. interp :ends_with string suffix) -> 1 | 0
		;ENDS_WITH(string, suffix) - test if string ends with suffix
		(defq s (str string) suf (str suffix) suf_len (length suf))
		(if (and (>= (length s) suf_len)
				(= (slice s (- (length s) suf_len) -1) suf))
			1
			0))

	(defmethod :includes (string substring)
		; (. interp :includes string substring) -> 1 | 0
		;INCLUDES(string, substring) - test if string contains substring
		(if (find (str substring) (str string)) 1 0))

	;Math functions (modern RexxJS-inspired)

	(defmethod :abs_value (number)
		; (. interp :abs_value number) -> absolute_value
		;ABS(number) - absolute value
		(defq n (int (str number)))
		(if (< n 0) (- 0 n) n))

	(defmethod :min_value (&rest numbers)
		; (. interp :min_value num1 num2 ...) -> minimum
		;MIN(num1, num2, ...) - minimum value
		(defq nums (map (lambda (n) (int (str n))) numbers))
		(if (empty? nums)
			0
			(apply min nums)))

	(defmethod :max_value (&rest numbers)
		; (. interp :max_value num1 num2 ...) -> maximum
		;MAX(num1, num2, ...) - maximum value
		(defq nums (map (lambda (n) (int (str n))) numbers))
		(if (empty? nums)
			0
			(apply max nums)))

	(defmethod :sign_value (number)
		; (. interp :sign_value number) -> -1 | 0 | 1
		;SIGN(number) - sign of number
		(defq n (int (str number)))
		(cond
			((< n 0) -1)
			((> n 0) 1)
			(:t 0)))

	(defmethod :random_value (&optional min max)
		; (. interp :random_value [min] [max]) -> random_number
		;RANDOM([min], [max]) - random number
		(defq low (if min (int (str min)) 0)
			high (if max (int (str max)) 100))
		(+ low (mod (time) (inc (- high low)))))

	(defmethod :time_value ()
		; (. interp :time_value) -> timestamp
		;TIME() - current time as string
		(str (time)))

	(defmethod :date_value ()
		; (. interp :date_value) -> date_string
		;DATE() - current date as string (simplified)
		(cat "2025-11-18"))  ;simplified - would need actual date

	;Extended math functions (RexxJS compatibility)

	(defmethod :math_ceil (number)
		; (. interp :math_ceil number) -> ceiling
		;MATH_CEIL(number) - smallest integer >= number
		(defq n (int (str number)))
		;for integers, already ceiling
		n)

	(defmethod :math_floor (number)
		; (. interp :math_floor number) -> floor
		;MATH_FLOOR(number) - largest integer <= number
		(defq n (int (str number)))
		;int() effectively floors for positive, need to adjust for negative
		n)

	(defmethod :math_round (number)
		; (. interp :math_round number) -> rounded
		;MATH_ROUND(number) - round to nearest integer
		(defq n (str number))
		;check if has decimal point
		(if (find "." n)
			(progn
				(defq parts (split n ".")
					int_part (int (first parts))
					dec_part (if (> (length parts) 1) (second parts) "0")
					first_decimal (if (> (length dec_part) 0)
						(int (slice dec_part 0 1))
						0))
				(if (>= first_decimal 5)
					(inc int_part)
					int_part))
			(int n)))

	(defmethod :math_sqrt (number)
		; (. interp :math_sqrt number) -> square_root
		;MATH_SQRT(number) - square root (Newton's method approximation)
		(defq n (int (str number)))
		(if (<= n 0)
			0
			(progn
				;Newton's method: x_new = (x + n/x) / 2
				(defq x n)
				;iterate for better approximation
				(setq x (/ (+ x (/ n x)) 2))
				(setq x (/ (+ x (/ n x)) 2))
				(setq x (/ (+ x (/ n x)) 2))
				(setq x (/ (+ x (/ n x)) 2))
				(setq x (/ (+ x (/ n x)) 2))
				x)))

	;Advanced string functions (RexxJS-inspired)

	(defmethod :center (string length &optional pad)
		; (. interp :center string length [pad]) -> centered_string
		;CENTER(string, length [,pad]) - center string in field
		(defq s (str string) l (int length) p (if pad (str pad) " ")
			s_len (length s))
		(if (>= s_len l)
			(slice s 0 l)
			(progn
				(defq total_pad (- l s_len)
					left_pad (/ total_pad 2)
					right_pad (- total_pad left_pad))
				(cat
					(apply cat (map (lambda (_) p) (range left_pad)))
					s
					(apply cat (map (lambda (_) p) (range right_pad)))))))

	(defmethod :datatype (string &optional type)
		; (. interp :datatype string [type]) -> result
		;DATATYPE(string [,type]) - check data type
		(defq s (str string))
		(if type
			;check specific type
			(defq t (ascii-upper (str type)))
			(cond
				((= t "NUM")
					(if (. this :is_numeric s) 1 0))
				((= t "ALPHANUM")
					(if (. this :is_alphanumeric s) 1 0))
				((= t "ALPHA")
					(if (. this :is_alpha s) 1 0))
				(:t 0))
			;return type name
			(cond
				((. this :is_numeric s) "NUM")
				((. this :is_alpha s) "CHAR")
				(:t "MIXED"))))

	(defmethod :index (haystack needle &optional start)
		; (. interp :index haystack needle [start]) -> position
		;INDEX(haystack, needle [,start]) - find position (same as POS)
		(. this :pos needle haystack start))

	(defmethod :changestr (old new string)
		; (. interp :changestr old new string) -> modified_string
		;CHANGESTR(old, new, string) - replace all occurrences
		(defq s (str string) o (str old) n (str new))
		(replace s o n))

	(defmethod :countstr (needle haystack)
		; (. interp :countstr needle haystack) -> count
		;COUNTSTR(needle, haystack) - count occurrences
		(defq h (str haystack) n (str needle) count 0 pos 0)
		(while (defq found (find n h pos))
			(setq count (inc count)
				pos (+ found (length n))))
		count)

	(defmethod :space (string &optional n pad)
		; (. interp :space string [n] [pad]) -> spaced_string
		;SPACE(string [,n] [,pad]) - normalize spacing
		(defq s (str string)
			words (split s " ")
			spacing (if n (int n) 1)
			p (if pad (str pad) " "))
		;remove empty strings
		(setq words (filter (lambda (w) (nempty? w)) words))
		(join words (apply cat (map (lambda (_) p) (range spacing)))))

	(defmethod :delstr (string pos &optional length)
		; (. interp :delstr string pos [length]) -> modified_string
		;DELSTR(string, pos [,length]) - delete substring
		(defq s (str string)
			start_pos (max 0 (dec (int pos)))
			del_len (if length (int length) (- (length s) start_pos)))
		(cat
			(slice s 0 start_pos)
			(slice s (+ start_pos del_len) -1)))

	(defmethod :insert (new target &optional pos length pad)
		; (. interp :insert new target [pos] [length] [pad]) -> result
		;INSERT(new, target [,pos] [,length] [,pad]) - insert string
		(defq n (str new) t (str target)
			insert_pos (if pos (int pos) 0)
			insert_len (if length (int length) (length n))
			p (if pad (str pad) " "))
		;truncate or pad new string
		(if (> insert_len (length n))
			(setq n (cat n (apply cat (map (lambda (_) p) (range (- insert_len (length n)))))))
			(setq n (slice n 0 insert_len)))
		(cat
			(slice t 0 insert_pos)
			n
			(slice t insert_pos -1)))

	(defmethod :overlay (new target &optional pos length pad)
		; (. interp :overlay new target [pos] [length] [pad]) -> result
		;OVERLAY(new, target [,pos] [,length] [,pad]) - overlay string
		(defq n (str new) t (str target)
			over_pos (if pos (max 0 (dec (int pos))) 0)
			over_len (if length (int length) (length n))
			p (if pad (str pad) " "))
		;ensure target is long enough
		(when (< (length t) over_pos)
			(setq t (cat t (apply cat (map (lambda (_) p) (range (- over_pos (length t))))))))
		;overlay
		(cat
			(slice t 0 over_pos)
			(slice n 0 over_len)
			(slice t (+ over_pos over_len) -1)))

	(defmethod :translate (string &optional tableo tablei pad)
		; (. interp :translate string [tableo] [tablei] [pad]) -> translated
		;TRANSLATE(string [,tableo] [,tablei] [,pad]) - character translation
		(defq s (str string)
			out_table (if tableo (str tableo) "")
			in_table (if tablei (str tablei) "")
			p (if pad (str pad) " "))
		;if no tables, convert to uppercase
		(if (and (empty? out_table) (empty? in_table))
			(ascii-upper s)
			;character-by-character translation
			(progn
				(defq result "")
				(each! 0 -1 (lambda (ch)
					(defq ch_str (ascii-char ch)
						in_pos (find ch_str in_table))
					(if in_pos
						(if (< in_pos (length out_table))
							(setq result (cat result (slice out_table in_pos (inc in_pos))))
							(setq result (cat result p)))
						(setq result (cat result ch_str))))
					s)
				result)))

	(defmethod :verify (string ref &optional option start)
		; (. interp :verify string ref [option] [start]) -> position
		;VERIFY(string, ref [,option] [,start]) - verify string contains only chars from ref
		;Returns position of first char NOT in ref (or 0 if all valid)
		;Option: "NOMATCH" returns position of first char IN ref
		(defq s (str string)
			reference (str ref)
			opt (if option (ascii-upper (str option)) "MATCH")
			start_pos (if start (dec (int start)) 0)
			nomatch (= opt "NOMATCH"))
		(defq pos start_pos)
		(each! start_pos -1 (lambda (ch)
			(defq ch_str (ascii-char ch)
				in_ref (find ch_str reference))
			(when (if nomatch in_ref (not in_ref))
				(return (inc pos)))
			(setq pos (inc pos)))
			s)
		0)  ;all chars verified

	(defmethod :compare (string1 string2 &optional pad)
		; (. interp :compare string1 string2 [pad]) -> position
		;COMPARE(string1, string2 [,pad]) - return position of first difference (0 if equal)
		(defq s1 (str string1)
			s2 (str string2)
			p (if pad (str pad) " ")
			len1 (length s1)
			len2 (length s2)
			max_len (max len1 len2))
		;pad shorter string
		(when (< len1 max_len)
			(setq s1 (cat s1 (apply cat (map (lambda (_) p) (range (- max_len len1)))))))
		(when (< len2 max_len)
			(setq s2 (cat s2 (apply cat (map (lambda (_) p) (range (- max_len len2)))))))
		;compare character by character
		(defq pos 0)
		(each! 0 -1 (lambda (idx)
			(when (not (= (slice s1 idx (inc idx)) (slice s2 idx (inc idx))))
				(return (inc idx)))
			(setq pos idx))
			(range max_len))
		0)  ;strings are equal

	(defmethod :copies (string n)
		; (. interp :copies string n) -> repeated
		;COPIES(string, n) - create n copies of string
		(defq s (str string)
			count (int n)
			result "")
		(each! 0 -1 (lambda (_)
			(setq result (cat result s)))
			(range count))
		result)

	;Conversion functions

	(defmethod :c2x (string)
		; (. interp :c2x string) -> hex
		;C2X(string) - convert character string to hex
		(defq s (str string)
			result "")
		(each! 0 -1 (lambda (ch)
			(defq hex_val (num-str ch 16))
			;pad to 2 digits
			(when (< (length hex_val) 2)
				(setq hex_val (cat "0" hex_val)))
			(setq result (cat result (ascii-upper hex_val))))
			s)
		result)

	(defmethod :x2c (hex_string)
		; (. interp :x2c hex_string) -> string
		;X2C(hex_string) - convert hex to character string
		(defq hex (str hex_string)
			result "")
		;process pairs of hex digits
		(defq pos 0)
		(while (< pos (length hex))
			(defq pair (slice hex pos (min (length hex) (+ pos 2)))
				val (int pair 16))
			(setq result (cat result (ascii-char val))
				pos (+ pos 2)))
		result)

	(defmethod :x2d (hex_string)
		; (. interp :x2d hex_string) -> decimal
		;X2D(hex_string) - convert hex to decimal
		(str (int (str hex_string) 16)))

	(defmethod :d2x (decimal &optional length)
		; (. interp :d2x decimal [length]) -> hex
		;D2X(decimal [,length]) - convert decimal to hex
		(defq num (int decimal)
			hex (ascii-upper (num-str num 16)))
		(if length
			(progn
				(defq len (int length))
				;pad with zeros
				(while (< (length hex) len)
					(setq hex (cat "0" hex)))
				hex)
			hex))

	(defmethod :x2b (hex_string)
		; (. interp :x2b hex_string) -> binary
		;X2B(hex_string) - convert hex to binary string
		(defq hex (str hex_string)
			result "")
		(each! 0 -1 (lambda (ch)
			(defq hex_char (ascii-char ch)
				val (int hex_char 16)
				binary (num-str val 2))
			;pad to 4 bits
			(while (< (length binary) 4)
				(setq binary (cat "0" binary)))
			(setq result (cat result binary)))
			hex)
		result)

	(defmethod :b2x (binary_string)
		; (. interp :b2x binary_string) -> hex
		;B2X(binary_string) - convert binary string to hex
		(defq bin (str binary_string)
			result "")
		;pad to multiple of 4
		(while (not (= 0 (% (length bin) 4)))
			(setq bin (cat "0" bin)))
		;process groups of 4 bits
		(defq pos 0)
		(while (< pos (length bin))
			(defq nibble (slice bin pos (+ pos 4))
				val (int nibble 2)
				hex_char (num-str val 16))
			(setq result (cat result (ascii-upper hex_char))
				pos (+ pos 4)))
		result)

	;Array operations (modern RexxJS-inspired)

	(defmethod :array_create (name size &optional initial)
		; (. interp :array_create name size [initial]) -> size
		;ARRAY(name, size [,initial]) - create array with stem variables
		(defq n (int size) init (if initial (str initial) ""))
		(each! 0 -1 (lambda (i)
			(. this :setvar (cat name "." (str (inc i))) init))
			(range n))
		(. this :setvar (cat name ".0") (str n))
		n)

	(defmethod :array_sort (name)
		; (. interp :array_sort name) -> count
		;SORT(name) - sort array in place
		(defq count (int (. this :getvar (cat name ".0") "0"))
			elements (list))
		;collect elements
		(each! 0 -1 (lambda (i)
			(push elements (. this :getvar (cat name "." (str (inc i))) "")))
			(range count))
		;sort (simple lexicographic sort)
		(setq elements (sort (lambda (a b) (cmp a b)) elements))
		;write back
		(each! 0 -1 (lambda (i)
			(. this :setvar (cat name "." (str (inc i))) (elem-get elements i)))
			(range count))
		count)

	(defmethod :array_reverse (name)
		; (. interp :array_reverse name) -> count
		;REVERSE_ARRAY(name) - reverse array in place
		(defq count (int (. this :getvar (cat name ".0") "0"))
			elements (list))
		;collect elements
		(each! 0 -1 (lambda (i)
			(push elements (. this :getvar (cat name "." (str (inc i))) "")))
			(range count))
		;reverse
		(setq elements (reverse elements))
		;write back
		(each! 0 -1 (lambda (i)
			(. this :setvar (cat name "." (str (inc i))) (elem-get elements i)))
			(range count))
		count)

	(defmethod :array_push (name value)
		; (. interp :array_push name value) -> new_count
		;PUSH(name, value) - append value to array
		(defq count (int (. this :getvar (cat name ".0") "0"))
			new_count (inc count))
		(. this :setvar (cat name "." (str new_count)) (str value))
		(. this :setvar (cat name ".0") (str new_count))
		new_count)

	(defmethod :array_pop (name)
		; (. interp :array_pop name) -> value
		;POP(name) - remove and return last element
		(defq count (int (. this :getvar (cat name ".0") "0")))
		(if (> count 0)
			(progn
				(defq value (. this :getvar (cat name "." (str count)) ""))
				(. this :setvar (cat name "." (str count)) "")
				(. this :setvar (cat name ".0") (str (dec count)))
				value)
			""))

	(defmethod :array_join (name delimiter)
		; (. interp :array_join name delimiter) -> string
		;JOIN(name, delimiter) - join array elements with delimiter
		(defq count (int (. this :getvar (cat name ".0") "0"))
			elements (list))
		(each! 0 -1 (lambda (i)
			(push elements (. this :getvar (cat name "." (str (inc i))) "")))
			(range count))
		(join elements (str delimiter)))

	;JSON operations (modern RexxJS-inspired)

	(defmethod :json_stringify (name)
		; (. interp :json_stringify name) -> json_string
		;JSON_STRINGIFY(name) - convert array to JSON string (simple implementation)
		(defq count (int (. this :getvar (cat name ".0") "0"))
			elements (list))
		(each! 0 -1 (lambda (i)
			(defq val (. this :getvar (cat name "." (str (inc i))) ""))
			(push elements (cat "\"" val "\"")))
			(range count))
		(cat "[" (join elements ",") "]"))

	(defmethod :json_parse (json_str name)
		; (. interp :json_parse json_str name) -> count
		;JSON_PARSE(json_str, name) - parse simple JSON array into stem variables
		;simplified: only supports arrays of strings
		(defq s (trim (str json_str)))
		(when (and (= (slice s 0 1) "[") (= (slice s -2 -1) "]"))
			(defq content (slice s 1 -2)
				parts (split content ",")
				count 0)
			(each! 0 -1 (lambda (part)
				(defq val (trim part))
				;remove quotes if present
				(when (and (> (length val) 1)
						(= (slice val 0 1) "\"")
						(= (slice val -2 -1) "\""))
					(setq val (slice val 1 -2)))
				(setq count (inc count))
				(. this :setvar (cat name "." (str count)) val))
				parts)
			(. this :setvar (cat name ".0") (str count))
			count))

	;Data transformation functions (RexxJS)

	(defmethod :csv_to_json (csv_str)
		; (. interp :csv_to_json csv_str) -> json_string
		;CSV_TO_JSON(csv_str) - convert CSV to JSON array (simplified)
		(defq lines (split (str csv_str) (ascii-char 10))
			result "[")
		(defq first_line :t)
		(each! 0 -1 (lambda (line)
			(defq trimmed (trim line))
			(when (nempty? trimmed)
				(if first_line
					(setq first_line :nil)
					(setq result (cat result ",")))
				(setq result (cat result "\"" trimmed "\""))))
			lines)
		(cat result "]"))

	(defmethod :json_to_csv (json_str)
		; (. interp :json_to_csv json_str) -> csv_string
		;JSON_TO_CSV(json_str) - convert JSON array to CSV (simplified)
		(defq s (trim (str json_str)))
		(when (and (= (slice s 0 1) "[") (= (slice s -2 -1) "]"))
			(defq content (slice s 1 -2)
				parts (split content ",")
				result "")
			(each! 0 -1 (lambda (part)
				(defq val (trim part))
				;remove quotes if present
				(when (and (> (length val) 1)
						(= (slice val 0 1) "\"")
						(= (slice val -2 -1) "\""))
					(setq val (slice val 1 -2)))
				(setq result (cat result (if (nempty? result) (cat (ascii-char 10) val) val))))
				parts)
			result))

	;Additional string functions (RexxJS)

	(defmethod :slug (string)
		; (. interp :slug string) -> slug_string
		;SLUG(string) - convert string to URL-friendly slug
		(defq s (ascii-lower (str string))
			result "")
		(each! 0 -1 (lambda (ch)
			(cond
				;alphanumeric - keep
				((or (and (>= ch (ascii-code "a")) (<= ch (ascii-code "z")))
					(and (>= ch (ascii-code "0")) (<= ch (ascii-code "9"))))
					(setq result (cat result (ascii-char ch))))
				;space or special - convert to dash
				(:t
					(when (and (nempty? result) (not (= (last result) (ascii-code "-"))))
						(setq result (cat result "-"))))))
			s)
		;remove trailing dash
		(if (and (nempty? result) (= (last result) (ascii-code "-")))
			(slice result 0 -2)
			result))

	(defmethod :repeat (string n)
		; (. interp :repeat string n) -> repeated_string
		;REPEAT(string, n) - repeat string n times (alias for COPIES)
		(. this :copies string n))

	(defmethod :indexof (haystack needle &optional start)
		; (. interp :indexof haystack needle [start]) -> position
		;INDEXOF(haystack, needle [,start]) - find position (0-indexed, RexxJS style)
		(defq h (str haystack)
			n (str needle)
			st (if start (int start) 0)
			pos (find n h st))
		(if pos pos -1))

	;Parse REXX command line
	(defmethod :parse_line (line)
		; (. interp :parse_line line) -> (list cmd args)
		;parse a line into command and arguments
		(defq trimmed (trim line)
			parts (split trimmed " "))
		(if (empty? parts)
			(list "" "")
			(list (ascii-upper (first parts))
				(if (> (length parts) 1)
					(slice trimmed (+ (find (first parts) trimmed) (length (first parts))) -1)
					""))))

	;Execute REXX statement
	(defmethod :execute_statement (statement)
		; (. interp :execute_statement statement) -> rc
		;execute a single REXX statement
		(defq statement (trim statement))
		;handle C-style // comments (modern feature)
		(when (defq comment_pos (find "//" statement))
			(setq statement (trim (slice statement 0 comment_pos))))
		;skip if empty or traditional ; comment
		(when (and (nempty? statement) (not (= (first statement) ";")))
			(bind '(cmd args) (. this :parse_line statement))
			(cond
				;LET instruction (RexxJS compatibility) - alias for variable assignment
				((= cmd "LET")
					(when (. this :should_execute)
						(. this :parse_assignment args))
					(setq rc 0))

				;IF instruction
				((= cmd "IF")
					(. this :handle_if args)
					(setq rc 0))

				;THEN instruction (multi-line IF)
				((= cmd "THEN")
					(when (and (nempty? control_stack) (= (first (elem-get control_stack -1)) 'if))
						(defq if_state (elem-get control_stack -1)
							condition_result (second if_state))
						(if condition_result
							;condition was true - execute THEN block
							(elem-set control_stack -1 (list 'if condition_result :then))
							;condition was false - skip THEN block
							(elem-set control_stack -1 (list 'if condition_result :skip_then))))
					(setq rc 0))

				;ELSE instruction
				((= cmd "ELSE")
					(when (and (nempty? control_stack) (= (first (elem-get control_stack -1)) 'if))
						(defq if_state (elem-get control_stack -1)
							condition_result (second if_state))
						(if condition_result
							;condition was true - skip ELSE block
							(elem-set control_stack -1 (list 'if condition_result :skip_else))
							;condition was false - execute ELSE block
							(elem-set control_stack -1 (list 'if condition_result :else))))
					(setq rc 0))

				;SELECT instruction
				((= cmd "SELECT")
					(push control_stack (list 'select :searching :nil))
					(setq rc 0))

				;WHEN instruction
				((= cmd "WHEN")
					(if (and (nempty? control_stack) (= (first (elem-get control_stack -1)) 'select))
						(progn
							(defq select_state (elem-get control_stack -1)
								state_mode (second select_state))
							(cond
								;still searching for true condition
								((= state_mode :searching)
									(if (. this :eval_condition args)
										;condition is true - execute and switch to :done mode
										(progn
											(elem-set control_stack -1 (list 'select :done :nil))
											(setq rc 0))
										;condition false - stay in :searching mode
										(setq rc 0)))
								;already found true condition - skip
								((= state_mode :done)
									(setq rc 0))
								;in otherwise - skip
								(:t (setq rc 0))))
						;WHEN outside SELECT - error
						(. this :say "Error: WHEN without SELECT"))
					(setq rc 0))

				;OTHERWISE instruction
				((= cmd "OTHERWISE")
					(if (and (nempty? control_stack) (= (first (elem-get control_stack -1)) 'select))
						(progn
							(defq select_state (elem-get control_stack -1)
								state_mode (second select_state))
							(if (= state_mode :searching)
								;no WHEN matched - execute OTHERWISE
								(elem-set control_stack -1 (list 'select :otherwise :nil))
								;already found match - skip OTHERWISE
								(elem-set control_stack -1 (list 'select :skip_otherwise :nil))))
						;OTHERWISE outside SELECT - error
						(. this :say "Error: OTHERWISE without SELECT"))
					(setq rc 0))

				;END instruction
				((= cmd "END")
					(if (nempty? control_stack)
						(progn
							(defq block_type (first (elem-get control_stack -1)))
							(cond
								((= block_type 'if)
									(pop control_stack)
									(setq rc 0))
								((= block_type 'select)
									(pop control_stack)
									(setq rc 0))
								((= block_type 'do)
									(. this :handle_do_end)
									(setq rc 0))
								(:t
									(pop control_stack)
									(setq rc 0))))
						;END without block - error
						(. this :say "Error: END without matching block"))
					(setq rc 0))

				;DO instruction
				((= cmd "DO")
					(. this :handle_do_start args)
					(setq rc 0))

				;LEAVE instruction
				((= cmd "LEAVE")
					(when (. this :should_execute)
						(when (nempty? loop_stack)
							;set loop control to exit current loop
							(setq loop_control 'leave)))
					(setq rc 0))

				;ITERATE instruction
				((= cmd "ITERATE")
					(when (. this :should_execute)
						(when (nempty? loop_stack)
							;set loop control to skip to next iteration
							(setq loop_control 'iterate)))
					(setq rc 0))

				;CALL instruction
				((= cmd "CALL")
					(when (. this :should_execute)
						(. this :handle_call args))
					(setq rc 0))

				;PROCEDURE instruction
				((= cmd "PROCEDURE")
					(when (. this :should_execute)
						(. this :handle_procedure args))
					(setq rc 0))

				;ARG instruction
				((= cmd "ARG")
					(when (. this :should_execute)
						(. this :handle_arg args))
					(setq rc 0))

				;INTERPRET instruction
				((= cmd "INTERPRET")
					(when (. this :should_execute)
						(. this :handle_interpret args))
					(setq rc 0))

				;SIGNAL instruction
				((= cmd "SIGNAL")
					(when (. this :should_execute)
						(. this :handle_signal args))
					(setq rc 0))

				;SAY instruction
				((= cmd "SAY")
					(when (. this :should_execute)
						(. this :say (. this :expand_vars args)))
					(setq rc 0))

				;ADDRESS instruction
				((= cmd "ADDRESS")
					(when (. this :should_execute)
						(. this :address (if (nempty? args) (. this :expand_vars args) :nil)))
					(setq rc 0))

				;PARSE instruction (simplified)
				((= cmd "PARSE")
					(when (. this :should_execute)
						(. this :parse_instruction args))
					(setq rc 0))

				;EXIT instruction
				((= cmd "EXIT")
					(when (. this :should_execute)
						(setq rc (if (nempty? args) (int (. this :expand_vars args)) 0)))
					rc)

				;RETURN instruction
				((= cmd "RETURN")
					(when (. this :should_execute)
						(setq result (. this :expand_vars args))
						(. this :setvar "RESULT" result)
						;if in a procedure call, mark for return
						(when (nempty? call_stack)
							(setq loop_control 'return)))
					(setq rc 0))

				;Variable assignment (VAR = value)
				((and (> (length statement) 2) (find "=" statement))
					(when (. this :should_execute)
						(. this :parse_assignment statement))
					(setq rc 0))

				;Command to current ADDRESS
				(:t
					(when (. this :should_execute)
						;send to current port
						(bind '(cmd_rc cmd_result cmd_error) (. this :send_command statement))
						(setq rc cmd_rc result cmd_result)
						(. this :setvar "RC" (str rc))
						(. this :setvar "RESULT" result)
						(when (nempty? cmd_error)
							(. this :say (cat "Error: " cmd_error))))
					rc)))
		0)

	;Parse assignment statement
	(defmethod :parse_assignment (statement)
		; (. interp :parse_assignment statement)
		;parse VAR = value assignment
		(defq parts (split statement "=" 2))
		(when (>= (length parts) 2)
			(defq var_name (trim (first parts))
				var_value (. this :expand_vars (trim (second parts))))
			(. this :setvar var_name var_value)))

	;Process escape sequences in string
	(defmethod :process_escapes (text)
		; (. interp :process_escapes text) -> text_with_escapes_processed
		;process escape sequences like \n, \t, \\, \", etc.
		(defq result "" i 0 len (length text))
		(while (< i len)
			(defq ch (elem-get text i))
			(if (= ch (ascii-code "\\"))
				;escape sequence
				(if (< (inc i) len)
					(progn
						(defq next_ch (elem-get text i))
						(cond
							((= next_ch (ascii-code "n"))
								;newline
								(setq result (cat result (ascii-char 10)))
								(setq i (inc i)))
							((= next_ch (ascii-code "t"))
								;tab
								(setq result (cat result (ascii-char 9)))
								(setq i (inc i)))
							((= next_ch (ascii-code "r"))
								;carriage return
								(setq result (cat result (ascii-char 13)))
								(setq i (inc i)))
							((= next_ch (ascii-code "\\"))
								;backslash
								(setq result (cat result "\\"))
								(setq i (inc i)))
							((= next_ch (ascii-code "\""))
								;quote
								(setq result (cat result "\""))
								(setq i (inc i)))
							(:t
								;unknown escape - keep backslash
								(setq result (cat result "\\" (ascii-char next_ch)))
								(setq i (inc i)))))
					;backslash at end - keep it
					(setq result (cat result "\\")))
				;normal character
				(progn
					(setq result (cat result (ascii-char ch)))
					(setq i (inc i)))))
		result)

	;Process string interpolation {variable}
	(defmethod :interpolate (text)
		; (. interp :interpolate text) -> text_with_interpolation
		;process {variable} interpolation in strings (modern RexxJS-style)
		(defq result "" i 0 len (length text))
		(while (< i len)
			(defq ch (elem-get text i))
			(if (= ch (ascii-code "{"))
				;potential interpolation
				(if (defq close_pos (find "}" text i))
					(progn
						;extract variable name
						(defq var_name (slice text (inc i) close_pos))
						;get variable value
						(defq var_value (. this :getvar var_name ""))
						(setq result (cat result var_value))
						(setq i (inc close_pos)))
					;no closing brace - keep the {
					(progn
						(setq result (cat result "{"))
						(setq i (inc i))))
				;normal character
				(progn
					(setq result (cat result (ascii-char ch)))
					(setq i (inc i)))))
		result)

	;Call built-in function
	(defmethod :call_function (func_name args_str)
		; (. interp :call_function func_name args_str) -> result
		;call built-in function with arguments
		(defq fname (ascii-upper func_name)
			args_list (split args_str ","))
		;trim arguments
		(setq args_list (map (lambda (arg) (. this :expand_vars (trim arg))) args_list))
		(cond
			;String functions
			((= fname "UPPER") (. this :upper (first args_list)))
			((= fname "LOWER") (. this :lower (first args_list)))
			((= fname "LENGTH") (str (. this :length (first args_list))))
			((= fname "REVERSE") (. this :reverse (first args_list)))
			((= fname "STRIP") (apply (lambda (s &optional o c) (. this :strip s o c)) args_list))
			((= fname "SUBSTR") (apply (lambda (s st &optional l) (. this :substr s st l)) args_list))
			((= fname "LEFT") (apply (lambda (s l &optional p) (. this :left s l p)) args_list))
			((= fname "RIGHT") (apply (lambda (s l &optional p) (. this :right s l p)) args_list))
			((= fname "WORD") (. this :word (first args_list) (second args_list)))
			((= fname "WORDS") (str (. this :words (first args_list))))
			((= fname "WORDPOS") (apply (lambda (p s &optional st) (str (. this :wordpos p s st))) args_list))
			((= fname "DELWORD") (apply (lambda (s n &optional l) (. this :delword s n l)) args_list))
			((= fname "SUBWORD") (apply (lambda (s n &optional l) (. this :subword s n l)) args_list))
			((= fname "ABBREV") (apply (lambda (i a &optional m) (str (. this :abbrev i a m))) args_list))
			((= fname "POS") (apply (lambda (n h &optional s) (str (. this :pos n h s))) args_list))
			;Additional string functions
			((= fname "TRIM_START") (. this :trim_start (first args_list)))
			((= fname "TRIM_END") (. this :trim_end (first args_list)))
			((= fname "PROPER") (. this :proper (first args_list)))
			((= fname "PAD_START") (apply (lambda (s l &optional p) (. this :pad_start s l p)) args_list))
			((= fname "PAD_END") (apply (lambda (s l &optional p) (. this :pad_end s l p)) args_list))
			((= fname "COPIES") (. this :copies (first args_list) (second args_list)))
			;Validation functions
			((= fname "IS_ALPHA") (str (. this :is_alpha (first args_list))))
			((= fname "IS_NUMERIC") (str (. this :is_numeric (first args_list))))
			((= fname "IS_ALPHANUMERIC") (str (. this :is_alphanumeric (first args_list))))
			((= fname "IS_EMPTY") (str (. this :is_empty (first args_list))))
			((= fname "STARTS_WITH") (str (. this :starts_with (first args_list) (second args_list))))
			((= fname "ENDS_WITH") (str (. this :ends_with (first args_list) (second args_list))))
			((= fname "INCLUDES") (str (. this :includes (first args_list) (second args_list))))
			;Array functions
			((= fname "ARRAY") (apply (lambda (n s &optional i) (str (. this :array_create n s i))) args_list))
			((= fname "SORT") (str (. this :array_sort (first args_list))))
			((= fname "REVERSE_ARRAY") (str (. this :array_reverse (first args_list))))
			((= fname "PUSH") (str (. this :array_push (first args_list) (second args_list))))
			((= fname "POP") (. this :array_pop (first args_list)))
			((= fname "JOIN") (. this :array_join (first args_list) (second args_list)))
			;JSON functions
			((= fname "JSON_STRINGIFY") (. this :json_stringify (first args_list)))
			((= fname "JSON_PARSE") (str (. this :json_parse (first args_list) (second args_list))))
			;Data transformation functions (RexxJS)
			((= fname "CSV_TO_JSON") (. this :csv_to_json (first args_list)))
			((= fname "JSON_TO_CSV") (. this :json_to_csv (first args_list)))
			;Additional string functions (RexxJS)
			((= fname "SLUG") (. this :slug (first args_list)))
			((= fname "REPEAT") (. this :repeat (first args_list) (second args_list)))
			((= fname "INDEXOF") (apply (lambda (h n &optional s) (str (. this :indexof h n s))) args_list))
			;Math functions
			((= fname "ABS") (str (. this :abs_value (first args_list))))
			((= fname "MIN") (str (apply (lambda (&rest nums) (. this :min_value nums)) args_list)))
			((= fname "MAX") (str (apply (lambda (&rest nums) (. this :max_value nums)) args_list)))
			((= fname "SIGN") (str (. this :sign_value (first args_list))))
			((= fname "RANDOM") (apply (lambda (&optional mn mx) (str (. this :random_value mn mx))) args_list))
			((= fname "TIME") (. this :time_value))
			((= fname "DATE") (. this :date_value))
			;Extended math functions
			((= fname "MATH_CEIL") (str (. this :math_ceil (first args_list))))
			((= fname "MATH_FLOOR") (str (. this :math_floor (first args_list))))
			((= fname "MATH_ROUND") (str (. this :math_round (first args_list))))
			((= fname "MATH_SQRT") (str (. this :math_sqrt (first args_list))))
			;Advanced string functions
			((= fname "CENTER") (apply (lambda (s l &optional p) (. this :center s l p)) args_list))
			((= fname "CENTRE") (apply (lambda (s l &optional p) (. this :center s l p)) args_list))
			((= fname "DATATYPE") (apply (lambda (s &optional t) (. this :datatype s t)) args_list))
			((= fname "INDEX") (apply (lambda (h n &optional s) (str (. this :index h n s))) args_list))
			((= fname "CHANGESTR") (. this :changestr (first args_list) (second args_list) (third args_list)))
			((= fname "COUNTSTR") (str (. this :countstr (first args_list) (second args_list))))
			((= fname "SPACE") (apply (lambda (s &optional n p) (. this :space s n p)) args_list))
			((= fname "DELSTR") (apply (lambda (s p &optional l) (. this :delstr s p l)) args_list))
			((= fname "INSERT") (apply (lambda (n t &optional p l pd) (. this :insert n t p l pd)) args_list))
			((= fname "OVERLAY") (apply (lambda (n t &optional p l pd) (. this :overlay n t p l pd)) args_list))
			((= fname "TRANSLATE") (apply (lambda (s &optional to ti p) (. this :translate s to ti p)) args_list))
			;Additional utility functions
			((= fname "VERIFY") (apply (lambda (s r &optional o st) (str (. this :verify s r o st))) args_list))
			((= fname "COMPARE") (apply (lambda (s1 s2 &optional p) (str (. this :compare s1 s2 p))) args_list))
			;Conversion functions
			((= fname "C2X") (. this :c2x (first args_list)))
			((= fname "X2C") (. this :x2c (first args_list)))
			((= fname "X2D") (. this :x2d (first args_list)))
			((= fname "D2X") (apply (lambda (d &optional l) (. this :d2x d l)) args_list))
			((= fname "X2B") (. this :x2b (first args_list)))
			((= fname "B2X") (. this :b2x (first args_list)))
			(:t "")))

	;Evaluate string concatenation with || operator
	(defmethod :eval_concatenation (expr)
		; (. interp :eval_concatenation expr) -> concatenated_string
		;handle || string concatenation operator (RexxJS compatibility)
		;example: "Hello" || " " || "World" -> "Hello World"
		(defq parts (split expr "||")
			result "")
		(each! 0 -1 (lambda (part)
			(defq part_trimmed (trim part)
				part_value "")
			;evaluate each part separately
			(cond
				;quoted string literal
				((and (> (length part_trimmed) 1)
						(= (first part_trimmed) "\"")
						(= (last part_trimmed) "\""))
					(setq part_value (slice part_trimmed 1 -2))
					;process escape sequences
					(setq part_value (. this :process_escapes part_value))
					;process {variable} interpolation
					(setq part_value (. this :interpolate part_value)))
				;function call
				((find "(" part_trimmed)
					(defq paren_pos (find "(" part_trimmed)
						before (slice part_trimmed 0 paren_pos)
						words (split before " "))
					(when (nempty? words)
						(defq func_name (elem-get words -1)
							close_paren (find ")" part_trimmed paren_pos))
						(when close_paren
							(defq args (slice part_trimmed (inc paren_pos) close_paren))
							(setq part_value (. this :call_function func_name args)))))
				;variable name or numeric literal
				(:t
					(setq part_value (. this :getvar part_trimmed part_trimmed))))
			(setq result (cat result part_value)))
			parts)
		result)

	;Expand variables and function calls in string
	(defmethod :expand_vars (text)
		; (. interp :expand_vars text) -> expanded_text
		;expand variables like VAR or "literal" in text, and handle function calls
		(defq result text)

		;handle || string concatenation operator (RexxJS compatibility)
		(when (find "||" result)
			(setq result (. this :eval_concatenation result)))

		;check for function calls like UPPER(arg) or LENGTH(arg)
		(when (find "(" result)
			(defq func_start 0)
			(while (defq paren_pos (find "(" result func_start))
				;find function name (word before parenthesis)
				(defq before (slice result 0 paren_pos)
					words (split before " "))
				(when (nempty? words)
					(defq func_name (elem-get words -1)
						close_paren (find ")" result paren_pos))
					(when close_paren
						(defq args (slice result (inc paren_pos) close_paren)
							func_result (. this :call_function func_name args)
							func_call (cat func_name "(" args ")"))
						(setq result (replace result func_call func_result))))
				(setq func_start (inc paren_pos))))
		;simple variable expansion (VAR or "literal")
		;remove quotes if present
		(when (and (> (length result) 1)
				(= (first result) "\"")
				(= (last result) "\""))
			(setq result (slice result 1 -2))
			;process escape sequences in quoted strings
			(setq result (. this :process_escapes result))
			;process {variable} interpolation (modern feature)
			(setq result (. this :interpolate result)))
		;expand simple variables
		(each! 0 -1 (lambda (kv)
			(defq var_name (first kv) var_value (second kv))
			(setq result (replace result var_name var_value)))
			(. vars :to_list))
		result)

	;Parse PARSE instruction
	(defmethod :parse_instruction (args)
		; (. interp :parse_instruction args)
		;PARSE instruction - parse input into variables with patterns
		;Supports: PARSE VALUE expr WITH pattern
		;          PARSE VAR variable WITH pattern
		;          PARSE ARG var1 var2 ...
		;          PARSE UPPER/LOWER VALUE/VAR ...
		(defq parts (split args " ")
			upper_flag :nil
			lower_flag :nil
			source :nil)

		;check for UPPER/LOWER modifiers
		(when (and (nempty? parts) (= (ascii-upper (first parts)) "UPPER"))
			(setq upper_flag :t
				parts (slice parts 1 -1)))
		(when (and (nempty? parts) (= (ascii-upper (first parts)) "LOWER"))
			(setq lower_flag :t
				parts (slice parts 1 -1)))

		(when (>= (length parts) 1)
			(setq source (ascii-upper (first parts)))
			(cond
				;PARSE VALUE expr WITH pattern
				((= source "VALUE")
					(. this :parse_value (slice parts 1 -1) upper_flag lower_flag))

				;PARSE VAR variable WITH pattern
				((= source "VAR")
					(. this :parse_var (slice parts 1 -1) upper_flag lower_flag))

				;PARSE ARG var1 var2 ...
				((= source "ARG")
					(defq var_names (slice parts 1 -1)
						arg_str (. this :getvar "ARG" "")
						arg_parts (split arg_str " "))
					(each! 0 -1 (lambda (idx)
						(defq val (if (< idx (length arg_parts))
							(elem-get arg_parts idx)
							""))
						(when upper_flag (setq val (ascii-upper val)))
						(when lower_flag (setq val (ascii-lower val)))
						(. this :setvar (elem-get var_names idx) val))
						(range (length var_names))))

				(:t :nil))))

	;PARSE VALUE expr WITH pattern
	(defmethod :parse_value (parts upper_flag lower_flag)
		; (. interp :parse_value parts upper_flag lower_flag)
		;Find WITH keyword position
		(defq with_pos :nil)
		(each! 0 -1 (lambda (idx)
			(when (= (ascii-upper (elem-get parts idx)) "WITH")
				(setq with_pos idx)))
			(range (length parts)))

		(when with_pos
			;expression is everything before WITH
			(defq expr_parts (slice parts 0 with_pos)
				expr_str (join expr_parts " ")
				value (. this :expand_vars expr_str))

			;apply case conversion
			(when upper_flag (setq value (ascii-upper value)))
			(when lower_flag (setq value (ascii-lower value)))

			;pattern is everything after WITH
			(defq pattern_parts (slice parts (inc with_pos) -1))
			(. this :parse_with_pattern value pattern_parts)))

	;PARSE VAR variable WITH pattern
	(defmethod :parse_var (parts upper_flag lower_flag)
		; (. interp :parse_var parts upper_flag lower_flag)
		;Find WITH keyword position
		(defq with_pos :nil)
		(each! 0 -1 (lambda (idx)
			(when (= (ascii-upper (elem-get parts idx)) "WITH")
				(setq with_pos idx)))
			(range (length parts)))

		(when with_pos
			;variable name is everything before WITH
			(defq var_parts (slice parts 0 with_pos)
				var_name (first var_parts)
				value (. this :getvar var_name ""))

			;apply case conversion
			(when upper_flag (setq value (ascii-upper value)))
			(when lower_flag (setq value (ascii-lower value)))

			;pattern is everything after WITH
			(defq pattern_parts (slice parts (inc with_pos) -1))
			(. this :parse_with_pattern value pattern_parts)))

	;Parse WITH pattern - extract parts of value into variables
	(defmethod :parse_with_pattern (value pattern_parts)
		; (. interp :parse_with_pattern value pattern_parts)
		;Parse pattern can be:
		; - variable names (word-based extraction)
		; - quoted literals (split on literal string)
		; - positional numbers (extract by column position)
		(defq pos 0
			remaining value)

		(each! 0 -1 (lambda (part)
			(defq pattern_item (trim part))
			(when (nempty? pattern_item)
				(cond
					;quoted literal - split on this delimiter
					((and (> (length pattern_item) 1)
							(= (first pattern_item) "\"")
							(= (last pattern_item) "\""))
						(defq delimiter (slice pattern_item 1 -2)
							delim_pos (find delimiter remaining pos))
						(when delim_pos
							(setq pos (+ delim_pos (length delimiter))
								remaining (slice value pos -1))))

					;positional number - absolute column position
					((. this :is_numeric pattern_item)
						(defq target_pos (dec (int pattern_item)))  ; convert to 0-indexed
						(setq pos target_pos
							remaining (if (< target_pos (length value))
								(slice value target_pos -1)
								"")))

					;variable name - extract next word or to next pattern
					(:t
						(defq var_name (ascii-upper pattern_item))
						;extract from current position to next space or end
						(defq word_end (find " " remaining))
						(if word_end
							(progn
								(. this :setvar var_name (slice remaining 0 word_end))
								(setq pos (+ pos word_end 1)
									remaining (slice value pos -1)))
							(progn
								;rest of string
								(. this :setvar var_name remaining)
								(setq pos (length value)
									remaining "")))))))
			pattern_parts))

	;Check if current statement should execute based on control flow
	(defmethod :should_execute ()
		; (. interp :should_execute) -> t | :nil
		;check control_stack to see if we should execute current statement
		;skip if LEAVE or ITERATE is active
		(if loop_control
			:nil
			(if (empty? control_stack)
				:t
				(progn
					(defq top (elem-get control_stack -1)
						block_type (first top)
						state (second top))
					(cond
						((= block_type 'if)
							(or (= state :then) (= state :else)))
						((= block_type 'select)
							(or (= state :done) (= state :otherwise)))
						((= block_type 'do)
							:t)
						(:t :t))))))

	;Handle IF statement
	(defmethod :handle_if (args)
		; (. interp :handle_if args)
		;handle IF statement - supports inline and multi-line forms
		;inline: IF condition THEN statement [ELSE statement]
		;multi-line: IF condition / THEN / statements / [ELSE / statements] / END
		(defq then_pos (find " THEN " (cat " " args " ")))
		(cond
			;inline IF THEN [ELSE]
			(then_pos
				(defq condition (trim (slice args 0 then_pos))
					after_then (trim (slice args (+ then_pos 6) -1))
					else_pos (find " ELSE " (cat " " after_then " ")))
				(if (. this :eval_condition condition)
					;condition true - execute THEN part
					(if else_pos
						(. this :execute_statement (trim (slice after_then 0 else_pos)))
						(. this :execute_statement after_then))
					;condition false - execute ELSE part if present
					(when else_pos
						(. this :execute_statement (trim (slice after_then (+ else_pos 6) -1))))))
			;multi-line IF (push to control stack)
			(:t
				(defq condition_result (. this :eval_condition args))
				(push control_stack (list 'if condition_result :waiting_then)))))

	;Evaluate condition for WHEN/IF statements
	(defmethod :eval_condition (condition_str)
		; (. interp :eval_condition condition_str) -> t | :nil
		;evaluate condition - supports comparisons
		;format: "var = value" or "var > value" etc.
		(defq condition (trim condition_str))
		;remove THEN if present
		(when (defq then_pos (find " THEN" condition))
			(setq condition (trim (slice condition 0 then_pos))))
		;check for comparison operators (enhanced)
		(cond
			;>= operator
			((find ">=" condition)
				(defq parts (split condition ">=" 2))
				(when (>= (length parts) 2)
					(defq left (int (trim (. this :expand_vars (first parts))))
						right (int (trim (. this :expand_vars (second parts)))))
					(>= left right)))
			;<= operator
			((find "<=" condition)
				(defq parts (split condition "<=" 2))
				(when (>= (length parts) 2)
					(defq left (int (trim (. this :expand_vars (first parts))))
						right (int (trim (. this :expand_vars (second parts)))))
					(<= left right)))
			;!= or <> or \= operators (not equal)
			((or (find "!=" condition) (find "<>" condition) (find "\\=" condition))
				(defq op (cond
					((find "!=" condition) "!=")
					((find "<>" condition) "<>")
					(:t "\\="))
					parts (split condition op 2))
				(when (>= (length parts) 2)
					(defq left (trim (. this :expand_vars (first parts)))
						right (trim (. this :expand_vars (second parts))))
					(not (= left right))))
			;= operator (must come after !=, >=, <=)
			((find "=" condition)
				(defq parts (split condition "=" 2))
				(when (>= (length parts) 2)
					(defq left (trim (. this :expand_vars (first parts)))
						right (trim (. this :expand_vars (second parts))))
					(= left right)))
			;> operator (must come after >=)
			((find ">" condition)
				(defq parts (split condition ">" 2))
				(when (>= (length parts) 2)
					(defq left (int (trim (. this :expand_vars (first parts))))
						right (int (trim (. this :expand_vars (second parts)))))
					(> left right)))
			;< operator (must come after <=)
			((find "<" condition)
				(defq parts (split condition "<" 2))
				(when (>= (length parts) 2)
					(defq left (int (trim (. this :expand_vars (first parts))))
						right (int (trim (. this :expand_vars (second parts)))))
					(< left right)))
			(:t
				;no operator - check if value is non-zero/non-empty
				(defq val (. this :expand_vars condition))
				(and (nempty? val) (not (= val "0"))))))

	;Handle DO loop start
	(defmethod :handle_do_start (args)
		; (. interp :handle_do_start args)
		;handle DO loop initialization
		;supports: DO, DO FOREVER, DO count, DO expr, DO WHILE condition, DO UNTIL condition, DO var = start TO end [BY step]
		(defq args_upper (ascii-upper (trim args)))
		(cond
			;DO FOREVER - infinite loop
			((= args_upper "FOREVER")
				(push loop_stack (list 'forever :nil :nil :nil :nil)))

			;DO WHILE condition
			((starts-with args_upper "WHILE ")
				(defq condition (slice args 6 -1))
				(push loop_stack (list 'while condition :nil :nil :nil)))

			;DO UNTIL condition
			((starts-with args_upper "UNTIL ")
				(defq condition (slice args 6 -1))
				(push loop_stack (list 'until condition :nil :nil :nil)))

			;DO var OVER array (RexxJS compatibility)
			((find " OVER " args_upper)
				(defq over_pos (find " OVER " args_upper)
					var_name (trim (slice args 0 over_pos))
					array_name (trim (slice args (+ over_pos 6) -1))
					array_count (int (. this :getvar (cat array_name ".0") "0")))
				(if (> array_count 0)
					(progn
						;initialize loop with first element
						(. this :setvar var_name (. this :getvar (cat array_name ".1") ""))
						(push loop_stack (list 'over var_name array_name 1 array_count)))
					;empty array - don't enter loop
					(push loop_stack (list 'over var_name array_name 0 0))))

			;DO var = start TO end [BY step]
			((find "=" args)
				(defq eq_pos (find "=" args)
					var_name (trim (slice args 0 eq_pos))
					rest (slice args (inc eq_pos) -1)
					to_pos (find " TO " (ascii-upper rest)))
				(when to_pos
					(defq start_val (int (trim (. this :expand_vars (slice rest 0 to_pos))))
						after_to (slice rest (+ to_pos 4) -1)
						by_pos (find " BY " (ascii-upper after_to))
						end_val (if by_pos
							(int (trim (. this :expand_vars (slice after_to 0 by_pos))))
							(int (trim (. this :expand_vars after_to))))
						step_val (if by_pos
							(int (trim (. this :expand_vars (slice after_to (+ by_pos 4) -1))))
							1))
					(. this :setvar var_name (str start_val))
					(push loop_stack (list 'counter var_name start_val end_val step_val))))

			;DO with count or expression
			(:t
				(if (nempty? args)
					;DO count/expr - evaluate expression
					(defq count (int (. this :expand_vars args)))
					;simple DO block (count = 1)
					(defq count 1))
				(push loop_stack (list 'simple count 0 :nil :nil))))
		(push control_stack (list 'do :active loop_stack)))

	;Handle DO loop end
	(defmethod :handle_do_end ()
		; (. interp :handle_do_end)
		;handle DO loop completion - check if should continue
		(when (nempty? loop_stack)
			;check for LEAVE/ITERATE
			(cond
				((= loop_control 'leave)
					;exit loop immediately
					(pop loop_stack)
					(setq loop_control :nil))
				((= loop_control 'iterate)
					;continue to next iteration (don't pop, just clear flag)
					(setq loop_control :nil)
					;still need to check loop conditions
					(defq loop_info (elem-get loop_stack -1)
						loop_type (first loop_info))
					(cond
						((= loop_type 'forever)
							;DO FOREVER never exits on ITERATE
							:nil)
						((= loop_type 'while)
							(defq condition (second loop_info))
							(unless (. this :eval_condition condition)
								(pop loop_stack)))
						((= loop_type 'until)
							(defq condition (second loop_info))
							(when (. this :eval_condition condition)
								(pop loop_stack)))
						((= loop_type 'counter)
							(defq var_name (second loop_info)
								current (third loop_info)
								end_val (elem-get loop_info 3)
								step_val (elem-get loop_info 4)
								next_val (+ current step_val))
							(if (<= next_val end_val)
								(progn
									(. this :setvar var_name (str next_val))
									(elem-set loop_info 2 next_val))
								(pop loop_stack)))
						((= loop_type 'simple)
							(defq count (second loop_info)
								current (third loop_info))
							(if (< current (dec count))
								(elem-set loop_info 2 (inc current))
								(pop loop_stack)))
						((= loop_type 'over)
							;DO OVER - advance to next array element
							(defq var_name (second loop_info)
								array_name (third loop_info)
								current_idx (elem-get loop_info 3)
								max_idx (elem-get loop_info 4)
								next_idx (inc current_idx))
							(if (<= next_idx max_idx)
								(progn
									;set variable to next array element
									(. this :setvar var_name (. this :getvar (cat array_name "." (str next_idx)) ""))
									(elem-set loop_info 3 next_idx))
								(pop loop_stack)))))
				(:t
					;normal loop end - check conditions
					(defq loop_info (elem-get loop_stack -1)
						loop_type (first loop_info))
					(cond
						((= loop_type 'forever)
							;DO FOREVER - always continue, never exit
							:nil)
						((= loop_type 'while)
							;check WHILE condition
							(defq condition (second loop_info))
							(if (. this :eval_condition condition)
								;continue loop - don't pop
								:nil
								;exit loop
								(pop loop_stack)))
						((= loop_type 'until)
							;check UNTIL condition
							(defq condition (second loop_info))
							(if (. this :eval_condition condition)
								;exit loop
								(pop loop_stack)
								;continue loop
								:nil))
						((= loop_type 'counter)
							;increment counter and check
							(defq var_name (second loop_info)
								current (third loop_info)
								end_val (elem-get loop_info 3)
								step_val (elem-get loop_info 4)
								next_val (+ current step_val))
							(if (<= next_val end_val)
								(progn
									;continue loop
									(. this :setvar var_name (str next_val))
									(elem-set loop_info 2 next_val))
								;exit loop
								(pop loop_stack)))
						((= loop_type 'simple)
							;decrement count
							(defq count (second loop_info)
								current (third loop_info))
							(if (< current (dec count))
								;continue
								(elem-set loop_info 2 (inc current))
								;exit
								(pop loop_stack)))
						((= loop_type 'over)
							;DO OVER - advance to next array element
							(defq var_name (second loop_info)
								array_name (third loop_info)
								current_idx (elem-get loop_info 3)
								max_idx (elem-get loop_info 4)
								next_idx (inc current_idx))
							(if (<= next_idx max_idx)
								(progn
									;set variable to next array element
									(. this :setvar var_name (. this :getvar (cat array_name "." (str next_idx)) ""))
									(elem-set loop_info 3 next_idx))
								;exit loop
								(pop loop_stack)))))))))

	;Handle CALL instruction
	(defmethod :handle_call (args)
		; (. interp :handle_call args)
		;CALL procname [arg1 arg2 ...]
		(defq parts (split args " ")
			proc_name (if (nempty? parts) (ascii-upper (first parts)) "")
			proc_args (if (> (length parts) 1)
				(map (lambda (arg) (. this :expand_vars arg)) (slice parts 1 -1))
				(list)))
		;check if procedure exists
		(defq proc_def (. procedures :find proc_name))
		(when proc_def
			;save current state
			(push call_stack (list 'call proc_name vars))
			;set ARG variable for procedure
			(. this :setvar "ARG" (join proc_args " "))
			;execute procedure body
			(each! 0 -1 (lambda (line)
				(when (not (= loop_control 'return))
					(. this :execute_statement line)))
				proc_def)
			;restore state
			(pop call_stack)
			;clear return flag
			(when (= loop_control 'return)
				(setq loop_control :nil))))

	;Handle PROCEDURE instruction
	(defmethod :handle_procedure (args)
		; (. interp :handle_procedure args)
		;PROCEDURE [EXPOSE var1 var2 ...]
		;creates local scope for procedure (simplified - just note it)
		;in full REXX, this would hide all vars except EXPOSEd ones
		:nil)

	;Handle ARG instruction
	(defmethod :handle_arg (args)
		; (. interp :handle_arg args)
		;ARG var1 [var2 var3 ...]
		;parse arguments into variables
		(defq arg_str (. this :getvar "ARG" "")
			arg_parts (if (nempty? arg_str) (split arg_str " ") (list))
			var_names (split args " "))
		(each! 0 -1 (lambda (idx)
			(. this :setvar (elem-get var_names idx)
				(if (< idx (length arg_parts))
					(elem-get arg_parts idx)
					"")))
			(range (length var_names))))

	;Handle INTERPRET instruction
	(defmethod :handle_interpret (args)
		; (. interp :handle_interpret args)
		;INTERPRET expression
		;dynamically execute code from a string
		(defq code_to_run (. this :expand_vars args))
		(. this :execute_statement code_to_run))

	;Handle SIGNAL instruction
	(defmethod :handle_signal (args)
		; (. interp :handle_signal args)
		;SIGNAL labelname - goto label (like CALL but doesn't return)
		;or SIGNAL ON/OFF condition
		(defq args_upper (ascii-upper (trim args)))
		(cond
			;SIGNAL ON/OFF not implemented - ignore
			((or (starts-with args_upper "ON ") (starts-with args_upper "OFF "))
				:nil)
			;SIGNAL labelname - set target and trigger jump
			(:t
				(defq label_name (ascii-upper (trim args)))
				(setq signal_target label_name
					loop_control 'signal))))

	;Register a procedure
	(defmethod :register_procedure (name body)
		; (. interp :register_procedure name body)
		;register procedure with name and body (list of lines)
		(. procedures :insert (ascii-upper name) body))

	;Process HEREDOC blocks (<<DELIMITER ... DELIMITER)
	(defmethod :process_heredocs (lines)
		; (. interp :process_heredocs lines) -> processed_lines
		;find and replace HEREDOC blocks with their multi-line content
		(defq result (list)
			i 0
			len (length lines))
		(while (< i len)
			(defq line (elem-get lines i)
				trimmed (trim line))
			;check for <<DELIMITER pattern
			(defq heredoc_pos (find "<<" trimmed))
			(if (and heredoc_pos (>= heredoc_pos 0))
				;found << marker
				(progn
					;extract delimiter (word after <<)
					(defq after_marker (slice trimmed (+ heredoc_pos 2) -1)
						delimiter_parts (split after_marker " ")
						delimiter (if (nempty? delimiter_parts) (trim (first delimiter_parts)) ""))
					(if (nempty? delimiter)
						;valid HEREDOC - collect content until delimiter
						(progn
							(defq content (list)
								prefix (if (> heredoc_pos 0) (slice trimmed 0 heredoc_pos) "")
								j (inc i)
								found_end :nil)
							(while (and (< j len) (not found_end))
								(defq content_line (elem-get lines j))
								(if (= (trim content_line) delimiter)
									;found end delimiter
									(setq found_end :t)
									;add to content
									(push content content_line))
								(setq j (inc j)))
							;create the result line with embedded content
							(defq heredoc_value (cat "\"" (join content (ascii-char 10)) "\"")
								new_line (if (nempty? prefix) (cat prefix heredoc_value) heredoc_value))
							(push result new_line)
							;skip past the heredoc block
							(setq i j))
						;no valid delimiter - just add line as-is
						(progn
							(push result line)
							(setq i (inc i)))))
				;no heredoc - add line as-is
				(progn
					(push result line)
					(setq i (inc i)))))
		result)

	;Execute REXX program
	(defmethod :run (source)
		; (. interp :run source) -> rc
		;execute REXX program source
		(. this :clear_output)
		(defq lines (split source (ascii-char 10))
			final_rc 0)

		;Pre-process HEREDOC blocks (<<DELIMITER ... DELIMITER)
		(setq lines (. this :process_heredocs lines))

		;first pass: find and register procedures (labels ending with :)
		(defq current_proc :nil
			proc_body (list)
			main_code (list))
		(each! 0 -1 (lambda (line)
			(defq trimmed (trim line))
			(cond
				;check if line is a label (ends with :)
				((and (> (length trimmed) 1) (= (slice trimmed -2 -1) ":"))
					;save previous procedure if any
					(when current_proc
						(. this :register_procedure current_proc proc_body))
					;start new procedure
					(setq current_proc (slice trimmed 0 -2)
						proc_body (list)))
				;in a procedure
				(current_proc
					(push proc_body line))
				;main code
				(:t
					(push main_code line))))
			lines)
		;save last procedure if any
		(when current_proc
			(. this :register_procedure current_proc proc_body))

		;second pass: execute main code only
		(each! 0 -1 (lambda (line)
			(when (not (or (= loop_control 'return) (= loop_control 'signal)))
				(defq stmt_rc (. this :execute_statement line))
				(when (and stmt_rc (> stmt_rc 0))
					(setq final_rc stmt_rc)))
			;check for SIGNAL
			(when (= loop_control 'signal)
				;find and execute target procedure
				(defq target_proc (. procedures :find signal_target))
				(when target_proc
					(setq loop_control :nil signal_target :nil)
					(each! 0 -1 (lambda (proc_line)
						(when (not (or (= loop_control 'return) (= loop_control 'signal)))
							(. this :execute_statement proc_line)))
						target_proc))))
			main_code)
		final_rc))

;Utility functions

(defun rexx-eval (source)
	; (rexx-eval source) -> (rc output result)
	;evaluate REXX source and return results
	(defq interp (RexxInterpreter))
	(defq rc (. interp :run source))
	(list rc (. interp :get_output) (. interp :getvar "RESULT")))

(defun rexx-format-output (rc output result)
	; (rexx-format-output rc output result) -> formatted_string
	;format REXX execution results
	(defq lines (list))
	(when (nempty? output)
		(push lines output))
	(when (> rc 0)
		(push lines (cat "RC=" (str rc))))
	(when (nempty? result)
		(push lines (cat "RESULT=" result)))
	(join lines (ascii-char 10)))
