;;;;;;;;;;;;;;;;;;;
; JSON Data Interchange
;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(defun json-skip-ws (stream &optional lookahead)
	; (json-skip-ws stream [lookahead]) -> str | :nil
	(defq curr (if lookahead lookahead (read-blk stream 1)))
	(while (and curr (find curr " \t\r\n"))
		(setq curr (read-blk stream 1)))
	curr)

(defun json-read-string (stream)
	; (json-read-string stream) -> str
	(defq ss (string-stream (cat "")))
	(while (and (defq ch (read-blk stream 1)) (nql ch "\q"))
		(cond
			((eql ch "\\")
				(case (defq esc (read-blk stream 1))
					("n" (write-blk ss "\n"))
					("t" (write-blk ss "\t"))
					("r" (write-blk ss "\r"))
					("b" (write-char ss 8))
					("f" (write-char ss 12))
					("\q" (write-blk ss "\q"))
					("\\" (write-blk ss "\\"))
					("/" (write-blk ss "/"))
					("u" (defq hex_str (read-blk stream 4))
						(if (and hex_str (= (length hex_str) 4))
							(write-blk ss (num-to-utf8 (str-to-num (cat "0x" hex_str))))
							(throw "JSON parse error: invalid unicode escape" hex_str)))
					(:t (if esc (write-blk ss esc)))))
			(:t (write-blk ss ch))))
	(if (nil? ch) (throw "JSON parse error: unterminated string" :nil))
	(str ss))

(defun json-read-number (stream first_c)
	; (json-read-number stream first_c) -> (val next_c)
	(defq ns (string-stream (cat "")) curr first_c)
	(write-blk ns curr)
	(setq curr (read-blk stream 1))
	(while (and curr (find curr "0123456789+-eE."))
		(write-blk ns curr)
		(setq curr (read-blk stream 1)))
	(defq num_str (str ns))
	(if (or (eql num_str "-") (eql num_str "+"))
		(throw "JSON parse error: invalid number" num_str))
	(defq num_val (cond
		((or (find "e" num_str) (find "E" num_str))
			(str-to-real num_str))
		((find "." num_str)
			(str-to-num num_str))
		((str-to-num num_str))))
	(list num_val curr))

(defmacro json-push-val (v)
	`(cond
		((empty? stack)
			(if has_root
				(throw "JSON parse error: multiple root elements" ,v))
			(setq root_val ,v has_root :t))
		(:t (defq top_frame (last stack))
			(case (first top_frame)
				(:array
					(unless (or (eql (third top_frame) :expect_val_or_end)
								(eql (third top_frame) :expect_val))
						(throw "JSON parse error: unexpected value in array" ,v))
					(push (second top_frame) ,v)
					(elem-set top_frame 2 :expect_comma_or_end))
				(:object
					(unless (eql (third top_frame) :expect_val)
						(throw "JSON parse error: unexpected value in object" ,v))
					(pinsert (second top_frame) (elem-get top_frame 3) ,v)
					(elem-set top_frame 2 :expect_comma_or_end)
					(elem-set top_frame 3 :nil))))))

(defun json-parse (input)
	; (json-parse str_or_stream) -> val
	(defq stream (if (str? input) (string-stream input) input)
		c (json-skip-ws stream) stack (list) root_val :nil has_root :nil)
	(if (nil? c) :nil
		(progn
			(while (and c (or (not has_root) (nempty? stack)))
				(case c
					("{"
						(unless (or (empty? stack)
									(and (eql (first (defq top_frame (last stack))) :array)
										 (or (eql (third top_frame) :expect_val_or_end)
											 (eql (third top_frame) :expect_val)))
									(and (eql (first top_frame) :object)
										 (eql (third top_frame) :expect_val)))
							(throw "JSON parse error: unexpected {" :nil))
						(push stack (list :object (pmap) :expect_key_or_end :nil))
						(setq c (json-skip-ws stream)))
					("["
						(unless (or (empty? stack)
									(and (eql (first (defq top_frame (last stack))) :array)
										 (or (eql (third top_frame) :expect_val_or_end)
											 (eql (third top_frame) :expect_val)))
									(and (eql (first top_frame) :object)
										 (eql (third top_frame) :expect_val)))
							(throw "JSON parse error: unexpected [" :nil))
						(push stack (list :array (list) :expect_val_or_end))
						(setq c (json-skip-ws stream)))
					("}"
						(if (empty? stack)
							(throw "JSON parse error: unexpected } at top level" :nil))
						(defq top_frame (last stack))
						(unless (eql (first top_frame) :object)
							(throw "JSON parse error: mismatched brackets, expected ]" :nil))
						(unless (or (eql (third top_frame) :expect_key_or_end)
									(eql (third top_frame) :expect_comma_or_end))
							(throw "JSON parse error: unexpected } in object" :nil))
						(defq popped (pop stack))
						(setq c (json-skip-ws stream))
						(json-push-val (second popped)))
					("]"
						(if (empty? stack)
							(throw "JSON parse error: unexpected ] at top level" :nil))
						(defq top_frame (last stack))
						(unless (eql (first top_frame) :array)
							(throw "JSON parse error: mismatched brackets, expected }" :nil))
						(unless (or (eql (third top_frame) :expect_val_or_end)
									(eql (third top_frame) :expect_comma_or_end))
							(throw "JSON parse error: unexpected ] in array" :nil))
						(defq popped (pop stack))
						(setq c (json-skip-ws stream))
						(json-push-val (second popped)))
					(":"
						(if (empty? stack)
							(throw "JSON parse error: unexpected : at top level" :nil))
						(defq top_frame (last stack))
						(unless (and (eql (first top_frame) :object)
									 (eql (third top_frame) :expect_colon))
							(throw "JSON parse error: unexpected :" :nil))
						(elem-set top_frame 2 :expect_val)
						(setq c (json-skip-ws stream)))
					(","
						(if (empty? stack)
							(throw "JSON parse error: unexpected , at top level" :nil))
						(defq top_frame (last stack))
						(cond
							((eql (first top_frame) :array)
								(unless (eql (third top_frame) :expect_comma_or_end)
									(throw "JSON parse error: unexpected , in array" :nil))
								(elem-set top_frame 2 :expect_val))
							((eql (first top_frame) :object)
								(unless (eql (third top_frame) :expect_comma_or_end)
									(throw "JSON parse error: unexpected , in object" :nil))
								(elem-set top_frame 2 :expect_key))
							(:t (throw "JSON parse error: unexpected ," :nil)))
						(setq c (json-skip-ws stream)))
					("\q"
						(cond
							((and (nempty? stack)
									(eql (first (defq top_frame (last stack))) :object)
									(or (eql (third top_frame) :expect_key_or_end)
										(eql (third top_frame) :expect_key)))
								(defq k (json-read-string stream))
								(elem-set top_frame 2 :expect_colon)
								(elem-set top_frame 3 (sym (if (starts-with ":" k) k (cat ":" k))))
								(setq c (json-skip-ws stream)))
							(:t (defq str_val (json-read-string stream))
								(setq c (json-skip-ws stream))
								(json-push-val str_val))))
					(("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "-" "+")
						(bind '(num_val next_c) (json-read-number stream c))
						(setq c (json-skip-ws stream next_c))
						(json-push-val num_val))
					("t"
						(unless (eql (read-blk stream 3) "rue")
							(throw "JSON parse error: expected true" :nil))
						(setq c (json-skip-ws stream))
						(json-push-val :t))
					("f"
						(unless (eql (read-blk stream 4) "alse")
							(throw "JSON parse error: expected false" :nil))
						(setq c (json-skip-ws stream))
						(json-push-val :nil))
					("n"
						(unless (eql (read-blk stream 3) "ull")
							(throw "JSON parse error: expected null" :nil))
						(setq c (json-skip-ws stream))
						(json-push-val :nil))
					(:t (throw "JSON parse error: unexpected character" c))))
			(when (nempty? stack)
				(throw "JSON parse error: unclosed container at EOF" :nil))
			(unless has_root
				(throw "JSON parse error: no value found" :nil))
			root_val)))

(defun json-escape-str (s)
	; (json-escape-str str) -> str
	(if (eql s "") "\q\q"
		(unless (find "q" (defq esc (escape s))) (cat "\q" esc "\q")
			(defq idxs (nums 0 0 0 1) start 0 len (length esc) idx (find "\\" esc 0))
			(while idx
				(cond
					((and (< (inc idx) len) (eql (elem-get esc (inc idx)) "q"))
						(push idxs start idx 1 3)
						(setq start (+ idx 2) idx (find "\\" esc start)))
					((and (< (inc idx) len) (eql (elem-get esc (inc idx)) "\\"))
						(setq idx (find "\\" esc (+ idx 2))))
					(:t (setq idx (find "\\" esc (+ idx 2))))))
			(push idxs start -1 0 1)
			(splice esc "\q\\\q" idxs))))

(defun json-serialize-scalar (obj)
	; (json-serialize-scalar obj) -> str
	(cond
		((eql obj :nil) "null")
		((eql obj :t) "true")
		((real? obj) (str (real-to-str obj)))
		((fixed? obj) (str obj))
		((num? obj) (str obj))
		((str? obj) (json-escape-str obj))
		((str obj))))

(defun json-container? (obj)
	; (json-container? obj) -> :t | :nil
	(or (pmap? obj) (list? obj) (array? obj)))

(defun json-get-items (obj)
	; (json-get-items obj) -> list
	(cond
		((pmap? obj) (partition obj 2))
		((array? obj) (map identity obj))
		(obj)))

(defun json-stringify (obj)
	; (json-stringify obj) -> str
	(cond
		((not (json-container? obj))
			(json-serialize-scalar obj))
		(:t (defq stack (list (list obj (if (pmap? obj) :pmap :list) (json-get-items obj) 0 (list) :nil))
				root_res "")
			(while (nempty? stack)
				(bind '(node type items idx parts prefix) (last stack))
				(cond
					((< idx (length items))
						(elem-set (last stack) 3 (inc idx))
						(defq curr (elem-get items idx))
						(cond
							((eql type :pmap)
								(bind '(k v) curr)
								(defq k_str (str k)
									k_clean (if (starts-with ":" k_str) (rest k_str) k_str)
									k_json (json-escape-str k_clean))
								(if (json-container? v)
									(push stack (list v (if (pmap? v) :pmap :list) (json-get-items v) 0 (list) k_json))
									(push parts (cat k_json ":" (json-serialize-scalar v)))))
							(:t (defq v curr)
								(if (json-container? v)
									(push stack (list v (if (pmap? v) :pmap :list) (json-get-items v) 0 (list) :nil))
									(push parts (json-serialize-scalar v))))))
					(:t (pop stack)
						(defq assembled (cond
							((eql type :pmap) (cat "{" (join parts ",") "}"))
							(:t (cat "[" (join parts ",") "]"))))
						(cond
							((empty? stack)
								(setq root_res assembled))
							(:t (bind '(& parent_type & & parent_parts &ignore) (last stack))
								(if (eql parent_type :pmap)
									(push parent_parts (cat prefix ":" assembled))
									(push parent_parts assembled)))))))
			root_res)))

(defun json-to-tre (input)
	; (json-to-tre str_or_stream) -> pmap | list | scalar
	(json-parse input))

(defun json-from-tre (obj)
	; (json-from-tre obj) -> str
	(json-stringify obj))

;module
(export-symbols '(json-parse json-stringify json-to-tre json-from-tre))
(env-pop)