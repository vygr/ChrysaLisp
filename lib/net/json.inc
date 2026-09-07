;;;;;;;;;;;;;;;;;;;
; lib/net/json.inc
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
; JSON Data Interchange
;;;;;;;;;;;;;;;;;;;

(import "lib/text/charclass.inc")
(import "lib/text/searching.inc")

;module
(env-push)

(defun json-skip-ws (stream)
	; (json-skip-ws stream) -> char_code | -1
	(defq c (read-char stream))
	(while (and (/= c -1) (bfind (char c) +char_class_white_space))
		(setq c (read-char stream)))
	c)

(defun json-parse-string-content (stream)
	; (json-parse-string-content stream) -> str
	(defq value (str-alloc str_gap)
		ss (string-stream value)
		char (read-char stream))
	(while (and (/= char -1) (/= char (ascii-code "\q")))
		(if (= char (ascii-code "\\"))
			(progn
				(setq char (read-char stream))
				(case char
					((ascii-code "n") (write-char ss (ascii-code "\\")) (write-char ss (ascii-code "n")))
					((ascii-code "t") (write-char ss (ascii-code "\\")) (write-char ss (ascii-code "t")))
					((ascii-code "r") (write-char ss (ascii-code "\\")) (write-char ss (ascii-code "r")))
					((ascii-code "\"") (write-char ss (ascii-code "\\")) (write-char ss (ascii-code "q")))
					((ascii-code "\\") (write-char ss (ascii-code "\\")) (write-char ss (ascii-code "\\")))
					(:t (write-char ss (ascii-code "\\")) (write-char ss char))))
			(write-char ss char))
		(setq char (read-char stream)))
	(stream-seek ss 0 0)
	(defq parts (list))
	(while (defq blk (read-blk ss 4096))
		(push parts blk))
	(unescape (apply (const cat) parts)))

(defun json-parse-string (stream)
	; (json-parse-string stream) -> str
	(json-parse-string-content stream))

(defun json-parse-number (stream first_c)
	; (json-parse-number stream first_c) -> num | real
	(defq ms (memory-stream)
		c first_c)
	(write-char ms c)
	(setq c (read-char stream))
	(while (and (/= c -1) (bfind (char c) "0123456789+-eE."))
		(write-char ms c)
		(setq c (read-char stream)))
	(stream-seek stream -1 1)
	(stream-seek ms 0 0)
	(defq num_str (read-blk ms 64))
	(if (or (find "." num_str) (find "e" num_str) (find "E" num_str))
		(str-to-real num_str)
		(str-to-num num_str)))

(defun json-parse-val (stream first_c)
	; (json-parse-val stream first_c) -> val
	(cond
		((= first_c (ascii-code "{"))
			(json-parse-object stream))
		((= first_c (ascii-code "["))
			(json-parse-array stream))
		((= first_c (ascii-code "\q"))
			(json-parse-string stream))
		((or (<= (ascii-code "0") first_c (ascii-code "9")) (= first_c (ascii-code "-")))
			(json-parse-number stream first_c))
		((= first_c (ascii-code "t"))
			(read-blk stream 3)
			:t)
		((= first_c (ascii-code "f"))
			(read-blk stream 4)
			:nil)
		((= first_c (ascii-code "n"))
			(read-blk stream 3)
			:nil)
		(:t (throw "JSON parse error: unexpected char" (ascii-char first_c)))))

(defun json-parse-object (stream)
	; (json-parse-object stream) -> pmap
	(defq res (pmap)
		c (json-skip-ws stream))
	(when (/= c (ascii-code "}"))
		(while :t
			(if (/= c (ascii-code "\q"))
				(throw "JSON parse error: expected string key" c))
			(defq k (json-parse-string-content stream)
				k_sym (sym (if (starts-with ":" k) k (cat ":" k))))
			(if (/= (json-skip-ws stream) (ascii-code ":"))
				(throw "JSON parse error: expected colon" c))
			(defq v (json-parse-val stream (json-skip-ws stream)))
			(pinsert res k_sym v)
			(setq c (json-skip-ws stream))
			(cond
				((= c (ascii-code "}")) (break))
				((= c (ascii-code ",")) (setq c (json-skip-ws stream)))
				(:t (throw "JSON parse error: expected comma or }" c)))))
	res)

(defun json-parse-array (stream)
	; (json-parse-array stream) -> list
	(defq res (list)
		c (json-skip-ws stream))
	(when (/= c (ascii-code "]"))
		(while :t
			(defq v (json-parse-val stream c))
			(push res v)
			(setq c (json-skip-ws stream))
			(cond
				((= c (ascii-code "]")) (break))
				((= c (ascii-code ",")) (setq c (json-skip-ws stream)))
				(:t (throw "JSON parse error: expected comma or ]" c)))))
	res)

(defun json-parse (input)
	; (json-parse str_or_stream) -> val
	(defq stream (if (str? input) (string-stream input) input)
		c (json-skip-ws stream))
	(if (= c -1) :nil (json-parse-val stream c)))

(defun json-serialize-scalar (obj)
	; (json-serialize-scalar obj) -> str
	(cond
		((eql obj :nil) "null")
		((eql obj :t) "true")
		((real? obj) (str (n2f obj)))
		((fixed? obj) (str obj))
		((num? obj) (str obj))
		((str? obj) (cat "\q" (join (split (escape obj) "\q") "\"") "\q"))
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
		(:t
			(defq stack (list (list obj (if (pmap? obj) :pmap :list) (json-get-items obj) 0 (list) :nil))
				root_res "")
			(while (nempty? stack)
				(defq frame (last stack)
					node (first frame)
					type (second frame)
					items (third frame)
					idx (fourth frame)
					parts (fifth frame)
					prefix (sixth frame))
				(cond
					((< idx (length items))
						(elem-set frame 3 (inc idx))
						(defq curr (elem-get items idx))
						(cond
							((eql type :pmap)
								(bind '(k v) curr)
								(defq k_str (str k)
									k_clean (if (starts-with ":" k_str) (rest k_str) k_str)
									k_json (cat "\q" (join (split (escape k_clean) "\q") "\"") "\q"))
								(if (json-container? v)
									(push stack (list v (if (pmap? v) :pmap :list) (json-get-items v) 0 (list) k_json))
									(push parts (cat k_json ":" (json-serialize-scalar v)))))
							(:t
								(defq v curr)
								(if (json-container? v)
									(push stack (list v (if (pmap? v) :pmap :list) (json-get-items v) 0 (list) :nil))
									(push parts (json-serialize-scalar v))))))
					(:t
						(pop stack)
						(defq assembled (cond
							((eql type :pmap) (cat "{" (join parts ",") "}"))
							(:t (cat "[" (join parts ",") "]"))))
						(if (empty? stack)
							(setq root_res assembled)
							(defq parent_frame (last stack)
								parent_parts (fifth parent_frame)
								parent_type (second parent_frame))
							(cond
								((eql parent_type :pmap)
									(push parent_parts (cat prefix ":" assembled)))
								(:t
									(push parent_parts assembled)))))))
			root_res)))

(defun json-to-tre (input)
	; (json-to-tre str_or_stream) -> pmap | list | scalar
	(json-parse input))

(defun tre-to-json (obj)
	; (tre-to-json obj) -> str
	(json-stringify obj))

;module
(export-symbols '(json-parse json-stringify json-to-tre tre-to-json))
(env-pop)