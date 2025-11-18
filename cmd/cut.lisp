(import "lib/options/options.inc")

(defq usage `(
(("-h" "--help")
"Usage: cut [options] [file ...]

	options:
		-h --help: this help info.
		-f --fields list: select only these fields (comma-separated).
		-d --delimiter char: use CHAR as field delimiter (default TAB).
		-c --characters list: select only these character positions.

	Extract fields or characters from lines of input.
	LIST format: N, N-M, N-, -M (comma separated).

	Examples:
		cut -f 1,3 file.txt     # fields 1 and 3
		cut -f 2- file.txt      # field 2 to end
		cut -c 1-10 file.txt    # characters 1-10")
(("-f" "--fields") ,(opt-str 'opt_fields))
(("-d" "--delimiter") ,(opt-str 'opt_delim))
(("-c" "--characters") ,(opt-str 'opt_chars))
))

(defun parse-ranges (spec)
	;parse range specification like "1,3-5,7-"
	(defq ranges (list))
	(each (lambda (part)
			(cond
				;range N-M
				((defq pos (position "-" part))
					(defq start (if (> pos 0) (num (slice 0 pos part)) 1))
					(defq end (if (< pos (dec (length part))) (num (slice (inc pos) -1 part)) 999999))
					(push ranges (list start end)))
				;single number N
				:t
					(defq n (num part))
					(push ranges (list n n))))
		(split spec ","))
	ranges)

(defun in-ranges (n ranges)
	;check if number n is in any of the ranges
	(some (lambda (r) (and (>= n (first r)) (<= n (second r)))) ranges))

(defun cut-fields (line delim ranges)
	(defq fields (split line delim))
	(defq result (list))
	(each-idx (lambda (idx field)
			(when (in-ranges (inc idx) ranges)
				(push result field)))
		fields)
	(join result delim))

(defun cut-chars (line ranges)
	(defq result (list))
	(defq chars (split line ""))
	(each-idx (lambda (idx c)
			(when (in-ranges (inc idx) ranges)
				(push result c)))
		chars)
	(apply cat result))

(defun process-stream (stream)
	(cond
		;field mode
		(opt_fields
			(defq ranges (parse-ranges opt_fields))
			(lines! (lambda (line)
					(print (cut-fields line opt_delim ranges)))
				stream))
		;character mode
		(opt_chars
			(defq ranges (parse-ranges opt_chars))
			(lines! (lambda (line)
					(print (cut-chars line ranges)))
				stream))
		:t
			(print "cut: must specify -f or -c")))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_fields :nil opt_chars :nil opt_delim (ascii-char 9) args (options stdio usage)))
		(if (<= (length args) 1)
			;read from stdin
			(process-stream (io-stream 'stdin))
			;read from files
			(each (# (when (defq stream (file-stream %0))
					(process-stream stream)))
				(rest args)))))
