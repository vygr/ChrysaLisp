;;;;;;;;;;;;;;;;;
; cmd/ctf.lisp
;;;;;;;;;;;;;;;;;
(import "lib/options/options.inc")
(import "lib/debug/frames.inc")
(import "lib/collections/collections.inc")

(defq usage `(
(("-h" "--help")
"Usage: ctf [options] [file] ...

    options:
        -h --help: this help info.
        -v --verbosity num: verbosity level, default 0.
        -c --ctf: convert/upgrade font file to latest .ctf spec (planned).

    Inspects and outputs information about ChrysaLisp Vector Font (.ctf)
    or OpenType/TrueType (.otf/.ttf) files. If no files are specified on the
    command line, file paths are read from stdin.")
    (("-c" "--ctf") ,(opt-flag 'opt_c))
    (("-v" "--verbosity") ,(opt-num 'opt_v))
))

(defun format-fixed-24 (val)
	(defq sign "" abs_val val)
	(if (< val 0)
		(setq sign "-" abs_val (neg val)))
	(defq int_part (>> abs_val 24)
		frac_part (logand abs_val 0xffffff)
		frac_dec (/ (* frac_part 10000) 16777216))
	(cat sign (str int_part) "." (pad frac_dec 4 "0")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Big-Endian Memory Getters (OTF)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-uint16-be (buf idx)
	(defq b0 (get-ubyte buf idx)
		b1 (get-ubyte buf (inc idx)))
	(+ (<< b0 8) b1))

(defun get-int16-be (buf idx)
	(defq val (get-uint16-be buf idx))
	(if (>= val 32768)
		(- val 65536)
		val))

(defun get-uint32-be (buf idx)
	(defq b0 (get-ubyte buf idx)
		b1 (get-ubyte buf (+ idx 1))
		b2 (get-ubyte buf (+ idx 2))
		b3 (get-ubyte buf (+ idx 3)))
	(+ (<< b0 24) (<< b1 16) (<< b2 8) b3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Little-Endian Memory Getters (CTF)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-uint16-le (buf idx)
	(defq b0 (get-ubyte buf idx)
		b1 (get-ubyte buf (inc idx)))
	(+ b0 (<< b1 8)))

(defun get-uint32-le (buf idx)
	(defq b0 (get-ubyte buf idx)
		b1 (get-ubyte buf (+ idx 1))
		b2 (get-ubyte buf (+ idx 2))
		b3 (get-ubyte buf (+ idx 3)))
	(+ b0 (<< b1 8) (<< b2 16) (<< b3 24)))

(defun get-int32-le (buf idx)
	(defq val (get-uint32-le buf idx))
	(if (>= val 0x80000000)
		(- val 0x100000000)
		val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; OpenType Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-otf-tables (buf)
	(defq version (get-uint32-be buf 0)
		num_tables (get-uint16-be buf 4)
		tables (Lmap)
		offset 12)
	(times num_tables
		(defq tag (get-str buf offset 4)
			checksum (get-uint32-be buf (+ offset 4))
			tbl_offset (get-uint32-be buf (+ offset 8))
			len (get-uint32-be buf (+ offset 12)))
		(. tables :insert tag (list tbl_offset len))
		(setq offset (+ offset 16)))
	(scatter (Lmap) :version version :tables tables))

(defun get-otf-head (buf tables)
	(bind '(offset len) (. tables :find "head"))
	(get-uint16-be buf (+ offset 18)))

(defun get-otf-hhea (buf tables)
	(bind '(offset len) (. tables :find "hhea"))
	(defq ascender (get-int16-be buf (+ offset 4))
		descender (get-int16-be buf (+ offset 6))
		num_h_metrics (get-uint16-be buf (+ offset 34)))
	(list ascender descender num_h_metrics))

(defun get-otf-maxp (buf tables)
	(bind '(offset len) (. tables :find "maxp"))
	(get-uint16-be buf (+ offset 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CTF Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-ctf-buf (buf)
	(defq ascent (get-uint32-le buf 0)
		descent (get-uint32-le buf 4)
		pages (list)
		pages_info (list)
		offset 8
		running :t)
	(defq font_db (scatter (Lmap)
		:file :nil
		:type "CTF"
		:ascent ascent
		:descent descent))
	(while running
		(defq pend (get-uint32-le buf offset))
		(setq offset (+ offset 4))
		(if (or (= pend 0) (= pend -1))
			(setq running :nil)
			(progn
				(defq pstart (get-uint32-le buf offset)
					count (inc (- pend pstart))
					offsets (list))
				(setq offset (+ offset 4))
				(times count
					(push offsets (get-uint32-le buf offset))
					(setq offset (+ offset 4)))
				(push pages_info (list pstart pend offsets)))))
	(defq pages_i 0)
	(while (< pages_i (length pages_info))
		(bind '(start end offsets) (elem-get pages_info pages_i))
		(defq glyphs (list)
			page_db (scatter (Lmap)
				:start start
				:end end)
			c start
			offsets_i 0)
		(while (< offsets_i (length offsets))
			(defq glyph_offset (elem-get offsets offsets_i)
				width (get-uint32-le buf glyph_offset)
				len (get-uint32-le buf (+ glyph_offset 4))
				min_x 0 max_x 0 min_y 0 max_y 0
				commands (list)
				g_offset (+ glyph_offset 8))
			(if (> len 0)
				(progn
					(defq bytes_read 0 coords_x (list) coords_y (list))
					(while (< bytes_read len)
						(defq type (get-int32-le buf g_offset))
						(setq g_offset (+ g_offset 4))
						(cond
							((or (= type 0) (= type 1))
								(defq rx (get-int32-le buf g_offset)
									ry (get-int32-le buf (+ g_offset 4)))
								(setq g_offset (+ g_offset 8)
									bytes_read (+ bytes_read 12))
								(push coords_x rx)
								(push coords_y ry)
								(push commands (list type rx ry)))
							((= type 2)
								(defq rx (get-int32-le buf g_offset)
									ry (get-int32-le buf (+ g_offset 4))
									rx1 (get-int32-le buf (+ g_offset 8))
									ry1 (get-int32-le buf (+ g_offset 12))
									rx2 (get-int32-le buf (+ g_offset 16))
									ry2 (get-int32-le buf (+ g_offset 20)))
								(setq g_offset (+ g_offset 24)
									bytes_read (+ bytes_read 28))
								(push coords_x rx rx1 rx2)
								(push coords_y ry ry1 ry2)
								(push commands (list type rx ry rx1 ry1 rx2 ry2)))
							(:t
								(setq bytes_read len))))
					(setq min_x (reduce min coords_x (first coords_x))
						max_x (reduce max coords_x (first coords_x))
						min_y (reduce min coords_y (first coords_y))
						max_y (reduce max coords_y (first coords_y)))))
			(push glyphs (scatter (Lmap)
				:char_code c
				:offset glyph_offset
				:advance width
				:min_x min_x
				:max_x max_x
				:min_y min_y
				:max_y max_y
				:commands commands))
			(++ c)
			(++ offsets_i))
		(push pages (scatter page_db :glyphs glyphs))
		(++ pages_i))
	(scatter font_db :pages pages))

(defun load-ctf (file)
	(if (defq buf (load file))
		(scatter (load-ctf-buf buf) :file file)
		:nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Writer and Printer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun write-ctf (font_db file)
	(if (and font_db (defq stream (file-stream file +file_open_write)))
		(progn
			(defq ascent (. font_db :find :ascent)
				descent (. font_db :find :descent)
				pages (. font_db :find :pages))
			; Write header
			(write-char stream ascent +int_size)
			(write-char stream descent +int_size)
			
			; Calculate glyph offsets
			; Header is 8 bytes
			(defq current_offset 8)
			(each (lambda (page_db)
				(defq start (. page_db :find :start)
					end (. page_db :find :end)
					count (inc (- end start)))
				(setq current_offset (+ current_offset 8 (* count 4))))
				pages)
			; Add 4 bytes for the sentinel (0)
			(setq current_offset (+ current_offset 4))
			
			; Write the page tables and populate the offsets
			(each (lambda (page_db)
				(defq start (. page_db :find :start)
					end (. page_db :find :end)
					glyphs (. page_db :find :glyphs))
				(write-char stream end +int_size)
				(write-char stream start +int_size)
				(each (lambda (glyph_db)
					(defq commands (. glyph_db :find :commands)
						len 0)
					(each (lambda (cmd)
						(defq type (first cmd))
						(if (= type 2)
							(setq len (+ len 28))
							(setq len (+ len 12))))
						commands)
					(. glyph_db :insert :offset current_offset)
					(write-char stream current_offset +int_size)
					(setq current_offset (+ current_offset 8 len)))
					glyphs))
				pages)
			
			; Write sentinel
			(write-char stream 0 +int_size)
			
			; Write the glyph data
			(each (lambda (page_db)
				(defq glyphs (. page_db :find :glyphs))
				(each (lambda (glyph_db)
					(defq width (. glyph_db :find :advance)
						commands (. glyph_db :find :commands)
						len 0)
					(each (lambda (cmd)
						(defq type (first cmd))
						(if (= type 2)
							(setq len (+ len 28))
							(setq len (+ len 12))))
						commands)
					(write-char stream width +int_size)
					(write-char stream len +int_size)
					(each (lambda (cmd)
						(defq type (first cmd))
						(write-char stream type +int_size)
						(if (= type 2)
							(progn
								(write-char stream (second cmd) +int_size)
								(write-char stream (third cmd) +int_size)
								(write-char stream (elem-get cmd 3) +int_size)
								(write-char stream (elem-get cmd 4) +int_size)
								(write-char stream (elem-get cmd 5) +int_size)
								(write-char stream (elem-get cmd 6) +int_size))
							(progn
								(write-char stream (second cmd) +int_size)
								(write-char stream (third cmd) +int_size))))
						commands))
					glyphs))
				pages)
			(stream-flush stream)
			:t)
		:nil))

(defun print-font (font_db verbosity)
	(when font_db
		(print "File: " (. font_db :find :file))
		(print "\tType: " (. font_db :find :type) " (ChrysaLisp Vector Font)")
		(print "\tAscent: " (format-fixed-24 (. font_db :find :ascent)))
		(print "\tDescent: " (format-fixed-24 (. font_db :find :descent)))
		(defq pages (. font_db :find :pages) total_glyphs 0)
		(each (lambda (page_db)
			(defq glyphs (. page_db :find :glyphs))
			(setq total_glyphs (+ total_glyphs (length glyphs))))
			pages)
		(print "\tTotal Pages: " (length pages))
		(each (lambda (page_db)
			(defq start (. page_db :find :start)
				end (. page_db :find :end)
				glyphs (. page_db :find :glyphs)
				count (length glyphs))
			(print "\tPage Range: " start " - " end " (Glyphs: " count ")")
			(when (> verbosity 0)
				(each (lambda (glyph_db)
					(defq c (. glyph_db :find :char_code)
						offset (. glyph_db :find :offset)
						width (. glyph_db :find :advance)
						min_x (. glyph_db :find :min_x)
						min_y (. glyph_db :find :min_y)
						max_x (. glyph_db :find :max_x)
						max_y (. glyph_db :find :max_y)
						commands (. glyph_db :find :commands))
					(defq g_width (- max_x min_x)
						g_height (- max_y min_y)
						char_str (if (<= 32 c 126) (cat "'" (char c) "'") "?"))
					(print "\t\tGlyph " char_str " (" c "): Offset: " offset
						"\n\t\t\tAdvance: " (format-fixed-24 width)
						"\n\t\t\tBounds: [" (format-fixed-24 min_x) ", " (format-fixed-24 min_y) "]"
						" to [" (format-fixed-24 max_x) ", " (format-fixed-24 max_y) "]"
						"\n\t\t\tSize: " (format-fixed-24 g_width) " x " (format-fixed-24 g_height))
					(when (and (> verbosity 1) (nempty? commands))
						(print "\t\t\tCommands:")
						(each (lambda (cmd)
							(defq type (first cmd))
							(cond
								((= type 0)
									(print "\t\t\t\tMoveto: [" (format-fixed-24 (second cmd)) ", " (format-fixed-24 (third cmd)) "]"))
								((= type 1)
									(print "\t\t\t\tLineto: [" (format-fixed-24 (second cmd)) ", " (format-fixed-24 (third cmd)) "]"))
								((= type 2)
									(print "\t\t\t\tCurveto: [" (format-fixed-24 (second cmd)) ", " (format-fixed-24 (third cmd)) "]"
										" (Control: [" (format-fixed-24 (elem-get cmd 3)) ", " (format-fixed-24 (elem-get cmd 4)) "],"
										" [" (format-fixed-24 (elem-get cmd 5)) ", " (format-fixed-24 (elem-get cmd 6)) "])"))))
							commands)))
					glyphs)))
			pages)
		(print "\tTotal Glyphs: " total_glyphs)
		(print)))

(defun process-otf-ttf (file type)
	(if (defq buf (load file))
		(progn
			(defq otf_db (parse-otf-tables buf)
				version (. otf_db :find :version)
				tables (. otf_db :find :tables))
			(print "File: " file)
			(print "\tType: " type " (OpenType/TrueType Font)")
			(print "\tVersion: " (long-to-hex-str version))
			
			; Read and print metrics
			(defq units_per_em (get-otf-head buf tables))
			(bind '(ascender descender num_h_metrics) (get-otf-hhea buf tables))
			(defq num_glyphs (get-otf-maxp buf tables))
			(print "\tUnitsPerEm: " units_per_em)
			(print "\tAscender: " ascender " (" (format-fixed-24 (/ (* ascender 16777216) units_per_em)) ")")
			(print "\tDescender: " descender " (" (format-fixed-24 (/ (* descender 16777216) units_per_em)) ")")
			(print "\tNumGlyphs: " num_glyphs)
			(print "\tNumberOfHMetrics: " num_h_metrics)
			(print))
		(print "Error: Cannot open font file " file)))

(defun process-file (file verbosity)
	(cond
		((ends-with ".ctf" file)
			(if (defq font_db (load-ctf file))
				(print-font font_db verbosity)
				(print "Error: Cannot open font file " file)))
		((or (ends-with ".otf" file) (ends-with ".ttf" file))
			(process-otf-ttf file (if (ends-with ".otf" file) "OTF" "TTF")))
		(:t
			(print "Error: Unsupported font file format " file)
			(print))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_c :nil opt_v 0 args (options stdio usage)))
		(defq files (rest args))
		(if (empty? files)
			(progn
				(defq temp_list (list))
				(lines! (# (push temp_list %0)) (io-stream 'stdin))
				(setq files temp_list)))
		(if opt_c
			(each (lambda (file)
				(if (ends-with ".ctf" file)
					(if (defq font_db (load-ctf file))
						(if (write-ctf font_db (cat file ".new"))
							(print "Wrote binary identical font: " (cat file ".new"))
							(print "Error: Failed to write " (cat file ".new")))
						(print "Error: Cannot open font file " file))
					(print "Error: -c option only supported for .ctf files currently")))
				files)
			(each (# (process-file %0 opt_v)) files))))