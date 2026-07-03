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

(defun load-ctf (file)
	(if (defq stream (file-stream file))
		(progn
			(defq ascent (read-uint stream)
				descent (read-uint stream)
				pages (list)
				pages_info (list)
				running :t)
			(defq font_db (scatter (Lmap)
				:file file
				:type "CTF"
				:ascent ascent
				:descent descent))
			(while running
				(defq pend (read-uint stream))
				(if (or (not pend) (= pend 0) (= pend -1))
					(setq running :nil)
					(progn
						(defq pstart (read-uint stream)
							count (inc (- pend pstart))
							offsets (list))
						(times count (push offsets (read-uint stream)))
						(push pages_info (list pstart pend offsets)))))
			(each (lambda ((start end offsets))
				(defq glyphs (list)
					page_db (scatter (Lmap)
						:start start
						:end end)
					c start)
				(each (lambda (offset)
					(stream-seek stream offset 0)
					(defq width (read-uint stream)
						len (read-uint stream)
						min_x 0 max_x 0 min_y 0 max_y 0
						commands (list))
					(if (> len 0)
						(progn
							(defq bytes_read 0 coords_x (list) coords_y (list))
							(while (< bytes_read len)
								(defq type (read-int stream))
								(if (not (num? type))
									(setq bytes_read len)
									(cond
										((or (= type 0) (= type 1))
											(defq rx (read-int stream) ry (read-int stream))
											(setq bytes_read (+ bytes_read 12))
											(push coords_x rx)
											(push coords_y ry)
											(push commands (list type rx ry)))
										((= type 2)
											(defq rx (read-int stream) ry (read-int stream)
												rx1 (read-int stream) ry1 (read-int stream)
												rx2 (read-int stream) ry2 (read-int stream))
											(setq bytes_read (+ bytes_read 28))
											(push coords_x rx rx1 rx2)
											(push coords_y ry ry1 ry2)
											(push commands (list type rx ry rx1 ry1 rx2 ry2))))))
							(setq min_x (reduce min coords_x (first coords_x))
								max_x (reduce max coords_x (first coords_x))
								min_y (reduce min coords_y (first coords_y))
								max_y (reduce max coords_y (first coords_y)))))
					(push glyphs (scatter (Lmap)
						:char_code c
						:offset offset
						:advance width
						:min_x min_x
						:max_x max_x
						:min_y min_y
						:max_y max_y
						:commands commands))
					(++ c))
					offsets)
				(push pages (scatter page_db :glyphs glyphs)))
				pages_info)
			(scatter font_db :pages pages))
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
						commands (. glyph_db :find :commands)
						g_width (- max_x min_x)
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
	(print "File: " file)
	(print "\tType: " type " (OpenType/TrueType Font)")
	(print "\tNotice: Parser / converter not yet implemented.")
	(print))

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
	(when (and
			(defq stdio (create-stdio))
			(defq opt_c :nil opt_v 0 args (options stdio usage)))
		(defq files (rest args))
		(if opt_c
			(progn
				(print "Notice: Font conversion/upgrade (-c) is planned but not yet implemented.")
				(print)))
		(if (empty? files)
			(lines! (# (process-file %0 opt_v)) (io-stream 'stdin))
			(each (# (process-file %0 opt_v)) files))))
