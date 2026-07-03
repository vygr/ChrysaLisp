;;;;;;;;;;;;;;;;;
; cmd/ctf.lisp
;;;;;;;;;;;;;;;;;
(import "lib/options/options.inc")

(defq usage `(
	(("-h" "--help")
"Usage: ctf [options] [file] ...

    options:
        -h --help: this help info.
        -v --verbosity num: verbosity level (0, 1, 2), default 0.
        -c --ctf: convert/upgrade font file to latest .ctf spec (planned).

    Inspects and outputs information about ChrysaLisp Vector Font (.ctf)
    or OpenType/TrueType (.otf/.ttf) files. If no files are specified on the
    command line, file paths are read from stdin.")
	(("-c" "--ctf") ,(opt-flag 'opt_c))
	(("-v" "--verbosity") ,(opt-num 'opt_v))
))

(defun format-fixed-24 (val)
	(defq int_part (>> val 24)
		  frac_part (logand val 0xffffff)
		  frac_dec (/ (* frac_part 10000) 16777216))
	(cat (str int_part) "." (pad frac_dec 4 "0")))

(defun process-ctf (file verbosity)
	(if (defq stream (file-stream file))
		(progn
			(defq ascent (read-uint stream)
				  descent (read-uint stream)
				  pages (list)
				  total_glyphs 0
				  running :t)
			(print "File: " file)
			(print "  Type: CTF (ChrysaLisp Vector Font)")
			(print "  Ascent: " (format-fixed-24 ascent))
			(print "  Descent: " (format-fixed-24 descent))
			(while running
				(defq pend (read-uint stream))
				(if (or (not pend) (= pend 0) (= pend -1))
					(setq running :nil)
					(progn
						(defq pstart (read-uint stream)
							  count (inc (- pend pstart))
							  offsets (list))
						(times count (push offsets (read-uint stream)))
						(push pages (list pstart pend count offsets))
						(setq total_glyphs (+ total_glyphs count)))))
			(print "  Total Pages: " (length pages))
			(each (lambda ((start end count offsets))
				(print "    Page Range: " start " - " end " (Glyphs: " count ")")
				(when (> verbosity 0)
					(defq c start)
					(each (lambda (offset)
						(stream-seek stream (+ 8 offset) 0)
						(defq width (read-uint stream)
							  len (read-uint stream)
							  min_x 0 max_x 0 min_y 0 max_y 0
							  g_width 0 g_height 0
							  char_str (if (<= 32 c 126) (cat "'" (char c) "'") "?")
							  commands (list))
						(if (> len 0)
							(progn
								(defq bytes_read 0 coords_x (list) coords_y (list))
								(while (< bytes_read len)
									(defq type (read-int stream))
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
										(push commands (list type rx ry rx1 ry1 rx2 ry2)))))
								(setq min_x (reduce min coords_x (first coords_x))
									  max_x (reduce max coords_x (first coords_x))
									  min_y (reduce min coords_y (first coords_y))
									  max_y (reduce max coords_y (first coords_y))
									  g_width (- max_x min_x)
									  g_height (- max_y min_y))))
						(print "\t\tGlyph " char_str " (" c "): Offset: " offset
							", Advance: " (format-fixed-24 width)
							", Bounds: [" min_x ", " min_y "] to [" max_x ", " max_y "]"
							", Size: " g_width " x " g_height)
						(when (and (> verbosity 1)  (> len 0))
							(print "\t\t\tCommands:")
							(each (lambda (cmd)
								(defq type (first cmd))
								(cond
									((= type 0)
										(print "\t\t\t  Moveto: [" (second cmd) ", " (third cmd) "]"))
									((= type 1)
										(print "\t\t\t  Lineto: [" (second cmd) ", " (third cmd) "]"))
									((= type 2)
										(print "\t\t\t  Curveto: [" (second cmd) ", " (third cmd) "]"
											" (Control: [" (fourth cmd) ", " (fifth cmd) "],"
											" [" (sixth cmd) ", " (seventh cmd) "])"))))
								commands))
						(++ c))
						offsets)))
				pages)
			(print "  Total Glyphs: " total_glyphs)
			(print))
		(print "Error: Cannot open font file " file)))

(defun process-otf-ttf (file type)
	(print "File: " file)
	(print "  Type: " type " (OpenType/TrueType Font)")
	(print "  Notice: Parser / converter not yet implemented.")
	(print))

(defun process-file (file verbosity)
	(cond
		((ends-with ".ctf" file)
			(process-ctf file verbosity))
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