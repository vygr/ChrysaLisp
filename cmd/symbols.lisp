(import "lib/options/options.inc")
(import "gui/lisp.inc")
(import "lib/font/symbol_set.inc")

(defq usage `(
(("-h" "--help")
"Usage: symbols [options] [name] ...

    options:
        -h --help: this help info.
        -l --list: list the symbols, each with its code.
        -p --picture file: a picture of the symbols, .tga or
            .cpm, from lib/font/symbol_set.inc as it is now,
            before the fonts are made. A block for each theme,
            in the order of the themes, Symbols, Light, Bold,
            Sharp: a row at the size of a toolbar, as close as
            on one, then the same, big. Only those whose names
            begin with a name given, media_ say, else all.

    Make the symbol fonts, fonts/Symbols*.ctf, one for each theme, and
    the names of the symbols, lib/consts/symbols.inc, from the symbols
    of lib/font/symbol_set.inc.

    A theme is the same symbols with another weight of stroke, and other
    ends and corners. Run it after a symbol is changed or added.")
(("-l" "--list") ,(opt-flag 'opt_l))
(("-p" "--picture") ,(opt-str 'opt_p))
))

(defq +pic_small 28 +pic_big 56 +pic_per 16 +pic_ink 0xff202020 +pic_paper 0xfff4f4f0)

(defun pic-row (canvas font codes x y step)
	;the symbols of those codes, each a glyph filled as the GUI fills one,
	;all of its outlines at once, so that a hole is a hole
	(each (lambda (code)
		(.-> canvas (:set_color +pic_ink)
			(:fpoly (n2f (+ x (* (!) step))) (n2f y) +winding_none_zero
				(map (# (path-transform (const (fixeds 0.5 0.0 0.0 0.0 0.5 0.0)) %0 %0))
					(font-glyph-paths font (num-to-utf8 code))))))
		codes))

(defun picture (file names)
	; (picture file names) -> :nil | (width height)
	(defq codes (list) lines (list))
	(each (lambda ((name &ignore))
			(if (or (empty? names) (some (# (starts-with %0 name)) names)) (push codes (+ +sf_base (!)))))
		*symbols*)
	(unless (empty? codes)
		(defq lines (partition codes +pic_per) per (min +pic_per (length codes))
			block (+ (* (length lines) (+ 6 +pic_small)) (* (length lines) (+ 8 +pic_big)) 16)
			w (+ 20 (* per (+ 8 +pic_big))) h (* block (length *sym_themes*))
			canvas (Canvas w h 1) at 0)
		(.-> canvas (:set_canvas_flags +canvas_flag_antialias) (:fill +pic_paper))
		(each (lambda ((font_file radius joint cap))
			;a font made of the set as it is, under a name of its own, as
			;a font is kept by its name once made
			(defq tmp (cat "obj/symbols_picture_" (str (pii-pid)) "_" (str (!)) ".ctf") y (+ 8 (* (!) block)))
			(save (sym-font *symbols* radius joint cap) tmp)
			(defq small (create-font tmp +pic_small) big (create-font tmp +pic_big))
			(each (lambda (codes)
					(pic-row canvas small codes 12 (setq y (+ y +pic_small 6)) (+ +pic_small 2)))
				lines)
			(each (lambda (codes)
					(pic-row canvas big codes 10 (setq y (+ y +pic_big 8)) (+ +pic_big 8)))
				lines)
			(pii-remove tmp))
			*sym_themes*)
		(if (canvas-save canvas file 32) (list w h))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(and
			(defq stdio (create-stdio))
			(defq opt_l :nil opt_p :nil args (options stdio usage))
		(cond
			(opt_l (each (lambda ((name &ignore))
				(print "0x" (sym-code-hex (+ +sf_base (!))) " " name)) *symbols*))
			(opt_p (if (defq size (picture opt_p (rest args)))
				(print opt_p " " (first size) "x" (second size))
				(print "No symbols of those names, or not a kind of picture that can be made, .tga or .cpm: " opt_p)))
			(:t (each (lambda ((file radius joint cap))
					(defq font (sym-font *symbols* radius joint cap))
					(save font file)
					(print file ", " (length *symbols*) " symbols, " (length font) " bytes"))
					*sym_themes*)
				(save (sym-names-text) "lib/consts/symbols.inc")
				(print "lib/consts/symbols.inc")))))
