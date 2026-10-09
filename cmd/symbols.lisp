(import "lib/options/options.inc")
(import "gui/lisp.inc")
(import "lib/font/symbol_set.inc")

(defq usage `(
(("-h" "--help")
"Usage: symbols [options]

    options:
        -h --help: this help info.
        -l --list: list the symbols, each with its code.

    Make the symbol fonts, fonts/Symbols*.ctf, one for each theme, and
    the names of the symbols, lib/consts/symbols.inc, from the symbols
    of lib/font/symbol_set.inc.

    A theme is the same symbols with another weight of stroke, and other
    ends and corners. Run it after a symbol is changed or added.")
(("-l" "--list") ,(opt-flag 'opt_l))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_l :nil args (options stdio usage)))
		(cond
			(opt_l (each (lambda ((name &ignore))
				(print "0x" (sym-code-hex (+ +sf_base (!))) " " name)) *symbols*))
			(:t (each (lambda ((file radius joint cap))
					(defq font (sym-font *symbols* radius joint cap))
					(save font file)
					(print file ", " (length *symbols*) " symbols, " (length font) " bytes"))
					*sym_themes*)
				(save (sym-names-text) "lib/consts/symbols.inc")
				(print "lib/consts/symbols.inc")))))
