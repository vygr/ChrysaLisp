(import "lib/options/options.inc")
(import "gui/lisp.inc")

(defq usage `(
(("-h" "--help")
"Usage: widgets [options]

    options:
        -h --help: this help info.
        -t --title str: the window whose title has this in it,
            default all of the screen.
        -f --find key=value: only the first view in it whose
            key is value, type=Button, text=Save, depth first.
        -d --depth num: the levels of children, default all; a
            view at the last says how many it has.
        -k --keys key,key: only these properties, as well as
            the type and bounds of each, default all.

    The tree of widgets of a window on the GUI's screen, or of
    the screen, as text, from the GUI of this network.")
(("-t" "--title") ,(opt-str 'opt_t))
(("-f" "--find") ,(opt-str 'opt_f))
(("-d" "--depth") ,(opt-num 'opt_d))
(("-k" "--keys") ,(opt-str 'opt_k))
))

(defun main ()
	(when (and (defq stdio (create-stdio))
			(defq opt_t :nil opt_f :nil opt_d :nil opt_k :nil args (options stdio usage)))
		(defq kv (if opt_f (split opt_f "=") (list)))
		(print (ifn (gui-tree-rpc opt_t (first kv) (second kv) opt_d
				(if opt_k (map (# (sym (cat ":" %0))) (split opt_k ","))))
			"widgets: no GUI"))))
