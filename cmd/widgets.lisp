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
        -w --wait num: seconds to wait for the window, as an app
            just launched has none yet, default 0.
        -r --raw: the tree as (ui-save) writes it, not a line a
            view.

    The tree of widgets of a window on the GUI's screen, or of
    the screen, as text, from the GUI of this network: a view a
    line, as deep in as it is, its type, its bounds in its
    parent, and its properties, key=value.")
(("-t" "--title") ,(opt-str 'opt_t))
(("-f" "--find") ,(opt-str 'opt_f))
(("-d" "--depth") ,(opt-num 'opt_d))
(("-k" "--keys") ,(opt-str 'opt_k))
(("-w" "--wait") ,(opt-num 'opt_w))
(("-r" "--raw") ,(opt-flag 'opt_r))
))

(defun lines-of (tree)
	;a view a line, those in it after it, each a tab further in. What is
	;still to print is a list, the next last, no function here calls itself
	(defq todo (list (list tree 0)))
	(while (defq next (pop todo))
		(bind '(view depth) next)
		(defq line (list (pad "" depth (ascii-char 9)) (slice (str (. view :find :type)) 1 -1)
			" " (join (map (const str) (. view :find :bounds)) " ")))
		(. view :each (lambda (k v)
			(unless (find k '(:type :bounds :children))
				(push line " " (slice (str k) 1 -1) "=" (str v)))))
		(print (apply (const cat) line))
		(each (# (push todo (list %0 (inc depth)))) (reverse (ifn (. view :find :children) (list))))))

(defun main ()
	(when (and (defq stdio (create-stdio))
			(defq opt_t :nil opt_f :nil opt_d :nil opt_k :nil opt_w 0 opt_r :nil args (options stdio usage)))
		(defq kv (if opt_f (split opt_f "=") (list)))
		(defq keys (if opt_k (map (# (sym (cat ":" %0))) (split opt_k ","))))
		(defq text (gui-wait (# (gui-tree-rpc opt_t (first kv) (second kv) opt_d keys)) opt_w))
		(cond
			((not text) (print "widgets: no GUI"))
			((or opt_r (not (starts-with "(" text))) (print text))
			(:t (lines-of (tree-load (string-stream text)))))))
