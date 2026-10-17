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
        -a --all: every property, the working state of each view
            too, which is left out unless asked for: caches, a
            drag, a pressed state, mailboxes.
        -r --raw: the tree as (ui-save) writes it, not a line a
            view.

    The tree of widgets of a window on the GUI's screen, or of
    the screen, as text, from the GUI of this network: a view a
    line, as deep in as it is, its type, its bounds in its
    parent, and its properties, key=value. A value with bytes
    that are not text in it, a mailbox, is said as <n bytes>.")
(("-t" "--title") ,(opt-str 'opt_t))
(("-f" "--find") ,(opt-str 'opt_f))
(("-d" "--depth") ,(opt-num 'opt_d))
(("-k" "--keys") ,(opt-str 'opt_k))
(("-w" "--wait") ,(opt-num 'opt_w))
(("-a" "--all") ,(opt-flag 'opt_a))
(("-r" "--raw") ,(opt-flag 'opt_r))
))

;what a view keeps of what it is doing, not what it is or how it is laid
;out, left out unless -a. The less there is to read the better, Chris:
;"usage is up since we do the screen grabbing"
(defq +working ''(:sym_cache :text_cache :offset :state :last_buttons :is_window
		:drag_mode :drag_offx :drag_offy :tip_mbox)
	+char_class_control (char-class (apply (const cat) (map (const char) (range 0 32)))))

(defun value-text (v)
	; (value-text v) -> str
	;a value as text, or how many bytes it is if some are not text
	(defq text (str v))
	(if (< (bskipn +char_class_control text 0) (length text))
		(cat "<" (str (length text)) " bytes>")
		text))

(defun lines-of (tree all)
	;a view a line, those in it after it, each a tab further in. What is
	;still to print is a list, the next last, no function here calls itself
	(defq todo (list (list tree 0)))
	(while (defq next (pop todo))
		(bind '(view depth) next)
		(defq line (list (pad "" depth (ascii-char 9)) (slice (str (. view :find :type)) 1 -1)
			" " (join (map (const str) (. view :find :bounds)) " ")))
		(. view :each (lambda (k v)
			(unless (or (find k '(:type :bounds :children)) (and (not all) (find k +working)))
				(push line " " (slice (str k) 1 -1) "=" (value-text v)))))
		(print (apply (const cat) line))
		(reach (# (push todo (list %0 (inc depth)))) (ifn (. view :find :children) (list)))))

(defun main ()
	(when (and (defq stdio (create-stdio))
			(defq opt_t :nil opt_f :nil opt_d :nil opt_k :nil opt_w 0 opt_a :nil opt_r :nil args (options stdio usage)))
		;key=value, split at the first =, so a value may hold one, text==
		(defq kv (if opt_f (ifn (defq eq_idx (find "=" opt_f)) (list opt_f)
				(list (slice opt_f 0 eq_idx) (slice opt_f (inc eq_idx) -1)))
			(list)))
		(defq keys (if opt_k (map (# (sym (cat ":" %0))) (split opt_k ","))))
		(defq text (gui-wait (# (gui-tree-rpc opt_t (first kv) (second kv) opt_d keys)) opt_w))
		(cond
			((not text) (print "widgets: no GUI"))
			((or opt_r (not (starts-with "(" text))) (print text))
			(:t (lines-of (tree-load (string-stream text)) opt_a)))))
