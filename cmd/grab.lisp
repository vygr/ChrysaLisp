(import "lib/options/options.inc")
(import "gui/canvas/lisp.inc")
(import "lib/image/png.inc")
(import "lib/image/cpm.inc")

(defq usage `(
(("-h" "--help")
"Usage: grab [options] file

    options:
        -h --help: this help info.
        -t --title str: the window whose title has this in it,
            default all of the screen.
        -f --find key=value: only the first view in it whose key
            is value, type=Radiobar, tip_text=snap, depth first,
            the part of it that can be seen.
        -w --wait num: seconds to wait for the window, as an app
            just launched has none yet, default 0.

    What is on the GUI's screen now, of a window, of a view in
    it, or of all of it, written to the file, as its ending
    says, .png or .cpm. It is read by the GUI from its back
    buffer, the last frame it drew, by a GUI of this network.
    The less there is of it the less it is to look at: a bar,
    not the window, if the bar is the question.")
(("-t" "--title") ,(opt-str 'opt_t))
(("-f" "--find") ,(opt-str 'opt_f))
(("-w" "--wait") ,(opt-num 'opt_w))
))

(defun main ()
	(when (and (defq stdio (create-stdio))
			(defq opt_t :nil opt_f :nil opt_w 0 args (options stdio usage)))
		;a view in the window: where it is seen, and that is what is grabbed
		;key=value, split at the first =, so a value may hold one, text==
		(defq kv (if opt_f (ifn (defq eq_idx (find "=" opt_f)) (list opt_f)
				(list (slice opt_f 0 eq_idx) (slice opt_f (inc eq_idx) -1))))
			at (if kv (gui-wait (# (gui-locate-rpc opt_t (first kv) (second kv))) opt_w)))
		(cond
			((< (length args) 2) (print "grab: what file ?"))
			((and kv (not at)) (print "grab: no GUI"))
			((str?? at) (print "grab: " at))
			((not (defq grabbed (gui-wait (# (gui-grab-rpc opt_t (if at (slice at 4 8)))) opt_w)))
				(print "grab: no GUI"))
			((str?? grabbed) (print "grab: " grabbed))
			(:t (bind '(w h pixels) grabbed)
				(defq canvas (Canvas w h 1) pixmap (getf canvas +canvas_pixmap 0))
				(setf pixmap +pixmap_type 32 0)
				(pixmap-read pixmap (string-stream pixels) 32)
				(if (canvas-save canvas (second args) 32)
					(print "grab: " w " by " h " to " (second args))
					(print "grab: could not write " (second args)))))))
