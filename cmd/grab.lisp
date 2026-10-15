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

    What is on the GUI's screen now, of a window or of all of
    it, written to the file, as its ending says, .png or .cpm.
    It is read by the GUI from its back buffer, the last frame
    it drew, by a GUI of this network.")
(("-t" "--title") ,(opt-str 'opt_t))
))

(defun main ()
	(and (defq stdio (create-stdio))
		(defq opt_t :nil args (options stdio usage))
		(cond
			((< (length args) 2) (print "grab: what file ?"))
			((not (defq grabbed (gui-grab-rpc opt_t)))
				(print "grab: no GUI"))
			((str? grabbed) (print "grab: " grabbed))
			(:t (bind '(w h pixels) grabbed)
				(defq canvas (Canvas w h 1) pixmap (getf canvas +canvas_pixmap 0))
				(setf pixmap +pixmap_type 32 0)
				(pixmap-read pixmap (string-stream pixels) 32)
				(if (canvas-save canvas (second args) 32)
					(print "grab: " w " by " h " to " (second args))
					(print "grab: could not write " (second args)))))))
