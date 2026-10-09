(import "lib/options/options.inc")
(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/cwb/palette.inc")

(defq usage `(
(("-h" "--help")
"Usage: cwb [options] file.cwb

    options:
        -h --help: this help info.
        -n --new size: an empty document of that size, 800x600, in
            place of what the file has, if there is one.
        -e --eval lisp: do this to it. The Lisp has board, the board
            of the document, lib/cwb/board.inc, and doc, the document,
            lib/cwb/doc.inc. Quote it for the shell.
        -s --script path: as -e, the Lisp is in a file.
        -p --pointers path: play a file of pointer events to the
            board, as a pen, a mouse and fingers would give them.
        -i --info: list what is in it.
        -o --out path: draw it to a picture, a .tga or a .cpm.
        -z --zoom num: the picture is that many times the size, 1.
        -b --back colour: the picture has that behind it, a number,
            0xffffffff is white. Default what the document has, which
            is nothing unless it was given one.
        -k --keep: do not save the file, whatever was done to it.

    Make, change, look at and draw a whiteboard document without a
    whiteboard. What is done is done in the order above, and then the
    file is saved if -n, -e, -s or -p changed it.

    A shape is an SVG path, text. Colours are 0xAARRGGBB.

        cwb -n 400x300 a.cwb -e \q(cwb-add doc (cwb-shape (cwb-d-rect 20 20 200 120 12) :fill 0xffffd070))\q
        cwb a.cwb -e \q(cwb-add doc (cwb-text {Start} 60 80))\q -i
        cwb a.cwb -o a.tga -z 2 -b 0xffffffff

    A line of a pointers file is the events of one moment, one or more,
    with ; between: id kind buttons x y. kind is mouse, pen, eraser or
    touch. buttons is 0 for up. # starts a note. Each line is a sixtieth
    of a second after the last, for what on the board moves by itself,
    and a line that is wait and a number is that many thousandths more.

        1 pen 1 100 100
        1 pen 1 180 140 ; 7 touch 1 400 300
        1 pen 0 180 140 ; 7 touch 0 400 300
        wait 500

    A hand that taps where there is nothing opens a palette there, as
    on the app's board, the right button of a mouse, or a finger.")
(("-n" "--new") ,(opt-str 'opt_n))
(("-e" "--eval") ,(opt-str 'opt_e))
(("-s" "--script") ,(opt-str 'opt_s))
(("-p" "--pointers") ,(opt-str 'opt_p))
(("-i" "--info") ,(opt-flag 'opt_i))
(("-o" "--out") ,(opt-str 'opt_o))
(("-z" "--zoom") ,(opt-num 'opt_z))
(("-b" "--back") ,(opt-num 'opt_b))
(("-k" "--keep") ,(opt-flag 'opt_k))
))

(defun size-of (text)
	;800x600 as (800 600), :nil if it is not that
	(defq nums (filter (# (and %0 (> %0 0))) (map (const str-to-num)
		(split text (const (char-class " xX,*"))))))
	(if (= (length nums) 2) (list (n2i (first nums)) (n2i (second nums)))))

(defun tenth (n)
	;a number to the nearest tenth, as text
	(defq n (n2i (floor (+ (* (n2f n) 10.0) 0.5))) whole (/ (abs n) 10) part (% (abs n) 10))
	(cat (if (< n 0) "-" "") (str whole) (if (= part 0) "" (cat "." (str part)))))

(defun pointer-events (line)
	;the events of a line of a pointers file, :nil if it has none
	(defq text (first (split (cat line " #") "#")) events (list))
	(each (lambda (part)
		(defq words (split part (const (char-class " \t\r"))))
		(when (= (length words) 5)
			(bind '(id kind buttons x y) words)
			(push events (ptr-event (str-to-num id) (sym (cat ":" kind)) (str-to-num buttons)
				(str-to-num x) (str-to-num y)))))
		(split text ";"))
	(if (nempty? events) events))

(defun info (doc)
	;what is in a document, a line for each item, those in a group under it
	(print "size " (. doc :find :width) "x" (. doc :find :height)
		" background " (. doc :find :background) " grid " (. doc :find :grid))
	(each (lambda ((name flags items))
		(print "layer " (!) " \q" name "\q"
			(if (bits? flags 1) " hidden" "") (if (bits? flags 2) " locked" "")
			" items " (length items))
		;a group, then what is in it, a list of what is left to say
		(defq todo (map (# (list %0 1)) (reverse items)))
		(while (defq next (pop todo))
			(bind '(item depth) next)
			(defq box (cwb-bounds (list item)))
			(print (pad "" (* depth 2)) (elem-get item +cwb_id) " "
				(if (cwb-group? item) "group" (rest (str (elem-get item +cwb_kind))))
				(if (nempty? (elem-get item +cwb_name)) (cat " \q" (elem-get item +cwb_name) "\q") "")
				(if box (cat " box " (join (map (const tenth) box) " ")) "")
				(cond
					((cwb-group? item) (cat " items " (str (length (elem-get item +cwb_items)))))
					((eql (elem-get item +cwb_kind) :text) (cat " \q" (elem-get item +cwb_text) "\q"))
					(:t (cat " d " (if (> (length (elem-get item +cwb_d)) 60)
						(cat (slice (elem-get item +cwb_d) 0 60) "...") (elem-get item +cwb_d))))))
			(if (cwb-group? item)
				(each (# (push todo (list %0 (inc depth)))) (reverse (elem-get item +cwb_items))))))
		(cwb-layers doc)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_n :nil opt_e :nil opt_s :nil opt_p :nil opt_i :nil opt_o :nil
				opt_z :nil opt_b :nil opt_k :nil
				args (options stdio usage)))
		(defq file (second args) changed :nil doc :nil)
		(cond
			((not file) (print "A .cwb file is needed, cwb -h."))
			((and opt_n (not (size-of opt_n))) (print "Not a size, 800x600: " opt_n))
			((and (not opt_n) (not (setq doc (cwb-load (file-stream file)))))
				(print "Not a whiteboard document: " file))
			(:t (when opt_n
					(setq doc (apply (const cwb-doc) (size-of opt_n)) changed :t))
				(defq board (palette-enable (Board doc)) clock 0)
				;what is done to it, in an environment that has board and doc
				(each (lambda (text)
					(when text
						(setq changed :t)
						(catch (repl (string-stream text) "cwb")
							(progn (print "Error: " (str _)) :t))))
					(list opt_e (if opt_s (load opt_s))))
				(when opt_p
					(setq changed :t)
					;the time is told to the board as the lines go, microseconds
					(lines! (lambda (line)
						(defq words (split line (const (char-class " \t\r"))))
						(if (and (= (length words) 2) (eql (first words) "wait") (defq ms (str-to-num (second words))))
							(setq clock (+ clock (* (n2i ms) 1000))))
						(if (defq events (pointer-events line)) (. board :pointers events))
						(. board :tick (setq clock (+ (max clock (get :time board)) 16667)))
						:nil) (file-stream opt_p)))
				(setq doc (. board :get_doc))
				(if opt_i (info doc))
				(when opt_o
					(defq zoom (n2f (ifn opt_z 1)) w (max 1 (n2i (* (n2f (. doc :find :width)) zoom)))
						h (max 1 (n2i (* (n2f (. doc :find :height)) zoom)))
						canvas (Canvas w h 1) m (if (= zoom 1.0) :nil (cwb-mat-scale zoom)))
					(.-> canvas (:set_canvas_flags +canvas_flag_antialias)
						(:fill (ifn opt_b (. doc :find :background))))
					(. board :draw canvas m)
					;what is on the board and not of the document is drawn too: the
					;handles of what is selected, a ruler, a palette, as it is at
					;the time it now is
					(. board :tick (max clock (get :time board)))
					(def board :zoom zoom)
					(. board :draw_overlay canvas m)
					(. board :draw_actors canvas m)
					(if (canvas-save canvas opt_o 32)
						(print opt_o " " w "x" h)
						(print "Not a kind of picture that can be made, .tga or .cpm: " opt_o)))
				(when (and changed (not opt_k))
					(cwb-save doc (file-stream file +file_open_write)))))))
