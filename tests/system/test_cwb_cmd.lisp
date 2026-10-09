(report-header "cwb: a whiteboard document made, changed, listed and drawn from a command line")

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/task/pipe.inc")
(import "lib/cwb/doc.inc")

(defun cc-run (cmdline)
	;what a command line says, its lines
	(defq out (list))
	(pipe-run cmdline (# (push out %0)))
	(filter (const nempty?) (split (apply (const cat) (cat (list "") out)) (ascii-char 10))))

(defq cc_file "tests/scratch/test_cwb_cmd.cwb" cc_pic "tests/scratch/test_cwb_cmd.tga"
	cc_ptr "tests/scratch/test_cwb_cmd.txt")
(each (# (if (pii-fstat %0) (pii-remove %0))) (list cc_file cc_pic cc_ptr))

;a new document, and Lisp that puts a box and words in it
(defq cc_out (cc-run (cat "cwb -n 640x400 " cc_file
	" -e \q(cwb-add doc (cwb-shape (cwb-d-rect 20 120 200 220 12) :fill 0xffffd070 :stroke 0xff000000 :width 3))"
	" (cwb-add doc (cwb-text {Start} 60 185 :font_size 24))\q -i")))
(assert-eq "a new document is the size asked for" "size 640x400 background 0 grid 32" (first cc_out))
(assert-eq "the Lisp given put two things in it" "layer 0 \qLayer 1\q items 2" (second cc_out))
(assert-true "a box, listed with its id, what it is, the box round it and its path"
	(starts-with "  1 path box 18.5 118.5 201.5 221.5 d M 32 120 L 188 120 A 12 12" (third cc_out)))
(assert-true "and words" (and (starts-with "  2 text box " (elem-get cc_out 3)) (ends-with "\qStart\q" (elem-get cc_out 3))))
(defq cc_doc (cwb-load (file-stream cc_file)))
(assert-true "it was saved, and loads" (and cc_doc (= (length (cwb-items cc_doc)) 2)))

;pointers played to it: a finger holds a ruler while a pen is run along it, and the mouse draws by hand
(save (cat "# a note" (ascii-char 10)
	"7 touch 1 300 330" (ascii-char 10)
	"1 pen 1 150 286 ; 7 touch 1 300 330" (ascii-char 10)
	"1 pen 1 300 281 ; 7 touch 1 300 330" (ascii-char 10)
	"1 pen 1 480 288 ; 7 touch 1 300 330   # two at once" (ascii-char 10)
	"1 pen 0 480 288 ; 7 touch 0 300 330" (ascii-char 10)
	"0 mouse 1 60 60" (ascii-char 10) "0 mouse 1 120 100" (ascii-char 10)
	"0 mouse 1 200 50" (ascii-char 10) "0 mouse 0 200 50" (ascii-char 10)) cc_ptr)
(defq cc_out (cc-run (cat "cwb " cc_file " -e \q(. (. board :get_stage) :add (Ruler board 320 330))\q -p " cc_ptr
	" -i -o " cc_pic " -b 0xffffffff")))
(assert-eq "two more things were drawn" "layer 0 \qLayer 1\q items 4" (second cc_out))
(assert-true "the pen run along the ruler drew a straight line along its side"
	(and (starts-with "  3 line box 148.5 288.5 481.5 291.5 d M 149.99" (elem-get cc_out 4)) (found? (elem-get cc_out 4) " 290 L 479.99")))
(assert-eq "the mouse drew a line by hand" "  4 pen box 58.5 48.5 201.5 86.1 d M 60 60 Q 120 100 160 75 L 200 50" (elem-get cc_out 5))
(assert-eq "and a picture of it was made" (cat cc_pic " 640x400") (last cc_out))
(assert-list-eq "the picture is a .tga of that size" '(640 400 32) (canvas-info cc_pic))
(defq cc_canvas (canvas-load cc_pic +load_flag_noswap))
(defun cc-pixel (canvas x y)
	(defq stream (memory-stream))
	(pixmap-write (getf canvas +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(bind '(w h) (. canvas :pref_size))
	(defq d (read-blk stream 10000000))
	(get-uint d (+ (- (length d) (* w h 4)) (* 4 (+ (* y w) x)))))
(when cc_canvas
	(assert-eq "the box is its colour in it" 0xffffd070 (cc-pixel cc_canvas 40 140))
	(assert-eq "what is behind is what was asked for" 0xffffffff (cc-pixel cc_canvas 600 20))
	(assert-true "the line along the ruler is there, black" (< (logand (cc-pixel cc_canvas 300 290) 0xffffff) 0x101010))
	(assert-true "and the ruler, which is on the board and not in the document, is drawn too"
		(/= 0xffffffff (cc-pixel cc_canvas 200 350))))

;changed and not saved
(defq cc_out (cc-run (cat "cwb " cc_file " -e \q(. board :select (list 1 2)) (. board :group)\q -i -k")))
(assert-list-eq "grouped, there are three things, two in one" '("layer 0 \qLayer 1\q items 3" "  5 group")
	(list (second cc_out) (slice (third cc_out) 0 9)))
(assert-true "what is in a group is listed under it" (starts-with "    1 path" (elem-get cc_out 3)))
(assert-eq "with -k the file is as it was" 4 (length (cwb-items (cwb-load (file-stream cc_file)))))

;the palette, by pointers alone: the right button down and up on nothing opens
;it, time goes by, a tap on the wedge that is a box, 54 below its middle, and
;what the left button then draws is a box
(save (cat "0 mouse 4 320 200" (ascii-char 10) "0 mouse 0 320 200" (ascii-char 10)
	"wait 400" (ascii-char 10)) cc_ptr)
(defq cc_out (cc-run (cat "cwb " cc_file " -p " cc_ptr " -o " cc_pic " -b 0xffffffff -k")))
(defq cc_canvas (canvas-load cc_pic +load_flag_noswap))
(when cc_canvas
	(assert-true "a palette opened by a pointers file is in the picture, dark, where one of its wedges is"
		(every (# (< (logand (>> (cc-pixel cc_canvas 343 69) %0) 0xff) 0x60)) '(0 8 16)))
	(assert-eq "and not off it" 0xffffffff (cc-pixel cc_canvas 600 380)))
(save (cat "0 mouse 4 320 200" (ascii-char 10) "0 mouse 0 320 200" (ascii-char 10)
	"wait 400" (ascii-char 10)
	"0 mouse 1 320 254" (ascii-char 10) "0 mouse 0 320 254" (ascii-char 10)
	"wait 300" (ascii-char 10)
	"0 mouse 1 420 40" (ascii-char 10) "0 mouse 1 600 100" (ascii-char 10) "0 mouse 0 600 100" (ascii-char 10)) cc_ptr)
(defq cc_out (cc-run (cat "cwb " cc_file " -p " cc_ptr " -i -o " cc_pic " -b 0xffffffff -k")))
(assert-eq "a tap on its box wedge, and the mouse draws a box" "  5 rect box 418.5 38.5 601.5 101.5 d M 420 40 L 600 40 600 100 420 100 Z"
	(elem-get cc_out 6))
(defq cc_canvas (canvas-load cc_pic +load_flag_noswap))
(when cc_canvas
	(assert-eq "the palette was put away by it, and is not in the picture" 0xffffffff (cc-pixel cc_canvas 343 69)))

;what it will not do
(assert-list-eq "a file that is not one" (list (cat "Not a whiteboard document: " cc_ptr)) (cc-run (cat "cwb " cc_ptr " -i")))
(assert-list-eq "a size that is not one" '("Not a size, 800x600: big") (cc-run (cat "cwb -n big " cc_file)))
(assert-list-eq "no file" '("A .cwb file is needed, cwb -h.") (cc-run "cwb -i"))
(assert-true "Lisp that throws is said, and the rest is done"
	(progn (defq cc_out (cc-run (cat "cwb " cc_file " -e \q(no-such-function 1)\q -i -k")))
		(and (starts-with "Error: " (first cc_out)) (starts-with "size 640x400" (second cc_out)))))
(assert-list-eq "a picture of a kind there is no way to make" '("Not a kind of picture that can be made, .tga or .cpm: tests/scratch/x.jpg")
	(cc-run (cat "cwb " cc_file " -o tests/scratch/x.jpg -k")))
(each (# (if (pii-fstat %0) (pii-remove %0))) (list cc_file cc_pic cc_ptr "tests/scratch/x.jpg"))
