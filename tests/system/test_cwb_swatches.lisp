(report-header "Whiteboard swatches: the colours of a custom ink, a menu on the board that any device taps")

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/cwb/swatches.inc")

(defun sw-tap (board id kind buttons at)
	;a pointer down and up at a point
	(bind '(x y) at)
	(. board :pointers (list (ptr-event id kind buttons x y)))
	(. board :pointers (list (ptr-event id kind 0 x y))))

(defq sw_colors (swatch-colors))
(assert-eq "twelve hues in five shades and the greys" 72 (length sw_colors))
(assert-list-eq "the middle row the pure hues from red, a hue each 30 degrees"
	'(0xffff0000 0xffff7f00 0xffffff00 0xff7fff00 0xff00ff00 0xff00ff7f)
	(slice sw_colors 24 30))
(assert-list-eq "the greys from black to white" '(0xff000000 0xffffffff)
	(list (elem-get sw_colors 60) (elem-get sw_colors 71)))

;colours, how see through, none and the cross, a tap of any device is told
;with the device as it went down
(defq sw_board (Board (cwb-doc 800 600)) sw_said (list)
	sw (Swatches sw_board 400 300 (lambda (on event what val) (push sw_said (list event what val)))))
(. (. sw_board :get_stage) :add sw)
(sw-tap sw_board 0x2000000 :pen +pev_right (. sw :where :color 0xffff0000))
(assert-eq "a tap of a pen's barrel on red is told, red, with the pen and its barrel"
	(str (list :color 0xffff0000 0x2000000 :pen +pev_right))
	(progn (bind '((e what val)) sw_said) (str (list what val (elem-get e +pev_id) (elem-get e +pev_kind) (elem-get e +pev_buttons)))))
(sw-tap sw_board 0 :mouse +pev_left (. sw :where :alpha 0x40))
(sw-tap sw_board 0x1000002 :touch 1 (. sw :where :none))
(assert-eq "a quarter see through, and none, it stays open for each"
	"((:alpha 64) (:none 0))" (str (map (# (slice %0 1 3)) (slice sw_said 1 3))))
(. sw_board :pointers (list (ptr-event 0 :mouse +pev_left 400 300)))
(bind '(sw_x sw_y) (. sw :where :color 0xff000000))
(. sw_board :pointers (list (ptr-event 0 :mouse 0 (+ sw_x 300.0) sw_y)))
(assert-eq "down on one and up off it, nothing" 3 (length sw_said))
(defq sw2 (Swatches sw_board 100 100 (lambda (on event what val) (push sw_said (list what val)))))
(. (. sw_board :get_stage) :add sw2)
(assert-eq "as many as are wanted, as floating menus, one for each pupil" 2 (length (swatches-on sw_board)))
(. sw2 :close)
;the first cell's middle, and above it the edge of the menu
(bind '(sw_x sw_y) (. sw :where :color (first sw_colors)))
(. sw_board :pointers (list (ptr-event 0 :mouse +pev_left sw_x (- sw_y 13.0))))
(. sw_board :pointers (list (ptr-event 0 :mouse +pev_left (+ sw_x 50.0) (- sw_y 13.0))))
(. sw_board :pointers (list (ptr-event 0 :mouse 0 (+ sw_x 50.0) (- sw_y 13.0))))
(assert-list-eq "dragged by its edge it moves, and nothing is picked" (list 450 300 3)
	(list (n2i (first (get :origin sw))) (n2i (second (get :origin sw))) (length sw_said)))
(defq sw_c (Canvas 800 600 1))
(. sw_c :fill 0)
(assert-true "it draws" (catch (progn (. sw :draw sw_c (Mat3x2-translate 0 0)) :t) :nil))
(sw-tap sw_board 0 :mouse +pev_left (. sw :where :close))
(assert-eq "its cross puts it away" 0 (length (swatches-on sw_board)))
