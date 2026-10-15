(import "lib/options/options.inc")
(import "usr/env.inc")
(import "sys/pii/lisp.inc")
(import "gui/lisp.inc")
(import "lib/consts/scodes.inc")
(import *env_keyboard_map*)

(defq usage `(
(("-h" "--help")
"Usage: input [options] action ...

    options:
        -h --help: this help info.
        -t --title str: the window whose title has this in it,
            default all of the screen.
        -f --find key=value: the first view in it whose key is
            value, type=Button, text=Save, depth first.
        -w --wait num: seconds to wait for the window, as an app
            just launched has none yet, default 0.

    actions, done in order. A place, x y, is from the top left of
    the window, or of the view found, and is its middle if left
    out. A view out of sight, scrolled away, is not clicked; turn
    the wheel over what it is in first, as a user would:
        move x y: the mouse to there.
        click [x y], rclick [x y], dclick [x y]: a click of the
            left button, of the right, two of the left.
        drag x y x1 y1: the left button down at the one place,
            moved to the other and let go there.
        wheel way n: n steps of the wheel, where the mouse is,
            up, down, left or right.
        key name: a key down and up, by its name in
            lib/consts/scodes.inc, return, a, pagedown, with any
            of shift+ ctrl+ alt+ cmd+ before it.
        type text: the keys of the text, as the keyboard map of
            the user has them.
        finger n down x y, finger n to x y, finger n up: contact n
            of a touch panel lands, moves there, lifts. Many may be
            down at once, each its own n.
        pen down x y [barrel], pen to x y, pen up: the pen, its tip
            touching, with the button of its barrel held if said.
        eraser down x y, eraser to x y, eraser up: the other end of
            the pen.
        A move of a contact goes there in steps from where it was,
        if it was down by this command, so a line is drawn.

    Given to the GUI of this network, as if from its mouse and
    keyboard, so to the windows of any app, as they would be.")
(("-t" "--title") ,(opt-str 'opt_t))
(("-f" "--find") ,(opt-str 'opt_f))
(("-w" "--wait") ,(opt-num 'opt_w))
))

(defq +mods (scatter (Fmap) "shift" +sc_lshift "ctrl" +sc_lctrl
		"alt" +sc_lalt "cmd" +sc_lgui "meta" +sc_lgui)
	+num_class (char-class "0123456789"))

(defun event (type x y &optional buttons count scode)
	; (event type x y [buttons count scode]) -> str
	(setf-> (str-alloc +gui_event_size)
		(+gui_event_type type) (+gui_event_x x) (+gui_event_y y)
		(+gui_event_buttons (ifn buttons 0)) (+gui_event_count (ifn count 0))
		(+gui_event_scode (ifn scode 0)) (+gui_event_direction 0)
		(+gui_event_id 0) (+gui_event_kind 0) (+gui_event_pressure 0)))

(defun number-word? (w)
	(and w (nempty? w) (every (# (bfind %0 +num_class)) w)))

(defun keys-of (scode mods)
	;a key down and up, inside the downs and ups of its modifiers
	(apply (const cat) (cat
		(map (# (event +gui_ev_key_down 0 0 0 0 %0)) mods)
		(list (event +gui_ev_key_down 0 0 0 0 scode) (event +gui_ev_key_up 0 0 0 0 scode))
		(map (# (event +gui_ev_key_up 0 0 0 0 %0)) (reverse mods)))))

(defun key-name (name)
	; (key-name name) -> :nil | events
	(defq parts (split name "+") mods (list))
	(when (every (# (if (defq m (. +mods :find %0)) (push mods m))) (slice parts 0 -2))
		(if (defq scode (get (sym (cat "+sc_" (last parts)))))
			(keys-of scode mods))))

(defun type-text (text)
	; (type-text text) -> :nil | events
	;each char by the key the keyboard map gives it, shifted or not
	(defq by_char (Fmap) out (list))
	(each (lambda ((m shift))
			(. m :each (# (unless (. by_char :find %1)
				(. by_char :insert %1 (list %0 shift))))))
		(list (list *normal_map* :nil) (list *shift_map* :t)))
	(when (every (lambda (c)
			(when (defq k (. by_char :find (code c)))
				(bind '(scode shift) k)
				(push out (keys-of scode (if shift (list +sc_lshift) (list)))))) text)
		(apply (const cat) out)))

;the words of the actions, the next one, and the view they are about,
;(x y w h) on the screen and the part of it that can be seen
(defq *words* (list) *next* 0 *view* (list 0 0 0 0 0 0 0 0))

(defun word-at (i)
	(if (< i (length *words*)) (elem-get *words* i)))

(defun next-word ()
	(word-at (dec (++ *next*))))

(defun numbers-next? ()
	(and (number-word? (word-at *next*)) (number-word? (word-at (inc *next*)))))

(defun wheel ()
	;steps of the wheel, a way and how many, up is away from the user
	(and (defq way (next-word)) (number-word? (defq n (next-word)))
		(defq xy (case (sym (cat ":" way))
			(:up '(0 1)) (:down '(0 -1)) (:left '(-1 0)) (:right '(1 0))))
		(event +gui_ev_mouse_wheel (* (first xy) (str-to-num n)) (* (second xy) (str-to-num n)))))

(defun place ()
	;a place on the screen from the next two words, or the middle of the part
	;of the view that can be seen
	(bind '(vx vy & & sx sy sw sh) *view*)
	(if (numbers-next?)
		(list (+ vx (str-to-num (next-word))) (+ vy (str-to-num (next-word))))
		(list (+ sx (/ sw 2)) (+ sy (/ sh 2)))))

(defun click (button counts)
	(bind '(x y) (place))
	(cat (event +gui_ev_mouse_motion x y)
		(apply (const cat) (map (# (cat
			(event +gui_ev_mouse_down x y button %0)
			(event +gui_ev_mouse_up x y button %0))) counts))))

(defun drag ()
	(bind '(x y) (place))
	(bind '(x1 y1) (place))
	(cat (event +gui_ev_mouse_motion x y)
		(event +gui_ev_mouse_down x y 1 1)
		(apply (const cat) (map (# (event +gui_ev_mouse_motion
			(+ x (/ (* (- x1 x) %0) 8)) (+ y (/ (* (- y1 y) %0) 8)) 1))
			(range 1 9)))
		(event +gui_ev_mouse_up x1 y1 1 1)))

;a pen or a finger, as the host gives one: its id, device << 24 | contact,
;its kind, the buttons held, 0 for none, and how hard, the most if down
(defun pointer-event (type id kind buttons x y)
	(setf-> (str-alloc +gui_event_size)
		(+gui_event_type type) (+gui_event_x x) (+gui_event_y y)
		(+gui_event_buttons buttons) (+gui_event_count 0) (+gui_event_scode 0)
		(+gui_event_direction 0) (+gui_event_id id) (+gui_event_kind kind)
		(+gui_event_pressure (if (= buttons 0) 0 65535))))

;the contacts of this command that are down, by id, (kind buttons x y)
(defq *contacts* (Fmap 11)
	+touch_device (<< 1 24) +pen_device (<< 2 24)
	;the host gives a pen with the button of its barrel held the right
	;button, in place of the left of its tip, src/host/gui_event.h
	+barrel 4)

(defun contact (id kind)
	;down, to or up of a contact, the words after the device
	(defq way (next-word) at (. *contacts* :find id))
	(case (sym (cat ":" way))
		(:down (bind '(x y) (place))
			(defq buttons (if (eql (word-at *next*) "barrel") (progn (next-word) +barrel) 1))
			(. *contacts* :insert id (list buttons x y))
			(pointer-event +gui_ev_pointer_down id kind buttons x y))
		(:to (bind '(x y) (place))
			(bind '(buttons ox oy) (ifn at (list 1 x y)))
			(. *contacts* :insert id (list buttons x y))
			(apply (const cat) (map (# (pointer-event +gui_ev_pointer_motion id kind buttons
				(+ ox (/ (* (- x ox) %0) 8)) (+ oy (/ (* (- y oy) %0) 8)))) (range 1 9))))
		(:up (bind '(& x y) (ifn at (cat '(0) (place))))
			(. *contacts* :erase id)
			(pointer-event +gui_ev_pointer_up id kind 0 x y))))

(defun finger ()
	(and (number-word? (defq n (next-word)))
		(contact (+ +touch_device (str-to-num n)) +gui_kind_touch)))

(defun action-events (action)
	; (action-events action) -> :nil | events
	(case (sym (cat ":" action))
		(:move (bind '(x y) (place)) (event +gui_ev_mouse_motion x y))
		(:click (click 1 '(1)))
		(:rclick (click 3 '(1)))
		(:dclick (click 1 '(1 2)))
		(:drag (drag))
		(:wheel (wheel))
		(:key (and (defq w (next-word)) (key-name w)))
		(:type (and (defq w (next-word)) (type-text w)))
		(:finger (finger))
		(:pen (contact +pen_device +gui_kind_pen))
		(:eraser (contact +pen_device +gui_kind_eraser))))

(defun main ()
	(when (and (defq stdio (create-stdio))
			(defq opt_t :nil opt_f :nil opt_w 0 args (options stdio usage)))
		(defq kv (if opt_f (split opt_f "=") (list))
			at (gui-wait (# (gui-locate-rpc opt_t (first kv) (second kv))) opt_w)
			why :nil)
		(cond
			((not at) (print "input: no GUI"))
			((str? at) (print "input: " at))
			(:t (setq *view* at *words* (rest args) *next* 0)
				(while (and (not why) (< *next* (length *words*)))
					(defq action (next-word) events (action-events action))
					(cond
						((not events) (setq why (cat "input: what is " action " ?")))
						((not (gui-input-rpc events)) (setq why "input: no answer from the GUI"))))
				(if why (print why))))))
