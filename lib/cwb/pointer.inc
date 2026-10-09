;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/cwb/pointer.inc - pointers, and a stage that hands them to actors
;
; A pointer is anything that points at the board: the mouse, a pen, the
; eraser end of a pen, a finger. There can be many at once. Each has an id
; that is its own for as long as it is there, and an event of it says what
; kind it is, which of its buttons are held, where it is, and how hard it
; is pressed. A finger has one button, the left, held while it touches.
;
; A stage has actors, from the back to the front: the surface that is
; drawn on, the handles of what is selected, a ruler, a palette. A batch of
; events, one for each pointer that has something to say, goes to the
; stage. Each pointer belongs to the actor it went down on, till it comes
; up, and one that is not down belongs to the frontmost actor it is over.
; Each actor is then given the events of its own pointers, all of them at
; once, so it can see two fingers as two fingers. One pen can hold a ruler
; while another draws along it and a finger moves something else.
;
; What a pointer does on the surface is looked up, not built in: bindings
; are rules that match a pointer, by kind, id and buttons, and say what it
; is to be, a pen of this colour, an eraser, the hand that moves things.
;
; Nothing here draws or needs a screen. A test, or anything that wants to
; drive the board, makes events and gives them to the stage.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/collections/collections.inc")

;an event of a pointer, a list
(enums +pev 0
	(enum id kind buttons x y pressure time))

;the buttons, as the host has them for a mouse
(defq +pev_left 1 +pev_middle 2 +pev_right 4)

(defun ptr-event (id kind buttons x y &optional pressure time)
	; (ptr-event id kind buttons x y [pressure time]) -> event
	;kind is :mouse :pen :eraser or :touch. buttons is those held, 0 for a
	;pointer that is only over the board, or has just come up. pressure is
	;0.0 to 1.0, default 1.0 if a button is held. time in microseconds
	(list id kind buttons (n2f x) (n2f y)
		(ifn pressure (if (= buttons 0) 0.0 1.0)) (ifn time 0)))

(defun ptr-down? (event) (/= (elem-get event +pev_buttons) 0))

;;;;;;;;;;
; bindings
;;;;;;;;;;

(defclass Bindings () :nil
	; (Bindings) -> bindings
	;rules, each what it matches and what a pointer it matches is to be.
	;The first rule that matches is the one, so the particular go before
	;the general
	(def this :rules (list))

	(defmethod :bind (kind id buttons what)
		; (. bindings :bind kind id buttons what) -> bindings
		;a rule, put before those there are. kind, id and buttons are each
		;what the pointer must have, or :nil for any. buttons matches if
		;every one of them is held. what is a list of keys and values,
		;(:tool :pen :color 0xff000000 :width 4.0)
		(def this :rules (cat (list (list kind id buttons what)) (get :rules this)))
		this)

	(defmethod :unbind (kind id buttons)
		; (. bindings :unbind kind id buttons) -> bindings
		;the rules that match just that are taken out
		(def this :rules (filter (lambda ((rkind rid rbuttons &ignore))
			(not (and (eql rkind kind) (eql rid id) (eql rbuttons buttons))))
			(get :rules this)))
		this)

	(defmethod :find (event)
		; (. bindings :find event) -> :nil | what
		(bind '(id kind buttons &ignore) event)
		(some (lambda ((rkind rid rbuttons what))
			(if (and (or (not rkind) (eql rkind kind))
					(or (not rid) (= rid id))
					(or (not rbuttons) (= (logand buttons rbuttons) rbuttons)))
				what))
			(get :rules this)))

	(defmethod :value (event key &optional default)
		; (. bindings :value event key [default]) -> value
		;one thing of what a pointer is to be
		(defq what (. this :find event) at (if what (find key what)))
		(if at (elem-get what (inc at)) default))
	)

(defun bindings-default ()
	; (bindings-default) -> bindings
	;how a board starts out. A pen draws and its other end rubs out, and
	;with the button on its side held it moves things. The mouse draws
	;with its left button, moves things with its right and the board
	;itself with its middle. A finger moves things
	(.-> (Bindings)
		(:bind :nil :nil :nil '(:tool :pen))
		(:bind :touch :nil :nil '(:tool :hand))
		(:bind :eraser :nil :nil '(:tool :eraser))
		(:bind :pen :nil +pev_right '(:tool :hand))
		(:bind :mouse :nil +pev_middle '(:tool :pan))
		(:bind :mouse :nil +pev_right '(:tool :hand))))

;;;;;;;
; actors
;;;;;;;

(defclass Actor () :nil
	; (Actor) -> actor
	;something on a stage that pointers can be on. It keeps what it likes
	;for each pointer it has, by the pointer's id
	(def this :states (Fmap 11))

	(defmethod :hit (x y)
		; (. actor :hit x y) -> :t | :nil
		;is a point on it, the point is in the space of the stage
		:nil)

	(defmethod :pointers (stage events)
		; (. actor :pointers stage events) -> actor
		;the events of the pointers it has, all of this batch
		this)

	(defmethod :leave (stage id)
		; (. actor :leave stage id) -> actor
		;a pointer that was over it, not down, is no longer
		this)

	(defmethod :tick (time)
		; (. actor :tick time) -> :t | :nil
		;the time is now that, microseconds, for one that moves by itself.
		;Is it still moving, and so to be drawn again
		:nil)

	(defmethod :dismiss (event)
		; (. actor :dismiss event) -> :t | :nil
		;a pointer has gone down on the surface. One that is only there
		;till then, a palette, goes. Did it
		:nil)

	(defmethod :draw (canvas &optional m)
		; (. actor :draw canvas [m]) -> actor
		;on a canvas, over the document, by the matrix the board is seen
		;by, one that has anything to draw
		this)

	(defmethod :state (id)
		; (. actor :state id) -> :nil | state
		(. (get :states this) :find id))

	(defmethod :set_state (id state)
		; (. actor :set_state id state) -> actor
		;what is kept for a pointer, :nil for nothing more
		(if state
			(. (get :states this) :insert id state)
			(. (get :states this) :erase id))
		this)

	(defmethod :held ()
		; (. actor :held) -> num
		;how many pointers it has something kept for, those that are down on it
		(. (get :states this) :size))
	)

;;;;;;;
; stage
;;;;;;;

(defclass Stage () :nil
	; (Stage) -> stage
	(def this :actors (list) :owners (Fmap 11) :over (Fmap 11))

	(defmethod :get_actors ()
		; (. stage :get_actors) -> actors
		;the back one first
		(get :actors this))

	(defmethod :add (actor)
		; (. stage :add actor) -> stage
		;put an actor at the front
		(def this :actors (cat (filter (# (not (eql %0 actor))) (get :actors this)) (list actor)))
		this)

	(defmethod :add_back (actor)
		; (. stage :add_back actor) -> stage
		(def this :actors (cat (list actor) (filter (# (not (eql %0 actor))) (get :actors this))))
		this)

	(defmethod :sub (actor)
		; (. stage :sub actor) -> stage
		;take an actor off, and it has no pointers
		(def this :actors (filter (# (not (eql %0 actor))) (get :actors this)))
		(defq owners (get :owners this) over (get :over this) gone (list))
		(. owners :each (lambda (id owner) (if (eql owner actor) (push gone id))))
		(each (# (. owners :erase %0)) gone)
		(clear gone)
		(. over :each (lambda (id owner) (if (eql owner actor) (push gone id))))
		(each (# (. over :erase %0)) gone)
		this)

	(defmethod :hit (x y)
		; (. stage :hit x y) -> :nil | actor
		;the frontmost actor a point is on
		(some! (# (if (. %0 :hit x y) %0)) (list (get :actors this)) :nil -1 0))

	(defmethod :owner (id)
		; (. stage :owner id) -> :nil | actor
		;the actor a pointer that is down belongs to
		(. (get :owners this) :find id))

	(defmethod :pointers (events)
		; (. stage :pointers events) -> actors
		;a batch of events, no more than one for a pointer. Each goes to
		;the actor its pointer belongs to, an actor is given all of its
		;own at once. The actors that were given any
		(defq owners (get :owners this) over (get :over this)
			groups (list) up (list))
		(each (lambda (event)
			(bind '(id kind buttons x y &ignore) event)
			(defq owner (. owners :find id) held owner)
			;one that is not down on an actor is the frontmost's it is over
			(unless owner (setq owner (. this :hit x y)))
			;it goes down on that actor, and is its till it comes up
			(if (and owner (not held) (/= buttons 0)) (. owners :insert id owner))
			(if (and held (= buttons 0)) (push up id))
			;what it was over and is not now is told
			(defq was (. over :find id))
			(if (and was (not (eql was owner))) (. was :leave this id))
			(cond
				;a finger that is up is gone, it is over nothing
				((and (eql kind :touch) (= buttons 0))
					(. over :erase id)
					(if (and owner (not held)) (setq owner :nil)))
				(owner (. over :insert id owner))
				(:t (. over :erase id)))
			(when owner
				(if (defq group (some (# (if (eql (first %0) owner) %0)) groups))
					(push (second group) event)
					(push groups (list owner (list event))))))
			events)
		(each (lambda ((owner owner_events)) (. owner :pointers this owner_events)) groups)
		(each (# (. owners :erase %0)) up)
		(map (const first) groups))
	)
