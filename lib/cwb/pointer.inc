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

(import "././collections/collections.inc")
(import "././math/polygon.inc")

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

(defun ptr-at (what key)
	; (ptr-at what key) -> :nil | index
	;where the value of key is in what a rule binds, keys and values in
	;turn. A key is looked for only where keys are: a value can be a
	;keyword that is a key's name too, (:tool :pen :pen 3)
	(defq i 0 n (length what) at :nil)
	(while (and (not at) (< i n))
		(if (eql (elem-get what i) key) (setq at (inc i)))
		(setq i (+ i 2)))
	at)

;What a press's buttons are, worked out here and nowhere else. A device
;has a main button and a second: the mouse's left and right, a pen's tip
;and barrel. A finger has one, and is all of its device. The middle of a
;mouse is both its buttons, Chris: "a middle button click should mean, both
;left/right bind to this". None held, a pointer that came up, is the main
(defun ptr-roles (kind buttons)
	; (ptr-roles kind buttons) -> (buttons ...)
	;the buttons of the rules of a device a press is, :nil for a finger
	(cond
		((eql kind :touch) (list :nil))
		((eql buttons 0) (list +pev_left))
		((and (eql kind :mouse) (eql buttons +pev_middle)) (list +pev_left +pev_right))
		((list buttons))))

(defun ptr-main (event)
	; (ptr-main event) -> event
	;the event as its main role, which is what a device's pen and width
	;are read by
	(bind '(id kind buttons x y &ignore) event)
	(ptr-event id kind (ifn (first (ptr-roles kind buttons)) buttons) x y))

(defun ptr-part (event)
	; (ptr-part event) -> :stroke | :fill | :both | :ink
	;what of a thing a pen given by this press colours: the main button the
	;outline, the second the fill, Chris: "left button bound pen to outline
	;and the right button bound pen to fill"; both, both; a finger, one pen,
	;what the thing shows
	(bind '(& kind buttons &ignore) event)
	(defq roles (ptr-roles kind buttons))
	(cond
		((eql kind :touch) :ink)
		((> (length roles) 1) :both)
		((eql (first roles) +pev_right) :fill)
		(:stroke)))

;Devices and contacts. Everything that points is a device, the mouse, each
;pen, each touch panel, and a device has contacts, the places it touches at
;once: a pen one, a panel one for each finger. Each finger that lands is
;given the next number, counting up, and no number is given again, so of
;two contacts of a device the one that landed later has the bigger. The
;host's driver gives both, and the id of a pointer is the two, device << 24
;| contact, the mouse device 0. Whose a touch is, is its device. An id
;below 16777216 is a device of the one contact, as the mouse is, and as a
;test or a script may name a pen or a finger

(defun ptr-device (id)
	; (ptr-device id) -> device
	;the device of a pointer's id, the id of its contact 0
	(if (< id 0x1000000) id (logand id -16777216)))

(defun ptr-contact (id)
	; (ptr-contact id) -> num
	;which contact of its device a pointer is
	(if (< id 0x1000000) 0 (logand id 0xffffff)))

;;;;;;;;;;
; bindings
;;;;;;;;;;

;the colours of the devices' squares by their place, (. board :device_uses)
;strong primaries, the mouse white, touch black, the teacher's pen yellow,
;the students' red, green and blue, Chris: "I think they should be strong
;primary colours", each drawn with a line of black round it
(defq +bindings_device_cols ''(0xffffffff 0xff000000 0xffffff00 0xffff0000 0xff00c000 0xff0040ff))

(defclass Bindings () :nil
	; (Bindings) -> bindings
	;rules, each what it matches and what a pointer it matches is to be.
	;The first rule that matches is the one, so the particular go before
	;the general
	(def this :rules (list))

	(defmethod :bind (kind id buttons what)
		; (. bindings :bind kind id buttons what) -> bindings
		;a rule, put before those there are. kind, id and buttons are each
		;what the pointer must have, or :nil for any. id is a device, what
		;(ptr-device) gives, and matches every contact of it: what a device
		;is bound to, its colour and tool, is whoever has that device's,
		;and each finger of a panel is the same hand. buttons matches if
		;every one of them is held. what is a list of keys and values,
		;(:tool :pen :color 0xff000000 :width 4.0)
		(def this :rules (cat (list (list kind id buttons what)) (get :rules this)))
		this)

	(defmethod :set (kind id buttons key value)
		; (. bindings :set kind id buttons key value) -> bindings
		;one thing of what that device, with those buttons, is to be, as a
		;binder sets it, a toolbar or a menu pressed by it. The rule that
		;names just that is made, if there is none, from all such a pointer
		;is now, so the rest of what it is stays as it was. Of each role the
		;press is, (ptr-roles): buttons 0 would match every press of the
		;device, before the rules of its buttons, so it is the main; the
		;middle of a mouse is both; a finger is all its device
		(each (lambda (buttons)
				(defq rule (some (lambda (r) (and (eql (first r) kind) (eql (second r) id) (eql (third r) buttons) r))
					(get :rules this)))
				(unless rule
					(. this :bind kind id buttons (cat (ifn (. this :find (list (ifn id 0) kind (ifn buttons 0))) (list))))
					(setq rule (first (get :rules this))))
				;a slot holds a pen, or an ink of its own, not both: the one
				;set puts the other away
				(when (defq other (case key (:pen :color) (:color :pen)))
					(when (defq at (ptr-at (elem-get rule 3) other))
						(defq was (elem-get rule 3))
						(elem-set rule 3 (cat (slice was 0 (dec at)) (slice was (inc at) -1)))))
				(defq what (elem-get rule 3) at (ptr-at what key))
				(if at (elem-set what at value) (push what key value))
				;a mode is what a pen draws: a rule that was another tool, the
				;hand of the right button, is a pen of that mode now, or the
				;mode bound would do nothing
				(and (eql key :mode) (defq at (ptr-at what :tool)) (elem-set what at :pen)))
			(ptr-roles kind buttons))
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
			(and (or (not rkind) (eql rkind kind))
					(or (not rid) (= rid (ptr-device id)))
					(or (not rbuttons) (= (logand buttons rbuttons) rbuttons))
				what))
			(get :rules this)))

	(defmethod :value (event key &optional default)
		; (. bindings :value event key [default]) -> value
		;one thing of what a pointer is to be
		(defq what (. this :find event) at (if what (ptr-at what key)))
		(if at (elem-get what at) default))
	)

(defun bindings-default ()
	; (bindings-default) -> bindings
	;how a board starts out. A pen draws, and rubs out with its other end
	;or with the button on its side held, Chris: "I frequently had left
	;pen bound to draw and right pen button bound to erase !" The mouse
	;draws with its left button, moves things with its right and the
	;board itself with its middle. A finger moves things. A main button
	;holds the board's first pen, black, a second button its second,
	;white, which is the fill of what the device draws, Chris: "I would
	;have left/right default to black/white"
	(.-> (Bindings)
		(:bind :nil :nil :nil '(:tool :pen))
		(:bind :touch :nil :nil '(:tool :hand))
		(:bind :eraser :nil :nil '(:tool :eraser :pen 1))
		(:bind :pen :nil +pev_right '(:tool :eraser :pen 1))
		(:bind :mouse :nil +pev_right '(:tool :hand :pen 1))))

;;;;;;;
; actors
;;;;;;;

(defclass Actor () :nil
	; (Actor) -> actor
	;something on a stage that pointers can be on. It keeps what it likes
	;for each pointer it has, by the pointer's id.
	;
	;Where it can be touched is its zones: lists of polygons in its own
	;space, each named for what a pointer that lands there is to do, its
	;body, the band along an edge, a bar to drag it by, (:zones). Their
	;union is its outline, all of where it is, made once and kept, and
	;made again when its zones change, (:zones_changed), or what they
	;depend on, (:zones_key). A point is its if it is in that outline,
	;(:hit), and then which zone it is in says what it is to do, (:zone_at)
	(def this :states (Fmap 11) :claim :nil)

	(defmethod :zones ()
		; (. actor :zones) -> ((name polygons) ...)
		;its zones in its own space, the first that has a point says what
		;the point is on. A polygon a path, or a list of x y
		(list))

	(defmethod :zones_key ()
		; (. actor :zones_key) -> key
		;what its zones depend on but its own make, how big it is seen say.
		;The outline is made again when this is not what it was
		:nil)

	(defmethod :zones_changed ()
		; (. actor :zones_changed) -> actor
		;its zones are not what they were, the outline is made again
		(def this :claim :nil)
		this)

	(defmethod :to_own (x y)
		; (. actor :to_own x y) -> (x y)
		;a point of the stage in its own space
		(list x y))

	(defmethod :claim ()
		; (. actor :claim) -> (key zones outline box)
		;its zones as paths, their union, and the box round that
		(defq key (. this :zones_key) claim (get :claim this))
		(unless (and claim (eql (first claim) key))
			(defq zones (map (lambda ((name polys))
					(list name (map (# (if (list? %0) (apply (const path) %0) %0)) polys)))
					(. this :zones))
				all (if (nempty? zones) (apply (const cat) (map (const second) zones)) (list))
				outline (if (nempty? all) (poly-union all (list) 1) (list))
				box :nil)
			(each (lambda (poly)
					(each (lambda ((x y))
							(setq box (if box (list (min x (first box)) (min y (second box)) (max x (third box)) (max y (elem-get box 3)))
								(list x y x y))))
						(partition poly 2)))
				outline)
			(def this :claim (setq claim (list key zones outline box))))
		claim)

	(defmethod :hit (x y &optional event)
		; (. actor :hit x y [event]) -> :t | :nil
		;is a point on it, the point is in the space of the stage, and is
		;the contact its own, by its own rules, with the event of the
		;contact as it lands: the stage asks, it tests nothing itself. By
		;its outline, a box round it first
		(bind '(& & outline box) (. this :claim))
		(when box
			(bind '(lx ly) (. this :to_own x y))
			(bind '(bx by bx1 by1) box)
			(and (>= lx bx) (<= lx bx1) (>= ly by) (<= ly by1)
				(vector-point-in-polygon (path lx ly) outline +winding_none_zero))))

	(defmethod :zone_at (lx ly)
		; (. actor :zone_at lx ly) -> :nil | name
		;the first of its zones a point of its own space is in
		(bind '(& zones &ignore) (. this :claim))
		(defq p (path lx ly))
		(some (lambda ((name polys)) (if (vector-point-in-polygon p polys +winding_none_zero) name)) zones))

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

	(defmethod :drawing? ()
		; (. actor :drawing?) -> :t | :nil
		;is any pointer that is down on it drawing something, a line that
		;is not yet done. One that has anything to draw with says
		:nil)
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

	(defmethod :hit (x y &optional event)
		; (. stage :hit x y [event]) -> :nil | actor
		;the frontmost actor that says a point is on it, and a contact that
		;lands there its own, Chris: "It just says I have this event
		;co-ordinate, is that you ?"
		(some! (# (if (. %0 :hit x y event) %0)) (list (get :actors this)) :nil -1 0))

	(defmethod :pass (actor id events)
		; (. stage :pass actor id events) -> :nil | actor
		;an actor that had a contact gives it to the first behind it that
		;takes it, as if it had landed there, and that one is given the
		;events of it so far, the first as it went down
		(defq actors (get :actors this) at (find actor actors) next :nil)
		(bind '(& & & x y &ignore) (first events))
		(while (and at (not next) (>= (setq at (dec at)) 0))
			(if (. (defq a (elem-get actors at)) :hit x y (first events)) (setq next a)))
		(when next
			(. (get :owners this) :insert id next)
			(. (get :over this) :insert id next)
			(. next :pointers this events))
		next)

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
			;one that is not down on an actor is the frontmost's it is over,
			;and that takes it
			(unless owner (setq owner (. this :hit x y event)))
			;it goes down on that actor, and is its till it comes up
			(and owner (not held) (/= buttons 0) (. owners :insert id owner))
			(and held (= buttons 0) (push up id))
			;what it was over and is not now is told
			(defq was (. over :find id))
			(and was (not (eql was owner)) (. was :leave this id))
			(cond
				;a finger that is up is gone, it is over nothing
				((and (eql kind :touch) (= buttons 0))
					(. over :erase id)
					(and owner (not held) (setq owner :nil)))
				(owner (. over :insert id owner))
				(:t (. over :erase id)))
			(when owner
				(if (defq group (some (# (if (eql (first %0) owner) %0)) groups))
					(push (second group) event)
					(push groups (list owner (list event))))))
			events)
		;each is given its own, all at once, the front one first, so what
		;they do goes from the front of the scene to the back, Chris: "I
		;suggest just front to back, so actions that generate flow from the
		;front to back of the scene"
		(defq actors (get :actors this))
		(sort groups (# (- (ifn (find (first %1) actors) -1) (ifn (find (first %0) actors) -1))))
		(each (lambda ((owner owner_events)) (. owner :pointers this owner_events)) groups)
		(each (# (. owners :erase %0)) up)
		(map (const first) groups))
	)
