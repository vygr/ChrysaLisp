# Lisp Classes

Lisp level Classes are implanted via a macro set in the `lib/class/class.inc`
file. This is included via `boot.inc` so is available to all applications
without need to import the library.

A class and it's object instances consist of two VP level `hmap` objects. One
holds the virtual function references for that class, and each instance is
itself a single VP `hmap` object that holds the instance property data for that
object plus a `:vtable` property that holds a reference to the shared virtual
function `hmap` for that class.

The idea of this structure is to indirect the method calls of an object through
the object instance itself. The object class knows what the named methods do.
This is why the same named method on different objects can perform different
actions, and why subclasses can redefine or enhance what a method does.

C++ has this model for a `:vtable`, and folks may question why I did not go for
a multi-method Lisp approach, but this model is very simple to understand and
in this implementation we still have the option to mix methods into a class at
runtime !!! as the `:vtable` set of functions is not static ! There is a whole
world of dynamic `mixin` method libraries possible here that C++ would not
allow due to the dynamic nature of the ChrysaLisp method call mechanism .

It's not a lot of source for the entire thing:

```file
lib/class/class.inc
```

Let's go through each of these macros and cover what they do.

## (defclass name ([arg ...]) (super ...) body)

This macro allows you to create a new class or subclass. The macro wraps around
the method declarations and instance properties initialization that form its
body code, sorts them, and organises the default construction actions for an
instance of this class.

The instance of a class is referred to by the symbol `this`. This is only a
convention and some class's methods use a different symbol for namespace
reasons.

The macro defines a function `(name [arg ...]) -> obj` as the constructor for
new instances. It also auto defines both a `(. :type_of this) -> (...
:grandparent :parent :name)` method, that returns the class inherence list for
the object, along with a predicate function of the form `(name? this) -> nil |
t`.

```vdu
(. (Button) :type_of)
(:hmap :View :Label :Button)

(Button? (Button))
3
(Button? (Label))
nil
```

It is recommended that classes start with a capital letter but this is not
enforced.

## (defmethod name (this [arg ...]) body)

A method is just a function that has a default parameter `this`. That's pretty
much the entire deal. You get passed a reference to the `hmap` containing the
local state and you manipulate it as you see fit.

## (. this [arg ...])

This built in function is how you call a method of an object. You provide the
instance reference and any additional arguments required.

The `.` function is provided as a VP built in function, not within this file,
but it's a dynamic bound function calling mechanism tailored to this way of
invoking a function. It could be implemented in Lisp level code, which it was
during the experimental phase of the classes work, but it's such a performance
critical method it was moved to a VP code implementation.

## (.-> this form ...)

This is a `threading` macro to call multiple methods, in sequence, that return
the `this` reference. Other languages refer to the idea as the `builder`
pattern.

If the method you are calling returns the `this` reference, which all methods
that have no other output should !, you can chain a sequence of calls in a more
compact form.

If the method takes no arguments you can omit that forms enclosing `()`.

Example from the Editor app actions on a text buffer and a View :mouse_down
method:

```vdu
(.-> buffer
	(:set_cursor (length trimed_line) _)
	(:delete (- (length line) (length trimed_line))))
```

```vdu
(.-> this :layout :dirty_all)
```

## (.? this method) -> nil | lambda

This will query if the method is defined on this object. A reflection call as
other languages would say. You get back `nil` or the `lambda` for the method.

Somtimes you do want to check if the method you would call is in fact defined
for this object ! The `Window` class does this within the event dispatch code
to avoid throwing any errors due to a method not found.

```vdu
(defmethod :event (this event)
	; (. window :event event) -> window
	(defq target (. this :find_id (getf event +ev_msg_target_id))
		type (getf event +ev_msg_type))
	(when target
		(cond
			((= type +ev_type_mouse)
				;so what state are we in ?
				(defq buttons (getf event +ev_msg_mouse_buttons))
				(cond
					((/= 0 (get :last_buttons this))
						;was down previously
						(cond
							((/= 0 buttons)
								;is down now, so move
								(if (defq fnc (.? target :mouse_move)) (fnc target event)))
							(t  ;is not down now, so release
								(set this :last_buttons 0)
								(if (defq fnc (.? target :mouse_up)) (fnc target event)))))
					(t  ;was not down previously
						(cond
							((/= 0 buttons)
								;is down now, so first down
								(set this :last_buttons buttons)
								(if (defq fnc (.? target :mouse_down)) (fnc target event)))
							(t  ;is not down now, so hover
								(if (defq fnc (.? target :mouse_hover)) (fnc target event)))))))
			((= type +ev_type_key)
				(if (>= (getf event +ev_msg_key_keycode) 0)
					(if (defq fnc (.? target :key_down)) (fnc target event))
					(if (defq fnc (.? target :key_up)) (fnc target event))))
			((= type +ev_type_wheel)
				(while (and target (not (Scroll? target))) (setq target (penv target)))
				(and target (defq fnc (.? target :mouse_wheel)) (fnc target event)))
			((= type +ev_type_enter)
				(if (defq fnc (.? target :mouse_enter)) (fnc target event)))
			((= type +ev_type_exit)
				(if (defq fnc (.? target :mouse_exit)) (fnc target event)))
			((= type +ev_type_action)
				(if (defq fnc (.? target :action)) (fnc target event)))))
	this)
```

## (.super this :method [arg ...])

When defining a subclass, of a class, you may need to invoke the functionality
of your parent class before or after your subclass method code.

Very often a subclass needs to let the parent class do `its stuff` and then
supplement that with some additional action/s. This macro allows you to call
the underling parent method without having to know what it was.

An example from the `:draw` method of the Textfield class:

```vdu
(defclass Textfield () (Label)
	; (Textfield) -> textfield
	(def this :cursor 0 :anchor 0 :clear_text "" :hint_text "" :text ""
		:mode nil :state 1)

	(defmethod :draw (this)
		; (. textfield :draw) -> textfield
		(.super this :draw)
		(bind '(w h) (. this :get_size))
		(raise :font :text :cursor :anchor)
		(when (and font text)
			(bind '(tw th) (font-glyph-bounds font (slice 0 cursor text)))
			(when (/= cursor anchor)
				(bind '(sw _) (font-glyph-bounds font (slice 0 (min anchor cursor) text)))
				(bind '(sw1 _) (font-glyph-bounds font (slice 0 (max anchor cursor) text)))
				(.-> this
					(:ctx_set_color (get :hint_color this))
					(:ctx_filled_box sw (>>> (- h th) 1) (- sw1 sw) th)))
			(.-> this
				(:ctx_set_color (get :ink_color this))
				(:ctx_filled_box tw (>>> (- h th) 1) 2 th)))
		this)
	)
```

## (raise field | (var val) ...) -> (defq var (get field this) ...)

This, and `(lower ...)` the opposite, is a macro to pull instance data
properties into the method lambda. It will declare, with `(defq ...)`, each
property as a local var without the leading ':'.

Optionally you can add extras to the generated `(defq ...)` if required by
wrapping them in '()'.

This is called `lifting` in some other languages.

## (lower field | (field val) ...) -> (set this field var ...)

The opposite of `(raise ...)`, this macro uses `(set this ...)` to lower the
local variables to the object property.

Optionally you can add extras to the generated `(set this ...)` by wrapping
them in '()'.

##  (defabstractmethod (this [arg ...]) body)

This macros lets you define in a `base` class that there should exist a
concrete method within each subclass.

If the subclass fails to declare an override for this method then an
expectation will be thrown, at run time, if called.

## (deffimethod name ffi)

Allows you to define a VP level implemented method.

An example from the `View` class:

```vdu
...
	(deffimethod :find_id "gui/view/lisp_find_id")
		; (. view :find_id target_id) -> nil | target_view

	(deffimethod :hit_tree "gui/view/lisp_hit_tree")
		; (. view :hit_tree x y) -> (hit_view | nil rx ry)

	(deffimethod :set_flags "gui/view/lisp_set_flags")
		; (. view :set_flags value mask) -> view

	(deffimethod :add_dirty "gui/view/lisp_add_dirty")
		; (. view :add_dirty x y width height) -> view

	(deffimethod :trans_dirty "gui/view/lisp_trans_dirty")
		; (. view :trans_dirty rx ry) -> view
...
```
