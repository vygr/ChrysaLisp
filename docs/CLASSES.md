# Lisp Classes

Lisp level Classes are implanted via a macro set in the `lib/class/class.inc`
file. This is included via `boot.inc` so is available to all applications
without need to import the library.

A class and it's object instances consist of two VP level hmap objects. One
holds the virtual function references for that class, and each instance is
itself a single VP hmap object that holds the instance property data for that
object plus a `:vtable` property that holds a reference to the shared virtual
function hmap for that class.

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

So let go through each of these macros and cover what they do.

## (defclass name ([arg ...]) (super ...) body)

This macro allows you to create a new class or subclass. The macro wraps around
the method declarations that are its body, sorts them, and organises the
default construction actions for an instance of the class.

The instance of a class is referred to by the symbol `this`. This is only a
convention and some class's methods use a different symbol for namespace
reasons.

## (defmethod name (this [arg ...]) body)

A method is just a function that has a default parameter `this`. That's pretty
much the entire deal. You get passed a reference to a hmap containing the local
state data and you manipulate it as you see fit.

## (. this [arg ...])

This built in function is how you call a method of an object. You provide the
instance reference and any additional arguments required.

The `.` function is provided as a VP built in function, not within this file,
but it's a dynamic bound function calling mechanism tailored to this way of
invoking a function. It could be implemented in Lisp level code, which it was
during the experimental phase of the classes work, but it's such a performance
critcial method it was moved to a VP code implementation.

## (.-> this form ...)

This is a `threading` macro to call multiple methods, in sequence, that return
the `this` reference. Other languages refer to the idea as the `builder`
pattern.

If the method you are calling returns the `this` reference, which all methods
that have no other output should !, you can chain a sequence of calls in a more
compact form.

For example from the Editor app actions on a buffer:

```vdu
(.-> buffer
	(:set_cursor (length trimed_line) _)
	(:delete (- (length line) (length trimed_line))))
```

## (.? this method) -> nil | lambda

This is query if the method is defined on this object. A reflection call as
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
								(if (.? target :mouse_move) (. target :mouse_move event)))
							(t  ;is not down now, so release
								(set this :last_buttons 0)
								(if (.? target :mouse_up) (. target :mouse_up event)))))
					(t  ;was not down previously
						(cond
							((/= 0 buttons)
								;is down now, so first down
								(set this :last_buttons buttons)
								(if (.? target :mouse_down) (. target :mouse_down event)))
							(t  ;is not down now, so hover
								(if (.? target :mouse_hover) (. target :mouse_hover event)))))))
			((= type +ev_type_key)
				(if (>= (getf event +ev_msg_key_keycode) 0)
					(if (.? target :key_down) (. target :key_down event))
					(if (.? target :key_up) (. target :key_up event))))
			((= type +ev_type_wheel)
				(while (and target (not (Scroll? target))) (setq target (penv target)))
				(and target (.? target :mouse_wheel) (. target :mouse_wheel event)))
			((= type +ev_type_enter)
				(if (.? target :mouse_enter) (. target :mouse_enter event)))
			((= type +ev_type_exit)
				(if (.? target :mouse_exit) (. target :mouse_exit event)))
			((= type +ev_type_action)
				(if (.? target :action) (. target :action event)))))
	this)
```

## (.super this :method [arg ...])

When defining a subclass, of a class, you may need to invoke the functionality
of your parent class before or after your subclass method code.

Very often a subclass needs to let the parent class do `its stuff` and then
supplement that with some additional action/s. This macro allows you to call
the underling parent method without having to know what it was.

## (raise field | (var val) ...) -> (defq var (get field this) ...)

This, and `lower` the opposite, is a macro to pull instance data properties
into the method lambda. It will declare, with `defq`, the property as a local
var without the leading ':'.

Optionally you can add extras to the `defq` if required by wrapping them in
'()'.

This is called `lifting` in some other languages.

## (lower field | (field val) ...) -> (set this field var ...)

The opposite of `raise`, this macro uses `set this ...` to lower the local
variables to the object property.

Optionally you can add extras to the `set this` by wrapping them in '()'.

##  (defabstractmethod (this [arg ...]) body)

This macros lets you define in a `base` class that there should exist a
concrete method within each subclass.

If the subclass fails to declare an override for this method then an
expectation will be thrown, at run time, if called.

## (deffimethod name ffi)

Allows you to define a VP level implemented method.
