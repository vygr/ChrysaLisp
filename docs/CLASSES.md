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

## (. (this [arg ...])

This built in function is how you call a method of an object. You provide the
instance reference and any additional arguments required.

The `.` function is provided as a VP built in function, not within this file,
but it's a dynamic bound function calling mechanism tailored to this way of
invoking a function. It could be implemented in Lisp level code, which it was
during the experimental phase of the classes work, but it's such a performance
critcial method it was moved to a VP code implementation.
