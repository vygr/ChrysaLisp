# UI Widgets and UI trees

A user widget is an instance of a Lisp class that inherits from the Lisp View
class. The View class is the base class for all UI widgets and this class
itself inherits from the VP `hmap` class.

Widgets can be added as children of other widgets. This uses exactly the same
parenting mechanism as when a called Lisp function is allocated a new local
environment when invoked.

This means that a widget instance IS a Lisp environment, and can hold symbol
bindings and properties in the same way the Lisp execution environment does !

When you use `(get :color this)` on a widget instance a search will be made
starting at this widget for the symbol binding, if not found at this level then
the search will proceed to the parent widget, all the way up to the root widget
if needed.

Although widgets inherit their methods from the parent class, they in effect
inherit their properties from the parent tree at run time.

This is extremely useful and very powerful in a UI context. As we already
mentioned above we could have a single property `:color` held at the root
widget and, if no descendant also defines that property, any look up of the
`:color` property will find the root definition. So the Window maybe can set
the colour scheme for the entire widget tree !

In the same way that `(get :color this)` will search the parent tree, useing
`(def? :color this)` will only check the current widget ! Some properties are
best not inherited from the parent tree. For example the `:min_width` property
would be a good candidate to be searched for with `(def?)`.

Use of `(set this :color +argb_red)` and `(def this :color +argb_black)`
likewise work exactly as they do in Lisp source programs. `(set)` will search
the tree, `(def)` will not. All the mechanisms we described in the properties
section in the ENVIRONMENT document apply to UI trees !
