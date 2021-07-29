# UI Widgets and UI trees

A UI widget is an instance of a Lisp class that inherits from the Lisp View
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
widget and, if no descendant in the UI tree also defines that property, any
look up of the `:color` property, at a leaf widget say, will find the root
definition. So the Window maybe can set the colour scheme for the entire widget
tree !

In the same way that `(get :color this)` will search the parent tree, using
`(def? :color this)` will only check the current widget ! Some properties are
best not inherited from the parent tree. For example the `:min_width` property
would be a good candidate to be searched for with `(def?)`.

Use of `(set this :color +argb_red)` and `(def this :color +argb_black)`
likewise work exactly as they do in Lisp source programs. `(set)` will search
the tree, `(def)` will not. All the mechanisms we described in the properties
section in the ENVIRONMENT document apply to UI trees !

## View class

The View class is the base widget class. Its main function is to interface to
the VP level GUI compositor. It defines a rectangular region that will be
rendered by calling the widgets `:draw` method. The GUI process will make this
call when and if it calculates that this widget needs to redraw itself, when it
does so each widget subclass can override the `:draw` method to call VP drawing
primitive methods using that context.

A simple example of a `:draw` method from the Backdrop class, in fact the
Backdrop class only consists of an overridden `:draw` method:

```file
gui/backdrop/lisp.inc
```

This class defines a `:style` and a `:spacing` property and these control what
pattern of lines, if any, will be drawn to form the widgets image.

The View class defines primitive methods to allow UI trees to be constructed
and examined, `:add_child, :sub, :children` etc. Plus defines ways to ask a
widget about it desired size and current position, `:pref_size, :get_pos,
:get_size` etc. Take a look at the `gui/view/lisp.inc` file for the full list.

Apart from its appearance a widget can also override methods that control its
interaction with the mouse or keyboard. `:mouse_down, :mouse_up, :mouse_move,
:mouse_enter, :mouse_exit, :mouse_wheel` for the mouse and `:key_down, :key_up`
for the keyboard.

These interaction methods are called from the Window class `:event` method. We
already covered that this call is made by an application for any internal
widget events as the default `main` mailbox dispatch action.

## Layout classes

A layout widget is an instance of a class that is used to position its children
in a particular way. It normally has no `:draw` method and is a transparent
object. Its only effect is to control the child widgets.

Let's jump straight in and show the Grid class:

```file
gui/grid/lisp.inc
```

This class uses two properties `:grid_width` and `:grid_height` to control the
position of child widgets. It does not give a default value to these on
construction, these are normally set useing the UI builder macros, more to come
on those later in this document.

Layout widgets override the `:layout` method to do the positioning, calling
`:change` on the child widget as required. This Grid class gives each child an
equal width and height in a regular grid spaceing.

These layout widgets also override the `:pref_size` method as they gather
information from their children in order to calculate what the prefered size of
this container should be.

For a more involved layout, take a look at the Flow widget,
`gui/flow/lisp.inc`, this allows you to specify a direction of flow (or none),
as well as various options for stretching and alignment of the child widgets.
