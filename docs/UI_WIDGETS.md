# UI Widgets and UI trees

A UI widget is an instance of a Lisp class that inherits from the Lisp View
class. The View class is the base class for all UI widgets and this class
itself inherits from the VP `hmap` class.

Widgets can be added as children of other widgets. This uses exactly the same
parenting mechanism as when a Lisp function is allocated a new local
environment when invoked.

This means that a widget instance IS a Lisp environment, and can hold symbol
bindings and properties in the same way the Lisp execution environment does !

When you use `(get :color this)` on a widget instance a search will be made
starting at this widget for the symbol binding, if not found at this level then
the search will proceed to the parent widget, all the way up to the root widget
if needed.

Although widgets inherit their methods from the parent class, they in effect
inherit their properties from the parent UI tree at run time.

This is extremely useful and very powerful in a UI context. As we already
mentioned above we could have a single property `:color` held at the root
widget and, if no descendant in the UI tree also defines that property, any
look up of the `:color` property, at a leaf widget say, will find the root
definition. So the Window can set the colour scheme for the entire widget tree
!

In the same way that `(get :color this)` will search the parent UI tree, using
`(def? :color this)` will only check the current widget ! Some properties are
best not inherited from the parent UI tree. For example the `:min_width`
property would be a good candidate to be searched for with `(def?)`.

Use of `(set this :color +argb_red)` and `(def this :color +argb_black)`
likewise work exactly as they do in Lisp source programs. `(set)` will search
the tree, `(def)` will not. All the mechanisms described in the properties
section of the `ENVIRONMENT` document apply to UI trees !

## View class

The View class is the base widget class. Its main function is to interface to
the VP level GUI compositor. It defines a rectangular region that will be
rendered by calling the widget `:draw` methods. The GUI process will make this
call when and if it calculates that this widget needs to redraw itself, when it
does so each widget subclass overrides the `:draw` method and uses VP drawing
methods to render that particular widget.

A simple example of a `:draw` method from the Backdrop class, in fact the
Backdrop class only consists of an overridden `:draw` method:

```file
gui/backdrop/lisp.inc
```

This class defines a `:style` and a `:spacing` property and these control what
pattern of lines, if any, will be drawn to form the widgets image.

The View class defines primitive methods to allow UI trees to be constructed
and examined, `:add_child, :sub, :children` etc. Plus defines ways to ask a
widget about its desired size and current position, `:pref_size, :get_pos,
:get_size` etc. Take a look at the `gui/view/lisp.inc` file for the full list.

Apart from its appearance a widget can also override methods that control its
interaction with the mouse or keyboard. `:mouse_down, :mouse_up, :mouse_move,
:mouse_enter, :mouse_exit, :mouse_wheel` for the mouse and `:key_down, :key_up`
for the keyboard.

These interaction methods are called from the Window class `:event` method. We
already covered that this call is made by an application for any internal
widget events as the default `main` mailbox dispatch action.

## Layout classes

A layout widget is an instance of a class that's used to position its children
in a particular way. It normally has no `:draw` method and is a transparent
object. Its only effect is to control the child widgets.

Let's jump straight in and show the Grid class:

```file
gui/grid/lisp.inc
```

This class uses two properties `:grid_width` and `:grid_height` to control the
position of child widgets. It does not give a default value to these on
construction, these are normally set using the UI builder macros, more to come
on those later in this document.

Layout widgets override the `:layout` method to do the positioning, calling
`:change` on the child widgets as required. This Grid class gives each child an
equal width and height in a regular grid spacing.

These layout widgets also override the `:pref_size` method, they gather
information from their children in order to calculate what the preferred size
of this container should be.

For a more involved layout, take a look at the Flow widget,
`gui/flow/lisp.inc`, this allows you to specify a direction of flow (or none),
as well as various options for stretching and alignment of the child widgets.

## UI builder macros

While you could manually create each widget instance and construct the UI tree
manually, defining each property on every widget by hand. This would prove
quite tedious and error prone.

The UI builder macros take away much of the effort in doing this. They rely on
the nested macro expansion facility of ChrysaLisp to create a `(progn)` that
calls widget constructors, sets default properties, plus lets you override
those defaults and define your own, set up user event ids with `:connect` and
take care of building the tree with the `:add_child` methods.

These macros are defined in the `gui/lisp.inc` file. Two of these macros are
used to start a tree definition. `(ui-window)` and `(ui-root)`. Then follow
with the macros for further nested widgets.

Some of these macros will create multiple widgets and their containers. Macros
that define layouts, or widgets that can have child objects, take an optional
body form that allows nesting, these nested body macro sub trees will be added
to their parent using the `:add_child` method call.

So let's show a simple example before detailing each macro. In the details
section we will list the relevant properties for each widget, those that are
directly used by that widget subclass. Please look through sources in the
`gui/` folder to familiarise yourself with the most up to date properties and
options.

This from the `apps/font/app.lisp` demo:

```vdu
(ui-window *window* ()
	(ui-title-bar window_title "" (0xea19) +event_close)
	(ui-tool-bar main_toolbar ()
		(ui-buttons (0xe91d 0xe91e) +event_prev))
	(ui-scroll symbol_scroll +scroll_flag_vertical))
```

Here we define a Window that has a Title bar, Toolbar and Scroll region. The
Title bar has a single close Button that will send an action event of
`+event_close`. The Toolbar contains two Buttons who's action events will be
`+event_prev` and `(+ +event_prev 1)`.

### (ui-window name [props] [body]) -> window

Creates a Window instance, with name (use _ if not required), optional property
list and nested forms.

The `(ui-props)` function is used to merge default properties with user
supplied overrides. For the Window widget you can see root defaults set for
properties that are not used by the window directly but are defaults for the
other widgets in the tree.

The `*env_xxx_xxx*` values come from the user login `pupa.inc` files. A user
can set these in their own private `pupa.inc` file. See the `apps/login/`
folder.

#### Properties

`:color :border :shadow :child`

```vdu
(defmacro ui-window (n &optional p &rest x)
	; (ui-window name [props] [body]) -> window
	(ui-props p
		:font *env_window_font*
		:ink_color *env_ink_col*
		:color *env_window_col*
		:hint_color *env_hint_col*
		:no_hint_color *env_no_hint_col*
		:border *env_window_border*
		:shadow *env_window_shadow*)
	`(ui-root ,n (Window) ,p
		(ui-flow _ (:flow_flags +flow_down_fill) ~x)))
```

### (ui-grid name [props] [body]) -> grid

Creates a Grid instance, with name, optional property list and nested forms.

#### Properties

`:grid_width :grid_height`

```vdu
(defmacro ui-grid (n &optional p &rest x)
	; (ui-grid name [props] [body]) -> grid
	`(ui-element ,n (Grid) ,p ~x))
```

### (ui-flow name [props] [body]) -> flow

Creates a Flow instance, with name, optional property list and nested forms.

#### Properties

`:flow_flags :min_width :min_height`

```vdu
(defmacro ui-flow (n &optional p &rest x)
	; (ui-flow name [props] [body]) -> flow
	`(ui-element ,n (Flow) ,p ~x))
```

### (ui-tree name event [props]) -> tree

Creates a Tree instance, with name, base event id and optional property list.

#### Properties

`:action_event :border :font`

```vdu
(defmacro ui-tree (n e &optional p)
	; (ui-tree name event [props]) -> tree
	`(ui-element ,n (Tree ,e) ,p))
```

### (ui-title name [props]) -> title

Creates a Title instance, with name and optional property list.

#### Properties

`:border :font :text :color :ink_color`

```vdu
(defmacro ui-title (n &optional p)
	; (ui-title name [props]) -> title
	(ui-props p
		:font *env_title_font*
		:border *env_title_border*)
	`(ui-element ,n (Title) ,p))
```

### (ui-title-bar name title symbols event [props]) -> flow

Creates a title bar, consiting of a Flow instance that contains Buttons and a
named Title instance.

The symbols list consists of unicode symbol values. You can use the Fonts
application to find a unicode symbol value.

```vdu
(defmacro ui-title-bar (n s b e &optional p)
	; (ui-title-bar name title symbols event [props]) -> flow
	(ui-props p
		:flow_flags +flow_left_fill
		:color *env_title_col*
		:font *env_title_buttons_font*)
	`(ui-flow _ ,p
		(ui-buttons ,b ,e (:border *env_title_buttons_border*))
		(ui-title ,n (:text ,s))))
```

### (ui-label name [props] [body]) -> label

Creates a Label instance, with name, optional property list and nested forms.

#### Properties

`:color :ink_color :text :font :border :min_width :min_height`

```vdu
(defmacro ui-label (n &optional p &rest x)
	; (ui-label name [props] [body]) -> label
	(ui-props p
		:flow_flags (num-intern (logior +flow_flag_right +flow_flag_align_vcenter))
		:border *env_label_border*)
	`(ui-element ,n (Label) ,p ~x))
```

### (ui-button name [props] [body]) -> button

Creates a Button instance, with name, optional property list and nested forms.

#### Properties

`:color :ink_color :text :font :border :min_width :min_height`

```vdu
(defmacro ui-button (n &optional p &rest x)
	; (ui-button name [props] [body]) -> button
	(ui-props p
		:flow_flags (num-intern (logior +flow_flag_down +flow_flag_align_hcenter +flow_flag_align_vcenter))
		:border *env_button_border*)
	`(ui-element ,n (Button) ,p ~x))
```

### (ui-buttons symbols event [props])

Creates a set of Button instances, with symbols list, event base and optional
property list.

The symbols list consists of UTF8 encoded strings or unicode symbol values. You
can use the Fonts application to find a unicode symbol value.

#### Properties

`:color :ink_color :text :font :border`

```vdu
(defmacro ui-buttons (s e &optional p)
	; (ui-buttons symbols event [props])
	(setq s (map (lambda (_) (if (num? _) (num-to-utf8 _) _)) s))
	(ui-props p
		:text _s)
	`(each (lambda (_s) (. (ui-button __ ,p) :connect (+ _ ,e))) '(~s)))
```

### (ui-tool-bar name [props] [body]) -> flow

Creates a Toolbar instance, with name, optional property list and nested forms.

```vdu
(defmacro ui-tool-bar (n &optional p &rest x)
	; (ui-tool-bar name [props] [body]) -> flow
	(ui-props p
		:flow_flags (num-intern (logior +flow_flag_right +flow_flag_fillh))
		:color *env_toolbar_col*
		:font *env_toolbar_font*)
	`(ui-flow ,n ,p ~x))
```

### (ui-textfield name [props]) -> textfield

Creates a Textfield instance, with name and optional property list.

#### Properties

`:color :ink_color :text :font :border :hint_color :no_hint_color :hint_text
:mode :cursor :anchor :min_width :min_height`

```vdu
(defmacro ui-textfield (n &optional p)
	; (ui-textfield name [props]) -> textfield
	(ui-props p
		:flow_flags (num-intern (logior +flow_flag_right +flow_flag_align_vcenter))
		:border *env_textfield_border*)
	`(ui-element ,n (Textfield) ,p))
```

### (ui-slider name [props]) -> slider

Creates a Slider instance, with name and optional property list.

#### Properties

`:color :value :maximum :portion`

```vdu
(defmacro ui-slider (n &optional p)
	; (ui-slider name [props]) -> slider
	(ui-props p
		:color *env_slider_col*)
	`(ui-element ,n (Slider) ,p))
```

### (ui-scroll name flags [props] [body]) -> scroll

Creates a Scroll instance, with name, flags, optional property list and nested
forms.

#### Properties

`:vslider :hslider :child :min_width :min_height`

```vdu
(defmacro ui-scroll (n f &optional p &rest x)
	; (ui-scroll name flags [props] [body]) -> scroll
	(ui-props p
		:color *env_slider_col*)
	`(ui-element ,n (Scroll ,f) ,p ~x))
```

### (ui-backdrop name [props] [body]) -> backdrop

Creates a Backdrop instance, with name, optional property list and nested
forms.

#### Properties

`:color :ink_color :style :spacing`

```vdu
(defmacro ui-backdrop (n &optional p &rest x)
	; (ui-backdrop name [props] [body]) -> backdrop
	(ui-props p
		:color *env_backdrop_col*
		:ink_color *env_backdrop_ink_col*)
	`(ui-element ,n (Backdrop) ,p ~x))
```

### (ui-progress name [props]) -> progress

Creates a Progress instance, with name and optional property list.

#### Properties

`:color :value :maximum`

```vdu
(defmacro ui-progress (n &optional p)
	; (ui-progress name [props]) -> progress
	`(ui-element ,n (Progress) ,p))
```

### (ui-canvas name width height scale &optional p) -> canvas

Creates a Canvas instance, with name, width, height, scale and optional
property list.

```vdu
(defmacro ui-canvas (n w h s &optional p)
	; (ui-canvas name width height scale [props]) -> canvas
	(ui-props p
		:color 0)
	`(ui-element ,n (Canvas ,w ,h ,s) ,p))
```

### (ui-vdu name [props]) -> vdu

Creates a Vdu instance, with name and optional property list.

#### Properties

`:vdu_width :vdu_height :min_width :min_height`

```vdu
(defmacro ui-vdu (n &optional p)
	; (ui-vdu name [props]) -> vdu
	(ui-props p
		:font *env_terminal_font*
		:color 0)
	`(ui-element ,n (Vdu) ,p))
```

### (ui-view name [props]) -> view

Creates a raw View instance, with name and optional property list.

#### Properties

`:min_width :min_height`

```vdu
(defmacro ui-view (n &optional p &rest x)
	; (ui-view name [props] [body]) -> view
	`(ui-element ,n (View) ,p ~x))
```
