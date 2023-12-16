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
section of the `ENVIRONMENT.md` document apply to UI trees !

## View class

The View class is the base widget class. Its main function is to interface to
the VP level GUI compositor. It defines a rectangular region that will be
rendered by calling the widget `:draw` methods. The GUI task will make this
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

This from the `apps/fonts/app.lisp` demo:

```lisp
(enums +event 0
	(enum close)
	(enum prev next))

(ui-window *window* ()
	(ui-title-bar *window_title* "Fonts" (0xea19) +event_close)
	(ui-tool-bar *main_toolbar* ()
		(ui-buttons (0xe91d 0xe91e) +event_prev))
	(ui-scroll *symbol_scroll* +scroll_flag_vertical
			(:min_width 256 :min_height 128)))

(ui-tool-tips *main_toolbar* '("prev" "next"))

*window*
```

Here we define a Window that has a Title bar, Toolbar and Scroll region. The
Title bar has a single close Button that will send an action event of
`+event_close`. The Toolbar contains two Buttons who's action events will be
`+event_prev` and `+event_next`.

### (ui-window name [props] [body]) -> window

```file
gui/lisp.inc "(defmacro ui-window" ""
```

```lisp
(ui-window *ui_window*
	(:min_width 128
	:min_height 128
	:font *env_window_font*
	:ink_color *env_ink_col*
	:color *env_window_col*
	:hint_color *env_hint_col*
	:no_hint_color *env_no_hint_col*
	:border *env_window_border*
	:shadow *env_window_shadow*)
	(ui-button _
		(:text "Main Area"
		:border 0)))

*ui_window*
```

Creates a Window instance, with name (use _ if not required), optional property
list and nested forms.

This macro creates a Window with a `+flow_down_fill` Flow child, nested forms
are added to that Flow.

The `(ui-props)` function is used to merge default properties with user
supplied overrides. For the Window widget you can see root defaults set for
properties that are not used by the window directly but are defaults for the
other widgets in the tree.

The `*env_xxx_xxx*` values come from the user login `env.inc` files. A user can
set these in their own private `env.inc` file. See the `apps/login/` folder.

#### Properties

`:color :border :shadow :child :min_width :min_height :tip_mbox`

### (ui-grid name [props] [body]) -> grid

```file
gui/lisp.inc "(defmacro ui-grid" ""
```

```lisp
(ui-window *ui_grid*
	(:min_width 128
	:min_height 0)
	(ui-grid _
		(:grid_width 3
		:grid_height 2)
		(ui-button _ (:text "Cell 1"))
		(ui-button _ (:text "Cell 2"))
		(ui-button _ (:text "Cell 3"))
		(ui-button _ (:text "Cell 4"))
		(ui-button _ (:text "Cell 5"))
		(ui-button _ (:text "Cell 6"))))

*ui_grid*
```

Creates a Grid instance, with name, optional property list and nested forms.

#### Properties

`:grid_width :grid_height`

If `:grid_width` or `:grid_height` equals 0 then the other property will be
calculated based on the number of child views contained.

### (ui-flow name [props] [body]) -> flow

```file
gui/lisp.inc "(defmacro ui-flow" ""
```

```lisp
(ui-window *ui_flow*
	(:min_width 128
	:min_height 0)
	(ui-flow _
		(:flow_flags +flow_right)
		(ui-button _ (:text "Cell 1"))
		(ui-button _ (:text "Cell 2"))
		(ui-button _ (:text "Cell 3"))
		(ui-button _ (:text "Cell 4"))
		(ui-button _ (:text "Cell 5"))
		(ui-button _ (:text "Cell 6"))))

*ui_flow*
```

Creates a Flow instance, with name, optional property list and nested forms.

#### Properties

`:flow_flags :min_width :min_height`

Flow flags defined in `gui/flow/class.inc`.

```
+flow_flag_left +flow_flag_right +flow_flag_up +flow_flag_down
+flow_flag_fillw +flow_flag_fillh +flow_flag_lastw +flow_flag_lasth
+flow_flag_align_hcenter +flow_flag_align_hleft +flow_flag_align_hright
+flow_flag_align_vcenter +flow_flag_align_vtop +flow_flag_align_vbottom
```

Useful flow combos:

```
+flow_down +flow_up +flow_right +flow_left
+flow_down_fill +flow_up_fill +flow_right_fill +flow_left_fill
+flow_stack_fill
```

### (ui-stack name tabs [props] [body]) -> stack

```file
gui/lisp.inc "(defmacro ui-stack" ""
```

```lisp
(ui-window *ui_stack*
	(:min_width 128
	:min_height 128)
	(ui-stack _ '("Tab 1" "Tab 2" "Tab 3")
		(:color +argb_white)
		(ui-button _ (:text "View 1"))
		(ui-button _ (:text "View 2"))
		(ui-button _ (:text "View 3"))))

*ui_stack*
```

Creates a Stack instance, with name, selection tabs from the given list,
optional property list and nested forms.

#### Properties

`:font`

### (ui-tree name event [props]) -> tree

```file
gui/lisp.inc "(defmacro ui-tree" ""
```

```lisp
(enums +event 0
	(enum file_folder_action file_leaf_action))

(ui-window *ui_tree*
	(:min_width 128
	:min_height 128)
	(ui-tree view +event_file_folder_action
		(:min_width 0
		:color 0
		:font *env_medium_terminal_font*)))

(. view :populate "docs/lisp/" ".md")
(bind '(w h) (. view :pref_size))
(. view :change 0 0 w h)

*ui_tree*
```

Creates a Tree instance, with name, base event id and optional property list.

#### Properties

`:action_event :font`

### (ui-spinner name [props]) -> spinner

```file
gui/lisp.inc "(defmacro ui-spinner" ""
```

```lisp
(ui-window *ui_spinner*
	(:min_width 0
	:min_height 0)
	(ui-spinner _
		(:value 50
		:maximum 100
		:minimum 0)))

*ui_spinner*
```

Creates a value Spinner instance, with name and optional property list.

#### Properties

`:value :maximum :minimum`

### (ui-hchart name title marks [props]) -> hchart

```file
gui/lisp.inc "(defmacro ui-hchart" ""
```

```lisp
(ui-window *ui_hchart*
	(:min_width 128
	:min_height 0)
	(ui-hchart view "My Chart" 10
		(:color +argb_green
		:units 1024)))

(times 3 (. view :add_bar))

*ui_hchart*
```

Creates a hchart instance, with name, title, marks and optional property list.

#### Properties

`:units :maximum`

### (ui-title name [props]) -> title

```file
gui/lisp.inc "(defmacro ui-title" ""
```

```lisp
(ui-window *ui_title*
	(:min_width 0
	:min_height 0)
	(ui-title _
		(:color *env_title_col*
		:ink_color *env_ink_col*
		:border *env_title_border*
		:font *env_title_font*
		:text "My Title")))

*ui_title*
```

Creates a Title instance, with name and optional property list.

#### Properties

`:border :font :text :color :ink_color`

### (ui-title-bar name title symbols event [props]) -> flow

```file
gui/lisp.inc "(defmacro ui-title-bar" ""
```

```lisp
(enums +event 0
	(enum close max min))

(ui-window *ui_title_bar*
	(:min_width 256
	:min_height 0)
	(ui-title-bar _ "My Title Bar" (0xea19 0xea1b 0xea1a) +event_close))

*ui_title_bar*
```

Creates a title bar, consisting of a Flow instance that contains Buttons and a
named Title instance.

The symbols list consists of unicode symbol values. You can use the Fonts
application to find a unicode symbol value.

### (ui-text name [props]) -> text

```file
gui/lisp.inc "(defmacro ui-text" ""
```

```lisp
(ui-window *ui_text*
	(:min_width 0
	:min_height 0)
	(ui-text _
		(:offset 0
		:color 0
		:ink_color *env_ink_col*
		:font *env_window_font*
		:text "Look At Me")))

*ui_text*
```

Creates a Text instance, with name and optional property list.

#### Properties

`:color :ink_color :text :font :min_width :min_height :offset`

### (ui-label name [props] [body]) -> label

```file
gui/lisp.inc "(defmacro ui-label" ""
```

```lisp
(ui-window *ui_label*
	(:min_width 0
	:min_height 0)
	(ui-label _
		(:color *env_window_col*
		:ink_color *env_ink_col*
		:border *env_label_border*
		:font *env_window_font*
		:text "Look At Me")))

*ui_label*
```

Creates a Label instance, with name, optional property list and nested forms.

Label allows `:flow_flags` that control position and alignment of the label
text, by default, set to `+flow_flag_right +flow_flag_align_vcenter`.

#### Properties

`:color :ink_color :text :font :border :min_width :min_height`

### (ui-button name [props] [body]) -> button

```file
gui/lisp.inc "(defmacro ui-button" ""
```

```lisp
(ui-window *ui_button*
	(:min_width 0
	:min_height 0)
	(ui-button _
		(:color *env_window_col*
		:ink_color *env_ink_col*
		:border *env_button_border*
		:font *env_window_font*
		:text "Click Me"
		:tip_text "a button")))

*ui_button*
```

Creates a Button instance, with name, optional property list and nested forms.

Button allows `:flow_flags` that control position and alignment of the label
text, by default, set to `+flow_flag_down +flow_flag_align_hcenter
+flow_flag_align_vcenter`.

#### Properties

`:color :ink_color :text :font :border :min_width :min_height :tip_text`

### (ui-buttons symbols event [props])

```file
gui/lisp.inc "(defmacro ui-buttons" ""
```

```lisp
(enums +event 0
	(enum undo redo rewind))

(ui-window *ui_buttons*
	(:min_width 0
	:min_height 0
	:font *env_toolbar_font*)
	(ui-buttons (0xe9fe 0xe99d 0xe9ff) +event_undo))

*ui_buttons*
```

Creates a set of Button instances, with symbols list, event base and optional
property list.

The symbols list consists of UTF8 encoded strings or unicode symbol values. You
can use the Fonts application to find a unicode symbol value.

#### Properties

`:color :ink_color :text :font :border`

### (ui-tool-bar name [props] [body]) -> flow

```file
gui/lisp.inc "(defmacro ui-tool-bar" ""
```

```lisp
(enums +event 0
	(enum undo redo rewind))

(ui-window *ui_toolbar*
	(:min_width 0
	:min_height 0)
	(ui-tool-bar view ()
		(ui-buttons (0xe9fe 0xe99d 0xe9ff) +event_undo)))

(ui-tool-tips view '("undo" "redo" "rewind"))

*ui_toolbar*
```

Creates a Toolbar instance, with name, optional property list and nested forms.

### (ui-textfield name [props]) -> textfield

```file
gui/lisp.inc "(defmacro ui-textfield" ""
```

```lisp
(ui-window *ui_textfield*
	(:min_width 256
	:min_height 0)
	(ui-textfield _
		(:offset 0
		:cursor 0
		:anchor 0
		:color +argb_white
		:ink_color *env_ink_col*
		:border *env_textfield_border*
		:font *env_window_font*
		:hint_text "type somthing"
		:hint_color *env_hint_col*
		:no_hint_color *env_no_hint_col*
		:text ""
		:clear_text "")))

*ui_textfield*
```

Creates a Textfield instance, with name and optional property list.

#### Properties

`:color :ink_color :text :clear_text :font :border :hint_color :no_hint_color
:hint_text :mode :cursor :anchor :min_width :min_height`

### (ui-slider name [props]) -> slider

```file
gui/lisp.inc "(defmacro ui-slider" ""
```

```lisp
(ui-window *ui_slider*
	(:min_width 256
	:min_height 0)
	(ui-slider _
		(:color *env_slider_col*
		:value 50
		:maximum 100
		:portion 25)))

*ui_slider*
```

Creates a Slider instance, with name and optional property list.

#### Properties

`:color :value :maximum :portion`

### (ui-scroll name flags [props] [body]) -> scroll

```file
gui/lisp.inc "(defmacro ui-scroll" ""
```

```lisp
(ui-window *ui_scroll*
	(:min_width 128
	:min_height 128)
	(ui-scroll _ +scroll_flag_both
		(:color *env_slider_col*
		:min_width 128
		:min_height 128)
		(ui-button view
			(:color +argb_orange
			:min_width 192
			:min_height 192
			:font *env_window_font*
			:text "Scroll View"))))

(bind '(w h) (. view :pref_size))
(. view :change 0 0 w h)

*ui_scroll*
```

Creates a Scroll instance, with name, flags, optional property list and nested
forms.

#### Properties

`:vslider :hslider :child :min_width :min_height`

Scroll flags defined in `gui/scroll/class.inc`.

```
+scroll_flag_vertical +scroll_flag_horizontal
```

Useful scroll combos:

```
+scroll_flag_both
```

### (ui-backdrop name [props] [body]) -> backdrop

```file
gui/lisp.inc "(defmacro ui-backdrop" ""
```

```lisp
(ui-window *ui_backdrop*
	(:min_width 128
	:min_height 128)
	(ui-backdrop _
		(:color +argb_black
		:ink_color +argb_white
		:style :grid
		:spacing 8)))

*ui_backdrop*
```

Creates a Backdrop instance, with name, optional property list and nested
forms.

#### Properties

`:color :ink_color :style :spacing`

Supported `:style` settings.

`:grid :axis :lines :plain`

### (ui-progress name [props]) -> progress

```file
gui/lisp.inc "(defmacro ui-progress" ""
```

```lisp
(ui-window *ui_progress*
	(:min_width 256
	:min_height 0)
	(ui-progress _
		(:color +argb_green
		:value 50
		:maximum 100)))

*ui_progress*
```

Creates a Progress instance, with name and optional property list.

#### Properties

`:color :value :maximum`

### (ui-canvas name width height scale &optional p) -> canvas

```file
gui/lisp.inc "(defmacro ui-canvas" ""
```

```lisp
(ui-window *ui_canvas*
	(:min_width 0
	:min_height 0)
	(ui-canvas view 128 128 1))

(.-> view
	(:fill +argb_white)
	(:set_color +argb_red)
	(:fbox 8 8 64 64)
	(:set_color +argb_green)
	(:fbox 32 48 88 64)
	(:set_color +argb_blue)
	(:fbox 16 100 90 16)
	(:swap 0))

*ui_canvas*
```

Creates a Canvas instance, with name, width, height, scale and optional
property list.

### (ui-vdu name [props]) -> vdu

```file
gui/lisp.inc "(defmacro ui-vdu" ""
```

```lisp
(ui-window *ui_vdu*
	(:min_width 0
	:min_height 0)
	(ui-flow _
		(:flow_flags +flow_stack_fill)
		(ui-vdu view
			(:vdu_width 16
			:vdu_height 4
			:ink_color +argb_green
			:font *env_editor_font*))
		(ui-backdrop _
			(:color +argb_grey1
			:style :plain))))

(bind '(w h) (. view :pref_size))
(. view :change 0 0 w h)
(. view :load
	'("This is line 1."
	"This is line 2."
	"This is line 3."
	"This is line 4.")
	0 0 0 0)

*ui_vdu*
```

Creates a Vdu instance, with name and optional property list.

#### Properties

`:vdu_width :vdu_height :min_width :min_height`

### (ui-view name [props]) -> view

```file
gui/lisp.inc "(defmacro ui-view" ""
```

Creates a raw View instance, with name and optional property list.

#### Properties

`:min_width :min_height`
