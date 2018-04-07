# Classes
## array
Super Class: sequence
### array::create -> class/array/create
### array::new -> class/array/new
### array::init -> class/array/init
```
inputs
r0 = array object (ptr)
r1 = vtable (pptr)
outputs
r0 = array object (ptr)
r1 = 0 if error, else ok
trashes
all but r0
```
### array::get_capacity -> class/array/get_capacity
```
inputs
r0 = array object (ptr)
outputs
r0 = array object (ptr)
r1 = capacity (uint)
```
### array::set_capacity -> class/array/set_capacity
```
inputs
r0 = array object (ptr)
r1 = capacity (uint)
outputs
r0 = array object (ptr)
trashes
r1-r7
```
### array::set_length -> class/array/set_length
```
inputs
r0 = array object (ptr)
r1 = length (uint)
outputs
r0 = array object (ptr)
```
### array::find -> class/array/find
```
inputs
r0 = array object (ptr)
r1 = element (long)
outputs
r0 = array object (ptr)
r1 = -1, else index (int)
trashes
r1-r4
```
### array::for_each -> class/array/for_each
```
inputs
r0 = array object (ptr)
r1 = element start index (uint)
r2 = element end index (uint)
r3 = predicate function (ptr)
r4 = predicate data (ptr)
outputs
r0 = array object (ptr)
r1 = 0, else break iterator (plong)
trashes
all but r0
callback predicate
inputs
r0 = predicate data (ptr)
r1 = element iterator (plong)
outputs
r1 = 0 if break, else not
trashes
all but r0
```
### array::sort -> class/array/sort
```
inputs
r0 = array object (ptr)
r1 = stack array object (ptr)
r2 = lower iter (plong)
r3 = upper iter (plong)
r4 = compare callback (ptr)
r5 = sort context (ptr)
outputs
r0 = array object (ptr)
trashes
all but r0
sort callback
inputs
r0 = context (ptr)
r1 = iter1 (plong)
r2 = iter2 (plong)
outputs
r0 = +, 0, -
trashes
all but r0
```
### array::partition -> class/array/partition
```
inputs
r0 = array object (ptr)
r1 = lower partition iter (plong)
r2 = upper partition iter (plong)
r3 = sort callback (ptr)
r4 = sort context (ptr)
outputs
r0 = array object (ptr)
r1 = partition iter (plong)
trashes
all but r0
sort callback
inputs
r0 = sort context (ptr)
r1 = iter1 (plong)
r2 = iter2 (plong)
outputs
r0 = +, 0, -
trashes
all but r0
```
### array::get_back -> class/array/get_back
```
inputs
r0 = array object (ptr)
outputs
r0 = array object (ptr)
r1 = element (long)
trashes
r2
```
### array::get_first -> class/array/get_first
```
inputs
r0 = array object (ptr)
outputs
r0 = array object (ptr)
r1 = element (long)
```
### array::get_second -> class/array/get_second
```
inputs
r0 = array object (ptr)
outputs
r0 = array object (ptr)
r1 = element (long)
```
### array::get_third -> class/array/get_third
```
inputs
r0 = array object (ptr)
outputs
r0 = array object (ptr)
r1 = element (long)
```
### array::get_element -> class/array/get_element
```
inputs
r0 = array object (ptr)
r1 = element index (uint)
outputs
r0 = array object (ptr)
r1 = element (long)
trashes
r2
```
### array::push_back -> class/array/push_back
```
inputs
r0 = array object (ptr)
r1 = element (long)
outputs
r0 = array object (ptr)
r1 = element (long)
trashes
r2-r7
```
### array::get_iter -> class/array/get_iter
```
inputs
r0 = array object (ptr)
r1 = element index (uint)
outputs
r0 = array object (ptr)
r1 = element iter (plong)
trashes
r2
```
### array::get_iters -> class/array/get_iters
```
inputs
r0 = array object (ptr)
outputs
r0 = array object (ptr)
r1 = begin element iter (plong)
r2 = end element iter (plong)
```
### array::get_begin -> class/array/get_begin
```
inputs
r0 = array object (ptr)
outputs
r0 = array object (ptr)
r1 = begin element iter (plong)
```
### array::get_end -> class/array/get_end
```
inputs
r0 = array object (ptr)
outputs
r0 = array object (ptr)
r1 = end element iter (plong)
trashes
r2
```
### array::each_callback -> class/obj/null
### array::sort_callback -> class/obj/null
### array::clear -> class/array/clear
```
inputs
r0 = array object (ptr)
outputs
r0 = array object (ptr)
trashes
r1
```
### array::ref_back -> class/array/ref_back
```
inputs
r0 = array object (ptr)
outputs
r0 = array object (ptr)
r1 = boxed_long object (ptr)
trashes
all but r0
```
### array::pop_back -> class/array/pop_back
```
inputs
r0 = array object (ptr)
outputs
r0 = array object (ptr)
trashes
r1
```
### array::set_element -> class/array/set_element
```
inputs
r0 = array object (ptr)
r1 = element (long)
r2 = element index (uint)
outputs
r0 = array object (ptr)
trashes
r2-r3
```
### array::append -> class/array/append
```
inputs
r0 = array object (ptr)
r1 = source array object (ptr)
r2 = element start index (uint)
r3 = element end index (uint)
outputs
r0 = array object (ptr)
trashes
r1-r7
```
### array::deinit -> class/array/deinit
```
inputs
r0 = array object (ptr)
trashes
all but r0
```
### array::get_length -> class/array/get_length
```
inputs
r0 = array object (ptr)
outputs
r0 = array object (ptr)
r1 = length (uint)
```
### array::ref_element -> class/array/ref_element
```
inputs
r0 = array object (ptr)
r1 = element index (uint)
outputs
r0 = array object (ptr)
r1 = boxed_long object (ptr)
trashes
all but r0
```
### array::slice -> class/array/slice
```
inputs
r0 = array object (ptr)
r1 = element start index (uint)
r2 = element end index (uint)
outputs
r0 = array object (ptr)
r1 = slice array object (ptr)
trashes
r1-r11
```
## boxed_long
Super Class: boxed_ptr
### boxed_long::create -> class/boxed_long/create
## boxed_ptr
Super Class: obj
### boxed_ptr::create -> class/boxed_ptr/create
### boxed_ptr::new -> class/boxed_ptr/new
### boxed_ptr::init -> class/boxed_ptr/init
```
inputs
r0 = boxed_ptr object (ptr)
r1 = vtable (pptr)
r2 = initial value (ptr)
outputs
r0 = boxed_ptr object (ptr)
r1 = 0 if error, else ok
```
### boxed_ptr::get_value -> class/boxed_ptr/get_value
```
inputs
r0 = boxed_ptr object (ptr)
outputs
r0 = boxed_ptr object (ptr)
r1 = value (ptr)
```
## button
Super Class: label
### button::create -> class/button/create
### button::new -> class/button/new
### button::init -> class/button/init
```
inputs
r0 = button object (ptr)
r1 = vtable (pptr)
outputs
r0 = button object (ptr)
r1 = 0 if error, else ok
```
### button::connect_click -> class/button/connect_click
```
inputs
r0 = button object (ptr)
r1 = target id (long)
outputs
r0 = button object (ptr)
trashes
all but r0
```
### button::deinit -> class/button/deinit
```
inputs
r0 = button object (ptr)
trashes
all but r0
```
### button::draw -> class/button/draw
```
inputs
r0 = button object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
### button::layout -> class/button/layout
```
inputs
r0 = button object (ptr)
trashes
all but r0
```
### button::mouse_down -> class/button/mouse_down
```
inputs
r0 = button object (ptr)
r1 = mouse event message (ptr)
trashes
all but r0
```
### button::mouse_up -> class/button/mouse_up
```
inputs
r0 = button object (ptr)
r1 = mouse event message (ptr)
trashes
all but r0
```
### button::mouse_move -> class/button/mouse_move
```
inputs
r0 = button object (ptr)
r1 = mouse event message (ptr)
trashes
all but r0
```
### button::lisp_create -> class/button/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## canvas
Super Class: view
### canvas::create -> class/canvas/create
```
inputs
r0 = width (pixels)
r1 = height (pixels)
r2 = aa scale (16.16)
outputs
r0 = 0 if error, else object (ptr)
trashes
r1-r6
```
### canvas::new -> class/canvas/new
### canvas::init -> class/canvas/init
```
inputs
r0 = canvas object (ptr)
r1 = vtable (pptr)
r2 = width (pixels)
r3 = height (pixels)
r4 = aa scale (16.16)
outputs
r0 = canvas object (ptr)
r1 = 0 if error, else ok
```
### canvas::swap -> class/canvas/swap
```
inputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::resize -> class/canvas/resize
```
inputs
r0 = canvas object (ptr)
r1 = source canvas object (ptr)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::fill -> class/canvas/fill
```
inputs
r0 = canvas object (ptr)
r1 = color (argb)
outputs
r0 = canvas object (ptr)
trashes
r2-r4
```
### canvas::to_premul -> class/canvas/to_premul
```
inputs
r0 = canvas object (ptr)
r1 = color (argb)
outputs
r0 = canvas object (ptr)
r1 = color premul (argb)
trashes
r2-r3
```
### canvas::to_argb -> class/canvas/to_argb
```
inputs
r0 = canvas object (ptr)
r1 = color premul (argb)
outputs
r0 = canvas object (ptr)
r1 = color (argb)
trashes
r2-r4
```
### canvas::as_argb -> class/canvas/as_argb
```
inputs
r0 = canvas object (ptr)
outputs
r0 = canvas object (ptr)
trashes
r1-r6
```
### canvas::as_premul -> class/canvas/as_premul
```
inputs
r0 = canvas object (ptr)
outputs
r0 = canvas object (ptr)
trashes
r1-r5
```
### canvas::fbox -> class/canvas/fbox
```
inputs
r0 = canvas object (ptr)
r1 = color (argb)
r2 = span function (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = w (pixels)
r10 = h (pixels)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::fpoly -> class/canvas/fpoly
```
inputs
r0 = canvas object (ptr)
r1 = point objects vector object (ptr)
r2 = color (argb)
r3 = winding mode (0/1)
r4 = span function (ptr)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::set_clip -> class/canvas/set_clip
```
inputs
r0 = canvas object (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = x1 (pixels)
r10 = y1 (pixels)
outputs
r0 = canvas object (ptr)
trashes
r1-r2
```
### canvas::set_span_noclip -> class/canvas/set_span_noclip
```
inputs
r0 = canvas object (ptr)
r1 = color (argb)
r7 = x (pixels)
r8 = y (pixels)
r9 = x1 (pixels)
outputs
r0 = canvas object (ptr)
trashes
r2-r3, r7-r9
```
### canvas::set_span -> class/canvas/set_span
```
inputs
r0 = canvas object (ptr)
r1 = color (argb)
r7 = x (pixels)
r8 = y (pixels)
r9 = x1 (pixels)
outputs
r0 = canvas object (ptr)
trashes
r2-r3, r7-r9
```
### canvas::set_fbox -> class/canvas/set_fbox
```
inputs
r0 = canvas object (ptr)
r1 = color (argb)
r7 = x (pixels)
r8 = y (pixels)
r9 = w (pixels)
r10 = h (pixels)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::set_fpoly -> class/canvas/set_fpoly
```
inputs
r0 = canvas object (ptr)
r1 = point objects vector object (ptr)
r2 = color (argb)
r3 = winding mode (0/1)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::blend_span_noclip -> class/canvas/blend_span_noclip
```
inputs
r0 = canvas object (ptr)
r1 = color (argb)
r7 = x (pixels)
r8 = y (pixels)
r9 = x1 (pixels)
outputs
r0 = canvas object (ptr)
trashes
r2-r3 r4-r9
```
### canvas::blend_span -> class/canvas/blend_span
```
inputs
r0 = canvas object (ptr)
r1 = color (argb)
r7 = x (pixels)
r8 = y (pixels)
r9 = x1 (pixels)
outputs
r0 = canvas object (ptr)
trashes
r2-r3 r4-r9
```
### canvas::blend_fbox -> class/canvas/blend_fbox
```
inputs
r0 = canvas object (ptr)
r1 = color (argb)
r7 = x (pixels)
r8 = y (pixels)
r9 = w (pixels)
r10 = h (pixels)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::blend_fpoly -> class/canvas/blend_fpoly
```
inputs
r0 = canvas object (ptr)
r1 = point objects vector object (ptr)
r2 = color (argb)
r3 = winding mode (0/1)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::deinit -> class/canvas/deinit
```
inputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::pref_size -> class/canvas/pref_size
```
inputs
r0 = canvas object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### canvas::draw -> class/canvas/draw
```
inputs
r0 = canvas object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
### canvas::lisp_create -> class/canvas/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## class
Super Class: null
### class::obj -> class/class_obj
### class::component -> class/class_component
### class::string -> class/class_string
### class::array -> class/class_array
### class::vector -> class/class_vector
### class::pair -> class/class_pair
### class::hash_set -> class/class_hash_set
### class::hash_map -> class/class_hash_map
### class::stream -> class/class_stream
### class::stream_str -> class/class_stream_str
### class::msg_out -> class/class_msg_out
### class::msg_in -> class/class_msg_in
### class::text -> class/class_text
### class::view -> class/class_view
### class::label -> class/class_label
### class::button -> class/class_button
### class::flow -> class/class_flow
### class::grid -> class/class_grid
### class::window -> class/class_window
### class::progress -> class/class_progress
### class::title -> class/class_title
### class::slave -> class/class_slave
### class::pipe -> class/class_pipe
### class::boxed_ptr -> class/class_boxed_ptr
### class::boxed_long -> class/class_boxed_long
### class::lisp -> class/class_lisp
### class::symbol -> class/class_symbol
### class::sequence -> class/class_sequence
### class::error -> class/class_error
### class::vdu -> class/class_vdu
### class::canvas -> class/class_canvas
### class::points -> class/class_points
### class::font -> class/class_font
### class::texture -> class/class_texture
## component
Super Class: hash_map
### component::init -> class/component/init
```
inputs
r0 = component object (ptr)
r1 = vtable (pptr)
outputs
r0 = component object (ptr)
r1 = 0 if error, else ok
```
### component::find_owner -> class/component/find_owner
```
inputs
r0 = component object (ptr)
outputs
r1 = 0, else tcb of owner (ptr)
trashes
r2
```
### component::emit -> class/component/emit
```
inputs
r0 = component object (ptr)
r1 = target id array object (ptr)
outputs
r0 = component object (ptr)
trashes
all but r0
```
### component::get_prop_sym -> class/component/get_prop_sym
```
inputs
r0 = component object (ptr)
r1 = property num (uint)
outputs
r0 = component object (ptr)
r1 = property symbol object (ptr)
trashes
all but r0
```
### component::get_prop -> class/component/get_prop
```
inputs
r0 = component object (ptr)
r1 = property num (uint)
outputs
r0 = component object (ptr)
r1 = 0 else, property object (ptr)
trashes
all but r0
```
### component::ref_prop -> class/component/ref_prop
```
inputs
r0 = component object (ptr)
r1 = property num (uint)
outputs
r0 = component object (ptr)
r1 = 0 else, property object (ptr)
trashes
all but r0
```
### component::set_long_prop -> class/component/set_long_prop
```
inputs
r0 = component object (ptr)
r1 = property num (uint)
r2 = property value (long)
outputs
r0 = component object (ptr)
trashes
all but r0
```
### component::get_long_prop -> class/component/get_long_prop
```
inputs
r0 = component object (ptr)
r1 = property num (uint)
outputs
r0 = component object (ptr)
r1 = property value (long)
trashes
all but r0
```
### component::set_font_prop -> class/component/set_font_prop
```
inputs
r0 = component object (ptr)
r1 = property num (uint)
r2 = font c string name (pubyte)
r3 = font size (points)
outputs
r0 = component object (ptr)
trashes
all but r0
```
### component::set_string_prop -> class/component/set_string_prop
```
inputs
r0 = component object (ptr)
r1 = property num (uint)
r2 = string pointer (ctsr)
outputs
r0 = component object (ptr)
trashes
all but r0
```
## error
Super Class: obj
### error::create -> class/error/create
### error::new -> class/error/new
### error::init -> class/error/init
```
inputs
r0 = error object (ptr)
r1 = vtable (pptr)
r2 = description c string (pubyte)
r3 = 0, else error msg index (uint)
r4 = error payload object (ptr)
r5 = filename c string (pubyte)
r6 = line number (uint)
outputs
r0 = error object (ptr)
r1 = 0 if error, else ok
trashes
r1-r6
```
### error::get_description -> class/error/get_description
```
inputs
r0 = error object (ptr)
outputs
r0 = error object (ptr)
r1 = string object (ptr)
```
### error::get_msg -> class/error/get_msg
```
inputs
r0 = error object (ptr)
outputs
r0 = error object (ptr)
r1 = error string object (ptr)
trashes
all but r0
```
### error::get_object -> class/error/get_object
```
inputs
r0 = error object (ptr)
outputs
r0 = error object (ptr)
r1 = error payload object (ptr)
```
### error::get_file -> class/error/get_file
```
inputs
r0 = error object (ptr)
outputs
r0 = error object (ptr)
r1 = string object (ptr)
```
### error::get_line -> class/error/get_line
```
inputs
r0 = error object (ptr)
outputs
r0 = error object (ptr)
r1 = line number (uint)
```
### error::deinit -> class/error/deinit
```
inputs
r0 = error object (ptr)
trashes
all but r0
```
## flow
Super Class: view
### flow::create -> class/flow/create
### flow::new -> class/flow/new
### flow::pref_size -> class/flow/pref_size
```
inputs
r0 = flow object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### flow::layout -> class/flow/layout
```
inputs
r0 = flow object (ptr)
trashes
all but r0
```
### flow::lisp_create -> class/flow/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## font
Super Class: obj
### font::open -> class/font/open
```
r0 = name c string (pubyte)
r1 = font size (points)
outputs
r0 = 0 if error, else font object (ptr)
trashes
all
```
### font::create -> class/font/create
### font::new -> class/font/new
### font::init -> class/font/init
```
inputs
r0 = font object (ptr)
r1 = vtable (pptr)
r2 = name c string (pubyte)
r3 = font size (points)
outputs
r0 = font object (ptr)
r1 = 0 if error, else ok
trashes
all but r0
```
### font::ref_word -> class/font/ref_word
```
inputs
r0 = font object (ptr)
r1 = string object (ptr)
outputs
r0 = font object (ptr)
r1 = texture object (ptr)
trashes
all but r0
```
### font::get_metrics -> class/font/get_metrics
```
inputs
r0 = font object (ptr)
outputs
r0 = font object (ptr)
r1 = ascent (pixels)
r2 = descent (pixels)
r3 = height (pixels)
```
### font::deinit -> class/font/deinit
```
inputs
r0 = font object (ptr)
trashes
all but r0
```
### font::lisp_create -> class/font/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## grid
Super Class: view
### grid::create -> class/grid/create
### grid::new -> class/grid/new
### grid::pref_size -> class/grid/pref_size
```
inputs
r0 = grid object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### grid::layout -> class/grid/layout
```
inputs
r0 = grid object (ptr)
trashes
all but r0
```
### grid::lisp_create -> class/grid/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## gui_ctx
Super Class: null
### gui_ctx::box -> gui/ctx/box
```
inputs
r0 = draw ctx (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = width (pixels)
r10 = height (pixels)
trashes
all
```
### gui_ctx::filled_box -> gui/ctx/filled_box
```
inputs
r0 = draw ctx (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = width (pixels)
r10 = height (pixels)
trashes
all
```
### gui_ctx::blit -> gui/ctx/blit
```
inputs
r0 = draw ctx (ptr)
r1 = texture id (ulong)
r2 = color mod (argb)
r7 = x (pixels)
r8 = y (pixels)
r9 = width (pixels)
r10 = height (pixels)
trashes
all
```
### gui_ctx::set_color -> gui/ctx/set_color
```
inputs
r0 = draw ctx (ptr)
r1 = color (argb)
trashes
all
```
### gui_ctx::panel -> gui/ctx/panel
```
inputs
r0 = draw ctx (ptr)
r1 = color (argb)
r2 = flags (ulong)
r3 = depth (int)
r7 = x (pixels)
r8 = y (pixels)
r9 = width (pixels)
r10 = height (pixels)
trashes
all
```
### gui_ctx::brighter -> gui/ctx/brighter
```
inputs
r1 = color (argb)
outputs
r1 = brighter color (argb)
trashes
r2, r3
```
### gui_ctx::darker -> gui/ctx/darker
```
inputs
r1 = color (argb)
outputs
r1 = darker color (argb)
trashes
r2, r3
```
## gui_gui
Super Class: null
### gui_gui::statics -> gui/gui_statics
### gui_gui::init -> gui/gui_init
```
inputs
r0 = sdl function table (pptr)
```
### gui_gui::update -> gui/gui_update
```
inputs
r0 = root view object (ptr)
trashes
all
```
### gui_gui::add -> gui/gui_add
```
inputs
r0 = view object (ptr)
trashes
all but r0
```
### gui_gui::gui -> gui/gui
```
gui process
```
## gui_region
Super Class: null
### gui_region::translate -> gui/region/translate
```
inputs
r1 = region listhead (ptr)
r7 = x translation (pixels)
r8 = y translation (pixels)
trashes
r1, r11-r14
```
### gui_region::bounds -> gui/region/bounds
```
inputs
r1 = region listhead (ptr)
outputs
r7 = x (pixels)
r8 = y (pixels)
r9 = x1 (pixels)
r10 = y1 (pixels)
trashes
r1, r11-r14
```
### gui_region::clip_rect -> gui/region/clip_rect
```
inputs
r0 = region heap (ptr)
r1 = source region listhead (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = x1 (pixels)
r10 = y1 (pixels)
trashes
r4-r14
```
### gui_region::remove_rect -> gui/region/remove_rect
```
inputs
r0 = region heap (ptr)
r1 = source region listhead (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = x1 (pixels)
r10 = y1 (pixels)
trashes
r1-r2, r4-r14
```
### gui_region::cut_rect -> gui/region/cut_rect
```
inputs
r0 = region heap (ptr)
r1 = source region listhead (ptr)
r2 = dest region listhead (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = x1 (pixels)
r10 = y1 (pixels)
```
### gui_region::copy_rect -> gui/region/copy_rect
```
inputs
r0 = region heap (ptr)
r1 = source region listhead (ptr)
r2 = dest region listhead (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = x1 (pixels)
r10 = y1 (pixels)
trashes
r1-r2, r4-r14
```
### gui_region::paste_rect -> gui/region/paste_rect
```
inputs
r0 = region heap (ptr)
r1 = dest region listhead (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = x1 (pixels)
r10 = y1 (pixels)
trashes
r1-r14
```
### gui_region::free -> gui/region/free
```
inputs
r0 = region heap (ptr)
r1 = source region listhead (ptr)
trashes
r1-r3
```
### gui_region::copy_region -> gui/region/copy_region
```
inputs
r0 = region heap (ptr)
r1 = source region listhead (ptr)
r2 = dest region listhead (ptr)
r3 = copy region listhead (ptr)
r7 = x translation (pixels)
r8 = y translation (pixels)
trashes
r1-r14
```
### gui_region::paste_region -> gui/region/paste_region
```
inputs
r0 = region heap (ptr)
r1 = source region listhead (ptr)
r2 = dest region listhead (ptr)
r7 = x translation (pixels)
r8 = y translation (pixels)
trashes
r1-r14
```
### gui_region::remove_region -> gui/region/remove_region
```
inputs
r0 = region heap (ptr)
r1 = source region listhead (ptr)
r2 = dest region listhead (ptr)
r7 = x translation (pixels)
r8 = y translation (pixels)
trashes
r1-r14
```
## hash_map
Super Class: hash_set
### hash_map::create -> class/hash_map/create
### hash_map::new -> class/hash_map/new
### hash_map::init -> class/hash_map/init
```
inputs
r0 = hash_map object (ptr)
r1 = vtable (pptr)
r2 = 0, else key compare callback (ptr)
r3 = num buckets (uint)
outputs
r0 = hash_map object (ptr)
r1 = 0 if error, else ok
trashes
r2-r7
```
### hash_map::find -> class/hash_map/find
```
inputs
r0 = hash_map object (ptr)
r1 = key object (ptr)
outputs
r0 = hash_map object (ptr)
r1 = 0, else found iterator (pptr)
r2 = bucket vector (ptr)
trashes
all but r0
```
### hash_map::copy -> class/hash_map/copy
```
inputs
r0 = hash_map object (ptr)
r1 = num buckets (uint)
outputs
r0 = hash_map object (ptr)
r1 = hash_map copy object (ptr)
trashes
all but r0
```
### hash_map::insert -> class/hash_map/insert
```
inputs
r0 = hash_map object (ptr)
r1 = key object (ptr)
r2 = value object (ptr)
outputs
r0 = hash_map object (ptr)
r1 = iterator (pptr)
r2 = bucket vector (ptr)
trashes
all but r0
```
### hash_map::search -> class/hash_map/search
```
inputs
r0 = hash_map object (ptr)
r1 = key object (ptr)
outputs
r0 = hash_map object (ptr)
r1 = 0, else iterator (pptr)
r2 = bucket vector (ptr)
trashes
all but r0
```
### hash_map::set -> class/hash_map/set
```
inputs
r0 = hash_map object (ptr)
r1 = key object (ptr)
r2 = value object (ptr)
outputs
r0 = hash_map object (ptr)
r1 = 0 if not found, else value object (ptr)
trashes
all but r0
```
### hash_map::get -> class/hash_map/get
```
inputs
r0 = hash_map object (ptr)
r1 = key object (ptr)
outputs
r0 = hash_map object (ptr)
r1 = 0 if not found, else value object (ptr)
trashes
all but r0
```
### hash_map::get_parent -> class/hash_map/get_parent
```
inputs
r0 = hash_map object (ptr)
outputs
r0 = hash_map object (ptr)
r1 = 0, else hash_map parent object (ptr)
```
### hash_map::set_parent -> class/hash_map/set_parent
```
inputs
r0 = hash_map object (ptr)
r1 = 0, else hash_map parent object (ptr)
outputs
r0 = hash_map object (ptr)
trashes
all but r0
```
### hash_map::deinit -> class/hash_map/deinit
```
inputs
r0 = hash_map object (ptr)
trashes
all but r0
```
## hash_set
Super Class: obj
### hash_set::create -> class/hash_set/create
### hash_set::new -> class/hash_set/new
### hash_set::init -> class/hash_set/init
```
inputs
r0 = hash_set object (ptr)
r1 = vtable (pptr)
r2 = 0, else key compare callback (ptr)
r3 = num buckets (uint)
outputs
r0 = hash_set object (ptr)
r1 = 0 if error, else ok
trashes
r2-r7
```
### hash_set::num_buckets -> class/hash_set/num_buckets
```
inputs
r0 = hash_set object (ptr)
outputs
r0 = hash_set object (ptr)
r1 = num buckets (uint)
```
### hash_set::get_bucket -> class/hash_set/get_bucket
```
inputs
r0 = hash_set object (ptr)
r1 = key object (ptr)
outputs
r0 = hash_set object (ptr)
r1 = bucket vector object (ptr)
trashes
all but r0
```
### hash_set::clear -> class/hash_set/clear
```
inputs
r0 = hash_set object (ptr)
outputs
r0 = hash_set object (ptr)
trashes
all but r0
```
### hash_set::for_each -> class/hash_set/for_each
```
inputs
r0 = hash_set object (ptr)
r1 = predicate function (ptr)
r2 = predicate data (ptr)
outputs
r0 = hash_set object (ptr)
r1 = 0, else break iterator (pptr)
r2 = 0, else bucket vector object (ptr)
trashes
all but r0
callback predicate
inputs
r0 = predicate data (ptr)
r1 = element iterator (pptr)
outputs
r1 = 0 if break, else not
trashes
all but r0
```
### hash_set::find -> class/hash_set/find
```
inputs
r0 = hash_set object (ptr)
r1 = key object (ptr)
outputs
r0 = hash_set object (ptr)
r1 = 0, else found iterator (pptr)
r2 = bucket vector object (ptr)
trashes
all but r0
```
### hash_set::insert -> class/hash_set/insert
```
inputs
r0 = hash_set object (ptr)
r1 = key object (ptr)
outputs
r0 = hash_set object (ptr)
r1 = element iterator (pptr)
r2 = bucket vector object (ptr)
trashes
all but r0
```
### hash_set::erase -> class/hash_set/erase
```
inputs
r0 = hash_set object (ptr)
r1 = element iterator (pptr)
r2 = bucket vector object (ptr)
outputs
r0 = hash_set object (ptr)
trashes
all but r0
```
### hash_set::copy -> class/hash_set/copy
```
inputs
r0 = hash_set object (ptr)
r1 = num buckets (uint)
outputs
r0 = hash_set object (ptr)
r1 = hash_set copy object (ptr)
trashes
all but r0
```
### hash_set::get_iters -> class/hash_set/get_iters
```
inputs
r0 = hash_set object (ptr)
outputs
r0 = hash_set object (ptr)
r1 = begin iter pointer (pptr)
r2 = end iter pointer (pptr)
trashes
r3-r4
```
### hash_set::key_callback -> class/obj/null
### hash_set::deinit -> class/hash_set/deinit
```
inputs
r0 = hash_set object (ptr)
trashes
all but r0
```
## kernel
Super Class: null
### kernel::id -> sys/kernel/id
```
outputs
r0 = cpu ID (uint)
```
### kernel::total -> sys/kernel/total
```
outputs
r0 = cpu total (uint)
```
### kernel::opts -> sys/kernel/opts
```
inputs
r0 = argv array (pptr)
trashes
all
info
process command options
```
### kernel::kernel -> sys/kernel/kernel
```
inputs
r0 = argv pointer (pptr)
r1 = SDL func table (pptr)
info
loader is already initialized when we get here !
```
### kernel::lisp_total -> sys/kernel/lisp_total
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## label
Super Class: view
### label::create -> class/label/create
### label::new -> class/label/new
### label::init -> class/label/init
```
inputs
r0 = label object (ptr)
r1 = vtable (pptr)
outputs
r0 = label object (ptr)
r1 = 0 if error, else ok
trashes
all but r0
```
### label::pref_size -> class/label/pref_size
```
inputs
r0 = label object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### label::draw -> class/label/draw
```
inputs
r0 = view object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
### label::layout -> class/label/layout
```
inputs
r0 = label object (ptr)
trashes
all but r0
```
### label::lisp_create -> class/label/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## lisp
Super Class: obj
### lisp::create -> class/lisp/create
### lisp::new -> class/lisp/new
### lisp::init -> class/lisp/init
```
inputs
r0 = lisp object object (ptr)
r1 = vtable (pptr)
r2 = stdin stream object (ptr)
r3 = stdout stream object (ptr)
r4 = stderr stream object (ptr)
outputs
r0 = lisp object object (ptr)
r1 = 0 if error, else ok
trashes
all
```
### lisp::deinit -> class/lisp/deinit
```
inputs
r0 = lisp object (ptr)
trashes
all but r0
```
### lisp::env_push -> class/lisp/env_push
```
inputs
r0 = lisp object (ptr)
outputs
r0 = lisp object (ptr)
```
### lisp::env_pop -> class/lisp/env_pop
```
inputs
r0 = lisp object (ptr)
outputs
r0 = lisp object (ptr)
```
### lisp::env_bind -> class/lisp/env_bind
```
inputs
r0 = lisp object (ptr)
r1 = vars list object (ptr)
r2 = vals sequence object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::env_args_set -> class/lisp/env_args_set
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
r2 = args dest (ptr)
r3 = args offset (uint)
outputs
r0 = lisp object (ptr)
```
### lisp::env_args_type -> class/lisp/env_args_type
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr) vector
r2 = type/s pointer
r3 = - or 0 all same type check, else + for type signature check
outputs
r0 = lisp object (ptr)
r1 = 0 if error, else ok
```
### lisp::read -> class/lisp/read
```
inputs
r0 = lisp object (ptr)
r1 = stream object (ptr)
r2 = next char (uint) (uint)
outputs
r0 = lisp object (ptr)
r1 = ast vector (ptr)
r2 = next char (uint) (uint)
```
### lisp::read_char -> class/lisp/read_char
```
inputs
r0 = lisp object (ptr)
r1 = stream object (ptr)
r2 = last char (uint)
outputs
r0 = lisp object (ptr)
r1 = next char (uint) (uint)
```
### lisp::read_rmacro -> class/lisp/read_rmacro
```
inputs
r0 = lisp object (ptr)
r1 = stream object (ptr)
r2 = next char (uint)
r3 = symbol object (ptr) 
outputs
r0 = lisp object (ptr)
r1 = list vector object (ptr)
r2 = next char (uint)
```
### lisp::read_list -> class/lisp/read_list
```
inputs
r0 = lisp object (ptr)
r1 = stream object (ptr)
r2 = next char (uint)
outputs
r0 = lisp object (ptr)
r1 = list vector object (ptr)
r2 = next char (uint)
```
### lisp::read_sym -> class/lisp/read_sym
```
inputs
r0 = lisp object (ptr)
r1 = stream object (ptr)
r2 = next char (uint)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
r2 = next char (uint)
```
### lisp::read_str -> class/lisp/read_str
```
inputs
r0 = lisp object (ptr)
r1 = stream object (ptr)
r2 = next char (uint)
r3 = closing char (uint)
outputs
r0 = lisp object (ptr)
r1 = string object (ptr)
r2 = next char (uint)
```
### lisp::read_num -> class/lisp/read_num
```
inputs
r0 = lisp object (ptr)
r1 = stream object (ptr)
r2 = next char (uint)
outputs
r0 = lisp object (ptr)
r1 = boxed_long object (ptr)
r2 = next char (uint)
```
### lisp::repl_eval -> class/lisp/repl_eval
```
inputs
r0 = lisp object (ptr)
r1 = form object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::repl_eval_list -> class/lisp/repl_eval_list
```
inputs
r0 = lisp object (ptr)
r1 = list vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::repl_apply -> class/lisp/repl_apply
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
r2 = boxed_ptr function object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
trashes
all but r0
```
### lisp::repl_print -> class/lisp/repl_print
```
inputs
r0 = lisp object (ptr)
r1 = stream object (ptr)
r2 = value
outputs
r0 = lisp object (ptr)
```
### lisp::repl_expand -> class/lisp/repl_expand
```
inputs
r0 = lisp object (ptr)
r1 = form object iter (pptr)
r2 = 0
outputs
r0 = lisp object (ptr)
r1 = expansion count (ulong)
```
### lisp::repl_error -> class/lisp/repl_error
```
inputs
r0 = lisp object (ptr)
r1 = description c string (pubyte)
r2 = 0, else error msg number (uint)
r3 = error payload object (ptr)
outputs
r0 = lisp object (ptr)
r1 = error object (ptr)
```
### lisp::func_ffi -> class/lisp/func_ffi
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_macroexpand_1 -> class/lisp/func_macroexpand_1
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_macroexpand -> class/lisp/func_macroexpand
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_defmacro -> class/lisp/func_defmacro
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_def -> class/lisp/func_def
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_defq -> class/lisp/func_defq
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_set -> class/lisp/func_set
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_setq -> class/lisp/func_setq
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_lambda -> class/lisp/func_lambda
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_quote -> class/lisp/func_quote
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_qquote -> class/lisp/func_qquote
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_add -> class/lisp/func_add
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_sub -> class/lisp/func_sub
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_mul -> class/lisp/func_mul
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_fmul -> class/lisp/func_fmul
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_div -> class/lisp/func_div
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_fdiv -> class/lisp/func_fdiv
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_mod -> class/lisp/func_mod
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_eql -> class/lisp/func_eql
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_eq -> class/lisp/func_eq
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_ne -> class/lisp/func_ne
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_lt -> class/lisp/func_lt
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_gt -> class/lisp/func_gt
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_le -> class/lisp/func_le
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_ge -> class/lisp/func_ge
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_cond -> class/lisp/func_cond
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_progn -> class/lisp/func_progn
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_length -> class/lisp/func_length
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_while -> class/lisp/func_while
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_print -> class/lisp/func_print
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_prin -> class/lisp/func_prin
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_env -> class/lisp/func_env
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = environment hasp_map object (ptr)
```
### lisp::func_sym -> class/lisp/func_sym
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_str -> class/lisp/func_str
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_cat -> class/lisp/func_cat
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_elem -> class/lisp/func_elem
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_elemset -> class/lisp/func_elemset
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_copy -> class/lisp/func_copy
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_gensym -> class/lisp/func_gensym
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_filestream -> class/lisp/func_filestream
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_strstream -> class/lisp/func_strstream
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_readchar -> class/lisp/func_readchar
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_readline -> class/lisp/func_readline
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_writechar -> class/lisp/func_writechar
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_writeline -> class/lisp/func_writeline
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_write -> class/lisp/func_write
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_char -> class/lisp/func_char
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_code -> class/lisp/func_code
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_slice -> class/lisp/func_slice
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_bshr -> class/lisp/func_bshr
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_basr -> class/lisp/func_basr
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_bshl -> class/lisp/func_bshl
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_band -> class/lisp/func_band
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_bor -> class/lisp/func_bor
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_bxor -> class/lisp/func_bxor
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_push -> class/lisp/func_push
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_pop -> class/lisp/func_pop
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_apply -> class/lisp/func_apply
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_repl -> class/lisp/func_repl
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_eval -> class/lisp/func_eval
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_catch -> class/lisp/func_catch
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_save -> class/lisp/func_save
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_load -> class/lisp/func_load
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_inst_of -> class/lisp/func_inst_of
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_split -> class/lisp/func_split
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_defined -> class/lisp/func_defined
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_find -> class/lisp/func_find
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_age -> class/lisp/func_age
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_error -> class/lisp/func_error
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_pipe -> class/lisp/func_pipe
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_rpipe -> class/lisp/func_rpipe
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_wpipe -> class/lisp/func_wpipe
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_time -> class/lisp/func_time
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_clear -> class/lisp/func_clear
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_match -> class/lisp/func_match
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_each -> class/lisp/func_each
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_some -> class/lisp/func_some
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_list -> class/lisp/func_list
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_cmp -> class/lisp/func_cmp
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_call -> class/lisp/func_call
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_array -> class/lisp/func_array
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_points -> class/lisp/func_points
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_fsqrt -> class/lisp/func_fsqrt
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_fmod -> class/lisp/func_fmod
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_frac -> class/lisp/func_frac
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_floor -> class/lisp/func_floor
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_fsin -> class/lisp/func_fsin
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_fcos -> class/lisp/func_fcos
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_bind -> class/lisp/func_bind
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_read -> class/lisp/func_read
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### lisp::func_merge -> class/lisp/func_merge
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## math
Super Class: null
### math::random -> sys/math/random
```
inputs
r0 = random range (ulong)
outputs
r0 = random number in range (ulong)
trashes
r1-r2
```
### math::isqrt -> sys/math/isqrt
```
inputs
r0 = number (ulong)
outputs
r0 = sqrt (ulong)
trashes
r1-r3
```
### math::fsqrt -> sys/math/fsqrt
```
inputs
r0 = number (16.16)
outputs
r0 = sqrt (16.16)
trashes
r1-r3
```
### math::fsin -> sys/math/fsin
```
inputs
r0 = angle in radians (16.16)
outputs
r0 = sine (16.16)
trashes
r1-r4
```
### math::fcos -> sys/math/fcos
```
inputs
r0 = angle in radians (16.16)
outputs
r0 = cosine (16.16)
trashes
r1-r4
```
### math::intersect -> sys/math/intersect
```
inputs
r0 = p1 xy (16.16/16.16)
r1 = v1 xy (16.16/16.16)
r2 = p2 xy (16.16/16.16)
r3 = v2 xy (16.16/16.16)
outputs
r0 = i xy (16.16/16.16)
trashes
all
```
### math::distance_sqd -> sys/math/distance_sqd
```
inputs
r0 = p xy (16.16/16.16)
r1 = p1 xy (16.16/16.16)
r2 = p2 xy (16.16/16.16)
outputs
r0 = distance squared (16.16)
trashes
all
```
## msg_in
Super Class: stream
### msg_in::create -> class/msg_in/create
### msg_in::new -> class/msg_in/new
### msg_in::init -> class/msg_in/init
```
inputs
r0 = msg_in object (ptr)
r1 = vtable (pptr)
outputs
r0 = msg_in object (ptr)
r1 = 0 if error, else ok
trashes
all but r0
```
### msg_in::next_seq -> class/msg_in/next_seq
```
inputs
r0 = list head (ptr)
r1 = 0, else new stream msg (ptr)
r2 = seqnum (ulong)
outputs
r0 = 0, else next stream msg (ptr)
r1 = 0, else string data (pubyte)
```
### msg_in::deinit -> class/msg_in/deinit
```
inputs
r0 = msg_in object (ptr)
trashes
all but r0
```
### msg_in::read_ready -> class/msg_in/read_ready
```
inputs
r0 = msg_in object (ptr)
outputs
r0 = msg_in object (ptr)
r1 = 0 if data not available
trashes
all but r0
```
### msg_in::read_next -> class/msg_in/read_next
```
inputs
r0 = msg_in object (ptr)
outputs
r0 = msg_in object (ptr)
r1 = -1 for EOF, else more data
trashes
all but r0
```
## msg_out
Super Class: stream
### msg_out::create -> class/msg_out/create
### msg_out::new -> class/msg_out/new
### msg_out::init -> class/msg_out/init
```
inputs
r0 = msg_out object (ptr)
r1 = vtable (pptr)
r2, r3 = target mailbox id (id)
outputs
r0 = msg_out object (ptr)
r1 = 0 if error, else ok
trashes
all but r0
```
### msg_out::set_state -> class/msg_out/set_state
```
inputs
r0 = msg_out object (ptr)
r1 = stream state (ulong)
```
### msg_out::deinit -> class/msg_out/deinit
```
inputs
r0 = msg_out object (ptr)
trashes
all but r0
```
### msg_out::write_flush -> class/msg_out/write_flush
```
inputs
r0 = msg_out object (ptr)
outputs
r0 = msg_out object (ptr)
trashes
all but r0
```
### msg_out::write_next -> class/msg_out/write_next
```
inputs
r0 = msg_out object (ptr)
outputs
r0 = msg_out object (ptr)
trashes
all but r0
```
## obj
Super Class: null
### obj::destroy -> class/obj/destroy
```
inputs
r0 = object (ptr)
trashes
all
```
### obj::init -> class/obj/init
```
inputs
r0 = object (ptr)
r1 = vtable (pptr)
outputs
r1 = 0 if error, else ok
```
### obj::inst_of -> class/obj/inst_of
```
inputs
r0 = object (ptr)
r1 = vtable of tested type (ptr)
outputs
r0 = object (ptr)
r1 = 0 if not, else vtable of object (ptr)
trashes
r2
```
### obj::ref -> class/obj/ref
```
inputs
r0 = object (ptr)
trashes
r1
```
### obj::deref -> class/obj/deref
```
inputs
r0 = object (ptr)
trashes
all
```
### obj::ref_if -> class/obj/ref_if
```
inputs
r0 = 0, else object (ptr)
trashes
r1
```
### obj::deref_if -> class/obj/deref_if
```
inputs
r0 = 0, else object (ptr)
trashes
all
```
### obj::delete -> sys/mem/free
```
inputs
r0 = address (ptr)
trashes
r0-r2
```
### obj::deinit -> class/obj/null
### obj::hash -> class/obj/hash
```
inputs
r0 = object (ptr)
outputs
r0 = object (ptr)
r1 = hash code (ulong)
trashes
all but r0
```
## pair
Super Class: obj
### pair::create -> class/pair/create
### pair::new -> class/pair/new
### pair::init -> class/pair/init
```
inputs
r0 = pair object (ptr)
r1 = vtable (pptr)
r2 = first object (ptr)
r3 = second object (ptr)
outputs
r0 = pair object (ptr)
r1 = 0 if error, else ok
```
### pair::ref_first -> class/pair/ref_first
```
inputs
r0 = pair object (ptr)
outputs
r0 = pair object (ptr)
r1 = object (ptr)
trashes
r2
```
### pair::ref_second -> class/pair/ref_second
```
inputs
r0 = pair object (ptr)
outputs
r0 = pair object (ptr)
r1 = object (ptr)
trashes
r2
```
### pair::get_first -> class/pair/get_first
```
inputs
r0 = pair object (ptr)
outputs
r0 = pair object (ptr)
r1 = object (ptr)
```
### pair::get_second -> class/pair/get_second
```
inputs
r0 = pair object (ptr)
outputs
r0 = pair object (ptr)
r1 = object (ptr)
```
### pair::set_first -> class/pair/set_first
```
inputs
r0 = pair object (ptr)
r1 = object (ptr)
outputs
r0 = pair object (ptr)
trashes
all but r0
```
### pair::set_second -> class/pair/set_second
```
inputs
r0 = pair object (ptr)
r1 = object (ptr)
outputs
r0 = pair object (ptr)
trashes
all but r0
```
### pair::deinit -> class/pair/deinit
```
inputs
r0 = pair object (ptr)
trashes
all but r0
```
## pii
Super Class: null
### pii::exit -> sys/pii/exit
```
inputs
r0 = code (long)
```
### pii::mmap -> sys/pii/mmap
```
inputs
r0 = address (ptr)
r1 = len (ulong)
r2 = prot (ulong)
r3 = flags (ulong)
r4 = fd (ulong)
r5 = pos (ulong)
outputs
r0 = buffer (ptr)
trashes
none
```
### pii::munmap -> sys/pii/munmap
```
inputs
r0 = buffer (ptr)
r1 = size (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### pii::mprotect -> sys/pii/mprotect
```
inputs
r0 = buffer (ptr)
r1 = size (ulong)
r2 = prot (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### pii::open -> sys/pii/open
```
inputs
r0 = c string filename (pubyte)
r1 = mode (ulong)
r2 = flags (ulong)
outputs
r0 = fd (ulong)
trashes
none
```
### pii::close -> sys/pii/close
```
inputs
r0 = fd (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### pii::ftruncate -> sys/pii/ftruncate
```
inputs
r0 = fd (ulong)
r1 = size (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### pii::unlink -> sys/pii/unlink
```
inputs
r0 = c string filename (pubyte)
outputs
r0 = error code (ulong)
trashes
none
```
### pii::stat -> sys/pii/stat
```
inputs
r0 = c string filename (pubyte)
r1 = stat buf (ptr)
outputs
r0 = error code (ulong)
trashes
none
```
### pii::fcntl -> sys/pii/fcntl
```
inputs
r0 = fd (ulong)
r1 = command (ulong)
r2 = arg (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### pii::write -> sys/pii/write
```
inputs
r0 = fd (ulong)
r1 = buffer (pubyte)
r2 = length (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### pii::write_char -> sys/pii/write_char
```
inputs
r0 = fd (ulong)
r1 = char (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### pii::write_str -> sys/pii/write_str
```
inputs
r0 = fd (ulong)
r1 = c string (pubyte)
outputs
r0 = error code (ulong)
trashes
none
```
### pii::write_num -> sys/pii/write_num
```
inputs
r0 = fd (ulong)
r1 = number (ulong)
r2 = base (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### pii::read -> sys/pii/read
```
inputs
r0 = fd (ulong)
r1 = buffer (ptr)
r2 = size (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### pii::read_char -> sys/pii/read_char
```
inputs
r0 = fd (ulong)
outputs
r0 = char (ulong)
trashes
none
```
### pii::age -> sys/pii/age
```
inputs
r0 = c string file name (pubyte)
outputs
r0 = 0 if error, else modified date (ulong)
trashes
none
```
### pii::time -> sys/pii/time
```
outputs
r0 = time in usec (ulong)
trashes
none
```
## pipe
Super Class: obj
### pipe::create -> class/pipe/create
### pipe::new -> class/pipe/new
### pipe::init -> class/pipe/init
```
inputs
r0 = pipe object (ptr)
r1 = vtable (pptr)
r2 = command buffer (pubyte)
r3 = length (bytes)
outputs
r0 = pipe object (ptr)
r1 = 0 if error, else ok
trashes
all but r0
```
### pipe::select -> class/pipe/select
```
inputs
r0 = pipe object (ptr)
r1 = user mailbox (ptr)
outputs
r0 = pipe object (ptr)
r1 = mailbox with mail (ptr)
trashes
all but r0
```
### pipe::get_state -> class/pipe/get_state
```
inputs
r0 = pipe object (ptr)
outputs
r0 = pipe object (ptr)
r1 = current state
```
### pipe::set_state -> class/pipe/set_state
```
inputs
r0 = pipe object (ptr)
r1 = current state (ulong)
outputs
r0 = pipe object (ptr)
```
### pipe::get_input -> class/pipe/get_input
```
inputs
r0 = pipe object (ptr)
outputs
r0 = pipe object (ptr)
r1 = input stream object (ptr)
```
### pipe::deinit -> class/pipe/deinit
```
inputs
r0 = pipe object (ptr)
trashes
all but r0
```
## points
Super Class: array
### points::create -> class/points/create
### points::filter_polyline -> class/points/filter_polyline
```
inputs
r0 = points object (ptr)
r1 = source points object, can be same (ptr)
r2 = tolerance (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::filter_polygon -> class/points/filter_polygon
```
inputs
r0 = points object (ptr)
r1 = source points object, can be same (ptr)
r2 = tolerance (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::transform -> class/points/transform
```
inputs
r0 = points object (ptr)
r1 = source points object, can be same (ptr)
r2 = m1 xy (16.16/16.16)
r3 = m2 xy (16.16/16.16)
r4 = tr xy (16.16/16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::simplify -> class/points/simplify
```
inputs
r0 = points object (ptr)
r1 = source points object
r2 = stack array object (ptr)
r3 = tolerance (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::gen_clerp -> class/points/gen_clerp
```
inputs
r0 = points object (ptr)
r1 = stack array object (ptr)
r2 = center xy (16.16/16.16)
r3 = v1 (16.16/16.16)
r4 = v2 (16.16/16.16)
r5 = radius (16.16)
r6 = tolerance (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::gen_arc -> class/points/gen_arc
```
inputs
r0 = points object (ptr)
r1 = stack array object (ptr)
r2 = center xy (16.16/16.16)
r3 = start angle (16.16)
r4 = end angle (16.16)
r5 = radius (16.16)
r6 = tolerance (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::gen_quadratic -> class/points/gen_quadratic
```
inputs
r0 = points object (ptr)
r1 = stack array object (ptr)
r2 = p1 xy (16.16/16.16)
r3 = p2 xy (16.16/16.16)
r4 = p3 xy (16.16/16.16)
r5 = tolerance (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::gen_cubic -> class/points/gen_cubic
```
inputs
r0 = points object (ptr)
r1 = stack array object (ptr)
r2 = p1 xy (16.16/16.16)
r3 = p2 xy (16.16/16.16)
r4 = p3 xy (16.16/16.16)
r5 = p4 xy (16.16/16.16)
r6 = tolerance (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::stroke_joints -> class/points/stroke_joints
```
inputs
r0 = points object (ptr)
r1 = stack array object (ptr)
r2 = in points start iter (ptr)
r3 = in points end iter (ptr)
r4 = p1 xy (16.16/16.16)
r5 = p2 xy (16.16/16.16)
r6 = join style (byte)
r7 = radius (16.16)
r8 = tolerance (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::stroke_polylines -> class/points/stroke_polylines
```
inputs
r0 = output polygons vector object (ptr)
r1 = stack array object (ptr)
r2 = input polylines vector object (ptr)
r3 = join style (byte)
r4 = cap style1 (byte)
r5 = cap style2 (byte)
r6 = radius (16.16)
r7 = tolerance (16.16)
outputs
r0 = output polygons vector object (ptr)
trashes
all but r0
```
### points::stroke_polygons -> class/points/stroke_polygons
```
inputs
r0 = output polygons vector object (ptr)
r1 = stack array object (ptr)
r2 = input polygons vector object (ptr)
r3 = join style (byte)
r4 = radius (16.16)
r5 = tolerance (16.16)
outputs
r0 = output polygons vector object (ptr)
trashes
all but r0
```
## progress
Super Class: view
### progress::create -> class/progress/create
### progress::new -> class/progress/new
### progress::pref_size -> class/progress/pref_size
```
inputs
r0 = progress object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### progress::draw -> class/progress/draw
```
inputs
r0 = window object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
### progress::layout -> class/progress/layout
```
inputs
r0 = progress object (ptr)
trashes
all but r0
```
### progress::lisp_create -> class/progress/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## sdl
Super Class: null
### sdl::sdl_set_main_ready -> nil
### sdl::sdl_init -> nil
### sdl::sdl_get_error -> nil
### sdl::sdl_quit -> nil
### sdl::sdl_create_window -> nil
### sdl::sdl_create_window_and_renderer -> nil
### sdl::sdl_destroy_window -> nil
### sdl::sdl_delay -> nil
### sdl::sdl_create_renderer -> nil
### sdl::sdl_set_render_draw_color -> nil
### sdl::sdl_render_fill_rect -> nil
### sdl::sdl_render_present -> nil
### sdl::sdl_render_set_clip_rect -> nil
### sdl::sdl_set_render_draw_blend_mode -> nil
### sdl::sdl_poll_event -> nil
### sdl::sdl_render_draw_rect -> nil
### sdl::sdl_free_surface -> nil
### sdl::sdl_create_texture_from_surface -> nil
### sdl::sdl_destroy_texture -> nil
### sdl::sdl_render_copy -> nil
### sdl::sdl_set_texture_blend_mode -> nil
### sdl::sdl_set_texture_color_mod -> nil
### sdl::sdl_create_rgb_surface_from -> nil
### sdl::ttf_init -> nil
### sdl::ttf_quit -> nil
### sdl::ttf_open_font -> nil
### sdl::ttf_close_font -> nil
### sdl::ttf_size_utf8 -> nil
### sdl::ttf_font_ascent -> nil
### sdl::ttf_font_descent -> nil
### sdl::ttf_font_height -> nil
### sdl::ttf_render_utf8_blended -> nil
## sequence
Super Class: obj
### sequence::get_length -> class/obj/null
### sequence::ref_element -> class/obj/null
### sequence::slice -> class/obj/null
## slave
Super Class: obj
### slave::create -> class/slave/create
### slave::new -> class/slave/new
### slave::init -> class/slave/init
```
inputs
r0 = slave object (ptr)
r1 = vtable (pptr)
outputs
r1 = 0 if error, else ok
trashes
all but r0
```
### slave::get_args -> class/slave/get_args
```
inputs
r0 = slave object (ptr)
outputs
r0 = slave object (ptr)
r1 = command args
```
### slave::deinit -> class/slave/deinit
```
inputs
r0 = slave object (ptr)
trashes
all but r0
```
### slave::lisp_create -> class/slave/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## stream
Super Class: obj
### stream::create -> class/stream/create
### stream::new -> class/stream/new
### stream::init -> class/stream/init
```
inputs
r0 = stream object (ptr)
r1 = vtable (pptr)
r2 = buffer object, 0 if none (ptr)
r3 = buffer data, 0 if none (ptr)
r4 = buffer start (pubyte)
r5 = buffer length (uint)
outputs
r0 = stream object (ptr)
r1 = 0 if error, else ok
trashes
all but r0
```
### stream::available -> class/stream/available
```
inputs
r0 = stream object (ptr)
outputs
r0 = stream object (ptr)
r1 = available space
trashes
r2
```
### stream::read_char -> class/stream/read_char
```
inputs
r0 = stream object (ptr)
outputs
r0 = stream object (ptr)
r1 = -1 for EOF, else char read (int)
trashes
all but r0
```
### stream::read_line -> class/stream/read_line
```
inputs
r0 = stream object (ptr)
outputs
r0 = stream object (ptr)
r1 = 0 for EOF, else string object (ptr)
trashes
all but r0
```
### stream::read -> class/stream/read
```
inputs
r0 = stream object (ptr)
r1 = buffer (pubyte)
r2 = buffer length (uint)
outputs
r0 = stream object (ptr)
r1 = -1 for EOF, else bytes read (int)
trashes
all but r0
```
### stream::write_char -> class/stream/write_char
```
inputs
r0 = stream object (ptr)
r1 = char (uint)
outputs
r0 = stream object (ptr)
trashes
all but r0
```
### stream::write -> class/stream/write
```
inputs
r0 = stream object (ptr)
r1 = buffer (pubyte)
r2 = buffer length (uint)
outputs
r0 = stream object (ptr)
trashes
all but r0
```
### stream::write_cstr -> class/stream/write_cstr
```
inputs
r0 = stream object (ptr)
r1 = buffer (pubyte)
outputs
r0 = stream object (ptr)
trashes
all but r0
```
### stream::skip -> class/stream/skip
```
inputs
r0 = stream object (ptr)
r1 = char to skip (uint)
trashes
all but r0
```
### stream::skip_not -> class/stream/skip_not
```
inputs
r0 = stream object (ptr)
r1 = char to not skip (uint)
trashes
all but r0
```
### stream::split -> class/stream/split
```
inputs
r0 = stream object (ptr)
r1 = split char (uint)
outputs
r0 = stream object (ptr)
r1 = split strings vector object (ptr)
trashes
all but r0
```
### stream::deinit -> class/stream/deinit
```
inputs
r0 = stream object (ptr)
trashes
all but r0
```
### stream::read_ready -> class/stream/read_ready
```
inputs
r0 = stream object (ptr)
outputs
r0 = stream object (ptr)
r1 = 0 if data not available
trashes
all but r0
```
### stream::read_next -> class/stream/read_next
```
inputs
r0 = stream object (ptr)
outputs
r0 = stream object (ptr)
r1 = -1 for EOF, else more data
trashes
all but r0
```
### stream::write_next -> class/stream/write_next
```
inputs
r0 = stream object (ptr)
outputs
r0 = stream object (ptr)
trashes
all but r0
```
### stream::write_flush -> class/stream/write_flush
```
inputs
r0 = stream object (ptr)
outputs
r0 = stream object (ptr)
trashes
r1
```
## stream_str
Super Class: stream
### stream_str::create -> class/stream_str/create
### stream_str::new -> class/stream_str/new
### stream_str::init -> class/stream_str/init
```
inputs
r0 = stream_str object (ptr)
r1 = vtable (pptr)
r2 = string object (ptr)
outputs
r1 = 0 if error, else ok
trashes
all but r0
```
### stream_str::ref_string -> class/stream_str/ref_string
```
inputs
r0 = stream_str object (ptr)
outputs
r0 = stream_str object (ptr)
r1 = string object
trashes
r2
```
### stream_str::write_next -> class/stream_str/write_next
```
inputs
r0 = stream_str object (ptr)
outputs
r0 = stream_str object (ptr)
trashes
all but r0
```
### stream_str::write_flush -> class/stream_str/write_flush
```
inputs
r0 = stream_str object (ptr)
outputs
r0 = stream_str object (ptr)
trashes
all but r0
```
## string
Super Class: sequence
### string::create_from_buffer -> class/string/create_from_buffer
```
inputs
r0 = buffer (pubyte)
r1 = buffer length (uint)
outputs
r0 = 0 if error, else object (ptr)
trashes
r1-r6
```
### string::create_from_cstr -> class/string/create_from_cstr
```
inputs
r0 = c string (pubyte)
outputs
r0 = 0 if error, else object (ptr)
trashes
r1-r6
```
### string::create_from_file -> class/string/create_from_file
```
inputs
r0 = c string (pubyte)
outputs
r0 = 0 if error, else object (ptr)
trashes
r1-r6
```
### string::create_from_long -> class/string/create_from_long
```
inputs
r0 = number (long)
r1 = base (long)
outputs
r0 = 0 if error, else object (ptr)
trashes
all
```
### string::append -> class/string/append
```
inputs
r0 = string object (ptr)
r1 = string object (ptr)
outputs
r0 = 0 if error, else new string object (ptr)
trashes
r1-r6
```
### string::cat -> class/string/cat
```
inputs
r0 = string objects vector object (ptr)
outputs
r0 = 0 if error, else new string object (ptr)
trashes
r1-r6
```
### string::new -> class/string/new
```
inputs
r0 = object size (bytes)
outputs
r0 = 0 if error, else object (ptr)
trashes
r1-r3
```
### string::init -> class/string/init
```
inputs
r0 = string object (ptr)
r1 = vtable (pptr)
r2 = 0 else, buffer (pubyte)
r3 = buffer length (uint)
outputs
r1 = 0 if error, else ok
```
### string::init1 -> class/string/init1
```
inputs
r0 = string object (ptr)
r1 = vtable (pptr)
r2 = string object (ptr)
r3 = string object (ptr)
outputs
r1 = 0 if error, else ok
```
### string::init2 -> class/string/init2
```
inputs
r0 = string object (ptr)
r1 = vtable (pptr)
r2 = c string filename (pubyte)
r3 = file length (uint)
outputs
r1 = 0 if error, else ok
```
### string::init3 -> class/string/init3
```
inputs
r0 = string object (ptr)
r1 = vtable (pptr)
r2 = string objects vector object (ptr)
outputs
r1 = 0 if error, else ok
```
### string::split -> class/string/split
```
inputs
r0 = string object (ptr)
r1 = split char (uint)
outputs
r0 = string object (ptr)
r1 = split string objects vector object (ptr)
trashes
all but r0
```
### string::compare -> class/string/compare
```
inputs
r0 = string object (ptr)
r1 = string object (ptr)
outputs
r0 = string object (ptr)
r1 = 0 if same, else -, +
trashes
r2-r7
```
### string::same -> class/string/same
```
inputs
r0 = string object (ptr)
r1 = string object (ptr)
outputs
r0 = string object (ptr)
r1 = 0 if same
trashes
r2-r6
```
### string::find -> class/string/find
```
inputs
r0 = string object (ptr)
r1 = search char (uint)
outputs
r0 = string object (ptr)
r1 = -1, else position (int)
trashes
r2-r4
```
### string::hash -> class/string/hash
```
inputs
r0 = string object (ptr)
outputs
r0 = string object (ptr)
r1 = hash code (ulong)
trashes
all but r0
```
### string::get_length -> class/string/get_length
```
inputs
r0 = string object (ptr)
outputs
r0 = string object (ptr)
r1 = string length (bytes)
```
### string::ref_element -> class/string/ref_element
```
inputs
r0 = string object (ptr)
r1 = char index (uint)
outputs
r0 = string object (ptr)
r1 = char string object (ptr)
```
### string::slice -> class/string/slice
```
inputs
r0 = string object (ptr)
r1 = element start index (uint)
r2 = element end index (uint)
outputs
r0 = string object (ptr)
r1 = string slice object (ptr)
trashes
r2-r6
```
## symbol
Super Class: string
### symbol::intern -> class/symbol/intern
```
inputs
r0 = symbol object (ptr)
outputs
r0 = interned symbol object (ptr)
trashes
all
```
### symbol::intern_cstr -> class/symbol/intern_cstr
```
inputs
r0 = c string (ptr)
outputs
r0 = interned symbol object (ptr)
trashes
all
```
## sys_heap
Super Class: null
### sys_heap::init -> sys/heap/init
```
inputs
r0 = heap (ptr)
r1 = cell size (bytes)
r2 = block size (bytes)
outputs
r0 = heap (ptr)
r1 = cell size (bytes)
trashes
r2-r3
```
### sys_heap::deinit -> sys/heap/deinit
```
inputs
r0 = heap (ptr)
outputs
r0 = heap (ptr)
trashes
r1-r3
```
### sys_heap::alloc -> sys/heap/alloc
```
inputs
r0 = heap (ptr)
outputs
r0 = heap (ptr)
r1 = cell (ptr)
trashes
r2-r3
```
### sys_heap::free -> sys/heap/free
```
inputs
r0 = heap (ptr)
r1 = cell (ptr)
trashes
r2
```
## sys_link
Super Class: null
### sys_link::init -> sys/link/init
### sys_link::link -> sys/link/link
```
started by kernel for each link
```
## sys_list
Super Class: null
### sys_list::init -> sys/list/init
```
inputs
r0 = list header (ptr)
```
## sys_load
Super Class: null
### sys_load::statics -> sys/load/statics
### sys_load::init -> sys/load/init
```
inputs
system argv
SDL function table
info
register inputs are dependant on the platform ABI
they are extracted via (sys-arg 0) and (sys-arg 1)
```
### sys_load::bind -> sys/load/bind
```
input
r0 = c string function path name (pubyte)
output
r0 = 0 else, function entry pointer (ptr)
trashes
r1-r7
```
## sys_mail
Super Class: null
### sys_mail::statics -> sys/mail/statics
```
info
mail static data
```
### sys_mail::init -> sys/mail/init
```
inputs
r1 = kernel mailbox (ptr)
```
### sys_mail::alloc -> sys/mail/alloc
```
inputs
r0 = mail size (bytes)
outputs
r0 = mail message (ptr)
r1 = string data (pubyte)
trashes
all
```
### sys_mail::free -> sys/mail/free
```
inputs
r0 = mail message (ptr)
trashes
all
```
### sys_mail::alloc_obj -> sys/mail/alloc_obj
```
inputs
r0 = object (ptr)
r1 = data (pubyte)
r2 = data length (bytes)
outputs
r0 = mail message (ptr)
trashes
all
```
### sys_mail::free_obj -> sys/mail/free_obj
```
inputs
r0 = mail message (ptr)
outputs
r0 = 0 if msg was 0, else object (ptr)
r1 = data (pubyte)
r2 = data length (bytes)
trashes
all
```
### sys_mail::send -> sys/mail/send
```
inputs
r0 = mail message (ptr)
trashes
r0-r2
```
### sys_mail::read -> sys/mail/read
```
inputs
r0 = mailbox address (ptr)
outputs
r0 = mail address (ptr)
r1 = string data (pubyte)
trashes
r2
```
### sys_mail::try_read -> sys/mail/try_read
```
inputs
r0 = mailbox address (ptr)
outputs
r0 = 0, else mail address (ptr)
r1 = string data (pubyte)
trashes
r2
```
### sys_mail::select -> sys/mail/select
```
inputs
r0 = mailbox address array (pptr)
r1 = mailbox count (unit)
outputs
r0 = mailbox address (ptr)
trashes
r1-r4
```
### sys_mail::mymail -> sys/mail/mymail
```
outputs
r0 = mail address (ptr)
r1 = string data (pubyte)
trashes
r2
```
### sys_mail::init_mailbox -> sys/mail/init_mailbox
```
outputs
r0 = mailbox address (ptr)
trashes
r1-r2
```
### sys_mail::in -> sys/mail/in
```
inputs
r0 = link input msg buffer (ptr)
trashes
all
```
### sys_mail::out -> sys/mail/out
```
info
parcels going off chip task
```
### sys_mail::lisp_mymail -> sys/mail/lisp_mymail
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### sys_mail::lisp_send -> sys/mail/lisp_send
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## sys_mem
Super Class: null
### sys_mem::statics -> sys/mem/statics
```
info
mem statics data
```
### sys_mem::init -> sys/mem/init
```
info
init mem statics
```
### sys_mem::deinit -> sys/mem/deinit
```
info
deinit mem statics
```
### sys_mem::alloc -> sys/mem/alloc
```
inputs
r0 = minimum amount (bytes)
outputs
r0 = 0 if failed, else address (ptr)
r1 = 0 if failed, else size given (bytes)
trashes
r2-r3
```
### sys_mem::calloc -> sys/mem/calloc
```
inputs
r0 = minimum amount (bytes)
outputs
r0 = 0 if failed, else address (ptr)
r1 = 0 if failed, else size given (bytes)
trashes
r2-r3
```
### sys_mem::free -> sys/mem/free
```
inputs
r0 = address (ptr)
trashes
r0-r2
```
### sys_mem::clear -> sys/mem/clear
```
inputs
r0 = address (ptr)
r1 = length (bytes)
outputs
r0 = address end (ptr)
trashes
r1-r2
```
### sys_mem::fill -> sys/mem/fill
```
inputs
r0 = address (ptr)
r1 = length (bytes)
r2 = fill byte (byte)
outputs
r0 = address end (ptr)
trashes
r1-r3
```
### sys_mem::copy -> sys/mem/copy
```
inputs
r0 = source address (ptr)
r1 = destination address (ptr)
r2 = length (bytes)
outputs
r0 = source address end (ptr)
r1 = destination address end (ptr)
trashes
r2-r3
```
### sys_mem::realloc -> sys/mem/realloc
```
inputs
r0 = block address (ptr)
r1 = block size (bytes)
r2 = new block min size (bytes)
outputs
r0 = new block address (ptr)
r1 = new block size (bytes)
trashes
r2-r7
```
### sys_mem::recalloc -> sys/mem/recalloc
```
inputs
r0 = block address (ptr)
r1 = block size (bytes)
r2 = new block min size (bytes)
outputs
r0 = new block address (ptr)
r1 = new block size (bytes)
trashes
r2-r7
```
### sys_mem::used -> sys/mem/used
```
outputs
r0 = amount (bytes)
```
## sys_string
Super Class: null
### sys_string::length -> sys/string/length
```
inputs
r0 = c string (pubyte)
outputs
r0 = c string (pubyte)
r1 = c string len (bytes)
trashes
r2
```
### sys_string::copy -> sys/string/copy
```
inputs
r0 = c string (pubyte)
r1 = c string copy (pubyte)
outputs
r0 = c string end (pubyte)
r1 = c string copy end (pubyte)
trashes
r2
```
### sys_string::compare -> sys/string/compare
```
inputs
r0 = c string1 (pubyte)
r1 = c string2 (pubyte)
outputs
r0 = 0 if same, else -, +
trashes
r0-r3
```
### sys_string::to_long -> sys/string/to_long
```
inputs
r0 = c string (pubyte)
r1 = base (ulong)
outputs
r0 = number (ulong)
trashes
r2-r4
```
### sys_string::from_long -> sys/string/from_long
```
inputs
r0 = number (ulong)
r1 = c string buffer (pubyte)
r2 = base (ulong)
trashes
r0-r4
```
## sys_task
Super Class: null
### sys_task::statics -> sys/task/statics
```
info
task statics data
```
### sys_task::init -> sys/task/init
```
info
init task statics
```
### sys_task::tcb -> sys/task/tcb
```
outputs
r0 = current task tcb (ptr)
```
### sys_task::mailbox -> sys/task/mailbox
```
outputs
r0, r1 = current task mailbox id (id)
```
### sys_task::callback -> sys/task/callback
```
inputs
r0 = user data address (ptr)
r1 = callback address (ptr)
trashes
all
```
### sys_task::start -> sys/task/start
```
inputs
r0 = new task func pointer (ptr)
outputs
r0 = new task control block (ptr)
r1, r2 = new task mailbox id (id)
trashes
r3-r5
```
### sys_task::stop -> sys/task/stop
```
info
stop current task, switch to next task
```
### sys_task::restore -> sys/task/restore
```
inputs
r14 = control block to restore (ptr)
info
restore next task
```
### sys_task::yield -> sys/task/yield
```
info
switch to next task
```
### sys_task::count -> sys/task/count
```
outputs
r0 = task count (uint)
```
### sys_task::sleep -> sys/task/sleep
```
inputs
r0 = time delay in usec (ulong)
```
### sys_task::suspend -> sys/task/suspend
```
info
suspend current task, switch to next task
```
### sys_task::resume -> sys/task/resume
```
inputs
r0 = task control node to resume (ptr)
trashes
r1-r2
```
### sys_task::open_child -> sys/task/open_child
```
inputs
r0 = name string object (ptr)
r1 = spawn type (uint)
outputs
r0, r1 = mailbox ID (id)
trashes
all
```
### sys_task::open_remote -> sys/task/open_remote
```
inputs
r0 = name string object (ptr)
r1 = cpu target (uint)
r2 = spawn type (uint)
outputs
r0, r1 = mailbox id (id)
trashes
all
```
### sys_task::open_farm -> sys/task/open_farm
```
inputs
r0 = name string object (ptr)
r1 = number to spawn (uint)
r2 = spawn type (uint)
outputs
r0 = mailbox id's array object (ptr)
trashes
all
```
### sys_task::open_pipe -> sys/task/open_pipe
```
inputs
r0 = string objects vector object (ptr)
outputs
r0 = mailbox id's array object (ptr)
trashes
all
```
### sys_task::lisp_mailbox -> sys/task/lisp_mailbox
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### sys_task::lisp_open_child -> sys/task/lisp_open_child
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### sys_task::lisp_open_remote -> sys/task/lisp_open_remote
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### sys_task::lisp_open_farm -> sys/task/lisp_open_farm
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### sys_task::lisp_open_pipe -> sys/task/lisp_open_pipe
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## text
Super Class: view
### text::create -> class/text/create
### text::new -> class/text/new
### text::init -> class/text/init
```
inputs
r0 = text object (ptr)
r1 = vtable (pptr)
outputs
r0 = text object (ptr)
r1 = 0 if error, else ok
```
### text::switch_text -> class/text/switch_text
```
inputs
r0 = text object (ptr)
trashes
all but r0
```
### text::deinit -> class/text/deinit
```
inputs
r0 = text object (ptr)
trashes
all but r0
```
### text::pref_size -> class/text/pref_size
```
inputs
r0 = text object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### text::draw -> class/text/draw
```
inputs
r0 = view object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
## texture
Super Class: obj
### texture::create -> class/texture/create
### texture::new -> class/texture/new
### texture::init -> class/texture/init
```
inputs
r0 = texture object (ptr)
r1 = vtable (pptr)
r2 = texture handle (ulong)
r3 = texture width (pixels)
r4 = texture height (pixels)
outputs
r0 = texture object (ptr)
r1 = 0 if error, else ok
```
### texture::get_metrics -> class/texture/get_metrics
```
inputs
r0 = texture object (ptr)
outputs
r0 = texture object (ptr)
r1 = texture handle (ulong)
r2 = width (pixels)
r3 = height (pixels)
```
### texture::deinit -> class/texture/deinit
```
inputs
r0 = texture object (ptr)
trashes
all but r0
```
## title
Super Class: label
### title::create -> class/title/create
### title::new -> class/title/new
### title::init -> class/title/init
```
inputs
r0 = title object (ptr)
r1 = vtable (pptr)
outputs
r0 = title object (ptr)
r1 = 0 if error, else ok
```
### title::mouse_down -> class/title/mouse_down
```
inputs
r0 = title object (ptr)
r1 = mouse event message (ptr)
trashes
all but r0
```
### title::mouse_move -> class/title/mouse_move
```
inputs
r0 = title object (ptr)
r1 = mouse event message (ptr)
trashes
all but r0
```
## vdu
Super Class: view
### vdu::create -> class/vdu/create
### vdu::new -> class/vdu/new
### vdu::init -> class/vdu/init
```
inputs
r0 = vdu object (ptr)
r1 = vtable (pptr)
outputs
r0 = vdu object (ptr)
r1 = 0 if error, else ok
```
### vdu::switch_font -> class/vdu/switch_font
```
inputs
r0 = vdu object (ptr)
trashes
all but r0
```
### vdu::switch_size -> class/vdu/switch_size
```
inputs
r0 = vdu object (ptr)
trashes
all but r0
```
### vdu::print_char -> class/vdu/print_char
```
inputs
r0 = vdu object (ptr)
r1 = char (uint)
outputs
r0 = vdu object (ptr)
```
### vdu::print_cstr -> class/vdu/print_cstr
```
inputs
r0 = vdu object (ptr)
r1 = c string pointer (pubyte)
outputs
r0 = vdu object (ptr)
```
### vdu::deinit -> class/vdu/deinit
```
inputs
r0 = vdu object (ptr)
trashes
all but r0
```
### vdu::pref_size -> class/vdu/pref_size
```
inputs
r0 = vdu object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### vdu::draw -> class/vdu/draw
```
inputs
r0 = view object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
## vector
Super Class: array
### vector::create -> class/vector/create
### vector::deinit -> class/vector/deinit
```
inputs
r0 = vector object (ptr)
trashes
all but r0
```
### vector::ref_element -> class/vector/ref_element
```
inputs
r0 = vector object (ptr)
r1 = element index (uint)
outputs
r0 = vector object (ptr)
r1 = element object (ptr)
trashes
r2
```
### vector::slice -> class/vector/slice
```
inputs
r0 = vector object (ptr)
r1 = element start index (uint)
r2 = element end index (uint)
outputs
r0 = vector object (ptr)
r1 = slice vector object (ptr)
trashes
r1-r10
```
### vector::clear -> class/vector/clear
```
inputs
r0 = vector object (ptr)
outputs
r0 = vector object (ptr)
trashes
all but r0
```
### vector::ref_back -> class/vector/ref_back
```
inputs
r0 = vector object (ptr)
outputs
r0 = vector object (ptr)
r1 = element object (ptr)
trashes
r2
```
### vector::pop_back -> class/vector/pop_back
```
inputs
r0 = vector object (ptr)
outputs
r0 = vector object (ptr)
trashes
all but r0
```
### vector::set_element -> class/vector/set_element
```
inputs
r0 = vector object (ptr)
r1 = element object (ptr)
r2 = element index (uint)
outputs
r0 = vector object (ptr)
trashes
all but r0
```
### vector::append -> class/vector/append
```
inputs
r0 = vector object (ptr)
r1 = source vector object (ptr)
r2 = element start index (uint)
r3 = element end index (uint)
outputs
r0 = vector object (ptr)
trashes
r1-r11
```
## view
Super Class: component
### view::init -> class/view/init
```
inputs
r0 = view object (ptr)
r1 = vtable (pptr)
outputs
r0 = view object (ptr)
r1 = 0 if error, else ok
```
### view::add_front -> class/view/add_front
```
inputs
r0 = view object (ptr)
r1 = parent view object (ptr)
trashes
r1-r3
```
### view::add_back -> class/view/add_back
```
inputs
r0 = view object (ptr)
r1 = child view object (ptr)
trashes
r1-r3
```
### view::sub -> class/view/sub
```
inputs
r0 = view object (ptr)
trashes
r1-r2
```
### view::to_front -> class/view/to_front
```
inputs
r0 = view object (ptr)
outputs
r0 = view object (ptr)
trashes
all but r0
```
### view::add_dirty -> class/view/add_dirty
```
inputs
r0 = view object (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = width (pixels)
r10 = height (pixels)
trashes
all but r0
```
### view::add_opaque -> class/view/add_opaque
```
inputs
r0 = view object (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = width (pixels)
r10 = height (pixels)
trashes
all but r0
```
### view::sub_opaque -> class/view/sub_opaque
```
inputs
r0 = view object (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = width (pixels)
r10 = height (pixels)
trashes
all but r0
```
### view::get_relative -> class/view/get_relative
```
inputs
r0 = view object (ptr)
r1 = ancestor view object (ptr)
r7 = view x (pixels)
r8 = view y (pixels)
outputs
r7 = relative x (pixels)
r8 = relative y (pixels)
trashes
r2, r9-r10
```
### view::dirty -> class/view/dirty
```
inputs
r0 = view object (ptr)
trashes
all but r0
```
### view::dirty_all -> class/view/dirty_all
```
inputs
r0 = view object (ptr)
trashes
all but r0
```
### view::opaque -> class/view/opaque
```
inputs
r0 = view object (ptr)
trashes
all but r0
```
### view::forward -> class/view/forward
```
inputs
r0 = view object (ptr)
r1 = user data pointer (ptr)
r2 = callback (ptr)
outputs
r0 = view object (ptr)
trashes
dependant on callback
callback api
inputs
r0 = child view object (ptr)
r1 = user data pointer (ptr)
outputs
r0 = child view object (ptr)
```
### view::backward -> class/view/backward
```
inputs
r0 = view object (ptr)
r1 = user data (ptr)
r2 = callback (ptr)
outputs
r0 = view object (ptr)
trashes
dependant on callback
callback api
inputs
r0 = child view object (ptr)
r1 = user data pointer (ptr)
outputs
r0 = child view object (ptr)
```
### view::forward_tree -> class/view/forward_tree
```
inputs
r0 = view object (ptr)
r1 = user data pointer
r2 = down callback (ptr)
r3 = up callback (ptr)
outputs
r0 = view object (ptr)
trashes
dependant on callbacks
callback api
inputs
r0 = view object (ptr)
r1 = user data pointer (ptr)
outputs
r0 = view object (ptr)
r1 = 0 if should not descend after down callback
```
### view::backward_tree -> class/view/backward_tree
```
inputs
r0 = view object (ptr)
r1 = user data pointer
r2 = down callback (ptr)
r3 = up callback (ptr)
outputs
r0 = view object (ptr)
trashes
dependant on callbacks
callback api
inputs
r0 = view object (ptr)
r1 = user data pointer (ptr)
outputs
r0 = view object (ptr)
r1 = 0 if should not descend after down callback
```
### view::change -> class/view/change
```
inputs
r0 = view object (ptr)
r7 = new x (pixels)
r8 = new y (pixels)
r9 = new w (pixels)
r10 = new h (pixels)
trashes
all but r0
```
### view::hit_tree -> class/view/hit_tree
```
inputs
r0 = view object (ptr)
r7 = x (pixels)
r8 = y (pixels)
outputs
r0 = view object (ptr)
r1 = 0 if not hit, else hit view object (ptr)
r7 = x relative to hit (pixels)
r8 = y relative to hit (pixels)
trashes
r1-r3
```
### view::find_id -> class/view/find_id
```
inputs
r0 = view object (ptr)
r1 = target id (long)
outputs
r0 = view object (ptr)
r1 = 0 if not found, else view object (ptr)
trashes
r1-r3
```
### view::draw_panel -> class/view/draw_panel
```
inputs
r0 = view object (ptr)
r1 = draw ctx (ptr)
r2 = flags (ulong)
r3 = depth (uint)
trashes
all but r0
```
### view::get_bounds -> class/view/get_bounds
```
inputs
r0 = view object (ptr)
outputs
r0 = view object (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = width (pixels)
r10 = height (pixels)
```
### view::set_bounds -> class/view/set_bounds
```
inputs
r0 = view object (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = width (pixels)
r10 = height (pixels)
```
### view::get_first -> class/view/get_first
```
inputs
r0 = view object (ptr)
outputs
r0 = view object (ptr)
r1 = 0 if empty, else first child view object (ptr)
trashes
r2
```
### view::get_last -> class/view/get_last
```
inputs
r0 = view object (ptr)
outputs
r0 = view object (ptr)
r1 = 0 if empty, else last child view object (ptr)
trashes
r2
```
### view::deinit -> class/view/deinit
```
inputs
r0 = view object (ptr)
trashes
all but r0
```
### view::add_child -> class/view/add_back
```
inputs
r0 = view object (ptr)
r1 = child view object (ptr)
trashes
r1-r3
```
### view::draw -> class/obj/null
### view::hit -> class/view/hit
```
inputs
r0 = view object (ptr)
r7 = x (pixels)
r8 = y (pixels)
outputs
r0 = view object (ptr)
r1 = 0 if not, else hit
```
### view::pref_size -> class/view/pref_size
```
inputs
r0 = view object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### view::layout -> class/obj/null
### view::event -> class/view/event
```
inputs
r0 = view object (ptr)
r1 = event data (ptr)
trashes
all but r0
```
### view::mouse_down -> class/obj/null
### view::mouse_up -> class/obj/null
### view::mouse_move -> class/obj/null
### view::mouse_hover -> class/obj/null
### view::key_down -> class/obj/null
### view::key_up -> class/obj/null
## window
Super Class: view
### window::new -> class/window/new
### window::create -> class/window/create
### window::init -> class/window/init
```
inputs
r0 = window object (ptr)
r1 = vtable (pptr)
r2 = options flags (ulong)
outputs
r0 = window object (ptr)
r1 = 0 if error, else ok
trashes
all but r0-r1
```
### window::set_title -> class/window/set_title
```
inputs
r0 = window object (ptr)
r1 = title c string (pubyte)
```
### window::set_status -> class/window/set_status
```
inputs
r0 = window object (ptr)
r1 = status c string (pubyte)
```
### window::connect_close -> class/window/connect_close
```
inputs
r0 = window object (ptr)
r1 = reciever id (long)
outputs
r0 = window object (ptr)
trashes
all but r0
```
### window::connect_max -> class/window/connect_max
```
inputs
r0 = window object (ptr)
r1 = reciever id (long)
outputs
r0 = window object (ptr)
trashes
all but r0
```
### window::connect_min -> class/window/connect_min
```
inputs
r0 = window object (ptr)
r1 = reciever id (long)
outputs
r0 = window object (ptr)
trashes
all but r0
```
### window::deinit -> class/window/deinit
```
inputs
r0 = window object (ptr)
trashes
all but r0
```
### window::add_child -> class/window/add_child
```
inputs
r0 = window object (ptr)
r1 = child view object (ptr)
trashes
all but r0
```
### window::pref_size -> class/window/pref_size
```
inputs
r0 = window object (ptr)
outputs
r0 = window object (ptr)
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### window::layout -> class/window/layout
```
inputs
r0 = window object (ptr)
trashes
all but r0
```
### window::draw -> class/window/draw
```
inputs
r0 = window object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
### window::mouse_down -> class/window/mouse_down
```
inputs
r0 = window object (ptr)
r1 = mouse event data (ptr)
trashes
all but r0
```
### window::mouse_move -> class/window/mouse_move
```
inputs
r0 = window object (ptr)
r1 = mouse event data (ptr)
trashes
all but r0
```
### window::lisp_create -> class/window/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
