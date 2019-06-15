# Classes
## array
Super Class: seq
### array::vtable -> class/array/vtable
### array::create -> class/array/create
### array::init -> class/array/init
```
inputs
r0 = array object (ptr)
r1 = vtable (pptr)
outputs
r0 = array object (ptr)
r1 = 0 if error, else ok
trashes
r2
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
r1-r5
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
all
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
all
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
all
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
### array::get_forth -> class/array/get_forth
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
r2-r5
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
r1 = integer object (ptr)
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
r1-r9
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
r1 = integer object (ptr)
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
r1-r8
```
### array::cat -> class/array/cat
```
inputs
r0 = array object (ptr)
r1 = vector of array objects (ptr)
outputs
r0 = 0 if error, else new array object (ptr)
trashes
r0-r11
```
### array::lisp_clear -> class/array/lisp_clear
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### array::lisp_push -> class/array/lisp_push
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### array::lisp_pop -> class/array/lisp_pop
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## backdrop
Super Class: view
### backdrop::vtable -> apps/boing/backdrop/vtable
### backdrop::create -> apps/boing/backdrop/create
### backdrop::pref_size -> apps/boing/backdrop/pref_size
```
inputs
r0 = backdrop object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### backdrop::draw -> apps/boing/backdrop/draw
```
inputs
r0 = view object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
### backdrop::layout -> apps/boing/backdrop/layout
```
inputs
r0 = backdrop object (ptr)
trashes
all but r0
```
### backdrop::lisp_create -> apps/boing/backdrop/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## button
Super Class: label
### button::vtable -> gui/button/vtable
### button::create -> gui/button/create
### button::init -> gui/button/init
```
inputs
r0 = button object (ptr)
r1 = vtable (pptr)
outputs
r0 = button object (ptr)
r1 = 0 if error, else ok
```
### button::connect_click -> gui/button/connect_click
```
inputs
r0 = button object (ptr)
r1 = target id (long)
outputs
r0 = button object (ptr)
trashes
r2-r5
```
### button::deinit -> gui/button/deinit
```
inputs
r0 = button object (ptr)
trashes
all but r0
```
### button::draw -> gui/button/draw
```
inputs
r0 = button object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
### button::layout -> gui/button/layout
```
inputs
r0 = button object (ptr)
trashes
all but r0
```
### button::mouse_down -> gui/button/mouse_move
```
inputs
r0 = button object (ptr)
r1 = mouse event data (ptr)
trashes
all but r0
```
### button::mouse_up -> gui/button/mouse_up
```
inputs
r0 = button object (ptr)
r1 = mouse event data (ptr)
trashes
all but r0
```
### button::mouse_move -> gui/button/mouse_move
```
inputs
r0 = button object (ptr)
r1 = mouse event data (ptr)
trashes
all but r0
```
### button::lisp_create -> gui/button/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### button::lisp_connect_click -> gui/button/lisp_connect_click
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
### canvas::vtable -> gui/canvas/vtable
### canvas::create -> gui/canvas/create
### canvas::create_shared -> gui/canvas/create_shared
### canvas::init -> gui/canvas/init
```
inputs
r0 = canvas object (ptr)
r1 = vtable (pptr)
r2 = width (pixels)
r3 = height (pixels)
r4 = aa scale (uint)
outputs
r0 = canvas object (ptr)
r1 = 0 if error, else ok
trashes
all
```
### canvas::init_shared -> gui/canvas/init_shared
```
inputs
r0 = canvas object (ptr)
r1 = vtable (pptr)
r2 = source canvas object (ptr)
outputs
r0 = canvas object (ptr)
r1 = 0 if error, else ok
trashes
all
```
### canvas::swap -> gui/canvas/swap
```
inputs
r0 = canvas object (ptr)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::resize -> gui/canvas/resize
```
inputs
r0 = canvas object (ptr)
r1 = source canvas object (ptr)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::resize_2 -> gui/canvas/resize_2
```
inputs
r0 = canvas object (ptr)
r1 = source canvas object (ptr)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::resize_3 -> gui/canvas/resize_3
```
inputs
r0 = canvas object (ptr)
r1 = source canvas object (ptr)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::fill -> gui/canvas/fill
```
inputs
r0 = canvas object (ptr)
r1 = color (argb)
outputs
r0 = canvas object (ptr)
trashes
r2-r4
```
### canvas::to_premul -> gui/canvas/to_premul
```
inputs
r1 = color (argb)
outputs
r1 = color premul (argb)
trashes
r2-r3
```
### canvas::to_argb -> gui/canvas/to_argb
```
inputs
r1 = color premul (argb)
outputs
r1 = color (argb)
trashes
r2-r4
```
### canvas::as_argb -> gui/canvas/as_argb
```
inputs
r0 = canvas object (ptr)
r1 = source canvas object (ptr)
outputs
r0 = canvas object (ptr)
trashes
r1-r9
```
### canvas::as_premul -> gui/canvas/as_premul
```
inputs
r0 = canvas object (ptr)
r1 = source canvas object (ptr)
outputs
r0 = canvas object (ptr)
trashes
r1-r8
```
### canvas::set_clip -> gui/canvas/set_clip
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
### canvas::set_edges -> gui/canvas/set_edges
```
inputs
r0 = canvas object (ptr)
r1 = vector of points objects (ptr)
r2 = x (16.16)
r3 = y (16.16)
r4 = y scale (int)
outputs
r0 = canvas object (ptr)
r1 = min_x (16.16)
r2 = min_y (16.16)
r3 = max_x (16.16)
r4 = max_y (16.16)
trashes
all but r0
```
### canvas::set_color -> gui/canvas/set_color
```
inputs
r0 = canvas object (ptr)
r1 = color (argb)
outputs
r0 = canvas object (ptr)
trashes
none
```
### canvas::set_flags -> gui/canvas/set_flags
```
inputs
r0 = canvas object (ptr)
r1 = flags (uint)
outputs
r0 = canvas object (ptr)
trashes
none
```
### canvas::span_noclip -> gui/canvas/span_noclip
```
inputs
r0 = canvas object (ptr)
r1 = coverage (ulong)
r7 = x (pixels)
r8 = y (pixels)
r9 = x1 (pixels)
outputs
r0 = canvas object (ptr)
trashes
r1-r3, r7-r9
info
coverage is 0x0 to 0x80
```
### canvas::span -> gui/canvas/span
```
inputs
r0 = canvas object (ptr)
r1 = coverage (ulong)
r7 = x (pixels)
r8 = y (pixels)
r9 = x1 (pixels)
outputs
r0 = canvas object (ptr)
trashes
r1-r3, r7-r9
info
coverage is 0x0 to 0x80
```
### canvas::pick -> gui/canvas/pick
```
inputs
r0 = canvas object (ptr)
r7 = x (pixels)
r8 = y (pixels)
outputs
r0 = canvas object (ptr)
r1 = color (argb)
trashes
all but r0
```
### canvas::plot -> gui/canvas/plot
```
inputs
r0 = canvas object (ptr)
r7 = x (pixels)
r8 = y (pixels)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::fbox -> gui/canvas/fbox
```
inputs
r0 = canvas object (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = w (pixels)
r10 = h (pixels)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::fpoly -> gui/canvas/fpoly
```
inputs
r0 = canvas object (ptr)
r1 = x (16.16)
r2 = y (16.16)
r3 = winding mode (0/1)
r4 = vector of points objects (ptr)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::load -> gui/canvas/load
```
inputs
r0 = c string (pubyte)
r1 = flags (uint)
outputs
r0 = 0 if error, else shared canvas object (ptr)
trashes
all
```
### canvas::load_cpm -> gui/canvas/load_cpm
```
inputs
r0 = stream object (ptr)
outputs
r0 = 0 if error, else canvas object (ptr)
trashes
all
```
### canvas::next_frame -> gui/canvas/next_frame
```
inputs
r0 = canvas object (ptr)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::read_pixel -> gui/canvas/read_pixel
```
inputs
r0 = stream object (ptr)
r1 = pixel type (uint)
r2 = bit pool (ulong)
r3 = bit pool size (uint)
outputs
r0 = -1 if error, else pixel (long)
r1 = bit pool (ulong)
r2 = bit pool size (uint)
trashes
all
```
### canvas::to_argb32 -> gui/canvas/to_argb32
```
inputs
r0 = col (uint)
r1 = pixel type (uint)
outputs
r0 = col (uint)
trashes
r1-r7
```
### canvas::deinit -> gui/canvas/deinit
```
inputs
r0 = canvas object (ptr)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::pref_size -> gui/canvas/pref_size
```
inputs
r0 = canvas object (ptr)
outputs
r0 = canvas object (ptr)
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### canvas::draw -> gui/canvas/draw
```
inputs
r0 = canvas object (ptr)
r1 = draw ctx (ptr)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::lisp_create -> gui/canvas/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### canvas::lisp_load -> gui/canvas/lisp_load
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### canvas::lisp_next_frame -> gui/canvas/lisp_next_frame
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### canvas::lisp_swap -> gui/canvas/lisp_swap
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### canvas::lisp_fill -> gui/canvas/lisp_fill
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### canvas::lisp_set_color -> gui/canvas/lisp_set_color
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### canvas::lisp_set_flags -> gui/canvas/lisp_set_flags
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### canvas::lisp_plot -> gui/canvas/lisp_plot
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### canvas::lisp_fbox -> gui/canvas/lisp_fbox
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### canvas::lisp_fpoly -> gui/canvas/lisp_fpoly
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## component
Super Class: hmap
### component::vtable -> gui/component/vtable
### component::init -> gui/component/init
```
inputs
r0 = component object (ptr)
r1 = vtable (pptr)
outputs
r0 = component object (ptr)
r1 = 0 if error, else ok
```
### component::find_owner -> gui/component/find_owner
```
inputs
r0 = component object (ptr)
outputs
r0 = component object (ptr)
r1 = 0, else tcb of owner (ptr)
trashes
r2
```
### component::emit -> gui/component/emit
```
inputs
r0 = component object (ptr)
r1 = target id array object (ptr)
outputs
r0 = component object (ptr)
trashes
all but r0
```
### component::get_prop_sym -> gui/component/get_prop_sym
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
### component::get_prop -> gui/component/get_prop
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
### component::ref_prop -> gui/component/ref_prop
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
### component::set_long_prop -> gui/component/set_long_prop
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
### component::get_long_prop -> gui/component/get_long_prop
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
### component::set_font_prop -> gui/component/set_font_prop
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
### component::set_str_prop -> gui/component/set_str_prop
```
inputs
r0 = component object (ptr)
r1 = property num (uint)
r2 = c string (pubyte)
outputs
r0 = component object (ptr)
trashes
all but r0
```
## ctx
Super Class: null
### ctx::box -> gui/ctx/box
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
### ctx::filled_box -> gui/ctx/filled_box
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
### ctx::blit -> gui/ctx/blit
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
### ctx::set_color -> gui/ctx/set_color
```
inputs
r0 = draw ctx (ptr)
r1 = color (argb)
trashes
all
```
### ctx::panel -> gui/ctx/panel
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
### ctx::brighter -> gui/ctx/brighter
```
inputs
r1 = color (argb)
outputs
r1 = brighter color (argb)
trashes
r2, r3
```
### ctx::darker -> gui/ctx/darker
```
inputs
r1 = color (argb)
outputs
r1 = darker color (argb)
trashes
r2, r3
```
## error
Super Class: obj
### error::vtable -> class/error/vtable
### error::create -> class/error/create
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
r1 = error c string (pubyte)
trashes
r1-r5
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
outputs
r0 = error object (ptr)
trashes
all but r0
```
### error::lisp_error -> class/error/lisp_error
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## flow
Super Class: view
### flow::vtable -> gui/flow/vtable
### flow::create -> gui/flow/create
### flow::pref_size -> gui/flow/pref_size
```
inputs
r0 = flow object (ptr)
outputs
r0 = flow object (ptr)
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### flow::layout -> gui/flow/layout
```
inputs
r0 = flow object (ptr)
outputs
r0 = flow object (ptr)
trashes
all but r0
```
### flow::lisp_create -> gui/flow/lisp_create
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
### font::vtable -> gui/font/vtable
### font::statics -> gui/font/statics
```
info
font static data
```
### font::open -> gui/font/open
```
r0 = name c string (pubyte)
r1 = font size (points)
outputs
r0 = 0 if error, else font object (ptr)
trashes
all
```
### font::create -> gui/font/create
### font::init -> gui/font/init
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
### font::flush_cache -> gui/font/flush_cache
```
trashes
all
```
### font::ref_word -> gui/font/ref_word
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
### font::ref_chars -> gui/font/ref_chars
```
inputs
r0 = font object (ptr)
outputs
r0 = font object (ptr)
r1 = char texture vector object (ptr)
trashes
all but r0
```
### font::get_metrics -> gui/font/get_metrics
```
inputs
r0 = font object (ptr)
outputs
r0 = font object (ptr)
r1 = ascent (pixels)
r2 = descent (pixels)
r3 = height (pixels)
```
### font::deinit -> gui/font/deinit
```
inputs
r0 = font object (ptr)
outputs
r0 = font object (ptr)
trashes
all but r0
```
### font::lisp_create -> gui/font/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## func
Super Class: num
### func::vtable -> class/func/vtable
### func::create -> class/func/create
## grid
Super Class: view
### grid::vtable -> gui/grid/vtable
### grid::create -> gui/grid/create
### grid::pref_size -> gui/grid/pref_size
```
inputs
r0 = grid object (ptr)
outputs
r0 = grid object (ptr)
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### grid::layout -> gui/grid/layout
```
inputs
r0 = grid object (ptr)
outputs
r0 = grid object (ptr)
trashes
all but r0
```
### grid::lisp_create -> gui/grid/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## gui
Super Class: null
### gui::statics -> gui/gui/statics
### gui::init -> gui/gui/init
### gui::update -> gui/gui/update
```
inputs
r0 = root view object (ptr)
trashes
all
```
### gui::add -> gui/gui/add
```
inputs
r0 = view object (ptr)
outputs
r0 = view object (ptr)
trashes
all but r0
```
### gui::gui -> gui/gui/gui
```
gui process
```
### gui::lisp_add -> gui/gui/lisp_add
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## hmap
Super Class: hset
### hmap::vtable -> class/hmap/vtable
### hmap::create -> class/hmap/create
### hmap::init -> class/hmap/init
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
### hmap::find -> class/hmap/find
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
### hmap::copy -> class/hmap/copy
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
### hmap::insert -> class/hmap/insert
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
### hmap::search -> class/hmap/search
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
### hmap::set -> class/hmap/set
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
### hmap::get -> class/hmap/get
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
### hmap::get_parent -> class/hmap/get_parent
```
inputs
r0 = hash_map object (ptr)
outputs
r0 = hash_map object (ptr)
r1 = 0, else hash_map parent object (ptr)
```
### hmap::set_parent -> class/hmap/set_parent
```
inputs
r0 = hash_map object (ptr)
r1 = 0, else hash_map parent object (ptr)
outputs
r0 = hash_map object (ptr)
trashes
all but r0
```
### hmap::deinit -> class/hmap/deinit
```
inputs
r0 = hash_map object (ptr)
trashes
all but r0
```
### hmap::lisp_env -> class/hmap/lisp_env
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = environment hash_map object (ptr)
```
### hmap::lisp_def -> class/hmap/lisp_def
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### hmap::lisp_defq -> class/hmap/lisp_defq
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### hmap::lisp_set -> class/hmap/lisp_set
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### hmap::lisp_setq -> class/hmap/lisp_setq
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### hmap::lisp_defined -> class/hmap/lisp_defined
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### hmap::lisp_undef -> class/hmap/lisp_undef
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## host
Super Class: null
### host::sdl_set_main_ready -> nil
### host::sdl_init -> nil
### host::sdl_get_error -> nil
### host::sdl_quit -> nil
### host::sdl_create_window -> nil
### host::sdl_create_window_and_renderer -> nil
### host::sdl_destroy_window -> nil
### host::sdl_delay -> nil
### host::sdl_create_renderer -> nil
### host::sdl_set_render_draw_color -> nil
### host::sdl_render_fill_rect -> nil
### host::sdl_render_present -> nil
### host::sdl_render_set_clip_rect -> nil
### host::sdl_set_render_draw_blend_mode -> nil
### host::sdl_poll_event -> nil
### host::sdl_render_draw_rect -> nil
### host::sdl_free_surface -> nil
### host::sdl_create_texture_from_surface -> nil
### host::sdl_destroy_texture -> nil
### host::sdl_render_copy -> nil
### host::sdl_set_texture_blend_mode -> nil
### host::sdl_set_texture_color_mod -> nil
### host::sdl_create_rgb_surface_from -> nil
### host::ttf_init -> nil
### host::ttf_quit -> nil
### host::ttf_open_font -> nil
### host::ttf_close_font -> nil
### host::ttf_size_utf8 -> nil
### host::ttf_font_ascent -> nil
### host::ttf_font_descent -> nil
### host::ttf_font_height -> nil
### host::ttf_render_utf8_blended -> nil
### host::exit -> nil
### host::stat -> nil
### host::open -> nil
### host::close -> nil
### host::unlink -> nil
### host::read -> nil
### host::write -> nil
### host::mmap -> nil
### host::munmap -> nil
### host::mprotect -> nil
### host::gettime -> nil
### host::open_shared -> nil
### host::close_shared -> nil
## hset
Super Class: obj
### hset::vtable -> class/hset/vtable
### hset::create -> class/hset/create
### hset::init -> class/hset/init
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
r2-r5
```
### hset::num_buckets -> class/hset/num_buckets
```
inputs
r0 = hash_set object (ptr)
outputs
r0 = hash_set object (ptr)
r1 = num buckets (uint)
```
### hset::get_bucket -> class/hset/get_bucket
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
### hset::clear -> class/hset/clear
```
inputs
r0 = hash_set object (ptr)
outputs
r0 = hash_set object (ptr)
trashes
all but r0
```
### hset::for_each -> class/hset/for_each
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
### hset::find -> class/hset/find
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
### hset::insert -> class/hset/insert
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
### hset::erase -> class/hset/erase
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
### hset::copy -> class/hset/copy
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
### hset::get_iters -> class/hset/get_iters
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
### hset::key_callback -> class/obj/null
### hset::deinit -> class/hset/deinit
```
inputs
r0 = hash_set object (ptr)
trashes
all but r0
```
## label
Super Class: view
### label::vtable -> gui/label/vtable
### label::create -> gui/label/create
### label::init -> gui/label/init
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
### label::pref_size -> gui/label/pref_size
```
inputs
r0 = label object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### label::draw -> gui/label/draw
```
inputs
r0 = view object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
### label::layout -> gui/label/layout
```
inputs
r0 = label object (ptr)
trashes
all but r0
```
### label::lisp_create -> gui/label/lisp_create
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
### lisp::vtable -> class/lisp/vtable
### lisp::create -> class/lisp/create
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
r0 = args vector object (ptr)
r3 = args dest (ptr)
r1 = args offset (uint)
trashes
r0-r5
```
### lisp::env_args_type -> class/lisp/env_args_type
```
inputs
r1 = args vector object (ptr) vector
r3 = type/sig pointer
r4 = - or 0 all same type check, else + for type signature check
outputs
r1 = 0 if error, else ok
trashes
r0-r5
```
### lisp::read -> class/lisp/read
```
inputs
r0 = lisp object (ptr)
r1 = stream object (ptr)
r2 = next char (uint)
outputs
r0 = lisp object (ptr)
r1 = ast vector (ptr)
r2 = next char (uint)
```
### lisp::read_char -> class/lisp/read_char
```
inputs
r0 = lisp object (ptr)
r1 = stream object (ptr)
r2 = last char (uint)
outputs
r0 = lisp object (ptr)
r1 = next char (uint)
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
r1 = integer object (ptr)
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
r2 = function object (ptr)
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
### lisp::func_eql -> class/lisp/func_eql
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
### lisp::func_copy -> class/lisp/func_copy
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
### lisp::func_inst_of -> class/lisp/func_inst_of
### lisp::func_age -> class/lisp/func_age
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
### lisp::func_list -> class/lisp/func_list
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
### lisp::func_type -> class/lisp/func_type
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## msg_in
Super Class: stream
### msg_in::vtable -> class/msg_in/vtable
### msg_in::create -> class/msg_in/create
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
### msg_in::next_msg -> class/msg_in/next_msg
```
inputs
r0 = msg_in object (ptr)
outputs
r0 = msg_in object (ptr)
trashes
all but r0
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
### msg_out::vtable -> class/msg_out/vtable
### msg_out::create -> class/msg_out/create
### msg_out::init -> class/msg_out/init
```
inputs
r0 = msg_out object (ptr)
r1 = vtable (pptr)
r2 = target mailbox id (id)
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
## num
Super Class: obj
### num::vtable -> class/num/vtable
### num::create -> class/num/create
### num::init -> class/num/init
```
inputs
r0 = integer object (ptr)
r1 = vtable (pptr)
r2 = initial value (long)
outputs
r0 = integer object (ptr)
r1 = 0 if error, else ok
```
### num::get_value -> class/num/get_value
```
inputs
r0 = integer object (ptr)
outputs
r0 = integer object (ptr)
r1 = value (long)
```
### num::lisp_add -> class/num/lisp_add
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_sub -> class/num/lisp_sub
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_mul -> class/num/lisp_mul
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_div -> class/num/lisp_div
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_mod -> class/num/lisp_mod
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_fmul -> class/num/lisp_fmul
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_fdiv -> class/num/lisp_fdiv
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_fmod -> class/num/lisp_fmod
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_band -> class/num/lisp_band
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_bor -> class/num/lisp_bor
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_bxor -> class/num/lisp_bxor
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_bshr -> class/num/lisp_bshr
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_basr -> class/num/lisp_basr
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_bshl -> class/num/lisp_bshl
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_min -> class/num/lisp_min
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_max -> class/num/lisp_max
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_eq -> class/num/lisp_eq
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_ne -> class/num/lisp_ne
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_lt -> class/num/lisp_lt
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_gt -> class/num/lisp_gt
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_le -> class/num/lisp_le
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### num::lisp_ge -> class/num/lisp_ge
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## obj
Super Class: null
### obj::vtable -> class/obj/vtable
### obj::null -> class/obj/null
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
trashes
none
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
### pair::vtable -> class/pair/vtable
### pair::create -> class/pair/create
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
## pipe
Super Class: obj
### pipe::vtable -> class/pipe/vtable
### pipe::create -> class/pipe/create
### pipe::init -> class/pipe/init
```
inputs
r0 = pipe object (ptr)
r1 = vtable (pptr)
r2 = command buffer (pubyte)
r3 = command buffer length (bytes)
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
r1 = mailbox index (uint)
trashes
r0-r5
```
### pipe::get_state -> class/pipe/get_state
```
inputs
r0 = pipe object (ptr)
outputs
r0 = pipe object (ptr)
r1 = current state (ulong)
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
### pipe::lisp_create -> class/pipe/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### pipe::lisp_read -> class/pipe/lisp_read
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### pipe::lisp_write -> class/pipe/lisp_write
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## points
Super Class: array
### points::vtable -> gui/points/vtable
### points::create -> gui/points/create
### points::get_first2 -> gui/points/get_first2
```
inputs
r0 = points object (ptr)
outputs
r0 = points object (ptr)
r1 = element1 (long)
r2 = element2 (long)
```
### points::get_element2 -> gui/points/get_element2
```
inputs
r0 = points object (ptr)
r1 = element index (uint)
outputs
r0 = points object (ptr)
r1 = element1 (long)
r2 = element2 (long)
```
### points::push_back2 -> gui/points/push_back2
```
inputs
r0 = points object (ptr)
r1 = element1 (long)
r2 = element2 (long)
outputs
r0 = points object (ptr)
r1 = element1 (long)
r2 = element2 (long)
trashes
r3-r5
```
### points::pop_back2 -> gui/points/pop_back2
```
inputs
r0 = points object (ptr)
outputs
r0 = points object (ptr)
trashes
r1
```
### points::add -> gui/points/add
```
inputs
r0 = points object (ptr)
r1 = source1 points object, can be same (ptr)
r2 = source2 points object, can be same (ptr)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::sub -> gui/points/sub
```
inputs
r0 = points object (ptr)
r1 = source1 points object, can be same (ptr)
r2 = source2 points object, can be same (ptr)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::mul -> gui/points/mul
```
inputs
r0 = points object (ptr)
r1 = source1 points object, can be same (ptr)
r2 = source2 points object, can be same (ptr)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::div -> gui/points/div
```
inputs
r0 = points object (ptr)
r1 = source1 points object, can be same (ptr)
r2 = source2 points object, can be same (ptr)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::mod -> gui/points/mod
```
inputs
r0 = points object (ptr)
r1 = source1 points object, can be same (ptr)
r2 = source2 points object, can be same (ptr)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::sum -> gui/points/sum
```
inputs
r0 = points object (ptr)
outputs
r0 = points object (ptr)
r1 = sum (long)
trashes
all but r0
```
### points::fmul -> gui/points/fmul
```
inputs
r0 = points object (ptr)
r1 = source1 points object, can be same (ptr)
r2 = source2 points object, can be same (ptr)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::fdiv -> gui/points/fdiv
```
inputs
r0 = points object (ptr)
r1 = source1 points object, can be same (ptr)
r2 = source2 points object, can be same (ptr)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::fmod -> gui/points/fmod
```
inputs
r0 = points object (ptr)
r1 = source1 points object, can be same (ptr)
r2 = source2 points object, can be same (ptr)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::fscale -> gui/points/fscale
```
inputs
r0 = points object (ptr)
r1 = source points object, can be same (ptr)
r2 = scale (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::abs -> gui/points/abs
```
inputs
r0 = points object (ptr)
r1 = source points object, can be same (ptr)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::frac -> gui/points/frac
```
inputs
r0 = points object (ptr)
r1 = source points object, can be same (ptr)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::filter_polyline -> gui/points/filter_polyline
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
### points::filter_polygon -> gui/points/filter_polygon
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
### points::transform -> gui/points/transform
```
inputs
r0 = points object (ptr)
r1 = source points object, can be same (ptr)
r2 = m1x (16.16)
r3 = m1y (16.16)
r4 = m2x (16.16)
r5 = m2y (16.16)
r6 = trx (16.16)
r7 = try (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::simplify -> gui/points/simplify
```
inputs
r0 = points object (ptr)
r1 = source points object (ptr)
r2 = stack array object (ptr)
r3 = tolerance (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::gen_clerp -> gui/points/gen_clerp
```
inputs
r0 = points object (ptr)
r1 = stack array object (ptr)
r2 = cx (16.16)
r3 = cy (16.16)
r4 = v1x (16.16)
r5 = v1y (16.16)
r6 = v2x (16.16)
r7 = v2y (16.16)
r8 = radius (16.16)
r9 = tolerance (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::gen_arc -> gui/points/gen_arc
```
inputs
r0 = points object (ptr)
r1 = stack array object (ptr)
r2 = cx (16.16)
r3 = cy (16.16)
r4 = start angle (16.16)
r5 = end angle (16.16)
r6 = radius (16.16)
r7 = tolerance (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::gen_quadratic -> gui/points/gen_quadratic
```
inputs
r0 = points object (ptr)
r1 = stack array object (ptr)
r2 = p1x (16.16)
r3 = p1y (16.16)
r4 = p2x (16.16)
r5 = p2y (16.16)
r6 = p3x (16.16)
r7 = p3y (16.16)
r8 = tolerance (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::gen_cubic -> gui/points/gen_cubic
```
inputs
r0 = points object (ptr)
r1 = stack array object (ptr)
r2 = p1x (16.16)
r3 = p1y (16.16)
r4 = p2x (16.16)
r5 = p2y (16.16)
r6 = p3x (16.16)
r7 = p3y (16.16)
r8 = p4x (16.16)
r9 = p4y (16.16)
r10 = tolerance (16.16)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::stroke_joints -> gui/points/stroke_joints
```
inputs
r0 = points object (ptr)
r1 = stack array object (ptr)
r2 = in points start iter (plong)
r3 = in points end iter (plong)
r4 = p1x (16.16)
r5 = p1y (16.16)
r6 = p2x (16.16)
r7 = p2y (16.16)
r8 = radius (16.16)
r9 = tolerance (16.16)
r10 = join style (byte)
outputs
r0 = points object (ptr)
trashes
all but r0
```
### points::stroke_polylines -> gui/points/stroke_polylines
```
inputs
r0 = output vector of points objects (ptr)
r1 = stack array object (ptr)
r2 = input vector of points objects (ptr)
r3 = radius (16.16)
r4 = tolerance (16.16)
r5 = join style (byte)
r6 = cap style1 (byte)
r7 = cap style2 (byte)
outputs
r0 = output vector of points objects (ptr)
trashes
all but r0
```
### points::stroke_polygons -> gui/points/stroke_polygons
```
inputs
r0 = output vector of points objects (ptr)
r1 = stack array object (ptr)
r2 = input vector of points objects (ptr)
r3 = radius (16.16)
r4 = tolerance (16.16)
r5 = join style (byte)
outputs
r0 = output vector of points objects (ptr)
trashes
all but r0
```
### points::cat -> gui/points/cat
```
inputs
r0 = points object (ptr)
r1 = vector of points objects (ptr)
outputs
r0 = 0 if error, else new points object (ptr)
trashes
r0-r11
```
### points::lisp_vecop1 -> gui/points/lisp_vecop1
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
r2 = points method (ptr)
outputs
r0 = lisp object (ptr)
r1 = 0 if error, else value object (ptr)
```
### points::lisp_vecop2 -> gui/points/lisp_vecop2
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
r2 = points method (ptr)
outputs
r0 = lisp object (ptr)
r1 = 0 if error, else value object (ptr)
```
### points::lisp_add -> gui/points/lisp_add
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_sub -> gui/points/lisp_sub
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_mul -> gui/points/lisp_mul
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_div -> gui/points/lisp_div
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_mod -> gui/points/lisp_mod
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_sum -> gui/points/lisp_sum
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_fmul -> gui/points/lisp_fmul
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_fdiv -> gui/points/lisp_fdiv
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_fmod -> gui/points/lisp_fmod
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_fscale -> gui/points/lisp_fscale
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_abs -> gui/points/lisp_abs
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_frac -> gui/points/lisp_frac
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_transform -> gui/points/lisp_transform
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_simplify -> gui/points/lisp_simplify
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_gen_quadratic -> gui/points/lisp_gen_quadratic
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_gen_cubic -> gui/points/lisp_gen_cubic
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_gen_arc -> gui/points/lisp_gen_arc
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_stroke_polylines -> gui/points/lisp_stroke_polylines
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### points::lisp_stroke_polygons -> gui/points/lisp_stroke_polygons
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## progress
Super Class: view
### progress::vtable -> gui/progress/vtable
### progress::create -> gui/progress/create
### progress::pref_size -> gui/progress/pref_size
```
inputs
r0 = progress object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### progress::draw -> gui/progress/draw
```
inputs
r0 = window object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
### progress::layout -> gui/view/opaque
```
inputs
r0 = view object (ptr)
trashes
all but r0
```
### progress::lisp_create -> gui/progress/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## region
Super Class: null
### region::translate -> gui/region/translate
```
inputs
r1 = region listhead (ptr)
r7 = x translation (pixels)
r8 = y translation (pixels)
trashes
r1, r11-r14
```
### region::bounds -> gui/region/bounds
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
### region::clip_rect -> gui/region/clip_rect
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
### region::remove_rect -> gui/region/remove_rect
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
### region::cut_rect -> gui/region/cut_rect
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
### region::copy_rect -> gui/region/copy_rect
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
### region::paste_rect -> gui/region/paste_rect
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
### region::free -> gui/region/free
```
inputs
r0 = region heap (ptr)
r1 = source region listhead (ptr)
trashes
r1-r3
```
### region::copy_region -> gui/region/copy_region
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
### region::paste_region -> gui/region/paste_region
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
### region::remove_region -> gui/region/remove_region
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
## scroll
Super Class: view
### scroll::vtable -> gui/scroll/vtable
### scroll::create -> gui/scroll/create
### scroll::init -> gui/scroll/init
```
inputs
r0 = scroll object (ptr)
r1 = vtable (pptr)
r2 = options flags (ulong)
outputs
r0 = scroll object (ptr)
r1 = 0 if error, else ok
trashes
all but r0-r1
```
### scroll::add_child -> gui/scroll/add_child
```
inputs
r0 = scroll object (ptr)
r1 = child view object (ptr)
trashes
all but r0
```
### scroll::layout -> gui/scroll/layout
```
inputs
r0 = scroll object (ptr)
trashes
all but r0
```
### scroll::pref_size -> gui/scroll/pref_size
```
inputs
r0 = scroll object (ptr)
outputs
r0 = scroll object (ptr)
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### scroll::action -> gui/scroll/action
```
inputs
r0 = scroll object (ptr)
r1 = event data (ptr)
trashes
all but r0
```
### scroll::lisp_create -> gui/scroll/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## seq
Super Class: obj
### seq::vtable -> class/seq/vtable
### seq::get_length -> class/obj/null
### seq::ref_element -> class/obj/null
### seq::slice -> class/obj/null
### seq::cat -> class/obj/null
### seq::lisp_length -> class/seq/lisp_length
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### seq::lisp_elem -> class/seq/lisp_elem
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### seq::lisp_find -> class/seq/lisp_find
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### seq::lisp_slice -> class/seq/lisp_slice
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### seq::lisp_cat -> class/seq/lisp_cat
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### seq::lisp_each -> class/seq/lisp_each
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### seq::lisp_some -> class/seq/lisp_some
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## slave
Super Class: obj
### slave::vtable -> class/slave/vtable
### slave::create -> class/slave/create
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
### slave::lisp_get_args -> class/slave/lisp_get_args
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## slider
Super Class: view
### slider::vtable -> gui/slider/vtable
### slider::create -> gui/slider/create
### slider::init -> gui/slider/init
```
inputs
r0 = slider object (ptr)
r1 = vtable (pptr)
outputs
r0 = slider object (ptr)
r1 = 0 if error, else ok
```
### slider::connect_value -> gui/slider/connect_value
```
inputs
r0 = slider object (ptr)
r1 = target id (long)
outputs
r0 = slider object (ptr)
trashes
r2-r5
```
### slider::deinit -> gui/slider/deinit
```
inputs
r0 = slider object (ptr)
trashes
all but r0
```
### slider::pref_size -> gui/slider/pref_size
```
inputs
r0 = slider object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### slider::draw -> gui/slider/draw
```
inputs
r0 = window object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
### slider::layout -> gui/view/opaque
```
inputs
r0 = view object (ptr)
trashes
all but r0
```
### slider::mouse_down -> gui/slider/mouse_down
```
inputs
r0 = slider object (ptr)
r1 = mouse event data (ptr)
trashes
all but r0
```
### slider::mouse_up -> gui/slider/mouse_up
```
inputs
r0 = slider object (ptr)
r1 = mouse event data (ptr)
trashes
all but r0
```
### slider::mouse_move -> gui/slider/mouse_move
```
inputs
r0 = slider object (ptr)
r1 = mouse event data (ptr)
trashes
all but r0
```
### slider::lisp_create -> gui/slider/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### slider::lisp_connect_value -> gui/slider/lisp_connect_value
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## str
Super Class: seq
### str::vtable -> class/str/vtable
### str::create_from_buffer -> class/str/create_from_buffer
```
inputs
r0 = buffer (pubyte)
r1 = buffer length (uint)
outputs
r0 = 0 if error, else string object (ptr)
trashes
r1-r6
```
### str::create_from_cstr -> class/str/create_from_cstr
```
inputs
r0 = c string (pubyte)
outputs
r0 = 0 if error, else string object (ptr)
trashes
r1-r6
```
### str::create_from_file -> class/str/create_from_file
```
inputs
r0 = file name c string (pubyte)
outputs
r0 = 0 if error, else string object (ptr)
trashes
r1-r6
```
### str::create_from_long -> class/str/create_from_long
```
inputs
r0 = number (long)
r1 = base, - for unsigned, (long)
outputs
r0 = 0 if error, else string object (ptr)
trashes
r1-r6
```
### str::append -> class/str/append
```
inputs
r0 = string object (ptr)
r1 = string object (ptr)
outputs
r0 = 0 if error, else new string object (ptr)
trashes
r1-r6
```
### str::init -> class/str/init
```
inputs
r0 = string object (ptr)
r1 = vtable (pptr)
r2 = 0 else, buffer (pubyte)
r3 = buffer length (uint)
outputs
r0 = string object (ptr)
r1 = 0 if error, else ok
trashes
r1-r6
```
### str::init1 -> class/str/init1
```
inputs
r0 = string object (ptr)
r1 = vtable (pptr)
r2 = string object (ptr)
r3 = string object (ptr)
outputs
r0 = string object (ptr)
r1 = 0 if error, else ok
trashes
r1-r6
```
### str::init2 -> class/str/init2
```
inputs
r0 = string object (ptr)
r1 = vtable (pptr)
r2 = file name c string (pubyte)
r3 = file length (uint)
outputs
r0 = string object (ptr)
r1 = 0 if error, else ok
trashes
r1-r6
```
### str::init3 -> class/str/init3
```
inputs
r0 = string object (ptr)
r1 = vtable (pptr)
r2 = vector of string objects (ptr)
outputs
r0 = string object (ptr)
r1 = 0 if error, else ok
trashes
r1-r6
```
### str::split -> class/str/split
```
inputs
r0 = string object (ptr)
r1 = split char (uint)
outputs
r0 = string object (ptr)
r1 = vector of string objects (ptr)
trashes
all but r0
```
### str::compare -> class/str/compare
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
### str::same -> class/str/same
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
### str::find -> class/str/find
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
### str::hash -> class/str/hash
```
inputs
r0 = string object (ptr)
outputs
r0 = string object (ptr)
r1 = hash code (ulong)
trashes
r2-r4
```
### str::get_length -> class/str/get_length
```
inputs
r0 = string object (ptr)
outputs
r0 = string object (ptr)
r1 = string length (bytes)
```
### str::ref_element -> class/str/ref_element
```
inputs
r0 = string object (ptr)
r1 = char index (uint)
outputs
r0 = string object (ptr)
r1 = char string object (ptr)
trashes
r2-r7
```
### str::slice -> class/str/slice
```
inputs
r0 = string object (ptr)
r1 = element start index (uint)
r2 = element end index (uint)
outputs
r0 = string object (ptr)
r1 = string slice object (ptr)
trashes
r2-r7
```
### str::cat -> class/str/cat
```
inputs
r0 = string object (ptr)
r1 = vector of string objects (ptr)
outputs
r0 = 0 if error, else new string object (ptr)
trashes
r1-r6
```
### str::lisp_str -> class/str/lisp_str
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### str::lisp_split -> class/str/lisp_split
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### str::lisp_code -> class/str/lisp_code
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### str::lisp_char -> class/str/lisp_char
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### str::lisp_cmp -> class/str/lisp_cmp
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### str::lisp_save -> class/str/lisp_save
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### str::lisp_load -> class/str/lisp_load
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
### stream::vtable -> class/stream/vtable
### stream::create -> class/stream/create
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
r1-r5
```
### stream::available -> class/stream/available
```
inputs
r0 = stream object (ptr)
outputs
r0 = stream object (ptr)
r1 = available space (bytes)
trashes
r2
```
### stream::read_bits -> class/stream/read_bits
```
inputs
r0 = stream object (ptr)
r1 = num bits (uint)
r2 = bit pool (ulong)
r3 = bit pool size (uint)
outputs
r0 = stream object (ptr)
r1 = -1 if eof, else data (long)
r2 = bit pool (ulong)
r3 = bit pool size (uint)
trashes
all but r0
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
outputs
r0 = stream object (ptr)
trashes
all but r0
```
### stream::skip_not -> class/stream/skip_not
```
inputs
r0 = stream object (ptr)
r1 = char to not skip (uint)
outputs
r0 = stream object (ptr)
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
### stream::write_next -> class/stream/write_flush
```
inputs
r0 = stream object (ptr)
outputs
r0 = stream object (ptr)
trashes
r1
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
### stream::lisp_filestream -> class/stream/lisp_filestream
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### stream::lisp_strstream -> class/stream/lisp_strstream
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### stream::lisp_available -> class/stream/lisp_available
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### stream::lisp_readchar -> class/stream/lisp_readchar
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### stream::lisp_readline -> class/stream/lisp_readline
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### stream::lisp_writechar -> class/stream/lisp_writechar
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### stream::lisp_write -> class/stream/lisp_write
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### stream::lisp_write_flush -> class/stream/lisp_write_flush
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## stream_str
Super Class: stream
### stream_str::vtable -> class/stream_str/vtable
### stream_str::create -> class/stream_str/create
### stream_str::init -> class/stream_str/init
```
inputs
r0 = stream_str object (ptr)
r1 = vtable (pptr)
r2 = string object (ptr)
outputs
r0 = stream_str object (ptr)
r1 = 0 if error, else ok
trashes
r1-r5
```
### stream_str::ref_string -> class/stream_str/ref_string
```
inputs
r0 = stream_str object (ptr)
outputs
r0 = stream_str object (ptr)
r1 = string object (ptr)
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
## sym
Super Class: str
### sym::vtable -> class/sym/vtable
### sym::statics -> class/sym/statics
```
info
symbol static data
```
### sym::flush -> class/sym/flush
```
trashes
all
```
### sym::intern -> class/sym/intern
```
inputs
r0 = symbol object (ptr)
outputs
r0 = interned symbol object (ptr)
trashes
all
```
### sym::intern_str -> class/sym/intern_str
```
inputs
r0 = string object (ptr)
outputs
r0 = interned symbol object (ptr)
trashes
all
```
### sym::intern_cstr -> class/sym/intern_cstr
```
inputs
r0 = c string pointer (pubyte)
outputs
r0 = interned symbol object (ptr)
trashes
all
```
### sym::lisp_sym -> class/sym/lisp_sym
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### sym::lisp_gensym -> class/sym/lisp_gensym
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
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
r2
```
### sys_heap::free -> sys/heap/free
```
inputs
r0 = heap (ptr)
r1 = cell (ptr)
trashes
r2
```
## sys_kernel
Super Class: null
### sys_kernel::id -> sys/kernel/id
```
outputs
r0 = cpu ID (uint)
```
### sys_kernel::total -> sys/kernel/total
```
outputs
r0 = cpu total (uint)
```
### sys_kernel::opts -> sys/kernel/opts
```
inputs
r0 = argv array (pptr)
trashes
all
info
process command options
```
### sys_kernel::declare -> sys/kernel/declare
```
inputs
r0 = mailbox name c string (pubyte)
r1 = mailbox id (ulong)
trashes
all
```
### sys_kernel::kernel -> sys/kernel/kernel
```
inputs
r0 = argv pointer (pptr)
info
loader is already initialized when we get here !
```
### sys_kernel::debug -> sys/kernel/debug
```
inputs
r0 = debug c string (pubyte)
trashes
all
```
### sys_kernel::debug_reg -> sys/kernel/debug_reg
```
inputs
r14 = debug c string (pubyte)
trashes
none
```
### sys_kernel::lisp_total -> sys/kernel/lisp_total
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### sys_kernel::lisp_declare -> sys/kernel/lisp_declare
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### sys_kernel::lisp_debug -> sys/kernel/lisp_debug
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
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
host function table
info
register inputs are dependant on the platform ABI
they are extracted via (abi-arg 0) and (abi-arg 1)
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
info
init the mbox array and mbox free array
```
### sys_mail::init1 -> sys/mail/init1
```
info
init the mail system
```
### sys_mail::alloc_mbox -> sys/mail/alloc_mbox
```
outputs
r0 = mailbox id (uint)
r1 = mailbox address (ptr)
trashes
r2-r5
```
### sys_mail::free_mbox -> sys/mail/free_mbox
```
inputs
r0 = mailbox id (uint)
trashes
r0-r5
```
### sys_mail::mbox_addr -> sys/mail/mbox_addr
```
inputs
r0 = mailbox id (uint)
outputs
r0 = mailbox address (ptr)
trashes
r0-r2
```
### sys_mail::alloc -> sys/mail/alloc
```
inputs
r0 = mail size (bytes)
outputs
r0 = mail message (ptr)
r1 = string data (pubyte)
trashes
r2-r7
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
r1-r5
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
r3-r5
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
r1 = mailbox count (uint)
outputs
r0 = mailbox index (uint)
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
### sys_mail::trymail -> sys/mail/trymail
```
outputs
r0 = 0, else mail address (ptr)
r1 = string data (pubyte)
trashes
r2
```
### sys_mail::declare -> sys/mail/declare
```
inputs
r0 = mailbox name c string (pubyte)
r1 = mailbox id (ulong)
trashes
all
```
### sys_mail::enquire -> sys/mail/enquire
```
inputs
r0 = mailbox name c string (pubyte)
outputs
r0 = 0 if error, else mailbox id (ulong)
trashes
all
```
### sys_mail::forget -> sys/mail/forget
```
inputs
r0 = mailbox name c string (pubyte)
trashes
all
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
### sys_mail::lisp_trymail -> sys/mail/lisp_trymail
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
### sys_mail::lisp_declare -> sys/mail/lisp_declare
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### sys_mail::lisp_enquire -> sys/mail/lisp_enquire
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## sys_math
Super Class: null
### sys_math::random -> sys/math/random
```
inputs
r0 = random range (ulong)
outputs
r0 = random number in range (ulong)
trashes
r1-r2
```
### sys_math::isqrt -> sys/math/isqrt
```
inputs
r0 = number (ulong)
outputs
r0 = sqrt (ulong)
trashes
r1-r3
```
### sys_math::fsqrt -> sys/math/fsqrt
```
inputs
r0 = number (16.16)
outputs
r0 = sqrt (16.16)
trashes
r1-r3
```
### sys_math::fsin -> sys/math/fsin
```
inputs
r0 = angle in radians (16.16)
outputs
r0 = sine (16.16)
trashes
r1-r4
```
### sys_math::fcos -> sys/math/fcos
```
inputs
r0 = angle in radians (16.16)
outputs
r0 = cosine (16.16)
trashes
r1-r4
```
### sys_math::intersect -> sys/math/intersect
```
inputs
r0 = p1x (16.16)
r1 = p1y (16.16)
r2 = p2x (16.16)
r3 = p2y (16.16)
r4 = v1x (16.16)
r5 = v1y (16.16)
r6 = v2x (16.16)
r7 = v2y (16.16)
outputs
r0 = ix (16.16)
r1 = iy (16.16)
trashes
all
```
### sys_math::distance_sqd -> sys/math/distance_sqd
```
inputs
r0 = px (16.16)
r1 = py (16.16)
r2 = p1x (16.16)
r3 = p1y (16.16)
r4 = p2x (16.16)
r5 = p2y (16.16)
outputs
r0 = distance squared (16.16)
trashes
all
```
### sys_math::lisp_fsin -> sys/math/lisp_fsin
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### sys_math::lisp_fcos -> sys/math/lisp_fcos
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### sys_math::lisp_fsqrt -> sys/math/lisp_fsqrt
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### sys_math::lisp_frac -> sys/math/lisp_frac
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### sys_math::lisp_floor -> sys/math/lisp_floor
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
r2
```
### sys_mem::calloc -> sys/mem/calloc
```
inputs
r0 = minimum amount (bytes)
outputs
r0 = 0 if failed, else address (ptr)
r1 = 0 if failed, else size given (bytes)
trashes
r2
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
r2-r5
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
## sys_pii
Super Class: null
### sys_pii::exit -> sys/pii/exit
```
inputs
r0 = code (long)
```
### sys_pii::mmap -> sys/pii/mmap
```
inputs
r0 = len (ulong)
r1 = fd (ulong)
r2 = mode (ulong)
outputs
r0 = buffer (ptr)
trashes
none
```
### sys_pii::munmap -> sys/pii/munmap
```
inputs
r0 = buffer (ptr)
r1 = len (ulong)
r2 = mode (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### sys_pii::mprotect -> sys/pii/mprotect
```
inputs
r0 = buffer (ptr)
r1 = len (ulong)
r2 = prot (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### sys_pii::open -> sys/pii/open
```
inputs
r0 = c string filename (pubyte)
r1 = mode (ulong)
outputs
r0 = fd (ulong)
trashes
none
```
### sys_pii::close -> sys/pii/close
```
inputs
r0 = fd (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### sys_pii::open_shared -> sys/pii/open_shared
```
inputs
r0 = c string filename (pubyte)
r1 = length (ulong)
outputs
r0 = handle (long)
trashes
none
```
### sys_pii::close_shared -> sys/pii/close_shared
```
inputs
r0 = c string filename (pubyte)
r1 = handle (long)
outputs
r0 = error code (ulong)
trashes
none
```
### sys_pii::unlink -> sys/pii/unlink
```
inputs
r0 = c string filename (pubyte)
outputs
r0 = error code (ulong)
trashes
none
```
### sys_pii::stat -> sys/pii/stat
```
inputs
r0 = c string filename (pubyte)
r1 = stat buf (ptr)
outputs
r0 = error code (ulong)
trashes
none
```
### sys_pii::write -> sys/pii/write
```
inputs
r0 = fd (ulong)
r1 = buffer (pubyte)
r2 = len (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### sys_pii::write_char -> sys/pii/write_char
```
inputs
r0 = fd (ulong)
r1 = char (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### sys_pii::write_str -> sys/pii/write_str
```
inputs
r0 = fd (ulong)
r1 = c string (pubyte)
outputs
r0 = error code (ulong)
trashes
none
```
### sys_pii::write_num -> sys/pii/write_num
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
### sys_pii::read -> sys/pii/read
```
inputs
r0 = fd (ulong)
r1 = buffer (ptr)
r2 = len (ulong)
outputs
r0 = error code (ulong)
trashes
none
```
### sys_pii::read_char -> sys/pii/read_char
```
inputs
r0 = fd (ulong)
outputs
r0 = char (ulong)
trashes
none
```
### sys_pii::time -> sys/pii/time
```
outputs
r0 = time in usec (ulong)
trashes
none
```
## sys_str
Super Class: null
### sys_str::length -> sys/str/length
```
inputs
r0 = c string (pubyte)
outputs
r0 = c string (pubyte)
r1 = c string len (bytes)
trashes
r2
```
### sys_str::copy -> sys/str/copy
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
### sys_str::compare -> sys/str/compare
```
inputs
r0 = c string1 (pubyte)
r1 = c string2 (pubyte)
outputs
r0 = 0 if same, else -, +
trashes
r0-r3
```
### sys_str::to_long -> sys/str/to_long
```
inputs
r0 = c string (pubyte)
r1 = base (ulong)
outputs
r0 = number (ulong)
trashes
r2-r4
```
### sys_str::from_long -> sys/str/from_long
```
inputs
r0 = number (ulong)
r1 = c string buffer (pubyte)
r2 = base (ulong)
outputs
r0 = c string buffer end (pubyte)
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
r0 = current task mailbox id (id)
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
r1 = new task mailbox id (id)
r2 = new task mailbox address (ptr)
trashes
all
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
r0 = name c string (pubyte)
r1 = spawn type (uint)
outputs
r0 = mailbox ID (id)
trashes
all
```
### sys_task::open_remote -> sys/task/open_remote
```
inputs
r0 = name c string (pubyte)
r1 = cpu target (uint)
r2 = spawn type (uint)
outputs
r0 = mailbox id (id)
trashes
all
```
### sys_task::open_farm -> sys/task/open_farm
```
inputs
r0 = name c string (pubyte)
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
r0 = vector of string objects (ptr)
outputs
r0 = mailbox id's array object (ptr)
trashes
all
```
### sys_task::lisp_sleep -> sys/task/lisp_sleep
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
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
### text::vtable -> gui/text/vtable
### text::create -> gui/text/create
### text::init -> gui/text/init
```
inputs
r0 = text object (ptr)
r1 = vtable (pptr)
outputs
r0 = text object (ptr)
r1 = 0 if error, else ok
```
### text::switch_text -> gui/text/switch_text
```
inputs
r0 = text object (ptr)
trashes
all but r0
```
### text::deinit -> gui/text/deinit
```
inputs
r0 = text object (ptr)
trashes
all but r0
```
### text::pref_size -> gui/text/pref_size
```
inputs
r0 = text object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### text::draw -> gui/text/draw
```
inputs
r0 = view object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
## texture
Super Class: obj
### texture::vtable -> gui/texture/vtable
### texture::create -> gui/texture/create
### texture::init -> gui/texture/init
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
### texture::get_metrics -> gui/texture/get_metrics
```
inputs
r0 = texture object (ptr)
outputs
r0 = texture object (ptr)
r1 = texture handle (ulong)
r2 = width (pixels)
r3 = height (pixels)
```
### texture::deinit -> gui/texture/deinit
```
inputs
r0 = texture object (ptr)
trashes
all but r0
```
## title
Super Class: label
### title::vtable -> gui/title/vtable
### title::create -> gui/title/create
### title::init -> gui/title/init
```
inputs
r0 = title object (ptr)
r1 = vtable (pptr)
outputs
r0 = title object (ptr)
r1 = 0 if error, else ok
```
### title::mouse_down -> gui/title/mouse_down
```
inputs
r0 = title object (ptr)
r1 = mouse event data (ptr)
trashes
all but r0
```
### title::mouse_move -> gui/title/mouse_move
```
inputs
r0 = title object (ptr)
r1 = mouse event data (ptr)
trashes
all but r0
```
## vdu
Super Class: view
### vdu::vtable -> gui/vdu/vtable
### vdu::create -> gui/vdu/create
### vdu::init -> gui/vdu/init
```
inputs
r0 = vdu object (ptr)
r1 = vtable (pptr)
outputs
r0 = vdu object (ptr)
r1 = 0 if error, else ok
```
### vdu::switch_font -> gui/vdu/switch_font
```
inputs
r0 = vdu object (ptr)
trashes
all but r0
```
### vdu::switch_size -> gui/vdu/switch_size
```
inputs
r0 = vdu object (ptr)
trashes
all but r0
```
### vdu::print_char -> gui/vdu/print_char
```
inputs
r0 = vdu object (ptr)
r1 = char (uint)
outputs
r0 = vdu object (ptr)
```
### vdu::print_cstr -> gui/vdu/print_cstr
```
inputs
r0 = vdu object (ptr)
r1 = c string (pubyte)
outputs
r0 = vdu object (ptr)
```
### vdu::lisp_create -> gui/vdu/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### vdu::lisp_print -> gui/vdu/lisp_print
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### vdu::deinit -> gui/vdu/deinit
```
inputs
r0 = vdu object (ptr)
trashes
all but r0
```
### vdu::pref_size -> gui/vdu/pref_size
```
inputs
r0 = vdu object (ptr)
outputs
r9 = preferred width (pixels)
r10 = preferred height (pixels)
trashes
all but r0
```
### vdu::draw -> gui/vdu/draw
```
inputs
r0 = view object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
## vector
Super Class: array
### vector::vtable -> class/vector/vtable
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
r1-r8
```
### vector::cat -> class/vector/cat
```
inputs
r0 = vector object (ptr)
r1 = vector of vector objects (ptr)
outputs
r0 = 0 if error, else new vector object (ptr)
trashes
r0-r11
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
r1-r9
```
### vector::lisp_elemset -> class/vector/lisp_elemset
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### vector::lisp_merge -> class/vector/lisp_merge
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### vector::lisp_part -> class/vector/lisp_part
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### vector::lisp_match -> class/vector/lisp_match
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
## view
Super Class: component
### view::vtable -> gui/view/vtable
### view::create -> gui/view/create
### view::init -> gui/view/init
```
inputs
r0 = view object (ptr)
r1 = vtable (pptr)
outputs
r0 = view object (ptr)
r1 = 0 if error, else ok
```
### view::add_front -> gui/view/add_front
```
inputs
r0 = view object (ptr)
r1 = parent view object (ptr)
trashes
r1-r3
```
### view::add_back -> gui/view/add_back
```
inputs
r0 = view object (ptr)
r1 = child view object (ptr)
trashes
r1-r3
```
### view::sub -> gui/view/sub
```
inputs
r0 = view object (ptr)
trashes
r1-r2
```
### view::to_front -> gui/view/to_front
```
inputs
r0 = view object (ptr)
outputs
r0 = view object (ptr)
trashes
all but r0
```
### view::add_dirty -> gui/view/add_dirty
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
### view::add_opaque -> gui/view/add_opaque
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
### view::sub_opaque -> gui/view/sub_opaque
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
### view::get_relative -> gui/view/get_relative
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
### view::dirty -> gui/view/dirty
```
inputs
r0 = view object (ptr)
trashes
all but r0
```
### view::dirty_all -> gui/view/dirty_all
```
inputs
r0 = view object (ptr)
trashes
all but r0
```
### view::opaque -> gui/view/opaque
```
inputs
r0 = view object (ptr)
trashes
all but r0
```
### view::forward -> gui/view/forward
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
### view::backward -> gui/view/backward
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
### view::forward_tree -> gui/view/forward_tree
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
### view::backward_tree -> gui/view/backward_tree
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
### view::change -> gui/view/change
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
### view::hit_tree -> gui/view/hit_tree
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
### view::find_id -> gui/view/find_id
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
### view::draw_panel -> gui/view/draw_panel
```
inputs
r0 = view object (ptr)
r1 = draw ctx (ptr)
r2 = flags (ulong)
r3 = depth (int)
trashes
all but r0
```
### view::get_bounds -> gui/view/get_bounds
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
### view::set_bounds -> gui/view/set_bounds
```
inputs
r0 = view object (ptr)
r7 = x (pixels)
r8 = y (pixels)
r9 = width (pixels)
r10 = height (pixels)
```
### view::get_first -> gui/view/get_first
```
inputs
r0 = view object (ptr)
outputs
r0 = view object (ptr)
r1 = 0 if empty, else first child view object (ptr)
trashes
r2
```
### view::get_last -> gui/view/get_last
```
inputs
r0 = view object (ptr)
outputs
r0 = view object (ptr)
r1 = 0 if empty, else last child view object (ptr)
trashes
r2
```
### view::lisp_create -> gui/view/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_sub -> gui/view/lisp_sub
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_add_child -> gui/view/lisp_add_child
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_add_front -> gui/view/lisp_add_front
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_add_back -> gui/view/lisp_add_back
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_pref_size -> gui/view/lisp_pref_size
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_change -> gui/view/lisp_change
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_set_bounds -> gui/view/lisp_set_bounds
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_get_bounds -> gui/view/lisp_get_bounds
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_add_opaque -> gui/view/lisp_add_opaque
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_sub_opaque -> gui/view/lisp_sub_opaque
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_opaque -> gui/view/lisp_opaque
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_add_dirty -> gui/view/lisp_add_dirty
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_dirty -> gui/view/lisp_dirty
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_dirty_all -> gui/view/lisp_dirty_all
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_layout -> gui/view/lisp_layout
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_event -> gui/view/lisp_event
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::lisp_find_id -> gui/view/lisp_find_id
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### view::deinit -> gui/view/deinit
```
inputs
r0 = view object (ptr)
trashes
all but r0
```
### view::add_child -> gui/view/add_back
```
inputs
r0 = view object (ptr)
r1 = child view object (ptr)
trashes
r1-r3
```
### view::draw -> class/obj/null
### view::hit -> gui/view/hit
```
inputs
r0 = view object (ptr)
r7 = x (pixels)
r8 = y (pixels)
outputs
r0 = view object (ptr)
r1 = 0 if not, else hit
```
### view::pref_size -> gui/view/pref_size
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
### view::event -> gui/view/event
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
### view::action -> class/obj/null
## window
Super Class: view
### window::vtable -> gui/window/vtable
### window::create -> gui/window/create
### window::init -> gui/window/init
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
### window::set_title -> gui/window/set_title
```
inputs
r0 = window object (ptr)
r1 = title c string (pubyte)
```
### window::set_status -> gui/window/set_status
```
inputs
r0 = window object (ptr)
r1 = status c string (pubyte)
```
### window::connect_layout -> gui/window/connect_layout
```
inputs
r0 = window object (ptr)
r1 = reciever id (long)
outputs
r0 = window object (ptr)
trashes
r2-r5
```
### window::connect_close -> gui/window/connect_close
```
inputs
r0 = window object (ptr)
r1 = reciever id (long)
outputs
r0 = window object (ptr)
trashes
all but r0
```
### window::connect_max -> gui/window/connect_max
```
inputs
r0 = window object (ptr)
r1 = reciever id (long)
outputs
r0 = window object (ptr)
trashes
all but r0
```
### window::connect_min -> gui/window/connect_min
```
inputs
r0 = window object (ptr)
r1 = reciever id (long)
outputs
r0 = window object (ptr)
trashes
all but r0
```
### window::deinit -> gui/window/deinit
```
inputs
r0 = window object (ptr)
trashes
all but r0
```
### window::add_child -> gui/window/add_child
```
inputs
r0 = window object (ptr)
r1 = child view object (ptr)
trashes
all but r0
```
### window::pref_size -> gui/window/pref_size
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
### window::layout -> gui/window/layout
```
inputs
r0 = window object (ptr)
trashes
all but r0
```
### window::draw -> gui/window/draw
```
inputs
r0 = window object (ptr)
r1 = draw ctx (ptr)
trashes
all but r0
```
### window::mouse_down -> gui/window/mouse_down
```
inputs
r0 = window object (ptr)
r1 = mouse event data (ptr)
trashes
all but r0
```
### window::mouse_move -> gui/window/mouse_move
```
inputs
r0 = window object (ptr)
r1 = mouse event data (ptr)
trashes
all but r0
```
### window::lisp_create -> gui/window/lisp_create
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### window::lisp_connect_layout -> gui/window/lisp_connect_layout
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### window::lisp_connect_close -> gui/window/lisp_connect_close
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### window::lisp_connect_min -> gui/window/lisp_connect_min
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### window::lisp_connect_max -> gui/window/lisp_connect_max
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### window::lisp_set_status -> gui/window/lisp_set_status
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
### window::lisp_set_title -> gui/window/lisp_set_title
```
inputs
r0 = lisp object (ptr)
r1 = args vector object (ptr)
outputs
r0 = lisp object (ptr)
r1 = return value object (ptr)
```
