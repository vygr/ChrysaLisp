# Classes
## array
Super Class: sequence
### array::vtable -> class/class_array
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
### array::set_length -> class/array/set_length
```
inputs
r0 = array object (ptr)
r1 = length (uint)
outputs
r0 = array object (ptr)
```
### array::find -> class/array/find
### array::for_each -> class/array/for_each
### array::sort -> class/array/sort
### array::partition -> class/array/partition
### array::get_back -> class/array/get_back
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
### array::deinit -> class/array/deinit
### array::get_length -> class/array/get_length
```
inputs
r0 = array object (ptr)
outputs
r0 = array object (ptr)
r1 = length (uint)
```
### array::ref_element -> class/array/ref_element
### array::slice -> class/array/slice
## backdrop
Super Class: view
### backdrop::vtable -> apps/boing/class_backdrop
### backdrop::create -> apps/boing/backdrop/create
### backdrop::new -> apps/boing/backdrop/new
### backdrop::pref_size -> apps/boing/backdrop/pref_size
### backdrop::draw -> apps/boing/backdrop/draw
### backdrop::layout -> apps/boing/backdrop/layout
### backdrop::lisp_create -> apps/boing/backdrop/lisp_create
## button
Super Class: label
### button::vtable -> class/class_button
### button::create -> class/button/create
### button::new -> class/button/new
### button::init -> class/button/init
### button::connect_click -> class/button/connect_click
### button::deinit -> class/button/deinit
### button::draw -> class/button/draw
### button::layout -> class/button/layout
### button::mouse_down -> class/button/mouse_down
### button::mouse_up -> class/button/mouse_up
### button::mouse_move -> class/button/mouse_move
### button::lisp_create -> class/button/lisp_create
### button::lisp_connect_click -> class/button/lisp_connect_click
## canvas
Super Class: view
### canvas::vtable -> class/class_canvas
### canvas::create -> class/canvas/create
### canvas::create_shared -> class/canvas/create_shared
### canvas::new -> class/canvas/new
### canvas::init -> class/canvas/init
### canvas::init1 -> class/canvas/init1
### canvas::swap -> class/canvas/swap
### canvas::resize -> class/canvas/resize
### canvas::fill -> class/canvas/fill
### canvas::to_premul -> class/canvas/to_premul
### canvas::to_argb -> class/canvas/to_argb
### canvas::as_argb -> class/canvas/as_argb
### canvas::as_premul -> class/canvas/as_premul
### canvas::plot -> class/canvas/plot
### canvas::fbox -> class/canvas/fbox
### canvas::fpoly -> class/canvas/fpoly
### canvas::set_clip -> class/canvas/set_clip
### canvas::set_pix_noclip -> class/canvas/set_pix_noclip
### canvas::set_span_noclip -> class/canvas/set_span_noclip
### canvas::set_span -> class/canvas/set_span
### canvas::set_plot -> class/canvas/set_plot
```
inputs
r0 = canvas object (ptr)
r1 = color (argb)
r7 = x (pixels)
r8 = y (pixels)
outputs
r0 = canvas object (ptr)
trashes
all but r0
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
r1 = vector of points objects (ptr)
r2 = color (argb)
r3 = winding mode (0/1)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::blend_pix_noclip -> class/canvas/blend_pix_noclip
### canvas::blend_span_noclip -> class/canvas/blend_span_noclip
### canvas::blend_span -> class/canvas/blend_span
### canvas::blend_plot -> class/canvas/blend_plot
```
inputs
r0 = canvas object (ptr)
r1 = color (argb)
r7 = x (pixels)
r8 = y (pixels)
outputs
r0 = canvas object (ptr)
trashes
all but r0
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
r1 = vector of points objects (ptr)
r2 = color (argb)
r3 = winding mode (0/1)
outputs
r0 = canvas object (ptr)
trashes
all but r0
```
### canvas::load -> class/canvas/load
### canvas::load_cpm -> class/canvas/load_cpm
### canvas::deinit -> class/canvas/deinit
### canvas::pref_size -> class/canvas/pref_size
### canvas::draw -> class/canvas/draw
### canvas::lisp_create -> class/canvas/lisp_create
### canvas::lisp_load -> class/canvas/lisp_load
### canvas::lisp_swap -> class/canvas/lisp_swap
### canvas::lisp_fill -> class/canvas/lisp_fill
### canvas::lisp_set_plot -> class/canvas/lisp_set_plot
### canvas::lisp_set_fbox -> class/canvas/lisp_set_fbox
### canvas::lisp_set_fpoly -> class/canvas/lisp_set_fpoly
### canvas::lisp_blend_plot -> class/canvas/lisp_blend_plot
### canvas::lisp_blend_fbox -> class/canvas/lisp_blend_fbox
### canvas::lisp_blend_fpoly -> class/canvas/lisp_blend_fpoly
## component
Super Class: hash_map
### component::vtable -> class/class_component
### component::init -> class/component/init
### component::find_owner -> class/component/find_owner
### component::emit -> class/component/emit
### component::get_prop_sym -> class/component/get_prop_sym
### component::get_prop -> class/component/get_prop
### component::ref_prop -> class/component/ref_prop
### component::set_long_prop -> class/component/set_long_prop
### component::get_long_prop -> class/component/get_long_prop
### component::set_font_prop -> class/component/set_font_prop
### component::set_string_prop -> class/component/set_string_prop
## error
Super Class: obj
### error::vtable -> class/class_error
### error::create -> class/error/create
### error::new -> class/error/new
### error::init -> class/error/init
### error::get_description -> class/error/get_description
```
inputs
r0 = error object (ptr)
outputs
r0 = error object (ptr)
r1 = string object (ptr)
```
### error::get_msg -> class/error/get_msg
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
## flow
Super Class: view
### flow::vtable -> class/class_flow
### flow::create -> class/flow/create
### flow::new -> class/flow/new
### flow::pref_size -> class/flow/pref_size
### flow::layout -> class/flow/layout
### flow::lisp_create -> class/flow/lisp_create
## font
Super Class: obj
### font::vtable -> class/class_font
### font::open -> class/font/open
### font::create -> class/font/create
### font::new -> class/font/new
### font::init -> class/font/init
### font::ref_word -> class/font/ref_word
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
### font::lisp_create -> class/font/lisp_create
## function
Super Class: obj
### function::vtable -> class/class_function
### function::create -> class/function/create
### function::new -> class/function/new
### function::init -> class/function/init
```
inputs
r0 = function object (ptr)
r1 = vtable (pptr)
r2 = initial value (ptr)
outputs
r0 = function object (ptr)
r1 = 0 if error, else ok
```
### function::get_value -> class/function/get_value
```
inputs
r0 = function object (ptr)
outputs
r0 = function object (ptr)
r1 = value (ptr)
```
## grid
Super Class: view
### grid::vtable -> class/class_grid
### grid::create -> class/grid/create
### grid::new -> class/grid/new
### grid::pref_size -> class/grid/pref_size
### grid::layout -> class/grid/layout
### grid::lisp_create -> class/grid/lisp_create
## gui_ctx
Super Class: null
### gui_ctx::box -> gui/ctx/box
### gui_ctx::filled_box -> gui/ctx/filled_box
### gui_ctx::blit -> gui/ctx/blit
### gui_ctx::set_color -> gui/ctx/set_color
### gui_ctx::panel -> gui/ctx/panel
### gui_ctx::brighter -> gui/ctx/brighter
### gui_ctx::darker -> gui/ctx/darker
## gui_gui
Super Class: null
### gui_gui::statics -> gui/gui_statics
### gui_gui::init -> gui/gui_init
### gui_gui::update -> gui/gui_update
### gui_gui::add -> gui/gui_add
### gui_gui::gui -> gui/gui
```
gui process
```
### gui_gui::lisp_add -> gui/lisp_add
## gui_region
Super Class: null
### gui_region::translate -> gui/region/translate
### gui_region::bounds -> gui/region/bounds
### gui_region::clip_rect -> gui/region/clip_rect
### gui_region::remove_rect -> gui/region/remove_rect
### gui_region::cut_rect -> gui/region/cut_rect
### gui_region::copy_rect -> gui/region/copy_rect
### gui_region::paste_rect -> gui/region/paste_rect
### gui_region::free -> gui/region/free
### gui_region::copy_region -> gui/region/copy_region
### gui_region::paste_region -> gui/region/paste_region
### gui_region::remove_region -> gui/region/remove_region
## hash_map
Super Class: hash_set
### hash_map::vtable -> class/class_hash_map
### hash_map::create -> class/hash_map/create
### hash_map::new -> class/hash_map/new
### hash_map::init -> class/hash_map/init
### hash_map::find -> class/hash_map/find
### hash_map::copy -> class/hash_map/copy
### hash_map::insert -> class/hash_map/insert
### hash_map::search -> class/hash_map/search
### hash_map::set -> class/hash_map/set
### hash_map::get -> class/hash_map/get
### hash_map::get_parent -> class/hash_map/get_parent
```
inputs
r0 = hash_map object (ptr)
outputs
r0 = hash_map object (ptr)
r1 = 0, else hash_map parent object (ptr)
```
### hash_map::set_parent -> class/hash_map/set_parent
### hash_map::deinit -> class/hash_map/deinit
## hash_set
Super Class: obj
### hash_set::vtable -> class/class_hash_set
### hash_set::create -> class/hash_set/create
### hash_set::new -> class/hash_set/new
### hash_set::init -> class/hash_set/init
### hash_set::num_buckets -> class/hash_set/num_buckets
```
inputs
r0 = hash_set object (ptr)
outputs
r0 = hash_set object (ptr)
r1 = num buckets (uint)
```
### hash_set::get_bucket -> class/hash_set/get_bucket
### hash_set::clear -> class/hash_set/clear
### hash_set::for_each -> class/hash_set/for_each
### hash_set::find -> class/hash_set/find
### hash_set::insert -> class/hash_set/insert
### hash_set::erase -> class/hash_set/erase
### hash_set::copy -> class/hash_set/copy
### hash_set::get_iters -> class/hash_set/get_iters
### hash_set::key_callback -> class/obj/null
### hash_set::deinit -> class/hash_set/deinit
## integer
Super Class: function
### integer::vtable -> class/class_integer
### integer::create -> class/integer/create
## label
Super Class: view
### label::vtable -> class/class_label
### label::create -> class/label/create
### label::new -> class/label/new
### label::init -> class/label/init
### label::pref_size -> class/label/pref_size
### label::draw -> class/label/draw
### label::layout -> class/label/layout
### label::lisp_create -> class/label/lisp_create
## lisp
Super Class: obj
### lisp::vtable -> class/class_lisp
### lisp::create -> class/lisp/create
### lisp::new -> class/lisp/new
### lisp::init -> class/lisp/init
### lisp::deinit -> class/lisp/deinit
### lisp::env_push -> class/lisp/env_push
### lisp::env_pop -> class/lisp/env_pop
### lisp::env_bind -> class/lisp/env_bind
### lisp::env_args_set -> class/lisp/env_args_set
### lisp::env_args_type -> class/lisp/env_args_type
### lisp::read -> class/lisp/read
### lisp::read_char -> class/lisp/read_char
### lisp::read_rmacro -> class/lisp/read_rmacro
### lisp::read_list -> class/lisp/read_list
### lisp::read_sym -> class/lisp/read_sym
### lisp::read_str -> class/lisp/read_str
### lisp::read_num -> class/lisp/read_num
### lisp::repl_eval -> class/lisp/repl_eval
### lisp::repl_eval_list -> class/lisp/repl_eval_list
### lisp::repl_apply -> class/lisp/repl_apply
### lisp::repl_print -> class/lisp/repl_print
### lisp::repl_expand -> class/lisp/repl_expand
### lisp::repl_error -> class/lisp/repl_error
### lisp::func_ffi -> class/lisp/func_ffi
### lisp::func_macroexpand_1 -> class/lisp/func_macroexpand_1
### lisp::func_macroexpand -> class/lisp/func_macroexpand
### lisp::func_defmacro -> class/lisp/func_defmacro
### lisp::func_def -> class/lisp/func_def
### lisp::func_defq -> class/lisp/func_defq
### lisp::func_set -> class/lisp/func_set
### lisp::func_setq -> class/lisp/func_setq
### lisp::func_lambda -> class/lisp/func_lambda
### lisp::func_quote -> class/lisp/func_quote
### lisp::func_qquote -> class/lisp/func_qquote
### lisp::func_add -> class/lisp/func_add
### lisp::func_sub -> class/lisp/func_sub
### lisp::func_mul -> class/lisp/func_mul
### lisp::func_fmul -> class/lisp/func_fmul
### lisp::func_div -> class/lisp/func_div
### lisp::func_fdiv -> class/lisp/func_fdiv
### lisp::func_mod -> class/lisp/func_mod
### lisp::func_min -> class/lisp/func_min
### lisp::func_max -> class/lisp/func_max
### lisp::func_eql -> class/lisp/func_eql
### lisp::func_eq -> class/lisp/func_eq
### lisp::func_ne -> class/lisp/func_ne
### lisp::func_lt -> class/lisp/func_lt
### lisp::func_gt -> class/lisp/func_gt
### lisp::func_le -> class/lisp/func_le
### lisp::func_ge -> class/lisp/func_ge
### lisp::func_cond -> class/lisp/func_cond
### lisp::func_progn -> class/lisp/func_progn
### lisp::func_length -> class/lisp/func_length
### lisp::func_while -> class/lisp/func_while
### lisp::func_print -> class/lisp/func_print
### lisp::func_prin -> class/lisp/func_prin
### lisp::func_env -> class/lisp/func_env
### lisp::func_sym -> class/lisp/func_sym
### lisp::func_str -> class/lisp/func_str
### lisp::func_cat -> class/lisp/func_cat
### lisp::func_elem -> class/lisp/func_elem
### lisp::func_elemset -> class/lisp/func_elemset
### lisp::func_copy -> class/lisp/func_copy
### lisp::func_gensym -> class/lisp/func_gensym
### lisp::func_filestream -> class/lisp/func_filestream
### lisp::func_strstream -> class/lisp/func_strstream
### lisp::func_readchar -> class/lisp/func_readchar
### lisp::func_readline -> class/lisp/func_readline
### lisp::func_writechar -> class/lisp/func_writechar
### lisp::func_writeline -> class/lisp/func_writeline
### lisp::func_write -> class/lisp/func_write
### lisp::func_char -> class/lisp/func_char
### lisp::func_code -> class/lisp/func_code
### lisp::func_slice -> class/lisp/func_slice
### lisp::func_bshr -> class/lisp/func_bshr
### lisp::func_basr -> class/lisp/func_basr
### lisp::func_bshl -> class/lisp/func_bshl
### lisp::func_band -> class/lisp/func_band
### lisp::func_bor -> class/lisp/func_bor
### lisp::func_bxor -> class/lisp/func_bxor
### lisp::func_push -> class/lisp/func_push
### lisp::func_pop -> class/lisp/func_pop
### lisp::func_apply -> class/lisp/func_apply
### lisp::func_repl -> class/lisp/func_repl
### lisp::func_eval -> class/lisp/func_eval
### lisp::func_catch -> class/lisp/func_catch
### lisp::func_save -> class/lisp/func_save
### lisp::func_load -> class/lisp/func_load
### lisp::func_inst_of -> class/lisp/func_inst_of
### lisp::func_split -> class/lisp/func_split
### lisp::func_defined -> class/lisp/func_defined
### lisp::func_find -> class/lisp/func_find
### lisp::func_age -> class/lisp/func_age
### lisp::func_error -> class/lisp/func_error
### lisp::func_pipe -> class/lisp/func_pipe
### lisp::func_rpipe -> class/lisp/func_rpipe
### lisp::func_wpipe -> class/lisp/func_wpipe
### lisp::func_time -> class/lisp/func_time
### lisp::func_clear -> class/lisp/func_clear
### lisp::func_match -> class/lisp/func_match
### lisp::func_each -> class/lisp/func_each
### lisp::func_some -> class/lisp/func_some
### lisp::func_list -> class/lisp/func_list
### lisp::func_cmp -> class/lisp/func_cmp
### lisp::func_array -> class/lisp/func_array
### lisp::func_points -> class/lisp/func_points
### lisp::func_fsqrt -> class/lisp/func_fsqrt
### lisp::func_fmod -> class/lisp/func_fmod
### lisp::func_frac -> class/lisp/func_frac
### lisp::func_floor -> class/lisp/func_floor
### lisp::func_fsin -> class/lisp/func_fsin
### lisp::func_fcos -> class/lisp/func_fcos
### lisp::func_bind -> class/lisp/func_bind
### lisp::func_read -> class/lisp/func_read
### lisp::func_merge -> class/lisp/func_merge
### lisp::func_part -> class/lisp/func_part
### lisp::func_type -> class/lisp/func_type
## msg_in
Super Class: stream
### msg_in::vtable -> class/class_msg_in
### msg_in::create -> class/msg_in/create
### msg_in::new -> class/msg_in/new
### msg_in::init -> class/msg_in/init
### msg_in::next_seq -> class/msg_in/next_seq
### msg_in::next_msg -> class/msg_in/next_msg
### msg_in::deinit -> class/msg_in/deinit
### msg_in::read_ready -> class/msg_in/read_ready
### msg_in::read_next -> class/msg_in/read_next
## msg_out
Super Class: stream
### msg_out::vtable -> class/class_msg_out
### msg_out::create -> class/msg_out/create
### msg_out::new -> class/msg_out/new
### msg_out::init -> class/msg_out/init
### msg_out::set_state -> class/msg_out/set_state
```
inputs
r0 = msg_out object (ptr)
r1 = stream state (ulong)
```
### msg_out::deinit -> class/msg_out/deinit
### msg_out::write_flush -> class/msg_out/write_flush
### msg_out::write_next -> class/msg_out/write_next
## obj
Super Class: null
### obj::vtable -> class/class_obj
### obj::null -> class/obj/null
### obj::destroy -> class/obj/destroy
### obj::init -> class/obj/init
```
inputs
r0 = object (ptr)
r1 = vtable (pptr)
outputs
r1 = 0 if error, else ok
```
### obj::inst_of -> class/obj/inst_of
### obj::ref -> class/obj/ref
```
inputs
r0 = object (ptr)
trashes
r1
```
### obj::deref -> class/obj/deref
### obj::ref_if -> class/obj/ref_if
### obj::deref_if -> class/obj/deref_if
### obj::delete -> sys/mem/free
### obj::deinit -> class/obj/null
### obj::hash -> class/obj/hash
## pair
Super Class: obj
### pair::vtable -> class/class_pair
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
### pair::ref_second -> class/pair/ref_second
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
### pair::set_second -> class/pair/set_second
### pair::deinit -> class/pair/deinit
## pipe
Super Class: obj
### pipe::vtable -> class/class_pipe
### pipe::create -> class/pipe/create
### pipe::new -> class/pipe/new
### pipe::init -> class/pipe/init
### pipe::select -> class/pipe/select
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
## points
Super Class: array
### points::vtable -> class/class_points
### points::create -> class/points/create
### points::filter_polyline -> class/points/filter_polyline
### points::filter_polygon -> class/points/filter_polygon
### points::transform -> class/points/transform
### points::simplify -> class/points/simplify
### points::gen_clerp -> class/points/gen_clerp
### points::gen_arc -> class/points/gen_arc
### points::gen_quadratic -> class/points/gen_quadratic
### points::gen_cubic -> class/points/gen_cubic
### points::stroke_joints -> class/points/stroke_joints
### points::stroke_polylines -> class/points/stroke_polylines
### points::stroke_polygons -> class/points/stroke_polygons
### points::lisp_transform -> class/points/lisp_transform
### points::lisp_simplify -> class/points/lisp_simplify
### points::lisp_gen_quadratic -> class/points/lisp_gen_quadratic
### points::lisp_gen_cubic -> class/points/lisp_gen_cubic
### points::lisp_gen_arc -> class/points/lisp_gen_arc
### points::lisp_stroke_polylines -> class/points/lisp_stroke_polylines
### points::lisp_stroke_polygons -> class/points/lisp_stroke_polygons
## progress
Super Class: view
### progress::vtable -> class/class_progress
### progress::create -> class/progress/create
### progress::new -> class/progress/new
### progress::pref_size -> class/progress/pref_size
### progress::draw -> class/progress/draw
### progress::layout -> class/progress/layout
### progress::lisp_create -> class/progress/lisp_create
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
### sequence::vtable -> class/class_sequence
### sequence::get_length -> class/obj/null
### sequence::ref_element -> class/obj/null
### sequence::slice -> class/obj/null
## slave
Super Class: obj
### slave::vtable -> class/class_slave
### slave::create -> class/slave/create
### slave::new -> class/slave/new
### slave::init -> class/slave/init
### slave::get_args -> class/slave/get_args
```
inputs
r0 = slave object (ptr)
outputs
r0 = slave object (ptr)
r1 = command args
```
### slave::deinit -> class/slave/deinit
### slave::lisp_create -> class/slave/lisp_create
### slave::lisp_get_args -> class/slave/lisp_get_args
## stream
Super Class: obj
### stream::vtable -> class/class_stream
### stream::create -> class/stream/create
### stream::new -> class/stream/new
### stream::init -> class/stream/init
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
### stream::read_char -> class/stream/read_char
### stream::read_line -> class/stream/read_line
### stream::read -> class/stream/read
### stream::write_char -> class/stream/write_char
### stream::write -> class/stream/write
### stream::write_cstr -> class/stream/write_cstr
### stream::skip -> class/stream/skip
### stream::skip_not -> class/stream/skip_not
### stream::split -> class/stream/split
### stream::lisp_available -> class/stream/lisp_available
### stream::lisp_write_flush -> class/stream/lisp_write_flush
### stream::deinit -> class/stream/deinit
### stream::read_ready -> class/stream/read_ready
### stream::read_next -> class/stream/read_next
### stream::write_next -> class/stream/write_next
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
### stream_str::vtable -> class/class_stream_str
### stream_str::create -> class/stream_str/create
### stream_str::new -> class/stream_str/new
### stream_str::init -> class/stream_str/init
### stream_str::ref_string -> class/stream_str/ref_string
### stream_str::write_next -> class/stream_str/write_next
### stream_str::write_flush -> class/stream_str/write_flush
## string
Super Class: sequence
### string::vtable -> class/class_string
### string::create_from_buffer -> class/string/create_from_buffer
### string::create_from_cstr -> class/string/create_from_cstr
### string::create_from_file -> class/string/create_from_file
### string::create_from_long -> class/string/create_from_long
### string::append -> class/string/append
### string::cat -> class/string/cat
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
### string::init1 -> class/string/init1
### string::init2 -> class/string/init2
### string::init3 -> class/string/init3
### string::split -> class/string/split
### string::compare -> class/string/compare
### string::same -> class/string/same
### string::find -> class/string/find
### string::hash -> class/string/hash
### string::get_length -> class/string/get_length
```
inputs
r0 = string object (ptr)
outputs
r0 = string object (ptr)
r1 = string length (bytes)
```
### string::ref_element -> class/string/ref_element
### string::slice -> class/string/slice
## symbol
Super Class: string
### symbol::vtable -> class/class_symbol
### symbol::intern -> class/symbol/intern
### symbol::intern_cstr -> class/symbol/intern_cstr
## sys_heap
Super Class: null
### sys_heap::init -> sys/heap/init
### sys_heap::deinit -> sys/heap/deinit
### sys_heap::alloc -> sys/heap/alloc
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
### sys_kernel::total -> sys/kernel/total
### sys_kernel::opts -> sys/kernel/opts
### sys_kernel::kernel -> sys/kernel/kernel
### sys_kernel::lisp_total -> sys/kernel/lisp_total
## sys_link
Super Class: null
### sys_link::init -> sys/link/init
### sys_link::link -> sys/link/link
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
### sys_load::bind -> sys/load/bind
## sys_mail
Super Class: null
### sys_mail::statics -> sys/mail/statics
### sys_mail::init -> sys/mail/init
### sys_mail::init1 -> sys/mail/init1
### sys_mail::alloc_mbox -> sys/mail/alloc_mbox
### sys_mail::free_mbox -> sys/mail/free_mbox
### sys_mail::mbox_addr -> sys/mail/mbox_addr
### sys_mail::alloc -> sys/mail/alloc
### sys_mail::free -> sys/mail/free
### sys_mail::alloc_obj -> sys/mail/alloc_obj
### sys_mail::free_obj -> sys/mail/free_obj
### sys_mail::send -> sys/mail/send
### sys_mail::read -> sys/mail/read
### sys_mail::try_read -> sys/mail/try_read
### sys_mail::select -> sys/mail/select
### sys_mail::mymail -> sys/mail/mymail
### sys_mail::trymail -> sys/mail/trymail
### sys_mail::in -> sys/mail/in
### sys_mail::out -> sys/mail/out
### sys_mail::lisp_mymail -> sys/mail/lisp_mymail
### sys_mail::lisp_trymail -> sys/mail/lisp_trymail
### sys_mail::lisp_send -> sys/mail/lisp_send
## sys_math
Super Class: null
### sys_math::random -> sys/math/random
### sys_math::isqrt -> sys/math/isqrt
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
### sys_math::fcos -> sys/math/fcos
### sys_math::intersect -> sys/math/intersect
### sys_math::distance_sqd -> sys/math/distance_sqd
## sys_mem
Super Class: null
### sys_mem::statics -> sys/mem/statics
### sys_mem::init -> sys/mem/init
### sys_mem::deinit -> sys/mem/deinit
### sys_mem::alloc -> sys/mem/alloc
### sys_mem::calloc -> sys/mem/calloc
### sys_mem::free -> sys/mem/free
### sys_mem::clear -> sys/mem/clear
### sys_mem::fill -> sys/mem/fill
### sys_mem::copy -> sys/mem/copy
### sys_mem::realloc -> sys/mem/realloc
### sys_mem::recalloc -> sys/mem/recalloc
### sys_mem::used -> sys/mem/used
## sys_pii
Super Class: null
### sys_pii::exit -> sys/pii/exit
### sys_pii::mmap -> sys/pii/mmap
### sys_pii::munmap -> sys/pii/munmap
### sys_pii::mprotect -> sys/pii/mprotect
### sys_pii::open -> sys/pii/open
### sys_pii::close -> sys/pii/close
### sys_pii::ftruncate -> sys/pii/ftruncate
### sys_pii::unlink -> sys/pii/unlink
### sys_pii::stat -> sys/pii/stat
### sys_pii::fcntl -> sys/pii/fcntl
### sys_pii::write -> sys/pii/write
### sys_pii::write_char -> sys/pii/write_char
### sys_pii::write_str -> sys/pii/write_str
### sys_pii::write_num -> sys/pii/write_num
### sys_pii::read -> sys/pii/read
### sys_pii::read_char -> sys/pii/read_char
### sys_pii::age -> sys/pii/age
### sys_pii::time -> sys/pii/time
## sys_string
Super Class: null
### sys_string::length -> sys/string/length
### sys_string::copy -> sys/string/copy
### sys_string::compare -> sys/string/compare
### sys_string::to_long -> sys/string/to_long
### sys_string::from_long -> sys/string/from_long
## sys_task
Super Class: null
### sys_task::statics -> sys/task/statics
### sys_task::init -> sys/task/init
### sys_task::tcb -> sys/task/tcb
### sys_task::mailbox -> sys/task/mailbox
### sys_task::callback -> sys/task/callback
### sys_task::start -> sys/task/start
### sys_task::stop -> sys/task/stop
### sys_task::restore -> sys/task/restore
### sys_task::yield -> sys/task/yield
### sys_task::count -> sys/task/count
### sys_task::sleep -> sys/task/sleep
### sys_task::suspend -> sys/task/suspend
### sys_task::resume -> sys/task/resume
### sys_task::open_child -> sys/task/open_child
### sys_task::open_remote -> sys/task/open_remote
### sys_task::open_farm -> sys/task/open_farm
### sys_task::open_pipe -> sys/task/open_pipe
### sys_task::lisp_sleep -> sys/task/lisp_sleep
### sys_task::lisp_mailbox -> sys/task/lisp_mailbox
### sys_task::lisp_open_child -> sys/task/lisp_open_child
### sys_task::lisp_open_remote -> sys/task/lisp_open_remote
### sys_task::lisp_open_farm -> sys/task/lisp_open_farm
### sys_task::lisp_open_pipe -> sys/task/lisp_open_pipe
## text
Super Class: view
### text::vtable -> class/class_text
### text::create -> class/text/create
### text::new -> class/text/new
### text::init -> class/text/init
### text::switch_text -> class/text/switch_text
### text::deinit -> class/text/deinit
### text::pref_size -> class/text/pref_size
### text::draw -> class/text/draw
## texture
Super Class: obj
### texture::vtable -> class/class_texture
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
## title
Super Class: label
### title::vtable -> class/class_title
### title::create -> class/title/create
### title::new -> class/title/new
### title::init -> class/title/init
### title::mouse_down -> class/title/mouse_down
### title::mouse_move -> class/title/mouse_move
## vdu
Super Class: view
### vdu::vtable -> class/class_vdu
### vdu::create -> class/vdu/create
### vdu::new -> class/vdu/new
### vdu::init -> class/vdu/init
### vdu::switch_font -> class/vdu/switch_font
### vdu::switch_size -> class/vdu/switch_size
### vdu::print_char -> class/vdu/print_char
### vdu::print_cstr -> class/vdu/print_cstr
### vdu::deinit -> class/vdu/deinit
### vdu::pref_size -> class/vdu/pref_size
### vdu::draw -> class/vdu/draw
## vector
Super Class: array
### vector::vtable -> class/class_vector
### vector::create -> class/vector/create
### vector::deinit -> class/vector/deinit
### vector::ref_element -> class/vector/ref_element
### vector::slice -> class/vector/slice
### vector::clear -> class/vector/clear
### vector::ref_back -> class/vector/ref_back
### vector::pop_back -> class/vector/pop_back
### vector::set_element -> class/vector/set_element
### vector::append -> class/vector/append
## view
Super Class: component
### view::vtable -> class/class_view
### view::init -> class/view/init
### view::add_front -> class/view/add_front
### view::add_back -> class/view/add_back
### view::sub -> class/view/sub
### view::to_front -> class/view/to_front
### view::add_dirty -> class/view/add_dirty
### view::add_opaque -> class/view/add_opaque
### view::sub_opaque -> class/view/sub_opaque
### view::get_relative -> class/view/get_relative
### view::dirty -> class/view/dirty
### view::dirty_all -> class/view/dirty_all
### view::opaque -> class/view/opaque
### view::forward -> class/view/forward
### view::backward -> class/view/backward
### view::forward_tree -> class/view/forward_tree
### view::backward_tree -> class/view/backward_tree
### view::change -> class/view/change
### view::hit_tree -> class/view/hit_tree
### view::find_id -> class/view/find_id
### view::draw_panel -> class/view/draw_panel
### view::get_bounds -> class/view/get_bounds
### view::set_bounds -> class/view/set_bounds
### view::get_first -> class/view/get_first
### view::get_last -> class/view/get_last
### view::lisp_sub -> class/view/lisp_sub
### view::lisp_add_child -> class/view/lisp_add_child
### view::lisp_add_front -> class/view/lisp_add_front
### view::lisp_add_back -> class/view/lisp_add_back
### view::lisp_pref_size -> class/view/lisp_pref_size
### view::lisp_change -> class/view/lisp_change
### view::lisp_set_bounds -> class/view/lisp_set_bounds
### view::lisp_get_bounds -> class/view/lisp_get_bounds
### view::lisp_add_opaque -> class/view/lisp_add_opaque
### view::lisp_sub_opaque -> class/view/lisp_sub_opaque
### view::lisp_opaque -> class/view/lisp_opaque
### view::lisp_add_dirty -> class/view/lisp_add_dirty
### view::lisp_dirty -> class/view/lisp_dirty
### view::lisp_dirty_all -> class/view/lisp_dirty_all
### view::lisp_layout -> class/view/lisp_layout
### view::lisp_event -> class/view/lisp_event
### view::lisp_find_id -> class/view/lisp_find_id
### view::deinit -> class/view/deinit
### view::add_child -> class/view/add_back
### view::draw -> class/obj/null
### view::hit -> class/view/hit
### view::pref_size -> class/view/pref_size
### view::layout -> class/obj/null
### view::event -> class/view/event
### view::mouse_down -> class/obj/null
### view::mouse_up -> class/obj/null
### view::mouse_move -> class/obj/null
### view::mouse_hover -> class/obj/null
### view::key_down -> class/obj/null
### view::key_up -> class/obj/null
## window
Super Class: view
### window::vtable -> class/class_window
### window::new -> class/window/new
### window::create -> class/window/create
### window::init -> class/window/init
### window::set_title -> class/window/set_title
### window::set_status -> class/window/set_status
### window::connect_close -> class/window/connect_close
### window::connect_max -> class/window/connect_max
### window::connect_min -> class/window/connect_min
### window::deinit -> class/window/deinit
### window::add_child -> class/window/add_child
### window::pref_size -> class/window/pref_size
### window::layout -> class/window/layout
### window::draw -> class/window/draw
### window::mouse_down -> class/window/mouse_down
### window::mouse_move -> class/window/mouse_move
### window::lisp_create -> class/window/lisp_create
### window::lisp_connect_close -> class/window/lisp_connect_close
### window::lisp_connect_min -> class/window/lisp_connect_min
### window::lisp_connect_max -> class/window/lisp_connect_max
### window::lisp_set_status -> class/window/lisp_set_status
### window::lisp_set_title -> class/window/lisp_set_title
