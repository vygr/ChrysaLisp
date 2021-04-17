# Syntax

## #

```lisp

(# (< %9 %0 %3) ...) -> (lambda (%0 %3 %9) (< %9 %0 %3) ...)

```

## %

```lisp

(% num num ...)

```

## *

```lisp

(* num num ...)

```

## +

```lisp

(+ num num ...)

```

## -

```lisp

(- num num ...)

```

## backdrop :draw

```lisp

(. backdrop :draw) -> backdrop

```

## button :draw

```lisp

(. button :draw) -> button

```

## button :layout

```lisp

(. button :layout) -> button

```

## button :mouse_down

```lisp

(. button :mouse_down event) -> button

```

## button :mouse_move

```lisp

(. button :mouse_move event) -> button

```

## button :mouse_up

```lisp

(. button :mouse_up event) -> button

```

## canvas :draw

```lisp

(. canvas :draw) -> canvas

```

## canvas :fbox

```lisp

(. canvas :fbox x y width height) -> canvas

```

## canvas :fill

```lisp

(. canvas :fill argb) -> canvas

```

## canvas :fpoly

```lisp

(. canvas :fpoly x y mode paths) -> canvas

```

## canvas :next_frame

```lisp

(. canvas :next_frame) -> canvas

```

## canvas :plot

```lisp

(. canvas :plot x y) -> canvas

```

## canvas :pref_size

```lisp

(. canvas :pref_size) -> (width height)

```

## canvas :resize

```lisp

(. canvas :resize canvas) -> canvas

```

## canvas :save

```lisp

(. canvas :save file format) -> nil | canvas

```

## canvas :set_canvas_flags

```lisp

(. canvas :set_canvas_flags flags) -> canvas

```

## canvas :set_color

```lisp

(. canvas :set_color argb) -> canvas

```

## canvas :swap

```lisp

(. canvas :swap) -> canvas

```

## emap :copy

```lisp

(. emap :copy) -> emap

```

## emap :deep_copy

```lisp

(. emap :deep_copy) -> emap

```

## emap :each

```lisp

(. emap :each lambda)

```

## emap :empty

```lisp

(. emap :empty) -> emap

```

## emap :erase

```lisp

(. emap :erase key) -> emap

```

## emap :find

```lisp

(. emap :find key) -> nil|val

```

## emap :insert

```lisp

(. emap :insert key val) -> emap

```

## emap :move

```lisp

(. emap :move) -> emap

```

## emap :resize

```lisp

(. emap :resize num_buckets) -> emap

```

## env sym

```lisp

(. env sym [...])

```

## flow :layout

```lisp

(. flow :layout) -> flow

```

## flow :pref_size

```lisp

(. flow :pref_size) -> (width height)

```

## grid :layout

```lisp

(. grid :layout) -> grid

```

## grid :pref_size

```lisp

(. grid :pref_size) -> (width height)

```

## label :add_child

```lisp

(. label :add_child child) -> label

```

## label :draw

```lisp

(. label :draw) -> label

```

## label :layout

```lisp

(. label :layout) -> label

```

## label :pref_size

```lisp

(. label :pref_size) -> (width height)

```

## progress :draw

```lisp

(. progress :draw) -> progress

```

## progress :pref_size

```lisp

(. progress :pref_size) -> (width height)

```

## scroll :action

```lisp

(. scroll :action data) -> scroll

```

## scroll :add_child

```lisp

(. scroll :add_child child) -> scroll

```

## scroll :layout

```lisp

(. scroll :layout) -> scroll

```

## scroll :pref_size

```lisp

(. scroll :pref_size) -> (width height)

```

## slider :draw

```lisp

(. slider :draw) -> slider

```

## slider :mouse_down

```lisp

(. slider :mouse_down event) -> slider

```

## slider :mouse_move

```lisp

(. slider :mouse_move event) -> slider

```

## slider :mouse_up

```lisp

(. slider :mouse_up event) -> slider

```

## slider :pref_size

```lisp

(. slider :pref_size) -> (width height)

```

## text :draw

```lisp

(. text :draw) -> text

```

## text :pref_size

```lisp

(. text :pref_size) -> (width height)

```

## textfield :draw

```lisp

(. textfield :draw) -> textfield

```

## textfield :key_down

```lisp

(. textfield :key_down event) -> textfield

```

## textfield :layout

```lisp

(. textfield :layout) -> textfield

```

## .

```lisp

(. this :method [arg ...])

```

## title :mouse_down

```lisp

(. title :mouse_down event) -> title

```

## title :mouse_move

```lisp

(. title :mouse_move event) -> title

```

## tree :action

```lisp

(. tree :action event) -> tree

```

## tree :add_route

```lisp

(. tree :add_route route) -> tree

```

## tree :get_route

```lisp

(. tree :get_route node) -> route

```

## vdu :char_size

```lisp

(. vdu :char_size) -> (width height)

```

## vdu :load

```lisp

(. vdu :load lines offset_x offset_y cursor_x cursor_y) -> vdu

```

## vdu :max_size

```lisp

(. vdu :max_size) -> (width height)

```

## vdu :pref_size

```lisp

(. vdu :pref_size) -> (width height)

```

## view :add_back

```lisp

(. view :add_back child) -> view

```

## view :add_child

```lisp

(. view :add_child child) -> view

```

## view :add_dirty

```lisp

(. view :add_dirty x y width height) -> view

```

## view :add_front

```lisp

(. view :add_front child) -> view

```

## view :add_opaque

```lisp

(. view :add_opaque x y width height) -> view

```

## view :change

```lisp

(. view :change x y width height) -> view

```

## view :change_dirty

```lisp

(. view :change_dirty x y width height) -> view

```

## view :children

```lisp

(. view :children) -> (child0 child1 ...)

```

## view :clr_opaque

```lisp

(. view :clr_opaque) -> view

```

## view :connect

```lisp

(. view :connect id) -> view

```

## view :ctx_box

```lisp

(. view :ctx_box x y width height) -> view

```

## view :ctx_filled_box

```lisp

(. view :ctx_filled_box tid col x y width height) -> view

```

## view :ctx_filled_box

```lisp

(. view :ctx_filled_box x y width height) -> view

```

## view :ctx_panel

```lisp

(. view :ctx_panel col flags depth x y width height) -> view

```

## view :ctx_set_color

```lisp

(. view :ctx_set_color col) -> view

```

## view :dirty

```lisp

(. view :dirty) -> view

```

## view :dirty_all

```lisp

(. view :dirty_all) -> view

```

## view :emit

```lisp

(. view :emit) -> view

```

## view :find_id

```lisp

(. view :find_id target_id) -> nil | target_view

```

## view :get_bounds

```lisp

(. view :get_bounds) -> (x y width height)

```

## view :get_flags

```lisp

(. view :get_flags) -> flags

```

## view :get_id

```lisp

(. view :get_id) -> id

```

## view :get_pos

```lisp

(. view :get_pos) -> (x y)

```

## view :get_size

```lisp

(. view :get_size) -> (width height)

```

## view :hide

```lisp

(. view :hide)

```

## view :hide

```lisp

(. view :hide) -> view

```

## view :layout

```lisp

(. view :layout) -> view

```

## view :lisp_sub

```lisp

(. view :lisp_sub) -> view

```

## view :pref_size

```lisp

(. view :pref_size) -> (width height)

```

## view :set_flags

```lisp

(. view :set_flags value mask) -> view

```

## view :set_size

```lisp

(. view :set_size width height) -> view

```

## view :set_size

```lisp

(. view :set_size x y width height) -> view

```

## view :sub_opaque

```lisp

(. view :sub_opaque x y width height) -> view

```

## view :to_back

```lisp

(. view :to_back) -> view

```

## view :to_front

```lisp

(. view :to_front) -> view

```

## view :trans_dirty

```lisp

(. view :trans_dirty rx ry) -> view

```

## window :add_child

```lisp

(. window :add_child child) -> window

```

## window :draw

```lisp

(. window :draw) -> window

```

## window :event

```lisp

(. window :event event) -> window

```

## window :layout

```lisp

(. window :layout) -> window

```

## window :mouse_down

```lisp

(. window :mouse_down event) -> window

```

## window :mouse_move

```lisp

(. window :mouse_move event) -> window

```

## window :pref_size

```lisp

(. window :pref_size) -> (width height)

```

## xmap :copy

```lisp

(. xmap :copy) -> xmap

```

## xmap :deep_copy

```lisp

(. xmap :deep_copy) -> xmap

```

## xmap :each

```lisp

(. xmap :each lambda)

```

## xmap :empty

```lisp

(. xmap :empty) -> xmap

```

## xmap :erase

```lisp

(. xmap :erase key) -> xmap

```

## xmap :find

```lisp

(. xmap :find key) -> nil|val

```

## xmap :insert

```lisp

(. xmap :insert key val) -> xmap

```

## xmap :move

```lisp

(. xmap :move) -> xmap

```

## xmap :resize

```lisp

(. xmap :resize num_buckets) -> xmap

```

## xset :copy

```lisp

(. xset :copy) -> xset

```

## xset :deep_copy

```lisp

(. xset :deep_copy) -> xset

```

## xset :difference

```lisp

(. xset :difference xset) -> xset

```

## xset :each

```lisp

(. xset :each lambda)

```

## xset :empty

```lisp

(. xset :empty) -> xset

```

## xset :erase

```lisp

(. xset :erase key) -> xset

```

## xset :find

```lisp

(. xset :find key) -> nil|key

```

## xset :insert

```lisp

(. xset :insert key) -> xset

```

## xset :intersect

```lisp

(. xset :intersect xset) -> xset

```

## xset :move

```lisp

(. xset :move) -> xset

```

## xset :not_intersect

```lisp

(. xset :not_intersect xset) -> xset

```

## xset :resize

```lisp

(. xset :resize num_buckets) -> xset

```

## xset :union

```lisp

(. xset :union xset) -> xset

```

## .->

```lisp

(.-> this form ...)

```

## .super

```lisp

(.super this :method [arg ...])

```

## /

```lisp

(/ num num ...)

```

## /=

```lisp

(/= num num ...)

```

## <

```lisp

(< num num ...)

```

## <<

```lisp

(<< num cnt)

```

## <=

```lisp

(<= num num ...)

```

## =

```lisp

(= num num ...)

```

## >

```lisp

(> num num ...)

```

## >=

```lisp

(>= num num ...)

```

## >>

```lisp

(>> num cnt)

```

## >>>

```lisp

(>>> num cnt)

```

## Backdrop

```lisp

(Backdrop)-> backdrop

```

## Button

```lisp

(Button) -> button

```

## Canvas

```lisp

(Canvas width height scale) -> canvas

```

## Canvas-from-file

```lisp

(Canvas-from-file file flags) -> nil | canvas

```

## Flow

```lisp

(Flow) -> flow

```

## Grid

```lisp

(Grid) -> grid

```

## Label

```lisp

(Label) -> label

```

## Progress

```lisp

(Progress) -> progress

```

## Scroll

```lisp

(Scroll flags) -> scroll

```

## Slider

```lisp

(Slider) -> slider

```

## Text

```lisp

(Text) -> text

```

## Textfield

```lisp

(Textfield) -> textfield

```

## Title

```lisp

(Title) -> title

```

## Tree

```lisp

(Tree event) -> tree

```

## Vdu

```lisp

(Vdu) -> vdu

```

## View

```lisp

(View) -> view

```

## Window

```lisp

(Window) -> window

```

## aand

```lisp

(aand [form] ...)

```

## abi

```lisp

(abi) -> sym

```

## abs

```lisp

(abs num)

```

## acond

```lisp

(acond (tst body) ...)

```

## aeach

```lisp

(aeach seq body)

```

## age

```lisp

(age path) -> 0 | time ns

```

## aif

```lisp

(aif form form [form])

```

## align

```lisp

(align num div) -> num

```

## and

```lisp

(and [tst] ...) -> t | nil | tst

```

## apply

```lisp

(apply lambda list)

```

## array

```lisp

(array [form ...])

```

## ascii-char

```lisp

(ascii-char num) -> char

```

## ascii-code

```lisp

(ascii-code char) -> num

```

## ascii-lower

```lisp

(ascii-lower num) -> num

```

## ascii-upper

```lisp

(ascii-upper num) -> num

```

## asome

```lisp

(asome seq body)

```

## awhen

```lisp

(awhen form body)

```

## awhile

```lisp

(awhile form body)

```

## bind

```lisp

(bind (param ...) seq)

```

## bits

```lisp

(bits name base [(bit field ...)] ...)

```

## canvas-brighter

```lisp

(canvas-brighter col)

```

## canvas-darker

```lisp

(canvas-darker col)

```

## canvas-fbox

```lisp

(canvas-fbox canvas x y w h)

```

## canvas-fill

```lisp

(canvas-fill canvas argb)

```

## canvas-fpoly

```lisp

(canvas-fpoly canvas x y mode list)

```

## canvas-from-argb32

```lisp

(canvas-from-argb32 pixel type)

```

## canvas-info

```lisp

(canvas-info path)

```

## canvas-load

```lisp

(canvas-load path flags)

```

## canvas-next-frame

```lisp

(canvas-next-frame canvas)

```

## canvas-plot

```lisp

(canvas-plot canvas x y)

```

## canvas-resize

```lisp

(canvas-resize canvas canvas)

```

## canvas-save

```lisp

(canvas-save canvas path format)

```

## canvas-swap

```lisp

(canvas-swap canvas)

```

## canvas-to-argb32

```lisp

(canvas-to-argb32 pixel type)

```

## cap

```lisp

(cap len array ...)

```

## case

```lisp

(case form [(key body)] ...)

```

## cat

```lisp

(cat seq ...)

```

## catch

```lisp

(catch form eform)

```

## char

```lisp

(char num [width])

```

## char-to-num

```lisp

(char-to-num char) -> num

```

## clear

```lisp

(clear array ...)

```

## cmp

```lisp

(cmp str str)

```

## code

```lisp

(code str [width index])

```

## collection?

```lisp

(collection? obj) -> t | nil

```

## compose

```lisp

(compose lambda lambda) -> lambda

```

## cond

```lisp

(cond [(tst [body])] ...)

```

## const

```lisp

(const form)

```

## copy

```lisp

(copy form)

```

## cos

```lisp

(cos angle)

```

## cpu

```lisp

(cpu) -> sym

```

## create-canvas

```lisp

(create-canvas width height scale)

```

## create-font

```lisp

(create-font name pixels)

```

## create-stdio

```lisp

(create-stdio)

```

## create-vdu

```lisp

(create-vdu)

```

## create-view

```lisp

(create-view)

```

## ctx-blit

```lisp

(ctx-blit view tid col x y w h)

```

## ctx-box

```lisp

(ctx-box view x y w h)

```

## ctx-filled-box

```lisp

(ctx-filled-box view x y w h)

```

## ctx-panel

```lisp

(ctx-panel view col flags depth x y w h)

```

## ctx-set-color

```lisp

(ctx-set-color view col)

```

## curry

```lisp

(curry lambda var ...) -> lambda

```

## debug

```lisp

(debug name form)

```

## debug-format

```lisp

(debug-format name env)

```

## debug-fun

```lisp

(debug-fun name list) -> list

```

## debug-fun?

```lisp

(debug-fun? form)

```

## debug-send

```lisp

(debug-send string)

```

## dec

```lisp

(dec num) -> num

```

## def

```lisp

(def env var val [var val] ...)

```

## def?

```lisp

(def? var [env])

```

## defabstractmethod

```lisp

(defabstractmethod (this [arg ...]) body)

```

## defclass

```lisp

(defclass name ([arg ...]) (super ...) body)

```

## deffimethod

```lisp

(deffimethod name ffi)

```

## defmethod

```lisp

(defmethod name (this [arg ...]) body)

```

## defq

```lisp

(defq var val [var val] ...)

```

## defun

```lisp

(defun name ([arg ...]) body)

```

## defun-debug

```lisp

(defun-debug name ([arg ...]) body)

```

## defun-unbound

```lisp

(defun-unbound name ([arg ...]) body)

```

## drop!

```lisp

(drop! collection key) -> collection

```

## each

```lisp

(each lambda seq ...)

```

## each!

```lisp

(each! start end lambda (seq ...))

```

## each-line

```lisp

(each-line lambda stream)

```

## each-mergeable

```lisp

(each-mergeable lambda seq) -> seq

```

## each-mergeable-rev

```lisp

(each-mergeable-rev lambda seq) -> seq

```

## each-rev

```lisp

(each-rev lambda seq ...)

```

## elem

```lisp

(elem index seq)

```

## elem-set

```lisp

(elem-set index list val)

```

## emap

```lisp

(emap [num_buckets]) -> emap

```

## emap-kv

```lisp

(emap-kv [key val ...]) -> emap

```

## empty

```lisp

(empty collection) -> collection | nil

```

## empty?

```lisp

(empty? form) -> bool

```

## ends-with

```lisp

(ends-with str str) -> t | nil

```

## entries

```lisp

(entries collection) ->  list | nil

```

## enums

```lisp

(enums name base [(enum field ...)] ...)

```

## env

```lisp

(env [num])

```

## env-pop

```lisp

(env-pop)

```

## env-push

```lisp

(env-push)

```

## env?

```lisp

(env? form) -> t | nil

```

## eql

```lisp

(eql form form)

```

## erase

```lisp

(erase seq start end) -> seq

```

## eval

```lisp

(eval form [env])

```

## even?

```lisp

(even? num) -> bool

```

## every

```lisp

(every lambda seq ...) -> nil|form

```

## exec

```lisp

(exec form)

```

## f2i

```lisp

(f2i fixed)

```

## f2r

```lisp

(f2r fixed)

```

## ffi

```lisp

(ffi sym path flags)

```

## file-stream

```lisp

(file-stream path [mode])

```

## filter

```lisp

(filter lambda seq) -> list

```

## find

```lisp

(find elem seq)

```

## find-rev

```lisp

(find-rev elem seq)

```

## first

```lisp

(first seq) -> el|nil

```

## fixeds

```lisp

(fixeds [form ...])

```

## floor

```lisp

(floor num)

```

## font-glyph-bounds

```lisp

(font-glyph-bounds font str)

```

## font-glyph-paths

```lisp

(font-glyph-paths font str)

```

## font-glyph-ranges

```lisp

(font-glyph-ranges font)

```

## font-sym-texture

```lisp

(font-sym-texture font sym)

```

## frac

```lisp

(frac num)

```

## func?

```lisp

(func? form) -> t | nil

```

## gensym

```lisp

(gensym)

```

## get

```lisp

(get var [env])

```

## get-byte

```lisp

(get-byte str index) -> num

```

## get-cstr

```lisp

(get-cstr str index) -> str

```

## get-field

```lisp

(get-field obj field size|0)

```

## get-int

```lisp

(get-int str index) -> num

```

## get-long

```lisp

(get-long str index) -> num

```

## get-netid

```lisp

(get-netid str index) -> netid

```

## get-nodeid

```lisp

(get-nodeid str index) -> nodeid

```

## get-short

```lisp

(get-short str index) -> num

```

## get-ubyte

```lisp

(get-ubyte str index) -> num

```

## get-uint

```lisp

(get-uint str index) -> num

```

## get-ushort

```lisp

(get-ushort str index) -> num

```

## getf

```lisp

(getf obj field [offset]) -> value

```

## gets

```lisp

(gets collection k [if_nil]) -> value | if_nil | nil

```

## gets-in

```lisp

(gets-in collection key-path) -> value | nil

```

## gui-add

```lisp

(gui-add view)

```

## gui-add-back

```lisp

(gui-add-back view)

```

## gui-info

```lisp

(gui-info)

```

## hash

```lisp

(hash obj)

```

## i2f

```lisp

(i2f num)

```

## i2r

```lisp

(i2r num)

```

## identity

```lisp

(identity any) -> any

```

## if

```lisp

(if tst form [else_form])

```

## import

```lisp

(import path [env]) -> env

```

## in-get-state

```lisp

(in-get-state in) -> num

```

## in-mbox

```lisp

(in-mbox in) -> mbox

```

## in-next-msg

```lisp

(in-next-msg in)

```

## in-set-state

```lisp

(in-set-state in num) -> in

```

## in-stream

```lisp

(in-stream)

```

## inc

```lisp

(inc num) -> num

```

## insert

```lisp

(insert seq pos seq) -> seq

```

## intern

```lisp

(intern list form [lambda]) -> form

```

## intern-seq

```lisp

(intern-seq seq [list lambda]) -> list

```

## into-fn

```lisp

(into-fn collection) -> fn

```

## into-map

```lisp

(into-map map list-of-pairs) -> map

```

## into-set

```lisp

(into-set set list-of-elements) -> set

```

## io-stream

```lisp

(io-stream io)

```

## join

```lisp

(join list seq) -> seq

```

## kernel-stats

```lisp

(kernel-stats)

```

## keys

```lisp

(keys collection) -> list | nil

```

## lambda

```lisp

(lambda ([arg ...]) body)

```

## lambda?

```lisp

(lambda? form) -> t | nil

```

## last

```lisp

(last seq) -> el|nil

```

## length

```lisp

(length seq)

```

## let

```lisp

(let ([(var val) ...]) body)

```

## list

```lisp

(list [form ...])

```

## list?

```lisp

(list? form) -> t | nil

```

## load

```lisp

(load path)

```

## load-path

```lisp

(load-path)

```

## load-stream

```lisp

(load-stream path) -> nil|stream

```

## log2

```lisp

(log2 num) -> num

```

## logand

```lisp

(logand [num] ...)

```

## logior

```lisp

(logior [num] ...)

```

## lognot

```lisp

(lognot num) -> num

```

## logxor

```lisp

(logxor [num] ...)

```

## macro?

```lisp

(macro? form) -> t | nil

```

## macroexpand

```lisp

(macroexpand form)

```

## mail-alloc-mbox

```lisp

(mail-alloc-mbox)

```

## mail-declare

```lisp

(mail-declare mbox name info)

```

## mail-enquire

```lisp

(mail-enquire prefix)

```

## mail-forget

```lisp

(mail-forget key)

```

## mail-free-mbox

```lisp

(mail-free-mbox mbox)

```

## mail-nodes

```lisp

(mail-nodes)

```

## mail-poll

```lisp

(mail-poll mboxs)

```

## mail-read

```lisp

(mail-read mbox)

```

## mail-select

```lisp

(mail-select mboxs)

```

## mail-send

```lisp

(mail-send mbox obj)

```

## mail-timeout

```lisp

(mail-timeout mbox ns)

```

## map

```lisp

(map lambda seq ...) -> list

```

## map-rev

```lisp

(map-rev lambda seq ...) -> list

```

## map?

```lisp

(map? object) -> t | nil

```

## match?

```lisp

(match? list list)

```

## max

```lisp

(max num num ...)

```

## merge-into!

```lisp

(merge-into! collection (collections)) -> collection

```

## merge-obj

```lisp

(merge-obj dlist slist) -> dlist

```

## merges

```lisp

(merges (collections)) -> collection

```

## min

```lisp

(min num num ...)

```

## neg

```lisp

(neg num)

```

## neg?

```lisp

(neg? num) -> bool

```

## nempty?

```lisp

(nempty? form) -> bool

```

## nil?

```lisp

(nil? o) -> bool

```

## nlo

```lisp

(nlo num) -> num

```

## nlz

```lisp

(nlz num) -> num

```

## not

```lisp

(not form) -> t | nil

```

## notany

```lisp

(notany lambda seq ...) -> t | nil

```

## notevery

```lisp

(notevery lambda seq ...) -> t | nil

```

## nto

```lisp

(nto num) -> num

```

## ntz

```lisp

(ntz num) -> num

```

## num-to-char

```lisp

(num-to-char num) -> char

```

## num-to-utf8

```lisp

(num-to-utf8 num) -> str

```

## num?

```lisp

(num? form) -> t | nil

```

## nums

```lisp

(nums [form ...])

```

## nums-abs

```lisp

(nums-abs nums [nums])

```

## nums-add

```lisp

(nums-add nums nums [nums])

```

## nums-div

```lisp

(nums-div nums nums [nums])

```

## nums-floor

```lisp

(nums-floor fixeds [fixeds])

```

## nums-frac

```lisp

(nums-frac fixeds [fixeds])

```

## nums-mod

```lisp

(nums-mod nums nums [nums])

```

## nums-mul

```lisp

(nums-mul nums nums [nums])

```

## nums-scale

```lisp

(nums-scale nums scale [nums])

```

## nums-sub

```lisp

(nums-sub nums nums [nums])

```

## nums-sum

```lisp

(nums-sum nums)

```

## obj-ref

```lisp

(obj-ref num)

```

## odd?

```lisp

(odd? num) -> bool

```

## open-child

```lisp

(open-child task mode) -> str

```

## open-pipe

```lisp

(open-pipe tasks) -> (list str ...)

```

## open-remote

```lisp

(open-remote task node mode) -> str

```

## open-task

```lisp

(open-task task node mode key_num reply)

```

## opt

```lisp

(opt var val [cond])

```

## or

```lisp

(or [tst] ...) -> nil | tst

```

## out-stream

```lisp

(out-stream mbox)

```

## pad

```lisp

(pad form width [str]) -> str

```

## pairs-into-kv

```lisp

(pairs-into-kv list) -> emap

```

## partition

```lisp

(partition count seq) -> list of lists

```

## path

```lisp

(path [form ...])

```

## path-filter

```lisp

(path-filter tol src dst)

```

## path-gen-arc

```lisp

(path-gen-arc cx cy start end radius tol dst) -> dst

```

## path-gen-cubic

```lisp

(path-gen-cubic p1x p1y p2x p2y p3x p3y p4x p4y tol dst) -> dst

```

## path-gen-quadratic

```lisp

(path-gen-quadratic p1x p1y p2x p2y p3x p3y tol dst) -> dst

```

## path-simplify

```lisp

(path-simplify tol src dst)

```

## path-stroke-polygons

```lisp

(path-stroke-polygons dst radius tol join src) -> dst

```

## path-stroke-polylines

```lisp

(path-stroke-polylines dst radius tol join cap1 cap2 src) -> dst

```

## path-transform

```lisp

(path-transform m1x m1y m2x m2y trx try src dst)

```

## penv

```lisp

(penv [env])

```

## pii-dirlist

```lisp

(pii-dirlist path)

```

## pii-fstat

```lisp

(pii-fstat path)

```

## pii-read-char

```lisp

(pii-read-char fd)

```

## pii-remove

```lisp

(pii-remove path)

```

## pii-time

```lisp

(pii-time)

```

## pii-write-char

```lisp

(pii-write-char fd char)

```

## pivot

```lisp

(pivot lambda list start end)

```

## pop

```lisp

(pop array)

```

## pos?

```lisp

(pos? num) -> bool

```

## pow

```lisp

(pow base exponent) -> integer

```

## prebind

```lisp

(prebind form)

```

## prin

```lisp

(prin [form ...])

```

## print

```lisp

(print [form ...])

```

## profile-print

```lisp

(profile-print name [stream]) -> stdout|stream

```

## profile-report

```lisp

(profile-report name [reset])

```

## progn

```lisp

(progn [form ...])

```

## push

```lisp

(push array form ...)

```

## quasi-quote

```lisp

(quasi-quote form)

```

## quote

```lisp

(quote form)

```

## r2f

```lisp

(r2f real)

```

## r2i

```lisp

(r2i real)

```

## random

```lisp

(random num)

```

## range

```lisp

(range start end [step]) -> list

```

## rcurry

```lisp

(rcurry lambda var ...) -> lambda

```

## read

```lisp

(read stream last_char)

```

## read-avail

```lisp

(read-avail stream)

```

## read-char

```lisp

(read-char stream [width])

```

## read-int

```lisp

(read-int stream) -> num

```

## read-line

```lisp

(read-line stream)

```

## read-long

```lisp

(read-long stream) -> num

```

## read-short

```lisp

(read-short stream) -> num

```

## reals

```lisp

(reals [form ...])

```

## recip

```lisp

(recip num)

```

## reduce

```lisp

(reduce lambda seq [accum]) -> form

```

## reduce-rev

```lisp

(reduce-rev lambda seq [accum]) -> form

```

## reduced

```lisp

(reduced accum)

```

## reduced-reduce

```lisp

(reduced-reduce lambda seq [accum]) -> form

```

## reduced-reduce-rev

```lisp

(reduced-reduce-rev lambda seq [accum]) -> form

```

## repl

```lisp

(repl stream path)

```

## rest

```lisp

(rest seq) -> nil|seq

```

## reverse

```lisp

(reverse list) -> list

```

## save

```lisp

(save str path)

```

## second

```lisp

(second seq) -> el|nil

```

## seq?

```lisp

(seq? form) -> t | nil

```

## set

```lisp

(set env var val [var val] ...)

```

## set-field

```lisp

(set-field obj field size|0 val)

```

## setd

```lisp

(setd var val [var val] ...)

```

## setf

```lisp

(setf obj field value [offset]) -> obj

```

## setf->

```lisp

(setf-> msg form ...)

```

## setq

```lisp

(setq var val [var val] ...)

```

## sets!

```lisp

(sets! collection key value) -> collection

```

## sets-pairs!

```lisp

(sets-pairs! collection))

```

## shuffle

```lisp

(shuffle list [start end]) -> list

```

## shuffled

```lisp

(shuffled list [start end]) -> list

```

## sign

```lisp

(sign num)

```

## sin

```lisp

(sin angle)

```

## slice

```lisp

(slice start end seq)

```

## some

```lisp

(some lambda seq ...) -> nil|form

```

## some!

```lisp

(some! start end mode lambda (seq ...))

```

## sort

```lisp

(sort fcmp list [start end]) -> list

```

## sorted

```lisp

(sorted fcmp list [start end]) -> list

```

## split

```lisp

(split str chars)

```

## sqrt

```lisp

(sqrt num)

```

## starts-with

```lisp

(starts-with str str) -> t | nil

```

## stdio-get-args

```lisp

(stdio-get-args stdio) -> list

```

## str

```lisp

(str [form ...])

```

## str-alloc

```lisp

(str-alloc size)

```

## str-to-num

```lisp

(str-to-num str) -> num

```

## str?

```lisp

(str? form) -> t | nil

```

## stream-avail

```lisp

(stream-avail stream)

```

## stream-flush

```lisp

(stream-flush stream)

```

## stream-seek

```lisp

(stream-seek stream offset pos)

```

## string-stream

```lisp

(string-stream str)

```

## structure

```lisp

(structure name base [(byte field ...)] ...)

```

## swap

```lisp

(swap list index index)

```

## sym

```lisp

(sym str)

```

## sym?

```lisp

(sym? form) -> t | nil

```

## task-mailbox

```lisp

(task-mailbox)

```

## task-sleep

```lisp

(task-sleep usec)

```

## texture-metrics

```lisp

(texture-metrics texture) -> (handle width height)

```

## throw

```lisp

(throw str form)

```

## times

```lisp

(times num body)

```

## to-lower

```lisp

(to-lower str) -> str

```

## to-net-id

```lisp

(to-net-id service_id) -> net_id

```

## to-service-id

```lisp

(to-service-id net_id) -> service_id

```

## to-upper

```lisp

(to-upper str) -> str

```

## tolist

```lisp

(tolist env)

```

## trim

```lisp

(trim str [str]) -> str

```

## trim-end

```lisp

(trim-end str [str]) -> str

```

## trim-start

```lisp

(trim-start str [str]) -> str

```

## type-of

```lisp

(type-of obj)

```

## type-to-size

```lisp

(type-to-size sym) -> num

```

## ui-backdrop

```lisp

(ui-backdrop name [props] [body]) -> backdrop

```

## ui-button

```lisp

(ui-button name [props] [body]) -> button

```

## ui-buttons

```lisp

(ui-buttons symbols events [props] [group])

```

## ui-canvas

```lisp

(ui-canvas name width height scale) -> canvas

```

## ui-element

```lisp

(ui-element name func [props] [body]) -> view

```

## ui-flow

```lisp

(ui-flow name [props] [body]) -> flow

```

## ui-grid

```lisp

(ui-grid name [props] [body]) -> grid

```

## ui-label

```lisp

(ui-label name [props] [body]) -> label

```

## ui-merge-props

```lisp

(ui-merge-props props) -> props

```

## ui-progress

```lisp

(ui-progress name [props]) -> progress

```

## ui-props

```lisp

(ui-props props [props]) -> props

```

## ui-root

```lisp

(ui-root name func [props] [body]) -> view

```

## ui-scroll

```lisp

(ui-scroll name [props] [body]) -> scroll

```

## ui-slider

```lisp

(ui-slider name [props]) -> slider

```

## ui-textfield

```lisp

(ui-textfield name [props]) -> textfield

```

## ui-title

```lisp

(ui-title name [props]) -> title

```

## ui-title-bar

```lisp

(ui-title-bar name title symbols events [props]) -> flow

```

## ui-tool-bar

```lisp

(ui-tool-bar name [props] [body]) -> flow

```

## ui-tree

```lisp

(ui-tree name event [props]) -> tree

```

## ui-vdu

```lisp

(ui-vdu name [props]) -> vdu

```

## ui-view

```lisp

(ui-view name [props] [body]) -> view

```

## ui-window

```lisp

(ui-window name [props] [body]) -> window

```

## undef

```lisp

(undef env var [var] ...)

```

## unless

```lisp

(unless tst body)

```

## until

```lisp

(until tst body)

```

## unzip

```lisp

(unzip seq buckets) -> buckets

```

## values

```lisp

(values collection) -> list | nil

```

## vdu-configure

```lisp

(vdu-configure vdu)

```

## vdu-load

```lisp

(vdu-load vdu lines ox oy cx cy)

```

## view-add-back

```lisp

(view-add-back parent child)

```

## view-add-dirty

```lisp

(view-add-dirty view x y w h)

```

## view-add-front

```lisp

(view-add-front parent child)

```

## view-add-opaque

```lisp

(view-add-opaque view x y w h)

```

## view-children

```lisp

(view-children view)

```

## view-clr-opaque

```lisp

(view-clr-opaque view)

```

## view-emit

```lisp

(view-emit view)

```

## view-find-id

```lisp

(view-find-id view id)

```

## view-fit

```lisp

(view-fit x y w h) -> (x y w h)

```

## view-locate

```lisp

(view-locate w h [flag]) -> (x y w h)

```

## view-set-flags

```lisp

(view-set-flags view flags mask)

```

## view-sub

```lisp

(view-sub view)

```

## view-sub-opaque

```lisp

(view-sub-opaque view x y w h)

```

## view-to-back

```lisp

(view-to-back view)

```

## view-to-front

```lisp

(view-to-front view)

```

## view-trans-dirty

```lisp

(view-trans-dirty view rx ry)

```

## walk-list

```lisp

(walk-list list fnc_element fnc_in fnc_out)

```

## weak-ref

```lisp

(weak-ref obj)

```

## when

```lisp

(when tst body)

```

## while

```lisp

(while tst body)

```

## within-compile-env

```lisp

(within-compile-env lambda)

```

## write

```lisp

(write stream str)

```

## write-char

```lisp

(write-char stream list|num [width])

```

## write-int

```lisp

(write-int stream num|list) -> stream

```

## write-line

```lisp

(write-line stream str) -> stream

```

## write-long

```lisp

(write-long stream num|list) -> stream

```

## write-short

```lisp

(write-short stream num|list) -> stream

```

## xmap

```lisp

(xmap [num_buckets cmp_fnc hash_fnc]) -> xmap

```

## xmap-kv

```lisp

(xmap-kv [key val ...]) -> xmap

```

## xset

```lisp

(xset [num_buckets cmp_fnc hash_fnc]) -> xset

```

## xset-k

```lisp

(xset-k [key ...]) -> xset

```

## zip

```lisp

(zip seq ...) -> list

```

