# Syntax

## #

(# (< %0 0)) -> (lambda (%0) (< %0 0))

## %

(% num num ...)

## *

(* num num ...)

## +

(+ num num ...)

## -

(- num num ...)

## backdrop :draw

(. backdrop :draw) -> backdrop

## button :draw

(. button :draw) -> button

## button :layout

(. button :layout) -> button

## button :mouse_down

(. button :mouse_down event) -> button

## button :mouse_move

(. button :mouse_move event) -> button

## button :mouse_up

(. button :mouse_up event) -> button

## canvas :draw

(. canvas :draw) -> canvas

## canvas :fbox

(. canvas :fbox x y width height) -> canvas

## canvas :fill

(. canvas :fill argb) -> canvas

## canvas :fpoly

(. canvas :fpoly x y mode paths) -> canvas

## canvas :next_frame

(. canvas :next_frame) -> canvas

## canvas :plot

(. canvas :plot x y) -> canvas

## canvas :pref_size

(. canvas :pref_size) -> (width height)

## canvas :resize

(. canvas :resize canvas) -> canvas

## canvas :save

(. canvas :save file format) -> nil | canvas

## canvas :set_canvas_flags

(. canvas :set_canvas_flags flags) -> canvas

## canvas :set_color

(. canvas :set_color argb) -> canvas

## canvas :swap

(. canvas :swap) -> canvas

## emap :copy

(. emap :copy) -> emap

## emap :deep_copy

(. emap :deep_copy) -> emap

## emap :each

(. emap :each lambda)

## emap :empty

(. emap :empty) -> emap

## emap :erase

(. emap :erase key) -> emap

## emap :find

(. emap :find key) -> nil|val

## emap :insert

(. emap :insert key val) -> emap

## emap :move

(. emap :move) -> emap

## emap :resize

(. emap :resize num_buckets) -> emap

## env sym

(. env sym [...])

## flow :layout

(. flow :layout) -> flow

## flow :pref_size

(. flow :pref_size) -> (width height)

## grid :layout

(. grid :layout) -> grid

## grid :pref_size

(. grid :pref_size) -> (width height)

## label :add_child

(. label :add_child child) -> label

## label :draw

(. label :draw) -> label

## label :layout

(. label :layout) -> label

## label :pref_size

(. label :pref_size) -> (width height)

## progress :draw

(. progress :draw) -> progress

## progress :pref_size

(. progress :pref_size) -> (width height)

## scroll :action

(. scroll :action data) -> scroll

## scroll :add_child

(. scroll :add_child child) -> scroll

## scroll :layout

(. scroll :layout) -> scroll

## scroll :pref_size

(. scroll :pref_size) -> (width height)

## slider :draw

(. slider :draw) -> slider

## slider :mouse_down

(. slider :mouse_down event) -> slider

## slider :mouse_move

(. slider :mouse_move event) -> slider

## slider :mouse_up

(. slider :mouse_up event) -> slider

## slider :pref_size

(. slider :pref_size) -> (width height)

## text :draw

(. text :draw) -> text

## text :pref_size

(. text :pref_size) -> (width height)

## textfield :draw

(. textfield :draw) -> textfield

## textfield :key_down

(. textfield :key_down event) -> textfield

## textfield :layout

(. textfield :layout) -> textfield

## .

(. this :method [arg ...])

## title :mouse_down

(. title :mouse_down event) -> title

## title :mouse_move

(. title :mouse_move event) -> title

## tree :action

(. tree :action event) -> tree

## tree :add_route

(. tree :add_route route) -> tree

## tree :get_route

(. tree :get_route node) -> route

## vdu :char_size

(. vdu :char_size) -> (width height)

## vdu :load

(. vdu :load lines offset_x offset_y cursor_x cursor_y) -> vdu

## vdu :max_size

(. vdu :max_size) -> (width height)

## vdu :pref_size

(. vdu :pref_size) -> (width height)

## view :add_back

(. view :add_back child) -> view

## view :add_child

(. view :add_child child) -> view

## view :add_dirty

(. view :add_dirty x y width height) -> view

## view :add_front

(. view :add_front child) -> view

## view :add_opaque

(. view :add_opaque x y width height) -> view

## view :change

(. view :change x y width height) -> view

## view :change_dirty

(. view :change_dirty x y width height) -> view

## view :children

(. view :children) -> (child0 child1 ...)

## view :clr_opaque

(. view :clr_opaque) -> view

## view :connect

(. view :connect id) -> view

## view :ctx_box

(. view :ctx_box x y width height) -> view

## view :ctx_filled_box

(. view :ctx_filled_box tid col x y width height) -> view

## view :ctx_filled_box

(. view :ctx_filled_box x y width height) -> view

## view :ctx_panel

(. view :ctx_panel col flags depth x y width height) -> view

## view :ctx_set_color

(. view :ctx_set_color col) -> view

## view :dirty

(. view :dirty) -> view

## view :dirty_all

(. view :dirty_all) -> view

## view :emit

(. view :emit) -> view

## view :find_id

(. view :find_id target_id) -> nil | target_view

## view :get_bounds

(. view :get_bounds) -> (x y width height)

## view :get_flags

(. view :get_flags) -> flags

## view :get_id

(. view :get_id) -> id

## view :get_pos

(. view :get_pos) -> (x y)

## view :get_size

(. view :get_size) -> (width height)

## view :hide

(. view :hide)

## view :hide

(. view :hide) -> view

## view :layout

(. view :layout) -> view

## view :lisp_sub

(. view :lisp_sub) -> view

## view :pref_size

(. view :pref_size) -> (width height)

## view :set_flags

(. view :set_flags value mask) -> view

## view :set_size

(. view :set_size width height) -> view

## view :set_size

(. view :set_size x y width height) -> view

## view :sub_opaque

(. view :sub_opaque x y width height) -> view

## view :to_back

(. view :to_back) -> view

## view :to_front

(. view :to_front) -> view

## view :trans_dirty

(. view :trans_dirty rx ry) -> view

## window :add_child

(. window :add_child child) -> window

## window :draw

(. window :draw) -> window

## window :event

(. window :event event) -> window

## window :layout

(. window :layout) -> window

## window :mouse_down

(. window :mouse_down event) -> window

## window :mouse_move

(. window :mouse_move event) -> window

## window :pref_size

(. window :pref_size) -> (width height)

## xmap :copy

(. xmap :copy) -> xmap

## xmap :deep_copy

(. xmap :deep_copy) -> xmap

## xmap :each

(. xmap :each lambda)

## xmap :empty

(. xmap :empty) -> xmap

## xmap :erase

(. xmap :erase key) -> xmap

## xmap :find

(. xmap :find key) -> nil|val

## xmap :insert

(. xmap :insert key val) -> xmap

## xmap :move

(. xmap :move) -> xmap

## xmap :resize

(. xmap :resize num_buckets) -> xmap

## xset :copy

(. xset :copy) -> xset

## xset :deep_copy

(. xset :deep_copy) -> xset

## xset :difference

(. xset :difference xset) -> xset

## xset :each

(. xset :each lambda)

## xset :empty

(. xset :empty) -> xset

## xset :erase

(. xset :erase key) -> xset

## xset :find

(. xset :find key) -> nil|key

## xset :insert

(. xset :insert key) -> xset

## xset :intersect

(. xset :intersect xset) -> xset

## xset :move

(. xset :move) -> xset

## xset :not_intersect

(. xset :not_intersect xset) -> xset

## xset :resize

(. xset :resize num_buckets) -> xset

## xset :union

(. xset :union xset) -> xset

## .->

(.-> this form ...)

## .super

(.super this :method [arg ...])

## /

(/ num num ...)

## /=

(/= num num ...)

## <

(< num num ...)

## <<

(<< num cnt)

## <=

(<= num num ...)

## =

(= num num ...)

## >

(> num num ...)

## >=

(>= num num ...)

## >>

(>> num cnt)

## >>>

(>>> num cnt)

## Backdrop

(Backdrop)-> backdrop

## Button

(Button) -> button

## Canvas

(Canvas width height scale) -> canvas

## Canvas-from-file

(Canvas-from-file file flags) -> nil | canvas

## Flow

(Flow) -> flow

## Grid

(Grid) -> grid

## Label

(Label) -> label

## Progress

(Progress) -> progress

## Scroll

(Scroll flags) -> scroll

## Slider

(Slider) -> slider

## Text

(Text) -> text

## Textfield

(Textfield) -> textfield

## Title

(Title) -> title

## Tree

(Tree event) -> tree

## Vdu

(Vdu) -> vdu

## View

(View) -> view

## Window

(Window) -> window

## aand

(aand [form] ...)

## abi

(abi) -> sym

## abs

(abs num)

## acond

(acond (tst body) ...)

## aeach

(aeach seq body)

## age

(age path) -> 0 | time ns

## aif

(aif form form [form])

## align

(align num pow2) -> num

## and

(and [tst] ...) -> t | nil | tst

## apply

(apply lambda list)

## array

(array [form ...])

## ascii-char

(ascii-char num) -> char

## ascii-code

(ascii-code char) -> num

## ascii-lower

(ascii-lower num) -> num

## ascii-upper

(ascii-upper num) -> num

## asome

(asome seq body)

## awhen

(awhen form body)

## awhile

(awhile form body)

## bind

(bind (param ...) seq)

## byte

(byte field ...)

## canvas-brighter

(canvas-brighter col)

## canvas-darker

(canvas-darker col)

## canvas-fbox

(canvas-fbox canvas x y w h)

## canvas-fill

(canvas-fill canvas argb)

## canvas-fpoly

(canvas-fpoly canvas x y mode list)

## canvas-from-argb32

(canvas-from-argb32 pixel type)

## canvas-info

(canvas-info path)

## canvas-load

(canvas-load path flags)

## canvas-next-frame

(canvas-next-frame canvas)

## canvas-plot

(canvas-plot canvas x y)

## canvas-resize

(canvas-resize canvas canvas)

## canvas-save

(canvas-save canvas path format)

## canvas-swap

(canvas-swap canvas)

## canvas-to-argb32

(canvas-to-argb32 pixel type)

## cap

(cap len array ...)

## case

(case form [(key body)] ...)

## cat

(cat seq ...)

## catch

(catch form eform)

## char

(char num [width])

## char-to-num

(char-to-num char) -> num

## clear

(clear array ...)

## cmp

(cmp str str)

## code

(code str [width index])

## collection?

(collection? obj) -> t | nil

## compose

(compose lambda lambda) -> lambda

## cond

(cond [(tst body)] ...)

## const

(const form)

## copy

(copy form)

## cos

(cos angle)

## cpu

(cpu) -> sym

## create-canvas

(create-canvas width height scale)

## create-font

(create-font name pixels)

## create-stdio

(create-stdio)

## create-vdu

(create-vdu)

## create-view

(create-view)

## ctx-blit

(ctx-blit view tid col x y w h)

## ctx-box

(ctx-box view x y w h)

## ctx-filled-box

(ctx-filled-box view x y w h)

## ctx-panel

(ctx-panel view col flags depth x y w h)

## ctx-set-color

(ctx-set-color view col)

## curry

(curry lambda var ...) -> lambda

## debug

(debug name form)

## debug-format

(debug-format name env)

## debug-fun

(debug-fun name list) -> list

## debug-fun?

(debug-fun? form)

## debug-send

(debug-send string)

## debug-str

(debug-str str) -> str

## dec

(dec num) -> num

## def

(def env var val [var val] ...)

## def?

(def? var [env])

## defabstractmethod

(defabstractmethod (this [arg ...]) body)

## defclass

(defclass name ([arg ...]) (super ...) body)

## deffimethod

(deffimethod name ffi)

## defmethod

(defmethod name (this [arg ...]) body)

## defq

(defq var val [var val] ...)

## defun

(defun name ([arg ...]) body)

## defun-debug

(defun-debug name ([arg ...]) body)

## defun-unbound

(defun-unbound name ([arg ...]) body)

## drop!

(drop! collection key) -> collection

## each

(each lambda seq ...)

## each!

(each! start end lambda (seq ...))

## each-line

(each-line lambda stream)

## each-mergeable

(each-mergeable lambda seq) -> seq

## each-mergeable-rev

(each-mergeable-rev lambda seq) -> seq

## each-rev

(each-rev lambda seq ...)

## elem

(elem index seq)

## elem-set

(elem-set index list val)

## emap

(emap [num_buckets]) -> emap

## emap-kv

(emap-kv [key val ...]) -> emap

## empty

(empty collection) -> collection | nil

## empty?

(empty? form) -> bool

## ends-with

(ends-with str str) -> t | nil

## entries

(entries collection) ->  list | nil

## env

(env [num])

## env?

(env? form) -> t | nil

## eql

(eql form form)

## erase

(erase seq start end) -> seq

## eval

(eval form [env])

## even?

(even? num) -> bool

## every

(every lambda seq ...) -> nil|form

## exec

(exec form)

## f2i

(f2i fixed)

## f2r

(f2r fixed)

## ffi

(ffi sym path flags)

## file-stream

(file-stream path [mode])

## filter

(filter lambda seq) -> list

## find

(find elem seq)

## find-rev

(find-rev elem seq)

## first

(first seq) -> el|nil

## fixeds

(fixeds [form ...])

## floor

(floor num)

## font-glyph-bounds

(font-glyph-bounds font str)

## font-glyph-paths

(font-glyph-paths font str)

## font-glyph-ranges

(font-glyph-ranges font)

## font-sym-texture

(font-sym-texture font sym)

## frac

(frac num)

## func?

(func? form) -> t | nil

## gensym

(gensym)

## get

(get var [env])

## get-byte

(get-byte str index) -> num

## get-cstr

(get-cstr str index) -> str

## get-field

(get-field obj field size|0)

## get-int

(get-int str index) -> num

## get-long

(get-long str index) -> num

## get-short

(get-short str index) -> num

## get-ubyte

(get-ubyte str index) -> num

## get-uint

(get-uint str index) -> num

## get-ushort

(get-ushort str index) -> num

## gets

(gets collection k [if_nil]) -> value | if_nil | nil

## gets-in

(gets-in collection key-path) -> value | nil

## gui-add

(gui-add view)

## gui-add-back

(gui-add-back view)

## gui-info

(gui-info)

## hash

(hash obj)

## i2f

(i2f num)

## i2r

(i2r num)

## identity

(identity any) -> any

## if

(if tst form [else_form])

## import

(import path [env]) -> env

## in-get-state

(in-get-state in) -> num

## in-mbox

(in-mbox in) -> mbox

## in-next-msg

(in-next-msg in)

## in-set-state

(in-set-state in num) -> in

## in-stream

(in-stream)

## inc

(inc num) -> num

## insert

(insert seq pos seq) -> seq

## int

(int field ...)

## intern

(intern list form [lambda]) -> form

## intern-seq

(intern-seq seq [list lambda]) -> list

## into-fn

(into-fn collection) -> fn

## into-map

(into-map map list-of-pairs) -> map

## into-set

(into-set set list-of-elements) -> set

## io-stream

(io-stream io)

## join

(join list seq) -> seq

## kernel-stats

(kernel-stats)

## keys

(keys collection) -> list | nil

## lambda

(lambda ([arg ...]) body)

## lambda?

(lambda? form) -> t | nil

## last

(last seq) -> el|nil

## length

(length seq)

## let

(let ([(var val) ...]) body)

## list

(list [form ...])

## list?

(list? form) -> t | nil

## load

(load path)

## load-path

(load-path)

## load-stream

(load-stream path) -> nil|stream

## log2

(log2 num) -> num

## logand

(logand [num] ...)

## logior

(logior [num] ...)

## lognot

(lognot num) -> num

## logxor

(logxor [num] ...)

## long

(long field ...)

## macro?

(macro? form) -> t | nil

## macroexpand

(macroexpand form)

## mail-alloc-mbox

(mail-alloc-mbox)

## mail-declare

(mail-declare mbox name info)

## mail-enquire

(mail-enquire prefix)

## mail-forget

(mail-forget key)

## mail-free-mbox

(mail-free-mbox mbox)

## mail-nodes

(mail-nodes)

## mail-poll

(mail-poll mboxs)

## mail-read

(mail-read mbox)

## mail-select

(mail-select mboxs)

## mail-send

(mail-send mbox obj)

## mail-timeout

(mail-timeout mbox ns)

## map

(map lambda seq ...) -> list

## map-rev

(map-rev lambda seq ...) -> list

## map?

(map? object) -> t | nil

## match?

(match? list list)

## max

(max num num ...)

## merge-into!

(merge-into! collection (collections)) -> collection

## merge-obj

(merge-obj dlist slist) -> dlist

## merges

(merges (collections)) -> collection

## min

(min num num ...)

## neg

(neg num)

## neg?

(neg? num) -> bool

## nempty?

(nempty? form) -> bool

## net_id

(net_id field ...)

## nil?

(nil? o) -> bool

## nlo

(nlo num) -> num

## nlz

(nlz num) -> num

## not

(not form) -> t | nil

## notany

(notany lambda seq ...) -> t | nil

## notevery

(notevery lambda seq ...) -> t | nil

## nto

(nto num) -> num

## ntz

(ntz num) -> num

## num-to-char

(num-to-char num) -> char

## num-to-utf8

(num-to-utf8 num) -> str

## num?

(num? form) -> t | nil

## nums

(nums [form ...])

## nums-abs

(nums-abs nums [nums])

## nums-add

(nums-add nums nums [nums])

## nums-div

(nums-div nums nums [nums])

## nums-floor

(nums-floor fixeds [fixeds])

## nums-frac

(nums-frac fixeds [fixeds])

## nums-mod

(nums-mod nums nums [nums])

## nums-mul

(nums-mul nums nums [nums])

## nums-scale

(nums-scale nums scale [nums])

## nums-sub

(nums-sub nums nums [nums])

## nums-sum

(nums-sum nums)

## obj-ref

(obj-ref num)

## odd?

(odd? num) -> bool

## offset

(offset field ...)

## open-child

(open-child task mode) -> str

## open-pipe

(open-pipe tasks) -> (list str ...)

## open-remote

(open-remote task node mode) -> str

## open-task

(open-task task node mode key_num reply)

## opt

(opt var val [cond])

## or

(or [tst] ...) -> nil | tst

## out-stream

(out-stream mbox)

## pad

(pad form width [str]) -> str

## pairs-into-kv

(pairs-into-kv list) -> emap

## partition

(partition count seq) -> list of lists

## path

(path [form ...])

## path-filter

(path-filter tol src dst)

## path-gen-arc

(path-gen-arc cx cy start end radius tol dst) -> dst

## path-gen-cubic

(path-gen-cubic p1x p1y p2x p2y p3x p3y p4x p4y tol dst) -> dst

## path-gen-quadratic

(path-gen-quadratic p1x p1y p2x p2y p3x p3y tol dst) -> dst

## path-simplify

(path-simplify tol src dst)

## path-stroke-polygons

(path-stroke-polygons dst radius tol join src) -> dst

## path-stroke-polylines

(path-stroke-polylines dst radius tol join cap1 cap2 src) -> dst

## path-transform

(path-transform m1x m1y m2x m2y trx try src dst)

## penv

(penv [env])

## pii-dirlist

(pii-dirlist path)

## pii-fstat

(pii-fstat path)

## pii-read-char

(pii-read-char fd)

## pii-remove

(pii-remove path)

## pii-time

(pii-time)

## pii-write-char

(pii-write-char fd char)

## pivot

(pivot lambda list start end)

## pop

(pop array)

## pos?

(pos? num) -> bool

## pow

(pow base exponent) -> integer

## prebind

(prebind form)

## prin

(prin [form ...])

## print

(print [form ...])

## profile-print

(profile-print name [stream]) -> stdout|stream

## profile-report

(profile-report name [reset])

## progn

(progn [form ...])

## ptr

(ptr field ...)

## push

(push array form ...)

## quasi-quote

(quasi-quote form)

## quote

(quote form)

## r2f

(r2f real)

## r2i

(r2i real)

## random

(random num)

## range

(range start end [step]) -> list

## rcurry

(rcurry lambda var ...) -> lambda

## read

(read stream last_char)

## read-avail

(read-avail stream)

## read-char

(read-char stream [width])

## read-int

(read-int stream) -> num

## read-line

(read-line stream)

## read-long

(read-long stream) -> num

## read-short

(read-short stream) -> num

## reals

(reals [form ...])

## recip

(recip num)

## reduce

(reduce lambda seq [accum]) -> form

## reduce-rev

(reduce-rev lambda seq [accum]) -> form

## reduced

(reduced accum)

## reduced-reduce

(reduced-reduce lambda seq [accum]) -> form

## reduced-reduce-rev

(reduced-reduce-rev lambda seq [accum]) -> form

## repl

(repl stream path)

## rest

(rest seq) -> nil|seq

## reverse

(reverse list) -> list

## save

(save str path)

## second

(second seq) -> el|nil

## seq?

(seq? form) -> t | nil

## set

(set env var val [var val] ...)

## set-field

(set-field obj field size|0 val)

## setd

(setd var val [var val] ...)

## setq

(setq var val [var val] ...)

## sets!

(sets! collection key value) -> collection

## sets-pairs!

(sets-pairs! collection))

## short

(short field ...)

## shuffle

(shuffle list [start end]) -> list

## shuffled

(shuffled list [start end]) -> list

## sign

(sign num)

## sin

(sin angle)

## slice

(slice start end seq)

## some

(some lambda seq ...) -> nil|form

## some!

(some! start end mode lambda (seq ...))

## sort

(sort fcmp list [start end]) -> list

## sorted

(sorted fcmp list [start end]) -> list

## split

(split str chars)

## sqrt

(sqrt num)

## starts-with

(starts-with str str) -> t | nil

## stdio-get-args

(stdio-get-args stdio) -> list

## str

(str [form ...])

## str-to-num

(str-to-num str) -> num

## str?

(str? form) -> t | nil

## stream-avail

(stream-avail stream)

## stream-flush

(stream-flush stream)

## stream-seek

(stream-seek stream offset pos)

## string-stream

(string-stream str)

## struct

(struct size field ...)

## structure

(structure name offset [body])

## swap

(swap list index index)

## sym

(sym str)

## sym?

(sym? form) -> t | nil

## task-mailbox

(task-mailbox)

## task-sleep

(task-sleep usec)

## texture-metrics

(texture-metrics texture) -> (handle width height)

## throw

(throw str form)

## times

(times num body)

## to-lower

(to-lower str) -> str

## to-net-id

(to-net-id service_id) -> net_id

## to-service-id

(to-service-id net_id) -> service_id

## to-upper

(to-upper str) -> str

## tolist

(tolist env)

## trim

(trim str [str]) -> str

## trim-end

(trim-end str [str]) -> str

## trim-start

(trim-start str [str]) -> str

## type-of

(type-of obj)

## type-to-size

(type-to-size sym) -> num

## ui-backdrop

(ui-backdrop name [props] [body]) -> backdrop

## ui-button

(ui-button name [props] [body]) -> button

## ui-buttons

(ui-buttons symbols events [props] [group])

## ui-canvas

(ui-canvas name width height scale) -> canvas

## ui-element

(ui-element name func [props] [body]) -> view

## ui-flow

(ui-flow name [props] [body]) -> flow

## ui-grid

(ui-grid name [props] [body]) -> grid

## ui-label

(ui-label name [props] [body]) -> label

## ui-merge-props

(ui-merge-props props) -> props

## ui-progress

(ui-progress name [props]) -> progress

## ui-props

(ui-props props [props]) -> props

## ui-root

(ui-root name func [props] [body]) -> view

## ui-scroll

(ui-scroll name [props] [body]) -> scroll

## ui-slider

(ui-slider name [props]) -> slider

## ui-textfield

(ui-textfield name [props]) -> textfield

## ui-title

(ui-title name [props]) -> title

## ui-title-bar

(ui-title-bar name title symbols events [props]) -> flow

## ui-tool-bar

(ui-tool-bar name [props] [body]) -> flow

## ui-tree

(ui-tree name event [props]) -> tree

## ui-vdu

(ui-vdu name [props]) -> vdu

## ui-view

(ui-view name [props] [body]) -> view

## ui-window

(ui-window name [props] [body]) -> window

## undef

(undef env var [var] ...)

## unless

(unless tst body)

## until

(until tst body)

## unzip

(unzip seq buckets) -> buckets

## values

(values collection) -> list | nil

## vdu-configure

(vdu-configure vdu)

## vdu-load

(vdu-load vdu lines ox oy cx cy)

## view-add-back

(view-add-back parent child)

## view-add-dirty

(view-add-dirty view x y w h)

## view-add-front

(view-add-front parent child)

## view-add-opaque

(view-add-opaque view x y w h)

## view-children

(view-children view)

## view-clr-opaque

(view-clr-opaque view)

## view-emit

(view-emit view)

## view-find-id

(view-find-id view id)

## view-fit

(view-fit x y w h) -> (x y w h)

## view-locate

(view-locate w h [flag]) -> (x y w h)

## view-set-flags

(view-set-flags view flags mask)

## view-sub

(view-sub view)

## view-sub-opaque

(view-sub-opaque view x y w h)

## view-to-back

(view-to-back view)

## view-to-front

(view-to-front view)

## view-trans-dirty

(view-trans-dirty view rx ry)

## weak-ref

(weak-ref obj)

## when

(when tst body)

## while

(while tst body)

## within-compile-env

(within-compile-env lambda)

## write

(write stream str)

## write-char

(write-char stream list|num [width])

## write-int

(write-int stream num|list) -> stream

## write-line

(write-line stream str) -> stream

## write-long

(write-long stream num|list) -> stream

## write-short

(write-short stream num|list) -> stream

## xmap

(xmap [num_buckets cmp_fnc hash_fnc]) -> xmap

## xmap-kv

(xmap-kv [key val ...]) -> xmap

## xset

(xset [num_buckets cmp_fnc hash_fnc]) -> xset

## xset-k

(xset-k [key ...]) -> xset

## zip

(zip seq ...) -> list

