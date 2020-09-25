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

## aand

(aand [form] ...)

## abi

(abi) -> sym

## abs

(abs num)

## acond

(acond (tst body) ...)

## age

(age path)

## aif

(aif form form [form])

## align

(align num pow2) -> num

## and

(and [tst] ...) -> t|nil|tst

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

## awhen

(awhen form body)

## awhile

(awhile form body)

## bind

(bind (param ...) seq)

## byte

(byte field ...)

## canvas-fbox

(canvas-fbox canvas x y w h)

## canvas-fill

(canvas-fill canvas argb)

## canvas-fpoly

(canvas-fpoly canvas x y mode list)

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

## canvas-set-color

(canvas-set-color canvas argb) -> canvas

## canvas-set-flags

(canvas-set-flags canvas flags) -> canvas

## canvas-swap

(canvas-swap canvas)

## cap

(cap len array ...)

## case

(case tst [(key body)] ...)

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

## component-connect

(component-connect component id)

## component-get-id

(component-get-id component) -> id

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

## create-backdrop

(create-backdrop)

## create-button

(create-button)

## create-canvas

(create-canvas width height scale)

## create-flow

(create-flow)

## create-font

(create-font name pixels)

## create-grid

(create-grid)

## create-label

(create-label)

## create-progress

(create-progress)

## create-scroll

(create-scroll flags)

## create-slider

(create-slider)

## create-stdio

(create-stdio)

## create-textfield

(create-textfield)

## create-title

(create-title)

## create-vdu

(create-vdu)

## create-view

(create-view)

## create-window

(create-window)

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

## dec

(dec num) -> num

## def

(def env var val [var val] ...)

## def:

(def: syms [env])

## defmacro

(defmacro name ([arg ...]) body)

## defmacro-bind

(defmacro-bind name ([arg ...]) body)

## defq

(defq var val [var val] ...)

## defun

(defun name ([arg ...]) body)

## defun-bind

(defun-bind name ([arg ...]) body)

## defun-debug

(defun-debug name ([arg ...]) body)

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

## empty?

(empty? form) -> bool

## ends-with

(ends-with str str) -> t|nil

## env

(env [num])

## env?

(env? form) -> t|nil

## eql

(eql form form)

## erase

(erase seq start end) -> seq

## eval

(eval form [env])

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

(first seq) -> el | nil

## fixeds

(fixeds [form ...])

## floor

(floor num)

## fnc?

(fnc? form) -> t|nil

## font-glyph-paths

(font-glyph-paths font str)

## font-glyph-ranges

(font-glyph-ranges font)

## frac

(frac num)

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

## if

(if tst form [eform])

## import

(import path [env])

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

## io-stream

(io-stream io)

## join

(join list seq) -> seq

## lambda

(lambda ([arg ...]) body)

## last

(last seq) -> el | nil

## length

(length seq)

## let

(let ([(var val) ...]) body)

## list

(list [form ...])

## load

(load path)

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

## lst?

(lst? form) -> t|nil

## macroexpand

(macroexpand form)

## mail-alloc-mbox

(mail-alloc-mbox)

## mail-declare

(mail-declare name mbox)

## mail-devices

(mail-devices)

## mail-enquire

(mail-enquire prefix)

## mail-forget

(mail-forget name mbox)

## mail-free-mbox

(mail-free-mbox id)

## mail-poll

(mail-poll mboxs)

## mail-read

(mail-read mbox)

## mail-select

(mail-select mboxs)

## mail-send

(mail-send obj mbox)

## map

(map lambda seq ...) -> list

## map-rev

(map-rev lambda seq ...) -> list

## match?

(match? list list)

## max

(max num num ...)

## mem-stats

(mem-stats options)

## merge-obj

(merge-obj dlist slist) -> dlist

## min

(min num num ...)

## neg

(neg num)

## nempty?

(nempty? form) -> bool

## nlo

(nlo num) -> num

## nlz

(nlz num) -> num

## not

(not form) -> t|nil

## notany

(notany lambda seq ...) -> t|nil

## notevery

(notevery lambda seq ...) -> t|nil

## nto

(nto num) -> num

## ntz

(ntz num) -> num

## num-to-char

(num-to-char num) -> char

## num-to-utf8

(num-to-utf8 num) -> str

## num?

(num? form) -> t|nil

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

## offset

(offset field ...)

## open-child

(open-child task mode)

## open-farm

(open-farm task num mode [devices])

## open-pipe

(open-pipe tasks)

## open-remote

(open-remote task node mode)

## opt

(opt var val [cond])

## or

(or [tst] ...) -> nil|tst

## out-stream

(out-stream mbox)

## pad

(pad form width [str]) -> str

## partition

(partition lambda list start end)

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

(penv env)

## pii-dirlist

(pii-dirlist path)

## pii-read-char

(pii-read-char fd)

## pii-remove

(pii-remove path)

## pii-write-char

(pii-write-char fd char)

## pop

(pop array)

## prebind

(prebind form) -> form

## prin

(prin [form ...])

## print

(print [form ...])

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

(rest seq) -> seq

## reverse

(reverse list) -> list

## save

(save str path)

## second

(second seq) -> el | nil

## seq?

(seq? form) -> t|nil

## set

(set env var val [var val] ...)

## set-field

(set-field obj field size|0 val)

## setd

(setd var val [var val] ...)

## setq

(setq var val [var val] ...)

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

(sort list [start end]) -> list

## sorted

(sorted list [start end]) -> list

## split

(split str chars)

## sqrt

(sqrt num)

## starts-with

(starts-with str str) -> t|nil

## stdio-get-args

(stdio-get-args stdio) -> list

## str

(str [form ...])

## str-to-num

(str-to-num str) -> num

## str?

(str? form) -> t|nil

## stream-avail

(stream-avail stream)

## stream-flush

(stream-flush stream)

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

(sym? form) -> t|nil

## task-mailbox

(task-mailbox)

## task-sleep

(task-sleep usec)

## throw

(throw str form)

## time

(time)

## times

(times num body)

## to-lower

(to-lower str) -> str

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

(ui-tree name func [props] [body]) -> view

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

## vdu-char-size

(vdu-char-size vdu) -> (w h)

## vdu-load

(vdu-load vdu lines ox oy cx cy)

## vdu-max-size

(vdu-max-size vdu) -> (w h)

## view-add-back

(view-add-back parent child)

## view-add-child

(view-add-child parent child)

## view-add-dirty

(view-add-dirty view x y w h)

## view-add-front

(view-add-front parent child)

## view-add-opaque

(view-add-opaque view x y w h)

## view-change

(view-change view x y w h)

## view-change-dirty

(view-change-dirty view x y w h)

## view-dirty

(view-dirty view)

## view-dirty-all

(view-dirty-all view) -> view

## view-event

(view-event view str)

## view-find-id

(view-find-id view id)

## view-fit

(view-fit x y w h) -> (x y w h)

## view-get-bounds

(view-get-bounds view) -> (x y w h)

## view-get-pos

(view-get-pos view) -> (x y)

## view-get-size

(view-get-size view) -> (w h)

## view-hide

(view-hide view)

## view-layout

(view-layout view)

## view-locate

(view-locate w h [flag]) -> (x y w h)

## view-pref-size

(view-pref-size view)

## view-set-bounds

(view-set-bounds view x y w h) -> view

## view-set-flags

(view-set-flags view flags mask)

## view-set-pos

(view-set-pos view x y) -> view

## view-set-size

(view-set-size view w h) -> view

## view-sub

(view-sub view)

## view-sub-opaque

(view-sub-opaque view x y w h)

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

## zip

(zip seq ...) -> list

