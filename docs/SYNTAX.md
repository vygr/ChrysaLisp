# Syntax

## aand

(aand [form] ...)

## abi

(abi) -> sym

## acond

(acond (tst body) ...)

## aif

(aif form form [form])

## align

(align num pow2) -> num

## and

(and [tst] ...) -> t|nil|tst

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

## byte

(byte field ...)

## canvas-set-color

(canvas-set-color canvas argb) -> canvas

## canvas-set-flags

(canvas-set-flags canvas flags) -> canvas

## case

(case tst [(key body)] ...)

## char-to-num

(char-to-num char) -> num

## component-get-id

(component-get-id component) -> id

## compose

(compose lambda lambda) -> lambda

## const

(const form)

## cpu

(cpu) -> sym

## curry

(curry lambda var ...) -> lambda

## debug

(debug name form)

## debug-format

(debug-format name env)

## debug-fun

(debug-fun name list) -> list

## dec

(dec num) -> num

## def:

(def: syms [env])

## defmacro

(defmacro name ([arg ...]) body)

## defmacro-bind

(defmacro-bind name ([arg ...]) body)

## defun

(defun name ([arg ...]) body)

## defun-bind

(defun-bind name ([arg ...]) body)

## defun-debug

(defun-debug name ([arg ...]) body)

## each

(each lambda seq ...)

## each-line

(each-line lambda stream)

## each-mergeable

(each-mergeable lambda seq) -> seq

## each-mergeable-rev

(each-mergeable-rev lambda seq) -> seq

## each-rev

(each-rev lambda seq ...)

## ends-with

(ends-with str str) -> t|nil

## erase

(erase seq start end) -> seq

## every

(every lambda seq ...) -> nil|form

## exec

(exec form)

## filter

(filter lambda seq) -> list

## first

(first seq) -> el

## fixeds

(fixeds [form ...])

## fnc?

(fnc? form) -> bool

## get

(get env form) -> val

## get-byte

(get-byte str index) -> num

## get-cstr

(get-cstr str index) -> str

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

## if

(if tst form [eform])

## import

(import path)

## in-get-state

(in-get-state in) -> num

## in-mbox

(in-mbox in) -> mbox

## in-set-state

(in-set-state in num) -> in

## inc

(inc num) -> num

## insert

(insert seq pos seq) -> seq

## int

(int field ...)

## is-debug-fun

(is-debug-fun form)

## join

(join list seq) -> seq

## last

(last seq) -> el

## let

(let ([(var val) ...]) body)

## list

(list [form ...])

## log2

(log2 num) -> num

## lognot

(lognot num) -> num

## long

(long field ...)

## lst?

(lst? form) -> bool

## map

(map lambda seq ...) -> list

## map-rev

(map-rev lambda seq ...) -> list

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

(num? form) -> bool

## nums

(nums [form ...])

## offset

(offset field ...)

## opt

(opt var val [cond])

## or

(or [tst] ...) -> nil|tst

## pad

(pad form width [str]) -> str

## path

(path [form ...])

## prin

(prin [form ...])

## print

(print [form ...])

## progn

(progn [form ...])

## ptr

(ptr field ...)

## range

(range start end [step]) -> list

## rcurry

(rcurry lambda var ...) -> lambda

## read-int

(read-int stream) -> num

## read-long

(read-long stream) -> num

## read-short

(read-short stream) -> num

## reals

(reals [form ...])

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

## rest

(rest seq) -> seq

## second

(second seq) -> el

## setd

(setd var val [var val] ...)

## short

(short field ...)

## shuffle

(shuffle list [start end]) -> list

## shuffled

(shuffled list [start end]) -> list

## some

(some lambda seq ...) -> nil|form

## sort

(sort list [start end]) -> list

## sorted

(sorted list [start end]) -> list

## starts-with

(starts-with str str) -> t|nil

## stdio-get-args

(stdio-get-args stdio) -> list

## str

(str [form ...])

## str-to-num

(str-to-num str) -> num

## str?

(str? form) -> bool

## struct

(struct size field ...)

## structure

(structure name offset [body])

## swap

(swap list index index)

## sym?

(sym? form) -> bool

## times

(times num body)

## to-lower

(to-lower str) -> str

## to-upper

(to-upper str) -> str

## trim

(trim str [str]) -> str

## trim-end

(trim-end str [str]) -> str

## trim-start

(trim-start str [str]) -> str

## tuple-get

(tuple-get index list) -> value

## tuple-set

(tuple-set index list value)

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

## unless

(unless tst body)

## until

(until tst body)

## unzip

(unzip seq buckets) -> buckets

## vdu-char-size

(vdu-char-size vdu) -> (w h)

## vdu-max-size

(vdu-max-size vdu) -> (w h)

## view-dirty-all

(view-dirty-all view) -> view

## view-get-bounds

(view-get-bounds view) -> (x y w h)

## view-get-pos

(view-get-pos view) -> (x y)

## view-get-size

(view-get-size view) -> (w h)

## view-set-bounds

(view-set-bounds view x y w h) -> view

## view-set-pos

(view-set-pos view x y) -> view

## view-set-size

(view-set-size view w h) -> view

## when

(when tst body)

## within-compile-env

(within-compile-env lambda)

## write-int

(write-int stream num|list) -> stream

## write-line

(write-line stream str) -> stream

## write-long

(write-long stream num|list) -> stream

## write-short

(write-short stream num|list) -> stream

