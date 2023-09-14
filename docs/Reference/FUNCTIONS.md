# Functions

### #

```code
(# (< %9 %0 %3) ...) -> (lambda (%0 %3 %9) (< %9 %0 %3) ...)
```code

### ,n

### ,n

### .->

```code
(.-> this form ...)
```code

### .?

```code
(.? this method) -> :nil | lambda
```code

### .super

```code
(.super this :method [arg ...])
```code

### Canvas-from-file

```code
(Canvas-from-file file flags) -> :nil | canvas
```code

### Emap-kv

```code
(Emap-kv [key val ...]) -> emap
```code

### Fmap-kv

```code
(Fmap-kv [key val ...]) -> fmap
```code

### Fset-k

```code
(Fset-k [key ...]) -> fset
```code

### Gridcell

### Lmap-kv

```code
(Lmap-kv [key val ...]) -> lmap
```code

### Mat3x2-f

### Mat3x2-rotz-f

### Mat3x2-skewx-f

### Mat3x2-skewy-f

### Mat3x3-rotx

### Mat3x3-roty

### Mat3x3-rotz

### Mat3x3-scale

### Mat3x3-unity

### Mat4x4-frustum

### Mat4x4-rotx

### Mat4x4-roty

### Mat4x4-rotz

### Mat4x4-scale

### Mat4x4-translate

### Mat4x4-unity

### Normal

### SVG-Canvas

```code
(SVG-Canvas stream [scale]) -> :nil | canvas
```code

### SVG-info

```code
(SVG-info stream) -> (width height type) | (-1 -1 -1)
```code

### Tri

### Vec2-f

### Vec3-f

### Vec3-r

### Vec4-f

### Vec4-r

### XML-parse

```code
(XML-parse stream fnc_in fnc_out fnc_text)

calls back to user code, so _ used for vars that would be in scope
break the stream into svg tokens, symbols, strings etc
parse the commands and attributes calling back to the user functions
```code

### Xmap-kv

```code
(Xmap-kv [key val ...]) -> xmap
```code

### Xset-k

```code
(Xset-k [key ...]) -> xset
```code

### _structure

### aand

```code
(aand [form] ...)
```code

### abi

```code
(abi) -> sym
```code

### abs-path

```code
(abs-path path [current]) -> path
```code

### acond

```code
(acond (tst body) ...)
```code

### aeach

```code
(aeach seq body)
```code

### age

```code
(age path) -> 0 | time ns
```code

### aif

```code
(aif form form [form])
```code

### align

```code
(align num div) -> num
```code

### all-dirs

```code
(all-dirs paths) -> paths

return all the dir paths
```code

### all-files

```code
(all-files &optional root exts n) -> paths

all source files from root downwards, none recursive
```code

### alloc-select

```code
(alloc-select size) -> ((task-netid) [temp_mbox] ...)
```code

### and

```code
(and [tst] ...) -> :t | :nil | tst
```code

### array?

```code
(array? form) -> :t | :nil
```code

### ascii-char

```code
(ascii-char num) -> char
```code

### ascii-code

```code
(ascii-code char) -> num
```code

### ascii-lower

```code
(ascii-lower num) -> num
```code

### ascii-upper

```code
(ascii-upper num) -> num
```code

### asome

```code
(asome seq body)
```code

### awhen

```code
(awhen form body)
```code

### awhile

```code
(awhile form body)
```code

### bits

```code
(bits name base [(bit field ...)] ...)
```code

### blank-line?

### boot-image

### bounding-box

```code
(bounding-box verts vec3-extract-fnc) -> (min_v3 max_v3)
```code

### bounding-sphere

```code
(bounding-sphere verts vec3-extract-fnc) -> (center_v3 radius)
```code

### byte-to-hex-str

```code
(byte-to-hex-str num) -> str
```code

### cache

### canvas-brighter

```code
(canvas-brighter col) -> col
```code

### canvas-darker

```code
(canvas-darker col) -> col
```code

### canvas-flush

```code
(canvas-flush)

flush any shared pixmaps that have no users.
4 refs are held by the cache Lmap and this loop !
```code

### case

```code
(case form [(key|(key ...) body)] ...)
```code

### char-to-num

```code
(char-to-num char) -> num
```code

### check-date

### circle

```code
cached circle generation, quantised to 1/4 pixel
```code

### compile

### compose

```code
(compose lambda lambda) -> lambda
```code

### const

```code
(const form)
```code

### cpp-node?

```code
(cpp_node? node) -> :t | :nil
```code

### cpu

```code
(cpu) -> sym
```code

### curry

```code
(curry lambda var ...) -> lambda
```code

### date

### day-of-the-week

### days-in-month

### days-in-year

### debug

```code
(debug name form)
```code

### debug-format

```code
(debug-format name env)
```code

### debug-fun

```code
(debug-fun name list) -> list
```code

### debug-fun?

```code
(debug-fun? form)
```code

### debug-send

```code
(debug-send form ...)
```code

### dec

```code
(dec num) -> num
```code

### decode-date

### def_real

### defabstractmethod

```code
(defabstractmethod ([arg ...]) body)

Declare a method as abstract and will
throw an error if invoked. Concrete
classes deriving from a class with abstractions
should provide the concreate handler
```code

### defclass

```code
(defclass Name ([arg ...]) (super ...) body)
```code

### defcvar

### deffimethod

```code
(deffimethod name ffi)
```code

### defgetmethod

```code
(defgetmethod field)
```code

### defmethod

```code
(defmethod name ([arg ...]) body)

(. this :method [arg ...])
```code

### defmethod

### defmethod

### defmethod_

```code
(defmethod_ name ([arg ...]) body)

(. _this :method [arg ...])
```code

### defsetmethod

```code
(defsetmethod field)
```code

### defun

### defun

```code
(defun name ([arg ...]) body)

(list [form ...])
(progn [form ...])
(prin [form ...])
(print [form ...])
(str [form ...])
```code

### defun

### defun

### deg-to-rad

### each

```code
(each lambda seq ...)
```code

### each-found

```code
(each-found lambda text substr)
```code

### each-line

```code
(each-line lambda stream)
```code

### each-match

```code
(each-match lambda text regexp)
```code

### each-mergeable

```code
(each-mergeable lambda seq) -> seq
```code

### each-mergeable-rev

```code
(each-mergeable-rev lambda seq) -> seq
```code

### each-rev

```code
(each-rev lambda seq ...)
```code

### elem-find

### empty?

```code
(empty? form) -> :t | :nil
```code

### encode-date

### ends-with

```code
(ends-with str str) -> :t | :nil
```code

### enums

```code
(enums name base [(enum field ...)] ...)
```code

### env?

```code
(env? form) -> :t | :nil
```code

### erase

```code
(erase seq start end) -> seq
```code

### erase-line

### even?

```code
(even? num) -> :t | :nil
```code

### every

```code
(every lambda seq ...) -> :nil | form
```code

### exec

```code
(exec form)
```code

### export

```code
(export env sym ...)
```code

### export-classes

```code
(export-classes class ...)
```code

### export-symbols

```code
(export-symbols sym ...)
```code

### filter

```code
(filter lambda seq) -> list
```code

### first

```code
(first seq) -> el | :nil
```code

### firstelem

### float-time

### framed-lambda

### free-select

```code
(free-select select)
```code

### func-load

```code
(func-load name) -> (body links refs)

cache loading of function blobs etc
```code

### func-obj-path

### func-refs

```code
(func-refs fobj) -> ([sym] ...)
```code

### func?

```code
(func? form) -> :t | :nil
```code

### gather

```code
(gather map key ...) -> (val ...)

gather a list of values
```code

### gen-norms

```code
(gen-norms verts tris) -> (norms new_tris)
```code

### get-by-val

### get-byte

```code
(get-byte str index) -> num
```code

### get-cstr

```code
(get-cstr str index) -> str
```code

### get-date

### get-int

```code
(get-int str index) -> num
```code

### get-long

```code
(get-long str index) -> num
```code

### get-netid

```code
(get-netid str index) -> netid
```code

### get-nodeid

```code
(get-nodeid str index) -> nodeid
```code

### get-selection

### get-short

```code
(get-short str index) -> num
```code

### get-ubyte

```code
(get-ubyte str index) -> num
```code

### get-uint

```code
(get-uint str index) -> num
```code

### get-ushort

```code
(get-ushort str index) -> num
```code

### get-year

### get-yeardays

### getf

```code
(getf obj field [offset]) -> value
```code

### gui-add-back

### gui-quit

### gui-rpc

### gui-sub

### identity

```code
(identity any) -> any

function that returns it's argument
```code

### import

```code
(import path [env]) -> env
```code

### in-get-state

```code
(in-get-state in) -> num
```code

### in-mbox

```code
(in-mbox in) -> mbox
```code

### in-set-state

```code
(in-set-state in num) -> in
```code

### inc

```code
(inc num) -> num
```code

### include

### insert

```code
(insert seq pos seq) -> seq
```code

### insert-line

### insertelem

### int-to-hex-str

```code
(int-to-hex-str num) -> str
```code

### intern

### is-allowed-number-char?

### is-bracket-char?

### is-esc-char?

### is-number-char?

### is-symbol-char?

### iso-surface

```code
(iso-surface grid isolevel) -> tris

determine the index into the edge table which
tells us which vertices are inside the surface
```code

### jit

### join

```code
(join list seq) -> seq
```code

### lambda-func?

```code
(lambda-func? form) -> :t | :nil
```code

### lambda?

```code
(lambda? form) -> :t | :nil
```code

### last

```code
(last seq) -> el | :nil
```code

### last-line

### leapyear?

### leapyear?

### leapyears-since-epoch

### let

```code
(let ([(var val) ...]) body)
```code

### let*

```code
(let* ([(var val) ...]) body)
```code

### lighting

```code
very basic attenuation and diffuse
```code

### lighting-at3

```code
very basic attenuation, diffuse and specular
```code

### lisp-node?

```code
(lisp_node? node) -> :t | :nil
```code

### lisp-nodes

```code
(lisp-nodes) -> nodes
```code

### list

### list?

```code
(list? form) -> :t | :nil
```code

### lists

```code
(lists n) -> ((list0) ... (listn-1))
```code

### lists2

```code
(lists2) -> ((list) (list))
```code

### load-stream

```code
(load-stream path) -> :nil | stream
```code

### log2

```code
(log2 num) -> num
```code

### lognot

```code
(lognot num) -> num
```code

### long-to-hex-str

```code
(long-to-hex-str num) -> str
```code

### lower

```code
(lower field | (field val) ...) -> (set this field var ...)
```code

### macro-func?

```code
(macro-func? form) -> :t | :nil
```code

### macro?

```code
(macro? form) -> :t | :nil
```code

### map

```code
(map lambda seq ...) -> list
```code

### map-rev

```code
(map-rev lambda seq ...) -> list
```code

### map?

```code
(map? object) -> :t | :nil

returns true if argument is a map type
```code

### mat3x2-mul-f

```code
(mat3x2-mul-f mat3x2_a mat3x2_b) -> mat3x2-f
```code

### mat3x3-mul

```code
(mat3x3-mul mat3x3_a mat3x3_b) -> mat3x3
```code

### mat3x3-vec3-mul

### mat4x4-invert

```code
(mat4x4-invert mat4x4) -> mat4x4
```code

### mat4x4-mul

```code
(mat4x4-mul mat4x4_a mat4x4_b) -> mat4x4
```code

### mat4x4-vec3-mul

### mat4x4-vec4-mul

### match?

```code
(match? text regexp) -> :t | :nil
```code

### matches

```code
(matches text regexp) -> (([(i0 i1)] ...) ([([(s0 s1)] ...)]...))
```code

### max-length

```code
(max-length list) -> max
```code

### month-of-the-year

### mutate-buffer

### neg?

```code
(neg? num) -> :t | :nil
```code

### nempty?

```code
(nempty? form) -> :t | :nil
```code

### new_bit

### new_enum

### new_field

### nil?

```code
(nil? o) -> :t | :nil
```code

### nlo

```code
(nlo num) -> num
```code

### nlz

```code
(nlz num) -> num
```code

### not

```code
(not form) -> :t | :nil
```code

### not-whole-word-char?

### notany

```code
(notany lambda seq ...) -> :t | :nil
```code

### notevery

```code
(notevery lambda seq ...) -> :t | :nil
```code

### nto

```code
(nto num) -> num
```code

### ntz

```code
(ntz num) -> num
```code

### num-to-char

```code
(num-to-char num) -> char
```code

### num-to-utf8

```code
(num-to-utf8 num) -> str
```code

### num?

```code
(num? form) -> :t | :nil
```code

### odd?

```code
(odd? num) -> :t | :nil
```code

### open-child

```code
(open-child task mode) -> str
```code

### open-pipe

```code
(open-pipe tasks) -> (str ...)
```code

### open-remote

```code
(open-remote task node mode) -> str
```code

### open-task

```code
(open-task task node mode key_num reply)
```code

### opt

```code
(opt var val [cond])
```code

### opt-mesh

```code
(opt-mesh verts norms tris) -> (new_verts new_norms new_tris)
```code

### opt-vectors

```code
(opt-vectors vectors) -> (new_vectors new_indexs)
```code

### options

```code
(options stdio optlist) -> :nil | args

scan the stdio args and process acording to the optlist
```code

### options-find

```code
(options-find optlist arg) -> :nil | opt_entry
```code

### options-print

```code
(options-print &rest _)
```code

### options-split

```code
(options-split args) -> (a0 [a1] ...)
```code

### or

```code
(or [tst] ...) -> :nil | tst
```code

### os

```code
(os) -> sym
```code

### output

### pad

```code
(pad form width [str]) -> str
```code

### partition

```code
(partition count seq) -> list of lists
```code

### path-fill-and-stroke

### path-gen-ellipse

```code
(path-gen-ellipse cx cy rx ry tol dst) -> dst
```code

### path-gen-paths

```code
(path-gen-paths svg_d tol) -> ((:nil|:t path) ...)

:t closed, :nil open
```code

### path-gen-rect

```code
(path-gen-rect x y x1 y1 rx ry tol dst) -> dst
```code

### path-stroke-polygons

```code
(path-stroke-polygons dst radius tol join src) -> dst
```code

### path-stroke-polylines

```code
(path-stroke-polylines dst radius tol join cap1 cap2 src) -> dst
```code

### pipe-run

```code
(pipe-run cmdline &optional outfun)
```code

### pipe-split

```code
(pipe-split cmdline) -> (e0 [e1] ...)
```code

### pixmap-cpm-info

```code
(pixmap-cpm-info stream) -> (width height type) | (-1 -1 -1)
```code

### pixmap-info

```code
(pixmap-info file) -> (width height type) | (-1 -1 -1)
```code

### pixmap-load

```code
(pixmap-load file) -> :nil | pixmap
```code

### pixmap-save

```code
(pixmap-save pixmap file type) -> :nil | pixmap
```code

### pixmap-tga-info

```code
(pixmap-tga-info stream) -> (width height type) | (-1 -1 -1)
```code

### pos?

```code
(pos? num) -> :t | :nil
```code

### postfix

### pow

```code
(pow base exponent) -> integer
```code

### print-mat

### print-vec

### profile-lambda

### profile-print

```code
(profile-print name [stream]) -> stdout | stream
```code

### profile-report

```code
(profile-report name [reset])
```code

### push-attributes

### quasi-quote?

```code
(quasi-quote? form) -> :t | :nil
```code

### query

```code
(query pattern whole_words regexp) -> (engine pattern meta)
```code

### quote?

```code
(quote? form) -> :t | :nil
```code

### raise

```code
(raise field | (var val) ...) -> (defq var (get field this) ...)
```code

### range

```code
(range start end [step]) -> list
```code

### rcurry

```code
(rcurry lambda var ...) -> lambda
```code

### read-attribute

### read-col

### read-data

```code
(read-data stream bytes) -> str
```code

### read-int

```code
(read-int stream) -> num
```code

### read-long

```code
(read-long stream) -> num
```code

### read-rgb

### read-short

```code
(read-short stream) -> num
```code

### read-transform

### redo

### reduce

```code
(reduce lambda seq [accum]) -> form
```code

### reduce-rev

```code
(reduce-rev lambda seq [accum]) -> form
```code

### render-object-tris

### render-object-verts

### replace

```code
(replace seq start end seq) -> seq
```code

### rest

```code
(rest seq) -> :nil | seq
```code

### restart

```code
restart a child
```code

### restart

```code
restart a child
```code

### reverse

```code
(reverse list) -> list
```code

### s2i

### search

```code
(search text cpat start) -> -1 | end

for each char in text
```code

### second

```code
(second seq) -> el | :nil
```code

### select-buffer

### select-lines

### select-paragraph

### select-word

### selection?

### seq?

```code
(seq? form) -> :t | :nil
```code

### set-line

### set?

```code
(set? object) -> :t | :nil

returns true if argument is a set type
```code

### setd

```code
(setd var val [var val] ...)
```code

### setf

```code
(setf obj field value [offset]) -> obj
```code

### setf->

```code
(setf-> msg form ...)
```code

### setoffset

```code
adjust text offset
```code

### short-to-hex-str

```code
(short-to-hex-str num) -> str
```code

### shuffle

```code
(shuffle list [start end]) -> list
```code

### shuffled

```code
(shuffled list [start end]) -> list
```code

### slot

### slot

### slot

### slot

### some

```code
(some lambda seq ...) -> :nil | form
```code

### some-rev

```code
(some-rev lambda seq ...) -> :nil | form
```code

### sort

```code
(sort fcmp list [start end]) -> list
```code

### sort-selection

### sorted

```code
(sorted fcmp list [start end]) -> list
```code

### start

```code
start a child
```code

### start

```code
start a child
```code

### starts-with

```code
(starts-with str str) -> :t | :nil
```code

### static-q

```code
(static-q form) -> 'form

static quoted
```code

### static-qq

```code
(static-qq form) -> `form

static quasi-quoted
```code

### stdio-get-args

```code
(stdio-get-args stdio) -> list
```code

### stop

```code
stop a child
```code

### stop

```code
stop a child
```code

### str-as-num

```code
(str-as-num str) -> num
```code

### str?

```code
(str? form) -> :t | :nil
```code

### structure

```code
(structure name base [(byte field ...)] ...)
```code

### substr

```code
(substr text substr) -> (([(i0 i1)] ...) ())
```code

### swap

```code
(swap list index index)
```code

### sym?

```code
(sym? form) -> :t | :nil
```code

### task-nodeid

### texture-metrics

```code
(texture-metrics texture) -> (handle width height)
```code

### third

```code
(third seq) -> el | :nil
```code

### time-in-seconds

```code
(time-in-seconds time) -> str
```code

### times

```code
(times num body)
```code

### timezone-init

### timezone-lookup

### to-lower

```code
(to-lower str) -> str
```code

### to-net-id

```code
(to-net-id service_id) -> net_id
```code

### to-service-id

```code
(to-service-id net_id) -> service_id
```code

### to-upper

```code
(to-upper str) -> str
```code

### tokenize

### trim

```code
(trim str [str]) -> str
```code

### trim-end

```code
(trim-end str [str]) -> str
```code

### trim-start

```code
(trim-start str [str]) -> str
```code

### type-to-size

```code
(type-to-size sym) -> num
```code

### ui-backdrop

```code
(ui-backdrop name [props] [body]) -> backdrop
```code

### ui-button

```code
(ui-button name [props] [body]) -> button
```code

### ui-buttons

```code
(ui-buttons symbols event [props])
```code

### ui-canvas

```code
(ui-canvas name width height scale [props]) -> canvas
```code

### ui-element

```code
(ui-element name func [props] [body]) -> view
```code

### ui-flow

```code
(ui-flow name [props] [body]) -> flow
```code

### ui-grid

```code
(ui-grid name [props] [body]) -> grid
```code

### ui-hchart

```code
(ui-hchart name title num_marks [props]) -> hchart
```code

### ui-label

```code
(ui-label name [props] [body]) -> label
```code

### ui-merge-props

```code
(ui-merge-props props) -> props
```code

### ui-progress

```code
(ui-progress name [props]) -> progress
```code

### ui-props

```code
(ui-props props [props]) -> props
```code

### ui-root

```code
(ui-root name func [props] [body]) -> view
```code

### ui-scroll

```code
(ui-scroll name flags [props] [body]) -> scroll
```code

### ui-slider

```code
(ui-slider name [props]) -> slider
```code

### ui-spinner

```code
(ui-spinner name [props]) -> spinner
```code

### ui-textfield

```code
(ui-textfield name [props]) -> textfield
```code

### ui-title

```code
(ui-title name [props]) -> title
```code

### ui-title-bar

```code
(ui-title-bar name title symbols event [props]) -> flow
```code

### ui-tool-bar

```code
(ui-tool-bar name [props] [body]) -> flow
```code

### ui-tree

```code
(ui-tree name event [props]) -> tree
```code

### ui-vdu

```code
(ui-vdu name [props]) -> vdu
```code

### ui-view

```code
(ui-view name [props] [body]) -> view
```code

### ui-window

```code
(ui-window name [props] [body]) -> window
```code

### undo

### undoable

### unless

```code
(unless tst body)
```code

### until

```code
(until tst body)
```code

### unzip

```code
(unzip seq buckets) -> buckets
```code

### vec-chebyshev-distance

### vec-clamp

### vec-collide-lines-2d

### vec-collide-thick-lines-2d

### vec-cross-3d

### vec-det

### vec-dist

### vec-dist-to-line

### vec-euclidean-distance

### vec-intersect-2d

### vec-intersect-lines-2d

### vec-length

### vec-length-squared

### vec-macro

### vec-manhattan-distance

### vec-norm

### vec-perp-2d

### vec-reflect

### vec-sdist

### vec-sdist-to-line

### vec-squared-euclidean-distance

### vertex-interp

```code
(vertex-interp isolevel p1 p2 valp1 valp2) -> p
```code

### view-fit

```code
(view-fit x y w h) -> (x y w h)
```code

### view-locate

```code
(view-locate w h [flag]) -> (x y w h)
```code

### walk-list

```code
(walk-list list fnc_element fnc_in fnc_out)

if fnc_in returns :nil, it'll step down into that list.
fnc_out is allways called to balence calls to fnc_in.
```code

### when

```code
(when tst body)
```code

### within-compile-env

```code
(within-compile-env lambda)
```code

### write-int

```code
(write-int stream num|list) -> stream
```code

### write-line

```code
(write-line stream str) -> stream
```code

### write-long

```code
(write-long stream num|list) -> stream
```code

### write-short

```code
(write-short stream num|list) -> stream
```code

### zip

```code
(zip seq ...) -> list
```code

