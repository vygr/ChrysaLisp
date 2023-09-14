# Functions

### #

```code
(# (< %9 %0 %3) ...) -> (lambda (%0 %3 %9) (< %9 %0 %3) ...)
```

### .->

```code
(.-> this form ...)
```

### .?

```code
(.? this method) -> :nil | lambda
```

### .super

```code
(.super this :method [arg ...])
```

### Canvas-from-file

```code
(Canvas-from-file file flags) -> :nil | canvas
```

### Emap-kv

```code
(Emap-kv [key val ...]) -> emap
```

### Fmap-kv

```code
(Fmap-kv [key val ...]) -> fmap
```

### Fset-k

```code
(Fset-k [key ...]) -> fset
```

### Gridcell

### Lmap-kv

```code
(Lmap-kv [key val ...]) -> lmap
```

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
```

### SVG-info

```code
(SVG-info stream) -> (width height type) | (-1 -1 -1)
```

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
```

### Xmap-kv

```code
(Xmap-kv [key val ...]) -> xmap
```

### Xset-k

```code
(Xset-k [key ...]) -> xset
```

### aand

```code
(aand [form] ...)
```

### abi

```code
(abi) -> sym
```

### abs-path

```code
(abs-path path [current]) -> path
```

### acond

```code
(acond (tst body) ...)
```

### aeach

```code
(aeach seq body)
```

### age

```code
(age path) -> 0 | time ns
```

### aif

```code
(aif form form [form])
```

### align

```code
(align num div) -> num
```

### all-dirs

```code
(all-dirs paths) -> paths

return all the dir paths
```

### all-files

```code
(all-files &optional root exts n) -> paths

all source files from root downwards, none recursive
```

### alloc-select

```code
(alloc-select size) -> ((task-netid) [temp_mbox] ...)
```

### and

```code
(and [tst] ...) -> :t | :nil | tst
```

### array?

```code
(array? form) -> :t | :nil
```

### ascii-char

```code
(ascii-char num) -> char
```

### ascii-code

```code
(ascii-code char) -> num
```

### ascii-lower

```code
(ascii-lower num) -> num
```

### ascii-upper

```code
(ascii-upper num) -> num
```

### asome

```code
(asome seq body)
```

### awhen

```code
(awhen form body)
```

### awhile

```code
(awhile form body)
```

### bits

```code
(bits name base [(bit field ...)] ...)
```

### blank-line?

### boot-image

### bounding-box

```code
(bounding-box verts vec3-extract-fnc) -> (min_v3 max_v3)
```

### bounding-sphere

```code
(bounding-sphere verts vec3-extract-fnc) -> (center_v3 radius)
```

### byte-to-hex-str

```code
(byte-to-hex-str num) -> str
```

### cache

### canvas-brighter

```code
(canvas-brighter col) -> col
```

### canvas-darker

```code
(canvas-darker col) -> col
```

### canvas-flush

```code
(canvas-flush)

flush any shared pixmaps that have no users.
4 refs are held by the cache Lmap and this loop !
```

### case

```code
(case form [(key|(key ...) body)] ...)
```

### char-to-num

```code
(char-to-num char) -> num
```

### check-date

### circle

```code
cached circle generation, quantised to 1/4 pixel
```

### compile

### compose

```code
(compose lambda lambda) -> lambda
```

### const

```code
(const form)
```

### cpp-node?

```code
(cpp_node? node) -> :t | :nil
```

### cpu

```code
(cpu) -> sym
```

### curry

```code
(curry lambda var ...) -> lambda
```

### date

### day-of-the-week

### days-in-month

### days-in-year

### debug

```code
(debug name form)
```

### debug-format

```code
(debug-format name env)
```

### debug-fun

```code
(debug-fun name list) -> list
```

### debug-fun?

```code
(debug-fun? form)
```

### debug-send

```code
(debug-send form ...)
```

### dec

```code
(dec num) -> num
```

### decode-date

### def_real

### defabstractmethod

```code
(defabstractmethod ([arg ...]) body)

Declare a method as abstract and will
throw an error if invoked. Concrete
classes deriving from a class with abstractions
should provide the concreate handler
```

### defclass

```code
(defclass Name ([arg ...]) (super ...) body)
```

### defcvar

### deffimethod

```code
(deffimethod name ffi)
```

### defgetmethod

```code
(defgetmethod field)
```

### defmethod

```code
(defmethod name ([arg ...]) body)

(. this :method [arg ...])
```

### defmethod

### defmethod

### defmethod_

```code
(defmethod_ name ([arg ...]) body)

(. _this :method [arg ...])
```

### defsetmethod

```code
(defsetmethod field)
```

### deg-to-rad

### each

```code
(each lambda seq ...)
```

### each-found

```code
(each-found lambda text substr)
```

### each-line

```code
(each-line lambda stream)
```

### each-match

```code
(each-match lambda text regexp)
```

### each-mergeable

```code
(each-mergeable lambda seq) -> seq
```

### each-mergeable-rev

```code
(each-mergeable-rev lambda seq) -> seq
```

### each-rev

```code
(each-rev lambda seq ...)
```

### elem-find

### empty?

```code
(empty? form) -> :t | :nil
```

### encode-date

### ends-with

```code
(ends-with str str) -> :t | :nil
```

### enums

```code
(enums name base [(enum field ...)] ...)
```

### env?

```code
(env? form) -> :t | :nil
```

### erase

```code
(erase seq start end) -> seq
```

### erase-line

### even?

```code
(even? num) -> :t | :nil
```

### every

```code
(every lambda seq ...) -> :nil | form
```

### exec

```code
(exec form)
```

### export

```code
(export env sym ...)
```

### export-classes

```code
(export-classes class ...)
```

### export-symbols

```code
(export-symbols sym ...)
```

### filter

```code
(filter lambda seq) -> list
```

### first

```code
(first seq) -> el | :nil
```

### firstelem

### float-time

### framed-lambda

### free-select

```code
(free-select select)
```

### func-load

```code
(func-load name) -> (body links refs)

cache loading of function blobs etc
```

### func-obj-path

### func-refs

```code
(func-refs fobj) -> ([sym] ...)
```

### func?

```code
(func? form) -> :t | :nil
```

### gather

```code
(gather map key ...) -> (val ...)

gather a list of values
```

### gen-norms

```code
(gen-norms verts tris) -> (norms new_tris)
```

### get-by-val

### get-byte

```code
(get-byte str index) -> num
```

### get-cstr

```code
(get-cstr str index) -> str
```

### get-date

### get-int

```code
(get-int str index) -> num
```

### get-long

```code
(get-long str index) -> num
```

### get-netid

```code
(get-netid str index) -> netid
```

### get-nodeid

```code
(get-nodeid str index) -> nodeid
```

### get-selection

### get-short

```code
(get-short str index) -> num
```

### get-ubyte

```code
(get-ubyte str index) -> num
```

### get-uint

```code
(get-uint str index) -> num
```

### get-ushort

```code
(get-ushort str index) -> num
```

### get-year

### get-yeardays

### getf

```code
(getf obj field [offset]) -> value
```

### gui-add-back

### gui-quit

### gui-rpc

### gui-sub

### identity

```code
(identity any) -> any

function that returns it's argument
```

### import

```code
(import path [env]) -> env
```

### in-get-state

```code
(in-get-state in) -> num
```

### in-mbox

```code
(in-mbox in) -> mbox
```

### in-set-state

```code
(in-set-state in num) -> in
```

### inc

```code
(inc num) -> num
```

### include

### insert

```code
(insert seq pos seq) -> seq
```

### insert-line

### insertelem

### int-to-hex-str

```code
(int-to-hex-str num) -> str
```

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
```

### jit

### join

```code
(join list seq) -> seq
```

### lambda-func?

```code
(lambda-func? form) -> :t | :nil
```

### lambda?

```code
(lambda? form) -> :t | :nil
```

### last

```code
(last seq) -> el | :nil
```

### last-line

### leapyear?

### leapyear?

### leapyears-since-epoch

### let

```code
(let ([(var val) ...]) body)
```

### let*

```code
(let* ([(var val) ...]) body)
```

### lighting

```code
very basic attenuation and diffuse
```

### lighting-at3

```code
very basic attenuation, diffuse and specular
```

### lisp-node?

```code
(lisp_node? node) -> :t | :nil
```

### lisp-nodes

```code
(lisp-nodes) -> nodes
```

### list

### list?

```code
(list? form) -> :t | :nil
```

### lists

```code
(lists n) -> ((list0) ... (listn-1))
```

### lists2

```code
(lists2) -> ((list) (list))
```

### load-stream

```code
(load-stream path) -> :nil | stream
```

### log2

```code
(log2 num) -> num
```

### lognot

```code
(lognot num) -> num
```

### long-to-hex-str

```code
(long-to-hex-str num) -> str
```

### lower

```code
(lower field | (field val) ...) -> (set this field var ...)
```

### macro-func?

```code
(macro-func? form) -> :t | :nil
```

### macro?

```code
(macro? form) -> :t | :nil
```

### map

```code
(map lambda seq ...) -> list
```

### map-rev

```code
(map-rev lambda seq ...) -> list
```

### map?

```code
(map? object) -> :t | :nil

returns true if argument is a map type
```

### mat3x2-mul-f

```code
(mat3x2-mul-f mat3x2_a mat3x2_b) -> mat3x2-f
```

### mat3x3-mul

```code
(mat3x3-mul mat3x3_a mat3x3_b) -> mat3x3
```

### mat3x3-vec3-mul

### mat4x4-invert

```code
(mat4x4-invert mat4x4) -> mat4x4
```

### mat4x4-mul

```code
(mat4x4-mul mat4x4_a mat4x4_b) -> mat4x4
```

### mat4x4-vec3-mul

### mat4x4-vec4-mul

### match?

```code
(match? text regexp) -> :t | :nil
```

### matches

```code
(matches text regexp) -> (([(i0 i1)] ...) ([([(s0 s1)] ...)]...))
```

### max-length

```code
(max-length list) -> max
```

### month-of-the-year

### mutate-buffer

### neg?

```code
(neg? num) -> :t | :nil
```

### nempty?

```code
(nempty? form) -> :t | :nil
```

### new_bit

### new_enum

### new_field

### nil?

```code
(nil? o) -> :t | :nil
```

### nlo

```code
(nlo num) -> num
```

### nlz

```code
(nlz num) -> num
```

### not

```code
(not form) -> :t | :nil
```

### not-whole-word-char?

### notany

```code
(notany lambda seq ...) -> :t | :nil
```

### notevery

```code
(notevery lambda seq ...) -> :t | :nil
```

### nto

```code
(nto num) -> num
```

### ntz

```code
(ntz num) -> num
```

### num-to-char

```code
(num-to-char num) -> char
```

### num-to-utf8

```code
(num-to-utf8 num) -> str
```

### num?

```code
(num? form) -> :t | :nil
```

### odd?

```code
(odd? num) -> :t | :nil
```

### open-child

```code
(open-child task mode) -> str
```

### open-pipe

```code
(open-pipe tasks) -> (str ...)
```

### open-remote

```code
(open-remote task node mode) -> str
```

### open-task

```code
(open-task task node mode key_num reply)
```

### opt

```code
(opt var val [cond])
```

### opt-mesh

```code
(opt-mesh verts norms tris) -> (new_verts new_norms new_tris)
```

### opt-vectors

```code
(opt-vectors vectors) -> (new_vectors new_indexs)
```

### options

```code
(options stdio optlist) -> :nil | args

scan the stdio args and process acording to the optlist
```

### options-find

```code
(options-find optlist arg) -> :nil | opt_entry
```

### options-print

```code
(options-print &rest _)
```

### options-split

```code
(options-split args) -> (a0 [a1] ...)
```

### or

```code
(or [tst] ...) -> :nil | tst
```

### os

```code
(os) -> sym
```

### output

### pad

```code
(pad form width [str]) -> str
```

### partition

```code
(partition count seq) -> list of lists
```

### path-fill-and-stroke

### path-gen-ellipse

```code
(path-gen-ellipse cx cy rx ry tol dst) -> dst
```

### path-gen-paths

```code
(path-gen-paths svg_d tol) -> ((:nil|:t path) ...)

:t closed, :nil open
```

### path-gen-rect

```code
(path-gen-rect x y x1 y1 rx ry tol dst) -> dst
```

### path-stroke-polygons

```code
(path-stroke-polygons dst radius tol join src) -> dst
```

### path-stroke-polylines

```code
(path-stroke-polylines dst radius tol join cap1 cap2 src) -> dst
```

### pipe-run

```code
(pipe-run cmdline &optional outfun)
```

### pipe-split

```code
(pipe-split cmdline) -> (e0 [e1] ...)
```

### pixmap-cpm-info

```code
(pixmap-cpm-info stream) -> (width height type) | (-1 -1 -1)
```

### pixmap-info

```code
(pixmap-info file) -> (width height type) | (-1 -1 -1)
```

### pixmap-load

```code
(pixmap-load file) -> :nil | pixmap
```

### pixmap-save

```code
(pixmap-save pixmap file type) -> :nil | pixmap
```

### pixmap-tga-info

```code
(pixmap-tga-info stream) -> (width height type) | (-1 -1 -1)
```

### pos?

```code
(pos? num) -> :t | :nil
```

### postfix

### pow

```code
(pow base exponent) -> integer
```

### print-mat

### print-vec

### profile-lambda

### profile-print

```code
(profile-print name [stream]) -> stdout | stream
```

### profile-report

```code
(profile-report name [reset])
```

### push-attributes

### quasi-quote?

```code
(quasi-quote? form) -> :t | :nil
```

### query

```code
(query pattern whole_words regexp) -> (engine pattern meta)
```

### quote?

```code
(quote? form) -> :t | :nil
```

### raise

```code
(raise field | (var val) ...) -> (defq var (get field this) ...)
```

### range

```code
(range start end [step]) -> list
```

### rcurry

```code
(rcurry lambda var ...) -> lambda
```

### read-attribute

### read-col

### read-data

```code
(read-data stream bytes) -> str
```

### read-int

```code
(read-int stream) -> num
```

### read-long

```code
(read-long stream) -> num
```

### read-rgb

### read-short

```code
(read-short stream) -> num
```

### read-transform

### redo

### reduce

```code
(reduce lambda seq [accum]) -> form
```

### reduce-rev

```code
(reduce-rev lambda seq [accum]) -> form
```

### render-object-tris

### render-object-verts

### replace

```code
(replace seq start end seq) -> seq
```

### rest

```code
(rest seq) -> :nil | seq
```

### restart

```code
restart a child
```

### restart

```code
restart a child
```

### reverse

```code
(reverse list) -> list
```

### s2i

### search

```code
(search text cpat start) -> -1 | end

for each char in text
```

### second

```code
(second seq) -> el | :nil
```

### select-buffer

### select-lines

### select-paragraph

### select-word

### selection?

### seq?

```code
(seq? form) -> :t | :nil
```

### set-line

### set?

```code
(set? object) -> :t | :nil

returns true if argument is a set type
```

### setd

```code
(setd var val [var val] ...)
```

### setf

```code
(setf obj field value [offset]) -> obj
```

### setf->

```code
(setf-> msg form ...)
```

### setoffset

```code
adjust text offset
```

### short-to-hex-str

```code
(short-to-hex-str num) -> str
```

### shuffle

```code
(shuffle list [start end]) -> list
```

### shuffled

```code
(shuffled list [start end]) -> list
```

### slot

### slot

### slot

### slot

### some

```code
(some lambda seq ...) -> :nil | form
```

### some-rev

```code
(some-rev lambda seq ...) -> :nil | form
```

### sort

```code
(sort fcmp list [start end]) -> list
```

### sort-selection

### sorted

```code
(sorted fcmp list [start end]) -> list
```

### start

```code
start a child
```

### start

```code
start a child
```

### starts-with

```code
(starts-with str str) -> :t | :nil
```

### static-q

```code
(static-q form) -> 'form

static quoted
```

### static-qq

```code
(static-qq form) -> `form

static quasi-quoted
```

### stdio-get-args

```code
(stdio-get-args stdio) -> list
```

### stop

```code
stop a child
```

### stop

```code
stop a child
```

### str-as-num

```code
(str-as-num str) -> num
```

### str?

```code
(str? form) -> :t | :nil
```

### structure

```code
(structure name base [(byte field ...)] ...)
```

### substr

```code
(substr text substr) -> (([(i0 i1)] ...) ())
```

### swap

```code
(swap list index index)
```

### sym?

```code
(sym? form) -> :t | :nil
```

### task-nodeid

### texture-metrics

```code
(texture-metrics texture) -> (handle width height)
```

### third

```code
(third seq) -> el | :nil
```

### time-in-seconds

```code
(time-in-seconds time) -> str
```

### times

```code
(times num body)
```

### timezone-init

### timezone-lookup

### to-lower

```code
(to-lower str) -> str
```

### to-net-id

```code
(to-net-id service_id) -> net_id
```

### to-service-id

```code
(to-service-id net_id) -> service_id
```

### to-upper

```code
(to-upper str) -> str
```

### tokenize

### trim

```code
(trim str [str]) -> str
```

### trim-end

```code
(trim-end str [str]) -> str
```

### trim-start

```code
(trim-start str [str]) -> str
```

### type-to-size

```code
(type-to-size sym) -> num
```

### ui-backdrop

```code
(ui-backdrop name [props] [body]) -> backdrop
```

### ui-button

```code
(ui-button name [props] [body]) -> button
```

### ui-buttons

```code
(ui-buttons symbols event [props])
```

### ui-canvas

```code
(ui-canvas name width height scale [props]) -> canvas
```

### ui-element

```code
(ui-element name func [props] [body]) -> view
```

### ui-flow

```code
(ui-flow name [props] [body]) -> flow
```

### ui-grid

```code
(ui-grid name [props] [body]) -> grid
```

### ui-hchart

```code
(ui-hchart name title num_marks [props]) -> hchart
```

### ui-label

```code
(ui-label name [props] [body]) -> label
```

### ui-merge-props

```code
(ui-merge-props props) -> props
```

### ui-progress

```code
(ui-progress name [props]) -> progress
```

### ui-props

```code
(ui-props props [props]) -> props
```

### ui-root

```code
(ui-root name func [props] [body]) -> view
```

### ui-scroll

```code
(ui-scroll name flags [props] [body]) -> scroll
```

### ui-slider

```code
(ui-slider name [props]) -> slider
```

### ui-spinner

```code
(ui-spinner name [props]) -> spinner
```

### ui-textfield

```code
(ui-textfield name [props]) -> textfield
```

### ui-title

```code
(ui-title name [props]) -> title
```

### ui-title-bar

```code
(ui-title-bar name title symbols event [props]) -> flow
```

### ui-tool-bar

```code
(ui-tool-bar name [props] [body]) -> flow
```

### ui-tree

```code
(ui-tree name event [props]) -> tree
```

### ui-vdu

```code
(ui-vdu name [props]) -> vdu
```

### ui-view

```code
(ui-view name [props] [body]) -> view
```

### ui-window

```code
(ui-window name [props] [body]) -> window
```

### undo

### undoable

### unless

```code
(unless tst body)
```

### until

```code
(until tst body)
```

### unzip

```code
(unzip seq buckets) -> buckets
```

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
```

### view-fit

```code
(view-fit x y w h) -> (x y w h)
```

### view-locate

```code
(view-locate w h [flag]) -> (x y w h)
```

### walk-list

```code
(walk-list list fnc_element fnc_in fnc_out)

if fnc_in returns :nil, it'll step down into that list.
fnc_out is allways called to balence calls to fnc_in.
```

### when

```code
(when tst body)
```

### within-compile-env

```code
(within-compile-env lambda)
```

### write-int

```code
(write-int stream num|list) -> stream
```

### write-line

```code
(write-line stream str) -> stream
```

### write-long

```code
(write-long stream num|list) -> stream
```

### write-short

```code
(write-short stream num|list) -> stream
```

### zip

```code
(zip seq ...) -> list
```

