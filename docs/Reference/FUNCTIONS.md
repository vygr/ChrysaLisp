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

### Lmap-kv

```code
(Lmap-kv [key val ...]) -> lmap
```

### SVG-Canvas

```code
(SVG-Canvas stream [scale]) -> :nil | canvas
```

### SVG-info

```code
(SVG-info stream) -> (width height type) | (-1 -1 -1)
```

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
(all-files &optional root exts cut_start cut_end) -> paths

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

### char-class

```code
(char-class key) -> str

create char class
these are sorted interend unescaped char strings
can be searched with (bfind)
```

### char-to-num

```code
(char-to-num char) -> num
```

### circle

```code
(circle r) -> path

cached circle generation, quantised to 1/4 pixel
```

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

### debug-brk

```code
(debug-brk name condtion)
```

### dec

```code
(dec num) -> num
```

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

### defmethod_

```code
(defmethod_ name ([arg ...]) body)

(. _this :method [arg ...])
```

### defsetmethod

```code
(defsetmethod field)
```

### each

```code
(each lambda seq ...)
```

### each-line

```code
(each-line lambda stream)
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

### empty?

```code
(empty? form) -> :t | :nil
```

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

### escape

```code
(escape str) -> str
```

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
(export env symbols)
```

### export-classes

```code
(export-classes classes)
```

### export-symbols

```code
(export-symbols symbols)
```

### filter

```code
(filter lambda seq) -> list
```

### free-select

```code
(free-select select)
```

### func-load

```code
(func-load name) -> (body links refs)

cache loading of function blobs etc
```

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

### get-byte

```code
(get-byte str index) -> num
```

### get-cstr

```code
(get-cstr str index) -> str
```

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

### getf

```code
(getf obj field [offset]) -> value
```

### id-decode

```code
(id-decode id) -> id
```

### id-encode

```code
(id-encode id) -> id
```

### identity

```code
(identity any) -> any

function that returns it's argument
```

### import

```code
(import path [env]) -> env
```

### import-from

```code
(import-from &optional symbols classes)
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

### insert

```code
(insert seq pos seq) -> seq
```

### int-to-hex-str

```code
(int-to-hex-str num) -> str
```

### iso-surface

```code
(iso-surface grid isolevel) -> tris

determine the index into the edge table which
tells us which vertices are inside the surface
```

### join

```code
(join seqs seq &optional mode) -> seq
```

### lambda-func?

```code
(lambda-func? form) -> :t | :nil
```

### lambda?

```code
(lambda? form) -> :t | :nil
```

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
(lighting col at)

very basic attenuation and diffuse
```

### lighting-at3

```code
(lighting-at3 col at sp)

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

### mat4x4-invert

```code
(mat4x4-invert mat4x4) -> mat4x4
```

### mat4x4-mul

```code
(mat4x4-mul mat4x4_a mat4x4_b) -> mat4x4
```

### match

```code
(match text cpat start) -> -1 | end
```

### match?

```code
(match? text regexp) -> :t | :nil
```

### matches

```code
(matches text regexp) -> matches
```

### max-length

```code
(max-length list) -> max
```

### memoize

```code
(memoize key form &optional num_buckets) -> (eval form)
```

### neg?

```code
(neg? num) -> :t | :nil
```

### nempty?

```code
(nempty? form) -> :t | :nil
```

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
(open-child task mode) -> net_id
```

### open-pipe

```code
(open-pipe tasks) -> ([net_id | 0] ...)
```

### open-remote

```code
(open-remote task node mode) -> net_id
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

### pad

```code
(pad form width [str]) -> str
```

### partition

```code
(partition count seq) -> list of slices
```

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

### pipe-farm

```code
(pipe-farm jobs &optional retry_timeout) -> ((job result) ...)

run pipe farm and collect output
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

### pow

```code
(pow base exponent) -> integer
```

### profile-report

```code
(profile-report name [reset])
```

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

### read

```code
(read stream &optional last_char_code) -> (form next_char_code)
```

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

### read-short

```code
(read-short stream) -> num
```

### reduce

```code
(reduce lambda seq [accum]) -> form
```

### reduce-rev

```code
(reduce-rev lambda seq [accum]) -> form
```

### replace

```code
(replace seq start end seq) -> seq
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

### rotate

```code
(rotate seq start mid end) -> seq
```

### scatter

```code
(scatter map key val ...) -> map

scatter a list of values
```

### search

```code
(search text cpat start) -> (list submatches {-1 | end})
```

### seq?

```code
(seq? form) -> :t | :nil
```

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

### slices

```code
(slices seq) -> seqs
```

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
(stdio-get-args stdio) -> cmd_line
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

### stream-diff

```code
(stream-diff a b c)

difference between streams a and b, write to stream c
```

### stream-patch

```code
(stream-patch a b c)

patch stream a with stream b, write to stream c
```

### structure

```code
(structure name base [(byte field ...)] ...)
```

### substr

```code
(substr text substr) -> matches
```

### swap

```code
(swap list index index)
```

### sym?

```code
(sym? form) -> :t | :nil
```

### texture-metrics

```code
(texture-metrics texture) -> (handle width height)
```

### time-in-seconds

```code
(time-in-seconds time) -> str
```

### times

```code
(times num body)
```

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

### tree-buckets

```code
(tree-buckets collection type) -> num
```

### tree-collection?

```code
(tree-collection? type) -> :nil | type
```

### tree-decode

```code
(tree-decode atom) -> atom
```

### tree-encode

```code
(tree-encode atom) -> atom
```

### tree-load

```code
(tree-load stream) -> tree
```

### tree-node

```code
(tree-node ((type &optional buckets)) -> collection
```

### tree-save

```code
(tree-save stream tree &optional key_filters) -> tree
```

### tree-type

```code
(tree-type collection) -> type
```

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
(ui-element name constructor [props] [body]) -> view
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
(ui-root name constructor [props] [body]) -> view
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

### ui-text

```code
(ui-text name [props]) -> text
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

### ui-tool-tips

```code
(ui-tool-tips toolbar tips)
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

### unescape

```code
(unescape str) -> str
```

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

