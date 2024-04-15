# Functions

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

### abi

```code
(abi) -> sym
```

### abs-path

```code
(abs-path path [current]) -> path
```

### age

```code
(age path) -> 0 | time ns
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

### all-file-depends

```code
(all-file-depends paths &optional imps) -> paths

create list of all dependencies, with implicit options
```

### all-files

```code
(all-files [root exts cut_start cut_end]) -> paths

all source files from root downwards, none recursive
```

### alloc-select

```code
(alloc-select size) -> ((task-netid) [temp_mbox] ...)
```

### array?

```code
(array? form) -> :t | :nil
```

### ascii-lower

```code
(ascii-lower num) -> num
```

### ascii-upper

```code
(ascii-upper num) -> num
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

### canvas-load

```code
(canvas-load file flags) -> :nil | canvas
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

### cpp-node?

```code
(cpp_node? node) -> :t | :nil
```

### cpu

```code
(cpu) -> sym
```

### debug-brk

```code
(debug-brk name condtion)
```

### each-line

```code
(each-line lambda stream [line_num])
```

### each-mergeable

```code
(each-mergeable lambda seq) -> seq
```

### empty?

```code
(empty? form) -> :t | :nil
```

### ends-with

```code
(ends-with str str) -> :t | :nil
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

### file-depends

```code
(file-depends path) -> paths

create list of immediate dependencies
```

### filter-array

```code
(filter-array lambda array) -> array
```

### fixed?

```code
(fixed? form) -> :t | :nil
```

### fixeds?

```code
(fixeds? form) -> :t | :nil
```

### found?

```code
(found? text substr) -> :t | :nil
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

### get-cstr

```code
(get-cstr str index) -> str
```

### handler

```code
(handler state page line) -> state
```

### id-decode

```code
(id-decode id) -> id
```

### id-encode

```code
(id-encode id) -> id
```

### import

```code
(import path [env]) -> env
```

### import-from

```code
(import-from [symbols classes])
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
(join seqs seq [mode]) -> seq
```

### lambda-func?

```code
(lambda-func? form) -> :t | :nil
```

### lambda?

```code
(lambda? form) -> :t | :nil
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

### macro-func?

```code
(macro-func? form) -> :t | :nil
```

### macro?

```code
(macro? form) -> :t | :nil
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

### min-length

```code
(min-length list) -> min
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

### nums?

```code
(nums? form) -> :t | :nil
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

### os

```code
(os) -> sym
```

### pad

```code
(pad form width [str]) -> str
```

### path-gen-ellipse

```code
(path-gen-ellipse cx cy rx ry dst) -> dst
```

### path-gen-paths

```code
(path-gen-paths svg_d) -> ((:nil|:t path) ...)

:t closed, :nil open
```

### path-gen-rect

```code
(path-gen-rect x y x1 y1 rx ry dst) -> dst
```

### path-smooth

```code
(path-smooth src) -> dst
```

### path-stroke-polygons

```code
(path-stroke-polygons dst radius join src) -> dst
```

### path-stroke-polylines

```code
(path-stroke-polylines dst radius join cap1 cap2 src) -> dst
```

### pipe-farm

```code
(pipe-farm jobs [retry_timeout]) -> ((job result) ...)

run pipe farm and collect output
```

### pipe-run

```code
(pipe-run cmdline [outfun])
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

### range

```code
(range start end [step]) -> list
```

### read-data

```code
(read-data stream bytes) -> str
```

### real?

```code
(real? form) -> :t | :nil
```

### reals?

```code
(reals? form) -> :t | :nil
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

### sort

```code
(sort fcmp list [start end]) -> list
```

### split

```code
(split seq sseq) -> seqs
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
(tree-node ((type [buckets])) -> collection
```

### tree-save

```code
(tree-save stream tree [key_filters]) -> tree
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

### ui-merge-props

```code
(ui-merge-props props) -> props
```

### ui-tool-tips

```code
(ui-tool-tips view tips)
```

### unescape

```code
(unescape str) -> str
```

### unique

```code
(unique seq) -> seq
```

### unzip

```code
(unzip seq cnt) -> seqs
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

### within-compile-env

```code
(within-compile-env lambda)
```

### write-line

```code
(write-line stream str) -> stream
```

### zip

```code
(zip seq ...) -> seq
```

