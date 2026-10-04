# Functions

### CPM-info

```code
(CPM-info stream) -> (width height type) | (-1 -1 -1)
```

### CPM-load

```code
(CPM-load stream) -> :nil | canvas
```

### CPM-save

```code
(CPM-save canvas stream type [rle lz4 ident]) -> canvas
```

### CWB-info

```code
(CWB-info stream) -> (width height type) | (-1 -1 -1)
```

### CWB-load

```code
(CWB-load stream [scale]) -> :nil | canvas
```

### SVG-info

```code
(SVG-info stream) -> (width height type) | (-1 -1 -1)
```

### SVG-load

```code
(SVG-load stream [scale]) -> :nil | canvas
```

### TGA-info

```code
(TGA-info stream) -> (width height type) | (-1 -1 -1)
```

### TGA-load

```code
(TGA-load stream) -> :nil | canvas
```

### XML-parse

```code
(XML-parse stream fnc_in fnc_out fnc_text)

break the stream into svg tokens, symbols, strings etc
parse the commands and attributes calling back to the user functions
```

### abi

```code
(abi) -> sym
```

### action-maximise

```code
step zoom up to +zoom_max (3x)
```

### action-minimise

```code
step zoom down to +zoom_min (1x)
```

### action-quit

```code
launch logout app
```

### age

```code
(age path) -> 0 | time ns
```

### ahead

```code
(ahead chain cands tokens) -> :nil | token index

look ahead. A break has been found inside the forms that open on this
line at the tokens in chain, outermost first. Those forms are going to
be over several lines. Better that such a form starts a line of its
own, near the left, than hangs off the end of this one. So give the
gap before the outermost of them, if there is one and it is more than
a last resort.
```

### align

```code
(align num div) -> num
```

### apply-tokens

```code
(apply-tokens tokens body ind end [start])

move the stack of open forms over the tokens up to end. The tokens
are of a line, or a part of one that begins at the char start.
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

### atom?

```code
(atom? o) -> :t | :nil
```

### bit-mask

```code
(bit-mask mask ...) -> val
```

### bitcnt

```code
(bitcnt n) -> num bits
```

### bracket-cursors

```code
(bracket-cursors cursors start_y end_y) -> (si ei)

find range of cursors intersecting [start_y, end_y].
we bracket the range by including one extra cursor before and after
to ensure merges are handled correctly.
```

### build-method-overrides

```code
initialize :overrides as an empty list for ALL existing method entries
```

### build_tree_and_codebook

```code
Builds a deterministic, canonical Huffman tree and codebook from a frequency map.

This version uses a more optimal insertion sort pattern for tree building.
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

### canvas-info

```code
(canvas-info file) -> (width height type) | (-1 -1 -1)
```

### canvas-load

```code
(canvas-load file flags [swap_mode]) -> :nil | canvas
```

### canvas-save

```code
(canvas-save canvas file type [optionals...]) -> :nil | canvas
```

### char-class

```code
(char-class key) -> str

create char class
these are sorted interned char strings
can be searched with (bfind)
```

### check-date

```code
(check-date td) -> :t | :nil
```

### circle

```code
(circle r) -> path

cached circle generation, quantised to 1/4 pixel
```

### civil-from-days

```code
(civil-from-days days) -> (year month day)

O(1) arithmetic calendar conversion (month 0..11, day 1..31)
```

### cpu

```code
(cpu) -> sym
```

### csr-cmp

```code
(csr-cmp csr1 csr2) -> + 0 -
```

### csr-floor

```code
(csr-floor csr) -> csr

floor cursor to line boundaries
sorted cursor with point towards the bottom
```

### csr-map-delete

```code
(csr-map-delete px py cx cy ax ay) -> (nx ny)

map a point (px py) against a deletion from (cx cy) to (ax ay)
```

### csr-map-insert

```code
(csr-map-insert px py icx icy ecx ecy dy ax ay) -> (nx ny)

map a point (px py) against an insertion at (icx icy) ending at (ecx ecy)
```

### csr-sort

```code
(csr-sort csr) -> csr

sort cursor so (cx cy) <= (ax ay)
```

### csr-within

```code
(csr-within csr1 csr2) -> :t | :nil

returns :t if csr2 is enclosed within (or equal to) csr1
```

### date

```code
(date [secs]) -> (sec min hour day month year dotw)
```

### day-of-the-week

```code
(day-of-the-week dotw) -> str
```

### days-from-civil

```code
(days-from-civil year month day) -> days
```

### days-in-month

```code
(days-in-month month year) -> days (28..31)
```

### days-in-year

```code
(days-in-year year) -> 365 | 366
```

### decode-date

```code
(decode-date str) -> td
```

### each-mergeable

```code
(each-mergeable lambda seq) -> seq
```

### elem-end

```code
(elem-end tokens i n) -> pos

where the element that starts at token i ends
```

### empty?

```code
(empty? form) -> :t | :nil
```

### encode-date

```code
(encode-date [td]) -> str
```

### env?

```code
(env? form) -> :t | :nil
```

### escape-regexp

```code
(escape-regexp str) -> str
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

### files-all

```code
(files-all [root exts cut_start cut_end]) -> paths

all source files from root downwards, none recursive
```

### files-all-depends

```code
(files-all-depends paths [imps end]) -> paths

create list of all dependencies, with implicit options
```

### files-all-vp-source

```code
(files-all-vp-source) -> ordered_files
```

### files-classes-info

```code
(files-classes-info [forced]) -> :nil | class_db
```

### files-depends

```code
(files-depends path [end]) -> paths

create list of immediate dependencies
```

### files-dirs

```code
(files-dirs paths) -> paths

return all the dir paths
```

### files-function-info

```code
(files-function-info [forced]) -> :nil | func_db
```

### files-scan

```code
(scan-files files handler [split_class comment]) -> files

iterates through files, processing lines.
new files returned by the handler are merged into the work list.
```

### find-break

```code
(find-break tokens body ind limit) -> :nil | token index
```

### find-gap

```code
(find-gap tokens body ind limit collect) -> :nil | :again | token index

where to break a line, by the rules of the forms on it. The gaps are
only gathered up if the line is too long, or collect is given, as most
lines are not, and need no more than a look for a break that must be.
:again if such a break was found, and the gaps are needed to look ahead.
```

### fixed?

```code
(fixed? form) -> :t | :nil
```

### fixeds?

```code
(fixeds? form) -> :t | :nil
```

### flatten

```code
(flatten list) -> list
```

### float-time

```code
(float-time [smooth]) -> (sec min hour)
```

### flush-bits

```code
(flush-bits stream (array bit_pool bit_pool_size))
```

### form-elems

```code
(form-elems tokens i n) -> num

the count of elements in the form that opens at token i, operator and
all, or 1000 if it does not close on this line
```

### form-op

```code
(form-op tokens body i) -> :nil | str

the operator of the form that opens at token i
```

### form-rule

```code
(form-rule op par_rule par_argc tokens body i n) -> rule

the rule for the form that opens at token i, given the rule of the
form around it, and the index of that form's last element
```

### format-lisp

```code
(format-lisp data [limit wide]) -> str

format the source text. limit is the line length to break at, default
80, VP assembler lines get half as much again. wide if it is all VP.
A limit of 0 keeps the line breaks of the source, and only indents and
tidies.
```

### found?

```code
(found? text substr) -> :t | :nil
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

### gap-weight

```code
(gap-weight rule j) -> :nil | num

the weight of the gap before argument j of a form, :nil for never
```

### gather

```code
(gather map|set [key] ...) -> (val|key|:nil ...)

gather a list of [key|val|:nil]
```

### gen-norms

```code
(gen-norms verts tris) -> (norms new_tris)
```

### get-cstr

```code
(get-cstr str idx) -> str
```

### gui-rpc

```code
(gui-rpc (view cmd) -> :nil | view
```

### handler

```code
(handler state page line) -> state
```

### hex-decode-stream

```code
(hex-decode-stream in_stream out_stream [chunk_size flags])
```

### hex-encode-stream

```code
(hex-encode-stream in_stream out_stream [chunk_size flags])
```

### http-body-str

```code
(http-body-str resp) -> str
```

### http-get

```code
(http-get url [headers dest_stream]) -> pmap | :nil
```

### http-head

```code
(http-head url [headers]) -> pmap | :nil
```

### http-pool-checkin

```code
(http-pool-checkin host port conn headers)
```

### http-pool-checkout

```code
(http-pool-checkout host port) -> ((in out) is_pooled) | :nil
```

### http-pool-clear

```code
(http-pool-clear)

Close and flush all idle pooled sockets
```

### http-post

```code
(http-post url body [headers dest_stream]) -> pmap | :nil
```

### http-read-body

```code
(http-read-body in [headers dest_stream]) -> stream
```

### http-read-headers

```code
(http-read-headers in) -> pmap
```

### http-request

```code
(http-request method url [headers body dest_stream]) -> pmap | :nil
```

### huffman-build-freq-map

```code
Scans a stream to build a frequency map for static Huffman coding.
```

### huffman-compress

```code
(huffman-compress in_stream out_stream token_bits)
```

### huffman-compress-static

```code
Compresses a stream using a pre-built static model.
```

### huffman-decompress

```code
(huffman-decompress in_stream out_stream token_bits)
```

### huffman-decompress-static

```code
Decompresses a stream using a pre-built static model.
```

### huffman-read-codebook

```code
Reads a self-describing codebook from a stream and reconstructs the model.
```

### huffman-write-codebook

```code
Writes a self-describing codebook (via the frequency map) to a stream.
```

### import

```code
(import path [env])
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

### json-container?

```code
(json-container? obj) -> :t | :nil
```

### json-escape-str

```code
(json-escape-str str) -> str
```

### json-from-tre

```code
(json-from-tre obj) -> str
```

### json-get-items

```code
(json-get-items obj) -> list
```

### json-parse

```code
(json-parse str_or_stream) -> val
```

### json-read-number

```code
(json-read-number stream first_c) -> (val next_c)
```

### json-read-string

```code
(json-read-string stream) -> str
```

### json-serialize-scalar

```code
(json-serialize-scalar obj) -> str
```

### json-skip-ws

```code
(json-skip-ws stream [lookahead]) -> str | :nil
```

### json-stringify

```code
(json-stringify obj) -> str
```

### json-to-tre

```code
(json-to-tre str_or_stream) -> pmap | list | scalar
```

### keep-word?

```code
(keep-word? text pos) -> :t | :nil

would a source scanner, reading a line that starts at pos, take it
for a form it looks for ? Such a line must start where it did in the
source, no more and no less.
```

### lambda-func?

```code
(lambda-func? form) -> :t | :nil
```

### lambda?

```code
(lambda? form) -> :t | :nil
```

### leapyear?

```code
(leapyear? year) -> :t | :nil
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

### line-indent

```code
(line-indent tokens body lead) -> indent

the indent of a line, from the open forms and what it starts with
```

### line-op

```code
(line-op tokens body) -> :nil | str

the operator of the form the line starts with
```

### lisp-nodes

```code
(lisp-nodes) -> nodes
```

### list?

```code
(list? form) -> :t | :nil
```

### list??

```code
(list?? form) -> :t | :nil
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

### lz4-compress

```code
(lz4-compress in_stream out_stream [window_size])
```

### lz4-decompress

```code
(lz4-decompress in_stream out_stream [window_size])
```

### macro-func?

```code
(macro-func? form) -> :t | :nil
```

### macro?

```code
(macro? form) -> :t | :nil
```

### mail-read-timeout

```code
(mail-read-timeout mbox [timeout_us]) -> msg | :nil
```

### mat3x2-mul-f

```code
(mat3x2-mul-f mat3x2_a mat3x2_b) -> mat3x2-f
```

### match

```code
(match text meta start) -> -1 | end
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

### month-of-the-year

```code
(month-of-the-year month) -> str
```

### msafe?

```code
(msafe? o) -> :t | :nil
```

### must-break?

```code
(must-break? rule elems) -> :t | :nil

is the form broken however short, as it has more than the one of its
last arguments
```

### neg?

```code
(neg? num) -> :t | :nil
```

### nempty?

```code
(nempty? form) -> :t | :nil
```

### net-quiet

```code
(net-quiet [delay_us] [stable_count] [last_cnt]) -> (node_id ...)
```

### next-op

```code
(next-op tokens body i n) -> :nil | str

the word a line would start with, were it to start at token i
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

### node-auto

```code
(node-auto [per_cpu]) -> (pid ...)

size the network to this machine. Start nodes till there are per_cpu
for each processor, default 2, and no more than 32, then wait for
them to be seen, so what is started next can spread over them.
```

### node-link

```code
(node-link name) -> net_id

start a shared memory link on this node. The node at the other end
starts one of the same name.
```

### node-spawn

```code
(node-spawn [num]) -> (pid ...)

start num more nodes on this machine, default 1, each linked to this
node and to each other. A pid of -1 is a node the host could not start.
```

### nto

```code
(nto num) -> num
```

### ntz

```code
(ntz num) -> num
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

### obj-args

```code
(obj-args field [offset]) -> args

convert to obj-get/obj-set common args
```

### obj-set-args

```code
(obj-set-args (field value [offset])) -> args

convert to obj-set args
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
(open-pipe tasks [modes]) -> ([net_id | 0] ...)
```

### open-remote

```code
(open-remote task node mode) -> net_id
```

### open-starts

```code
(open-starts tokens end) -> (token index ...)

the forms that open on this line, and are still open at token end, as
the tokens they open at, outermost first
```

### open-task

```code
(open-task task node mode key_num reply)
```

### opt-flag

```code
(opt-flag 'opt_var) -> args
```

### opt-mesh

```code
(opt-mesh verts norms tris) -> (new_verts new_norms new_tris)
```

### opt-num

```code
(opt-num 'opt_var) -> args
```

### opt-nums

```code
(opt-nums cnt 'opt_var) -> args
```

### opt-str

```code
(opt-str 'opt_var) -> args
```

### opt-toggle

```code
(opt-toggle 'opt_var) -> args
```

### opt-vector

```code
(opt-vector vector part) -> (new_vector new_indices)
```

### options

```code
(options stdio optlist) -> :nil | args

scan the stdio args and process according to the optlist
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

### out-set-state

```code
(out-set-state out num) -> out
```

### pad

```code
(pad form width [str]) -> str
```

### pairs-value?

```code
(pairs-value? rule j) -> :t | :nil

is argument j of a form, with this rule, the value of a binding
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

### path-to-absolute

```code
(path-to-absolute target [current]) -> path

transform a relative filename to an absolute one
```

### path-to-file

```code
(path-to-file) -> path

the path to this file
```

### path-to-relative

```code
(path-to-relative target [current]) -> path

transform an absolute filename to a relative one
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
(pipe-split cmdline) -> ((mode cmd) ...)
```

### pmap?

```code
(pmap? form) -> :t | :nil
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

### pset?

```code
(pset? form) -> :t | :nil
```

### quasi-quote?

```code
(quasi-quote? form) -> :t | :nil
```

### query

```code
(query pattern whole_words regexp ignore_case) -> (engine meta pattern)

whole words is done by the regexp engine, a plain pattern is escaped
for it. An empty pattern stays empty, and matches as it would without.
```

### quote?

```code
(quote? form) -> :t | :nil
```

### range

```code
(range start end [step]) -> list
```

### real-to-str

```code
(real-to-str real [precision]) -> str
```

### real?

```code
(real? form) -> :t | :nil
```

### reals?

```code
(reals? form) -> :t | :nil
```

### reflow

```code
(reflow words line_width [indent tab_width]) -> lines
```

### render-object-tris

```code
project verts to screen
```

### repl-error?

```code
(repl-error? tokens body i n) -> :t | :nil

is the form that opens at token i a jump or call to :repl_error, such
a line holds the usage of a function, and is never broken
```

### replace-compile

```code
(replace-compile rep_str) -> (p_nums z_nums c_map rep_str)

memoize the compilation. atomic due to cooperative scheduling.
```

### replace-edits

```code
(replace-edits text matches compiled|rep_str) -> ((start end rep_str) ...)

returns a list of edit operations compatible with Editor buffers
```

### replace-matches

```code
(replace-matches text matches compiled|rep_str) -> text

with nothing matched the text is given back as it is, which also
covers an empty text, that has no parts to splice
```

### replace-regex

```code
(replace-regex text pattern compiled|rep_str) -> text
```

### replace-regex-edits

```code
(replace-regex-edits text pattern compiled|rep_str) -> ((start end rep_str) ...)
```

### replace-str

```code
(replace-str text pattern compiled|rep_str) -> text

substr returns a list of matches, each match is a list containing just ((start end))
this works perfectly with $0 in replace-matches
```

### replace-str-edits

```code
(replace-str-edits text pattern compiled|rep_str) -> ((start end rep_str) ...)
```

### restart

```code
restart a child
```

### restart

```code
restart a child
```

### rle-compress

```code
(rle-compress in_stream out_stream [token_bits run_bits])
```

### rle-decompress

```code
(rle-decompress in_stream out_stream [token_bits run_bits max_tokens])
```

### rpad

```code
(rpad form width [str]) -> str
```

### rule

```code
a rule is kept as (weights low one fill pairs clauses), all worked out

here, the once, so that applying a rule is only a matter of looking
```

### scan-line

```code
(scan-line body instr) -> (tokens instr)

the tokens of a line as (kind start end), as the Lisp reader would see
them. instr is the class of the closing char if in a string.
```

### scatter

```code
(scatter map|set [key]|[key val] ...) -> map|set

scatter a list of [key]|[key val]
```

### search

```code
(search text meta start) -> (list submatches {-1 | end})
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

### slices

```code
(slices list) -> ((s0 e0) (s1 e1) ...)
```

### solid-frame

```code
(solid-frame) -> :nil | frame

the innermost open form that is not transparent
```

### sort

```code
(sort list [fcmp start end]) -> list

the default fcmp is cmp, which is for strings
```

### squeeze

```code
(squeeze body tokens) -> str

the line with one space between its tokens, and none inside a bracket.
The gap before a comment at the end is kept, it may line comments up.
```

### start

```code
start a child
```

### start

```code
start a child
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

return as type num even for fixed point
```

### str-to-real

```code
(str-to-real str) -> real

handles scientific notation like 1.5e-3 or 9.97231e-09
```

### str?

```code
(str? form) -> :t | :nil
```

### stream-diff

```code
(stream-diff a b c)

difference between streams a and b, write to stream c
outputs in standard "Normal diff" format
```

### stream-patch

```code
(stream-patch a b c)

patch stream a with stream b, write to stream c
accepts standard "Normal diff" format
```

### substr

```code
(substr text substr) -> matches
```

### swap

```code
(swap list idx idx) -> list
```

### sym?

```code
(sym? form) -> :t | :nil
```

### task-mboxes

```code
(task-mboxes size) -> ((task-mbox) [temp_mbox] ...)
```

### task-nodeid

```code
(task-nodeid [mbox]) -> nodeid
```

### task-timeout

```code
(task-timeout s) -> ns
```

### texture-metrics

```code
(texture-metrics texture) -> (handle width height)
```

### time-from-date

```code
(time-from-date td) -> secs
```

### time-in-seconds

```code
(time-in-seconds time) -> str
```

### timezone-init

```code
(timezone-init tz_loc) -> tz
```

### timezone-lookup

```code
(timezone-lookup query [field]) -> tz | :nil
```

### timezone-offset-seconds

```code
(timezone-offset-seconds tz) -> seconds
```

### tok-rule

```code
(tok-rule tokens body i n prule pargc) -> token

the token that opens a form, with the rule of the form, whether the
form must be broken, and its operator, kept on the end of it. They are
worked out the once, the first time the token is come to.
```

### transfer

```code
(transfer src_map dst_map [key val] ...) -> dst_map

transfer a list of [key val]
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
(tree-load stream) -> tree | :nil

:nil if there is no stream, or nothing in it to read
```

### tree-node

```code
(tree-node ((type [buckets])) -> collection
```

### tree-save

```code
(tree-save stream tree [key_filters]) -> tree | :nil
```

### tree-type

```code
(tree-type collection) -> type
```

### trim

```code
(trim str [cls]) -> str

when it is all cls the end is before the start, and slice would reverse
```

### trim-end

```code
(trim-end str [cls]) -> str
```

### trim-start

```code
(trim-start str [cls]) -> str
```

### tsort

```code
(tsort roots dep_fnc) -> order

iterative topological sort using a heap-allocated DFS stack
```

### type-to-size

```code
(type-to-size sym) -> num
```

### ui-merge-props

```code
(ui-merge-props props) -> props
```

### ui-save

```code
(ui-save stream view) -> tree | :nil
```

### ui-tool-tips

```code
(ui-tool-tips view tips)
```

### unbreak

```code
(unbreak data) -> (lines frozen)

take out the line breaks inside forms, so the layout is then made
from nothing but the code. A line stays a line of its own if it is
blank, a comment, follows a comment, is in a string or the help text
of a command, or starts with a form the source scanners look for.
A frozen line is one that must not then be broken. The doc builder
reads the comments under a definition, and the lines of a key map.
```

### unique

```code
(unique seq) -> seq
```

### unzip

```code
(unzip seq cnt) -> seqs
```

### url-decode

```code
(url-decode str [query_flag]) -> str
```

### url-encode

```code
(url-encode str [query_flag]) -> str
```

### url-format

```code
(url-format u) -> str
```

### url-parse

```code
(url-parse url_str) -> pmap
```

### url-path-query

```code
(url-path-query u) -> str
```

### url-query-format

```code
(url-query-format q) -> str
```

### url-query-parse

```code
(url-query-parse query_str) -> pmap
```

### url-scheme-port

```code
(url-scheme-port scheme) -> num
```

### usort

```code
(usort list [fcmp start end]) -> list
```

### vector-bounds-2d

```code
(vector-bounds-2d paths) -> (min_v2 max_v2)
```

### vector-bounds-3d

```code
(vector-bounds-3d verts [stride]) -> (min_v3 max_v3)
```

### vector-bounds-sphere

```code
(vector-bounds-sphere verts [stride]) -> (center_v3 radius)
```

### vector-point-in-polygon

```code
(vector-point-in-polygon p paths winding_mode) -> :t | :nil
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

### within-compile-env

```code
(within-compile-env lambda)
```

### zip

```code
(zip seq ...) -> seq
```

