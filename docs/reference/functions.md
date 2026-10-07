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

### action-resized

```code
the host window is a new size
```

### action-shown

```code
the host window is on show again
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

### canvas-shader-create

```code
(canvas-shader-create vertex fragment) -> 0 | shader

a shader from the text of its vertex and fragment stages
```

### canvas-shader-destroy

```code
(canvas-shader-destroy shader) -> shader
```

### canvas-shader-format

```code
(canvas-shader-format) -> 0 | 1 | 2

the shading language the host GUI driver takes for a shader,
0 if it can not draw one, 1 for MSL, 2 for SPIR-V
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

### cpu-arith

```code
one step of + - * /, by the types of the two sides
```

### cpu-block

```code
forms for a block. With no flag it is the body of the function,

and the value of the last form is the return value. With a flag
it is the body of a loop, and the flag is set to leave the loop.
```

### cpu-exits?

```code
does this block hold a return, or a break of the loop it is in ?
```

### cpu-fold

```code
run it now if all the args are constants
```

### cpu-loop-return?

```code
does this block hold a return from inside a loop ?
```

### cpu-op

```code
a built in op
```

### cpu-pow

```code
whole part of the power by squaring, the rest by repeated roots
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

### exfat-add

```code
a new file or directory, of this data, in the directory it names
```

### exfat-alloc

```code
(exfat-alloc vol want) -> :nil | (clusters no_fat)

clusters for a file. A run of free clusters if there is one long
enough, and the file then needs no chain. If not, the first free
clusters there are, with a chain through them. :nil if there are
too few. The clusters are marked as used.
```

### exfat-begin

```code
(exfat-begin vol) -> vol

the start of some writing. Every (exfat-begin) has its (exfat-end), and
they can be one inside another. The writing in between goes to the
device at the last (exfat-end), so many small files cost little more
than one.
```

### exfat-bitmap-save

```code
the bitmap back to the clusters it is kept in, the ones that changed
```

### exfat-block

```code
a block of the device, from those kept if it is there
```

### exfat-busy

```code
the volume says of itself that it is being written, and that goes to

the device at once, before anything else does. If the device is pulled
out, or the power goes, whoever mounts it next can tell.
```

### exfat-chain

```code
(exfat-chain vol start len no_fat) -> (cluster ...)

the clusters of a file or a directory. A len of :nil is all of the chain
```

### exfat-checksum

```code
the checksum of the entries of a file, all but the checksum itself
```

### exfat-cluster-offset

```code
where a cluster is on the device, the first is cluster 2
```

### exfat-data

```code
(exfat-data vol start len no_fat [keep]) -> str

the bytes of a file or a directory, a run of clusters is one read.
keep is for a directory, its blocks are kept
```

### exfat-delete

```code
(exfat-delete vol path) -> :nil | :t

a file, or a directory that has nothing in it
```

### exfat-dir

```code
(exfat-dir vol entry) -> ((name dir size start no_fat index count) ...)

what is in a directory
```

### exfat-dir-put

```code
entries to a directory, from an index on. They can cross from one

cluster of the directory to the next, so they go an entry at a time
```

### exfat-drop

```code
a block is no longer kept, and is not to be written
```

### exfat-end

```code
(exfat-end vol) -> vol
```

### exfat-entries

```code
(exfat-entries vol start len no_fat) -> ((name dir size start no_fat index count) ...)

the files and directories of a directory. A file is a set of entries,
one for the file, one for its data, then as many as its name needs
```

### exfat-eset

```code
the entries of what was found, as they are in the directory above it
```

### exfat-fat-set

```code
the cluster that follows a cluster, to the allocation table
```

### exfat-find

```code
(exfat-find vol path) -> :nil | (name dir size start no_fat index count)
```

### exfat-flush

```code
(exfat-flush vol) -> vol

the blocks that were changed, to the device, in the order they are on
it, and those that are next to each other as one write
```

### exfat-format

```code
(exfat-format path | stream size [label cluster_shift]) -> :nil | :t

a new volume of that many bytes with nothing in it. A path is a new
file of the host, the image of a disk. A stream is written where it
is, and must be that long, a memory stream of zeros say.
cluster_shift is the sectors of a cluster as a power of 2, it is 4096
byte clusters up to 256MB, 32768 up to 32GB and 131072 over that if
it is not given.
```

### exfat-free

```code
(exfat-free vol) -> bytes

the room there is left, the clusters that are not in use
```

### exfat-grow

```code
(exfat-grow vol found) -> :nil | found

a directory is given one more cluster. found is the entries from the
root down to it, and comes back with the directory as it now is.
```

### exfat-hash

```code
the hash of a name, of its upper case, a byte at a time
```

### exfat-keep

```code
a block is kept. When there are too many, those that were changed go

to the device and all are let go
```

### exfat-list

```code
(exfat-list vol path) -> :nil | ((name dir size start no_fat index count) ...)
```

### exfat-load

```code
(exfat-load vol path) -> :nil | str
```

### exfat-mkdir

```code
(exfat-mkdir vol path) -> :nil | :t

a new directory, of one cluster with nothing in it
```

### exfat-mount

```code
(exfat-mount path | stream) -> :nil | vol

a path is a file of the host that is the image of a disk, opened
to be read and written. A stream can be a memory stream.
```

### exfat-name

```code
a name is 16 bit characters, low byte first, here made UTF-8
```

### exfat-next

```code
the cluster that follows this one, from the allocation table
```

### exfat-path

```code
(exfat-path vol path) -> :nil | (root ... entry)

the entries from the root down to a path
```

### exfat-raw-read

```code
whole blocks from the device, :nil if they are not all there
```

### exfat-raw-write

```code
whole sectors to the device
```

### exfat-read

```code
(exfat-read vol offset len [keep]) -> str

bytes of the device. A block that is kept comes from where it is kept,
it may have been changed. A run of blocks that are not is one read of
the device, and they are kept if the run is short or if keep is asked
for, as it is for a directory, which is read over and over.
```

### exfat-release

```code
the clusters of a file are free again
```

### exfat-rename

```code
(exfat-rename vol from to) -> :nil | :t

a file or a directory is given a new name, a new directory to be in,
or both. Its data is not moved. :nil if there is one of that name
there, or if a directory would end up inside itself.
```

### exfat-replace

```code
a file that is there is given new data. Its entries stay where they

are and are changed. The new data is put beside the old, which is let
go when the new is in place. Only if there is no room for both is the
old let go first.
```

### exfat-root

```code
the root as an entry, its length is not kept, its chain is followed
```

### exfat-same?

```code
are two names the same, as the volume sees it, with no regard to case
```

### exfat-save

```code
(exfat-save vol path data) -> :nil | :t

a file of this data. One of that name is replaced, and is still there
as it was if there is no room for the new data.
```

### exfat-scan

```code
look for a run of free clusters from one cluster up to another. The

first few free ones are noted on the way, in case there is no run.
A byte of the bitmap that is all in use is stepped over in one.
```

### exfat-set

```code
(exfat-set vol name dir size start no_fat) -> str

the entries of a file, one for the file, one for its data, and as
many as its name needs, with the checksum of them all
```

### exfat-slot

```code
(exfat-slot vol found want) -> :nil | (index found)

room for that many entries, one after another, in a directory. A
deleted entry is room, and so is everything from the end mark on.
If there is none the directory is grown, till there is.
```

### exfat-split

```code
(exfat-split path) -> (parent name)
```

### exfat-stamp

```code
the time now, as a directory entry keeps it, months from 1
```

### exfat-store

```code
data to clusters, a run is one write. What is left of the last block

is made zero. The rest of the last cluster is not written, nobody is
given it to read, and a directory is always whole clusters
```

### exfat-sum

```code
the checksum of the boot sectors and of the upper case table, 32 bit
```

### exfat-sync

```code
(exfat-sync vol) -> vol

all that was written is on the device. A file stream holds its last
write till it is flushed or closed, and a task that ends with the
stream open would lose it. A memory stream is not flushed, a flush
ends the string where it stands.
```

### exfat-units

```code
(exfat-units name) -> (unit ...)

a UTF-8 name as the 16 bit characters the disk holds
```

### exfat-upcase-load

```code
the table of the volume, the upper case of every character in turn.

0xffff then a count stands for that many that are their own upper case
```

### exfat-upcase-table

```code
(exfat-upcase-table) -> str
```

### exfat-used?

```code
is a cluster in use, the bitmap has a bit for each, cluster 2 is bit 0
```

### exfat-walk

```code
(exfat-walk vol path fnc) -> vol

(fnc path entry)
every file and directory under a path, a directory before what is in
it. It keeps a list of the directories still to do, it does not call
itself, so the depth of the tree costs no stack.
```

### exfat-write

```code
(exfat-write vol offset data) -> vol

bytes to the device. A run of whole blocks goes straight to it. The
rest, the ends of a long write and all of a short one, change the
blocks that are kept, a block that is only part written is read first.
```

### exfat-zeros

```code
(exfat-zeros len) -> str

always a string of its own, it is often changed in place
```

### exfat-zone

```code
the time zone a stamp is in, quarter hours from UTC, and the bit

that says it is known, or a reader takes the stamp as its own time
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

### fcluster

```code
the boot sectors, 12 of them, and the same again as a spare
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

### grow

```code
start children till the herd is the size the worker nodes call for
```

### gui-rpc

```code
(gui-rpc (view cmd) -> :nil | view
```

### handler

```code
(handler state page line) -> state
```

### held-mouse-id

```code
the view the mouse is on. While a button is held that is the view it

went down on, a window being dragged can lag behind the pointer, and
to look under the pointer again would let go of it
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
(lisp-nodes [system]) -> nodes

the nodes known. With a system id, only those on that machine, and
with :t, only those on the same machine as this node, so those that
share its file system.
```

### lisp-systems

```code
(lisp-systems) -> systems

the system ids of the machines known, this one first. A node not yet
heard from has no system id, all zero.
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

### msl-name

```code
a name is given a trailing _ so it can not be a word of MSL, half say
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

wait till the number of nodes known has stayed the same for a while.
A network that never settles must not hang the caller, so it gives
up, with what it has, after 20 times as long as a settled one takes.
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
for each processor, default 1, and no more than 32, then wait for
them to be seen, so what is started next can spread over them. One
for each is where a full build is quickest, more only share them out.
```

### node-link

```code
(node-link name) -> net_id

start a shared memory link on this node. The node at the other end
starts one of the same name.
```

### node-spawn

```code
(node-spawn [num kind script]) -> (pid ...)

start num more nodes on this machine, default 1, each linked to this
node and to each other. A pid of -1 is a node the host could not start.
kind is the host program, :gui or :tui, this node's own if not given,
so a GUI node can be added to a TUI network, and a TUI node to a GUI
one. script is run on each new node. A node with a script is a front
of its session, a way in to it, a desktop is service/gui/app.lisp.
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

### pixmap-key

```code
(pixmap-key) -> str

a name for a shared pixmap that no other task will come up with
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

### sh-block

```code
statements in a scope of their own
```

### sh-const-int

```code
a loop bound, an int literal or an int constant
```

### sh-decimal

```code
the Lisp reader gives a float literal as a 16.16 fixed, which is good

to 4 decimal places, so that is what it is rounded to. A number that
needs more is written as a str, and is taken as it stands.
```

### sh-float-to-real

```code
a real from the bits of a 32 bit float
```

### sh-literal

```code
default, min or max of an input
```

### sh-name

```code
a new name must be a plain symbol, and must not hide anything
```

### sh-op-type

```code
result type of a built in op, or :nil if the types are wrong
```

### sh-real-to-float

```code
the bits of a 32 bit float, from a real, a fixed or an int
```

### sh-returns?

```code
does every path through the block end at a return ?
```

### sh-swizzle

```code
component indices of a swizzle like :xyz or :rgb
```

### sh-tail

```code
the last form of a function is its value, as in Lisp, so if it is not

a statement it is returned. And so for the last form of each arm of an
if that is last, and of a progn that is last. The back ends still see
a return, they are statement languages.
```

### shader-compile

```code
(shader-compile forms) -> program
```

### shader-cpu

```code
(shader-cpu program) -> lambda

(lambda x y x1 y1 input ...) -> (vec4 ...)
the lambda shades the pixels of the tile, row by row, the
centre of pixel x y is at frag coord x + 0.5, y + 0.5
```

### shader-cpu-args

```code
(shader-cpu-args program [((name val) ...)]) -> (val ...)

the input args for the lambda, defaults for those not given
```

### shader-dim

```code
(shader-dim type) -> :nil | 2 | 3 | 4
```

### shader-glsl

```code
(shader-glsl program) -> str
```

### shader-gui

```code
(shader-gui program) -> :nil | shader

a shader the GPU can draw into a canvas, with (. canvas :shade shader
block), where block is from (shader-pack). :nil if this host can not.
The driver builds it in its own time, till then :shade draws nothing.
```

### shader-layout

```code
(shader-layout program) -> (size (name type offset) ...)
```

### shader-load

```code
(shader-load file) -> program
```

### shader-msl

```code
(shader-msl program) -> str

the entry point is fragment_main
```

### shader-msl-vertex

```code
(shader-msl-vertex) -> str

the vertex shader that goes with every fragment shader, one triangle
that covers the target. Its uniform is the size of the target, and it
gives each pixel its frag coord, with y up. The entry point is vertex_main.
```

### shader-pack

```code
(shader-pack program [((name val) ...)]) -> block

a value not given is the default of the input. A float can be a
real, a fixed or an int, a vector is a sequence of them.
```

### shader-read

```code
(shader-read stream) -> forms
```

### shader-real

```code
(shader-real num) -> real

a fixed is rounded as a float literal of the language is
```

### shader-spirv

```code
(shader-spirv program) -> str

the entry point is fragment_main
```

### shader-spirv-vertex

```code
(shader-spirv-vertex) -> str

the vertex shader that goes with every fragment shader, one triangle
that covers the target. Its uniform, set 1 binding 0, is the size of
the target, and it gives each pixel its frag coord, with y up. The
entry point is vertex_main.
```

### shader-unpack

```code
(shader-unpack program block) -> ((name val) ...)

a float comes back as a real, a vector as a reals
```

### shader-vp

```code
(shader-vp program) -> (shade frame_size)

the native function for a program. It is assembled if this is
the first time this CPU has been given this program.
```

### shader-vp-argb

```code
(shader-vp-argb native frame x y x1 y1 [height]) -> str

the pixels of a tile, row by row, each a 32 bit argb. If the height
of the frame is given then row 0 is the top row, as a canvas has it.
```

### shader-vp-frame

```code
(shader-vp-frame program native [((name val) ...)]) -> frame

the frame the native function works in, with the inputs set
```

### shader-vp-pixels

```code
(shader-vp-pixels native frame x y x1 y1) -> (vec4 ...)

the pixels of a tile, row by row, each a reals of 4
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

### spv-block

```code
the statements of a block, up to the one that leaves it
```

### spv-block-var

```code
a uniform block of these members, -> (var_id ptr_type_id ...) the

pointer types are those of the members
```

### spv-const

```code
a float is given as the bits of it
```

### spv-emit

```code
an instruction that gives a value of this type, to the block
```

### spv-expr

```code
(spv-expr node) -> id
```

### spv-ext

```code
an instruction of the GLSL.std.450 set
```

### spv-fold

```code
an op of two, over more than two
```

### spv-function

```code
a function, fnc is given the ids of its parameters and makes its body
```

### spv-global

```code
a variable of the module
```

### spv-local

```code
a variable of the function, they all go at its start
```

### spv-memo

```code
the id of a type or a constant, made the first time it is asked for
```

### spv-module

```code
the bytes of the module, model is 0 vertex, 4 fragment
```

### spv-op

```code
an instruction, its first word has its length and its opcode
```

### spv-ptr

```code
a pointer type, class is 1 input, 2 uniform, 3 output, 6 private, 7 function
```

### spv-splat

```code
a float that goes with a vector is made a vector
```

### spv-str

```code
the words of a string, it ends with a zero byte
```

### spv-var

```code
(spv-var name) -> (id type)
```

### squeeze

```code
(squeeze body tokens) -> str

the line with one space between its tokens, and none inside a bracket.
The gap before a comment at the end is kept, it may line comments up.
```

### start

```code
start a child, on the worker nodes in turn
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

### sv-arith

```code
one step of + - * /, by the types of the two sides
```

### sv-branch

```code
code to jump to the label if the bool is as sense says
```

### sv-calls?

```code
does this expression call a function of the shader ?
```

### sv-const

```code
a float constant, to a new register
```

### sv-expr

```code
code for an expression, the registers that hold the value
```

### sv-floor

```code
floor of a register, in place
```

### sv-op

```code
a built in op
```

### sv-pow

```code
x to the power y, both kept, to a new register
```

### sv-sin

```code
sin or cos of a register, which is given up, to a new register
```

### sv-slots

```code
room in the frame for a value of this type
```

### sv-source

```code
-> (text frame_size)

the VP source of the native function for a program
```

### sv-spill

```code
save the live registers, all of them or those of a list
```

### sv-src

```code
a float goes with every component of a vector
```

### sv-var

```code
the frame offset of a variable, the last one of that name declared
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

