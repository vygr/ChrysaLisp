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

each pixel is scale by scale samples, default 1
```

### FLM-info

```code
(FLM-info stream) -> (width height type) | (-1 -1 -1)
```

### FLM-load

```code
(FLM-load stream) -> :nil | canvas
```

### JPG-info

```code
(JPG-info stream) -> (width height type) | (-1 -1 -1)
```

### JPG-load

```code
(JPG-load stream) -> :nil | canvas
```

### Mat3x2

```code
(Mat3x2 [a b tx c d ty]) -> matrix
```

### Mat3x2-box

```code
(Mat3x2-box box m) -> (x y x1 y1)

the box round a box that a matrix has moved. Each side is the least
or the most of what each of its two parts can come to
```

### Mat3x2-invert

```code
(Mat3x2-invert m) -> matrix | :nil

the matrix that undoes it. One that flattens everything to a line
has none, and that of no change is given
```

### Mat3x2-mul

```code
(Mat3x2-mul ma mb) -> matrix | :nil

the one matrix that does what mb does and then what ma does
```

### Mat3x2-point

```code
(Mat3x2-point m x y) -> (x y)
```

### Mat3x2-rot

```code
(Mat3x2-rot angle [cx cy]) -> matrix

by an angle in radians, the way from the x axis to the y axis, about
a point, default the origin
```

### Mat3x2-scale

```code
(Mat3x2-scale sx [sy cx cy]) -> matrix

about a point, default the origin
```

### Mat3x2-translate

```code
(Mat3x2-translate tx ty) -> matrix
```

### PNG-info

```code
(PNG-info stream) -> (width height type) | (-1 -1 -1)
```

### PNG-load

```code
(PNG-load stream) -> :nil | canvas
```

### PNG-save

```code
(PNG-save canvas stream) -> canvas

the canvas as a PNG of 8 bits of red, green, blue and alpha, the rows
filtered by none, a stage of a pipeline each as the load is: its rows,
deflate and the chunks
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

### TGA-save

```code
(TGA-save canvas stream type) -> canvas

a canvas as a .tga, not packed, 32 bits a pixel whatever type is,
the top row first. It is the pixels as they are with a header of 18
bytes before them, so it is quick, and most things that show
pictures show it
```

### WAV-head

```code
(WAV-head stream rate) -> stream

the head of a 16 bit mono file whose length is not known yet
```

### WAV-info

```code
(WAV-info stream) -> (rate channels bits bytes) | (-1 -1 -1 -1)

the stream is left at the first sample
```

### WAV-tail

```code
(WAV-tail stream rate bytes) -> stream

the length put in the head, once it is known
```

### XML-parse

```code
(XML-parse stream fnc_in fnc_out fnc_text)

break the stream into svg tokens, symbols, strings etc
parse the commands and attributes calling back to the user functions
```

### _structure

```code
a line of a structure, the base after it. A union is lines that all

start at the one base, and it ends where the longest of them does. A
union can have a union in it. This did call itself for each, by name,
and it is a function of a module, its name is gone once the module
has loaded, so a structure with a union in it could not be made. The
unions that are open are a list, each (lines next base longest)
```

### _structure_line

```code
a line that is not a union, the base after it
```

### aac-block

```code
(aac-block decoder packet) -> (channel ...) | :nil

the channels of a raw data block, the elements read till END, and how
many bits it took
```

### aac-book

```code
(aac-book codes) -> (fmap longest)

a book to read codes of: each code, its length high, to what it stands for
```

### aac-book-values

```code
(aac-book-values kind) -> (values kind)

of a book, each code's values for the native reader, lib/media/lisp.vp
media_aac_values: 6 bits each and 16 more than it is, the first highest,
signed as the book has them; and what kind of book it is to that reader
```

### aac-channel

```code
(aac-channel reader info rate_index [common]) -> channel

an individual channel stream: its gain, window, unless it has the one
of its pair, sections, scales, pulses, TNS and spectrum
```

### aac-config

```code
(aac-config setup) -> :nil | (type rate_index rate channels sbr ps)

what an AudioSpecificConfig says: the object type, 2 is AAC-LC, the
index of its rate, the rate, the channels, and if SBR and parametric
stereo are there, HE-AAC and its v2, the core then the type, ps :no if
it is said not to be, :nil if it is not said. They are
said first, types 5 and 29 before the core's, or after its config by
the sync extensions 0x2b7 and 0x548, which a decoder of AAC-LC alone
does not read
```

### aac-decode

```code
(aac-decode decoder packet) -> ((reals) ...)

the 1024 samples of each channel of a packet
```

### aac-decoder

```code
(aac-decoder config) -> decoder

what a channel carries from one block to the next: the half of its
window that overlaps the next, and the shape of its window. The native
of the DCT-IV is taken now, not as this is read
```

### aac-dequant

```code
(aac-dequant channel tables) -> reals

the 1024 lines of a channel, each |v|^(4/3) by the gain of its band,
the pulses added to them first; noise and intensity bands left 0
```

### aac-descriptor

```code
(aac-descriptor str at) -> (tag len body_at)

a descriptor of an esds, its tag, and its length of 1 to 4 bytes of
7 bits each, the top bit set but in the last
```

### aac-escape

```code
(aac-escape reader) -> num

a value of 16 or more of book 11: so many 1s and a 0, then 4 bits more
than there were 1s
```

### aac-esds

```code
(aac-esds str) -> :nil | str

the AudioSpecificConfig of what is in an esds box, past its version
```

### aac-huff

```code
(aac-huff reader table) -> index

the code next, what it stands for, by the native reader of a book's
table, lib/media/lisp.vp media_huff
```

### aac-huff-lisp

```code
(aac-huff-lisp reader book) -> index

the same in Lisp, the bits of the longest code looked at, one more
each time till one is a code: what the native one is checked against
```

### aac-ics-info

```code
(aac-ics-info reader rate_index) -> info

its window, its shape, how many bands are coded, and the groups of
short windows, how many windows in each, and the bands' bounds
```

### aac-imdct

```code
(aac-imdct decoder lines info channel) -> reals

the 1024 samples of a channel's lines: the IMDCT, windowed, its first
half added to what the block before left, its second half left
```

### aac-noise

```code
(aac-noise channel lines tables decoder)

each band of noise filled, random lines of the band's energy
```

### aac-pulses

```code
(aac-pulses reader) -> (start (offset amp) ...)
```

### aac-root

```code
(aac-root x n) -> real

the nth root of a real of 1 or more, by Newton's steps, from above it,
so each is less than the one before till it is there: a step that is
not less ends it, as the last bit may go to and fro
```

### aac-sbr

```code
(aac-sbr decoder element) -> sbr

the SBR of an element, made as it is first wanted
```

### aac-scales

```code
(aac-scales reader info sections global) -> ((scale ...) ...)

the scale of each band of each group: of a spectral book, from the
global gain by the differences; of intensity, its position; of noise,
its energy, the first of 9 bits of its own
```

### aac-sections

```code
(aac-sections reader info) -> ((book ...) ...)

the book of each band of each group, in runs
```

### aac-spectrum

```code
(aac-spectrum reader info sections) -> nums

the 1024 quantised lines, of a long window, or of 8 short ones one
after the other, 128 each. A group's lines are sent band by band, each
band of each of its windows in turn
each band of each window by the native reader, lib/media/lisp.vp
media_aac_values, as aac-values reads them one code at a time
```

### aac-stereo

```code
(aac-stereo left right l_lines r_lines tables)

of a pair with a common window: M/S, L = M + S and R = M - S, and
intensity, R the L of the band by its position, its sign by its book
and by the M/S mask
```

### aac-tables

```code
(aac-tables) -> tables

made the once, as the first decoder is
```

### aac-tns

```code
(aac-tns reader info) -> (((length direction (coef ...)) ...) ...)

the filters of each window, each over so many bands down from the top,
its direction and its coefficients, as the indices they were sent as
```

### aac-tns-apply

```code
(aac-tns-apply channel lines rate_index)

each filter of each window, its parcor coefficients to those of a
filter, run over its bands, up or down
```

### aac-track-config

```code
(aac-track-config track) -> :nil | config

of a sound track of an mp4, from its esds, or one in a QuickTime wave,
or as Matroska has it, the AudioSpecificConfig alone, :asc
```

### aac-type

```code
(aac-type reader) -> type
```

### aac-unfold

```code
(aac-unfold decoder lines n) -> reals
```

### aac-values

```code
(aac-values reader book) -> (value ...)

the 2 or 4 values of the next code of a book, signed
```

### aac-windows

```code
(aac-windows n) -> (sine kbd)

the rising halves of the windows, alpha 4 for the long, 6 for the short
```

### abi

```code
(abi) -> sym
```

### ac3-aht

```code
(ac3-aht decoder reader channel) -> reals

the mantissas of a channel of AHT for all 6 blocks, all sent in the
first: the gains of the gain adaptive quantisation read here, then by
the native reader, lib/media/lisp.vp media_ac3_aht, each bin by its high
efficiency bap, 0 noise, 1 to 7 a vector of 6 by one code, 8 and more 6
numbers, the DCT of 6 back across the blocks, by 2^-exponent, the
exponents the same in all 6: a reals of 6 blocks of 256
```

### ac3-aht-coeffs

```code
(ac3-aht-coeffs decoder reader channel blk) -> reals

a block's coefficients of a channel of AHT, its mantissas read in the
first block, by 2^-exponent
```

### ac3-bands

```code
(ac3-bands reader blk eac3 start end default struct) -> (struct sizes)

which subbands of 12 bins are in the band before, of coupling or of
spectral extension, read or left as they were, and the bands' sizes
```

### ac3-bap

```code
(ac3-bap decoder psd mask start end snr tab) -> bap

the bits of each mantissa, the address of the power above the mask,
by the table of AC-3 or of AHT. Native, lib/media/lisp.vp media_ac3_bap
```

### ac3-block

```code
(ac3-block decoder reader header blk) -> (reals ...)

a block: its strategies, coupling, exponents, the bit allocation where
it changes, and the mantissas, made coefficients, those of the coupled
channels by their coordinates; a reals of 256 a channel, 1 to fbw and
the LFE, by 2^-exponent
```

### ac3-bsi

```code
the rest of AC-3's: dialogue level, compression, language, production,

time codes or the other syntax of id 6, and more to step over
```

### ac3-decoder

```code
(ac3-decoder header) -> decoder

what is carried from block to block, and frame to frame, of each
channel: 0 the coupling channel, 1 to fbw the full ones, fbw+1 the LFE
```

### ac3-drc

```code
(ac3-drc code) -> real

a gain of dynamic range, 2^x by 1 to 2 in 32nds, x signed of 3 bits
```

### ac3-eac3-bsi

```code
the rest of E-AC-3's header, its stream id, dialogue level and

compression, the channel map of a dependent stream, mixing and
information metadata, all stepped over
```

### ac3-frame

```code
(ac3-frame decoder packet) -> ((reals ...) ...)

the samples of a frame, 256 a block a channel, each channel's blocks
one after another: the header, the frame's strategies, and each
block's coefficients through the IMDCT of 512, or two of 256 where a
channel switched to short blocks, the KBD window of alpha 5, and half
overlapped with the block before, as A/52 has it in its halves
```

### ac3-frame-info

```code
(ac3-frame-info reader header) -> header

of E-AC-3, the strategies for all the blocks of a frame: which syntax
each block has, coupling, the exponent strategy of each channel in
each block, AHT, SNR offset, and what is stepped over
```

### ac3-grouped

```code
(ac3-grouped codes per levels) -> (real ...)

the levels of each code of a grouped bap, per to a code, of a code v the
first v mod levels, the next of v by levels, the last what is left, in
the order a list of them is popped, the last first
```

### ac3-groups

```code
(ac3-groups reader strategy groups prev) -> (exp ...)

exponents sent as differences of 5 values, 3 to 7 bits, each from the
one before, each so many bins wide by its strategy
```

### ac3-header

```code
(ac3-header reader) -> :nil | header

the sync word, and what the frame is: AC-3 if the stream id is 10 or
less, E-AC-3 if 11 to 16; the size of the frame in bytes, the rate,
the blocks, the channels and if there is an LFE. What is only to
inform or to mix is stepped over
of AC-3 ids 9 and 10 are its half and quarter rates, not here, the
masking of the bit allocation by the rate's own bands
```

### ac3-mantissas

```code
(ac3-mantissas decoder reader channel groups) -> reals

the coefficients of a channel's bins, each its mantissa by 2^-exponent:
bap 1 2 and 4 are levels sent three or two to a code, those codes
shared by the channels of the block, groups; 3 and 5 levels of 3 and 4
bits; 6 to 15 numbers of so many bits; 0 is dither or nothing. Read by
the native reader, lib/media/lisp.vp media_ac3_mantissas
```

### ac3-mask

```code
(ac3-mask decoder band_psd start end fast_gain lfe dba) -> mask

the masking curve of each band: the excitation by fast and slow leaks
and low frequency compensation, the hearing threshold, native, lib/media/
lisp.vp media_ac3_mask, and the deltas
```

### ac3-psd

```code
(ac3-psd exps start end) -> (psd band_psd)

the power of each bin from its exponent, and of each band, the first 28
bins a band each, the bins of the others added as logs are, by the
table. Native, lib/media/lisp.vp media_ac3_psd
```

### ac3-spx

```code
(ac3-spx decoder header out)

the bins of spectral extension of each channel using it: the bins from
its start copied up, again from the start each time the copying comes
to where they are copied to, each band blended with noise to the
energy it is said to have, and a notch where a copy begins
```

### ac3-stereo

```code
(ac3-stereo decoder chans) -> (left right)

the full channels of a frame made two, as 16 bit samples are: by
-2^24, the sign the standard's, the size of mantissas by 2^-exponent
through the IMDCT as 16 bit samples, matched to afconvert's decode;
and of more than two the centre and the surrounds in
each side at 0.707, the sum held to full scale; the LFE left out
```

### action-maximise

```code
step zoom up to +zoom_max (3x)
```

### action-minimise

```code
step zoom down to +zoom_min (1x)
```

### action-pointer

```code
a pen or a finger. It goes to the view it is on, or the one it went

down on while it is down. The mouse is not moved by it
```

### action-quit

```code
launch logout app. A desktop never seen, -hidden, a test's, has nobody to

ask: its host says so, x 1, as SDL makes a TERM signal a quit, and all its
apps are closed and it goes, the node with it
```

### action-resized

```code
the host window is a new size
```

### action-shown

```code
the host window is on show again
```

### adler32

```code
(adler32 data [adler]) -> adler
```

### adpcm-ima-block

```code
(adpcm-ima-block str channels) -> (left right)

a block of IMA ADPCM, as WAV has it
```

### adpcm-ms-block

```code
(adpcm-ms-block str channels coefs) -> (left right)

a block of Microsoft ADPCM
```

### adpcm-ms-coefs

```code
(adpcm-ms-coefs extra) -> (coef1 coef2 ...)

the pairs of coefficients a format of Microsoft ADPCM says, after its
samples a block and how many pairs, or the seven every one has
```

### adpcm-pcm

```code
(adpcm-pcm str channels bits) -> (left right)

PCM of 8 bits, offset by 128, or of 16 or 24 signed, low byte first,
channels one after another in each frame
```

### aead-open

```code
(aead-open key nonce aad sealed) -> :nil | str
```

### aead-open-lisp

```code
(aead-open-lisp key nonce aad sealed) -> :nil | str
```

### aead-seal

```code
(aead-seal key nonce aad data) -> str
```

### aead-tag

```code
the tag of what was encrypted and what goes with it. The key of the

tag is the start of block 0 of the stream, and is for this nonce
alone. Each of the two is made up with 0 to a whole 16 bytes, and
how long they were comes last
```

### aes-block

```code
(aes-block keys block) -> str
```

### aes-block

```code
(aes-block keys block) -> str
```

### aes-count

```code
(aes-count counter n) -> str

the counter n blocks on, its last 4 bytes counted up, high first
```

### aes-count

```code
(aes-count counter n) -> str

the counter n blocks on, its last 4 bytes counted up, high first
```

### aes-ctr

```code
(aes-ctr keys counter data) -> str
```

### aes-ctr

```code
(aes-ctr keys counter data) -> str
```

### aes-key

```code
(aes-key key) -> keys

the round keys, 4 numbers of 32 bits a round, as a str
```

### aes-key

```code
(aes-key key) -> keys

the round keys, 4 numbers of 32 bits a round, as a str
```

### aes-sbox

```code
(aes-sbox) -> str

what each byte becomes, its inverse in the field turned by the affine
map of the standard: 3 is a generator, so its powers and their
logarithms give each inverse
```

### aes-sbox

```code
(aes-sbox) -> str

what each byte becomes, its inverse in the field turned by the affine
map of the standard: 3 is a generator, so its powers and their
logarithms give each inverse
```

### aes-sub

```code
each byte of a word through the box
```

### aes-sub

```code
each byte of a word through the box
```

### aes-tables

```code
(aes-tables) -> str

the four tables of a round, 256 numbers of 4 bytes each, then the box
itself, as the native code wants them. A byte of row r of a column
goes to the four rows as the column mixing has it
```

### aes-tables

```code
(aes-tables) -> str

the four tables of a round, 256 numbers of 4 bytes each, then the box
itself, as the native code wants them. A byte of row r of a column
goes to the four rows as the column mixing has it
```

### aes-xtime

```code
b times x in the field of AES
```

### aes-xtime

```code
b times x in the field of AES
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

### array??

```code
(array?? form) -> :t | :nil
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

### audio-add-rpc

```code
(audio-add-rpc file_path) -> handle
```

### audio-change-rpc

```code
(audio-change-rpc handle state)
```

### audio-hush-rpc

```code
(audio-hush-rpc who)
```

### audio-listen-rpc

```code
(audio-listen-rpc who) -> :t | :nil

who is to be sent what the microphone hears, again before the lease is
out to go on being. :nil if there is no microphone
```

### audio-play-rpc

```code
(audio-play-rpc handle [pan])
```

### audio-remove-rpc

```code
(audio-remove-rpc handle)
```

### audio-service

```code
(audio-service) -> :nil | net_id

the audio service to use. Each desktop has one of its own, on its
node, sound is played by the host of the node. An app with a window
is on its desktop's node and has that one, so a desktop that quits
takes its own sound with it and nobody else's. A task on a node with
no desktop has any one there is on the machine
```

### audio-stream-close-rpc

```code
(audio-stream-close-rpc handle)
```

### audio-stream-left-rpc

```code
(audio-stream-left-rpc handle) -> :nil | frames

how many frames of a stream are still to be played, what was written
less what has been heard: a player keeps its pictures to its sound so
```

### audio-stream-open-rpc

```code
(audio-stream-open-rpc [rate channels]) -> :nil | handle

a stream to play as it comes, :nil if there is no room or no sound. Of
16 bit samples, 1 or 2 channels, left first, at a rate of 8000 to
48000; 16000 mono, +sound_rate, if not said
```

### audio-stream-write-rpc

```code
(audio-stream-write-rpc handle data)

the next of a stream, 16 bit samples, a frame each of its channels,
no answer
```

### avi-avc

```code
(avi-avc stream video)

of a stream of H.264, its avcC, made of its first SPS and PPS if its
format has none, and the order its frames are shown in: by IDR, each
frame from one to the next in the order of its picture order count
```

### avi-avc-key?

```code
(avi-avc-key? str) -> :t | :nil

has a frame of H.264, its NALs each after a start code, an IDR slice
```

### avi-avc-packet

```code
(avi-avc-packet reel n) -> :nil | str

a frame of H.264 of Annex B as the host's decoder takes it, as an mp4
has it: each NAL after four bytes of its length, the delimiters left out
```

### avi-avc-poc

```code
(avi-avc-poc str nals sps) -> :nil | (idr ref lsb)

of a frame's first slice, is it an IDR, is it a reference, and its
picture order count's low bits
```

### avi-avcc-sps

```code
(avi-avcc-sps avcc) -> :nil | rbsp

the first SPS of an avcC, its bytes after its header
```

### avi-children

```code
(avi-children str at end) -> ((id body stop kind) ...)

the chunks of a str from at to end: its four letters, where what it
holds begins and ends, and of a LIST or RIFF the four letters of what
kind. A length past the end is the end, a file cut short
```

### avi-chunk

```code
(avi-chunk track offset size key)

a chunk of a stream's into its tables, where what it holds is in the
file, how long, a key or not
```

### avi-close

```code
(avi-close codec) -> :nil
```

### avi-codec

```code
(avi-codec reel [scale]) -> :nil | codec

Motion JPEG by the host's, the hardware's, if it has one, else ours
```

### avi-frame

```code
(avi-frame codec n) -> :nil | canvas
```

### avi-idx1

```code
(avi-idx1 stream at stop movi tracks) -> :t | :nil

the chunks of idx1, each 16 bytes, its id, flags, where it is and how
long. Where, from the movi's four letters as it should be, or of some
writers from the start of the file: the first is looked at to see
```

### avi-mjpeg-codec

```code
(avi-mjpeg-codec reel [scale]) -> codec

a picture of each frame by lib/image/jpg.inc, put on a canvas of its own
```

### avi-mjpeg-frame

```code
(avi-mjpeg-frame codec n) -> :nil | canvas

frame n, a picture of its own, made the size of the canvas and put on
it. One that does not decode leaves the one before
```

### avi-mp4v-headers

```code
(avi-mp4v-headers stream track) -> str

the VOS, VO and VOL of MPEG-4 Part 2: the extra bytes of its format if
they have its VOL, else the start of its first frame, all before its
first VOP
```

### avi-mp4v-key?

```code
(avi-mp4v-key? str) -> :t | :nil

has a frame of MPEG-4 Part 2 a VOP coded as I: its start code, 0x1b6,
and the two bits after it 0
```

### avi-nals

```code
(avi-nals str [prefix]) -> ((type at end) ...)

the NALs of a frame: each after a start code, Annex B, or with prefix,
of an avcC, each after a number of that many bytes high first
```

### avi-number

```code
(avi-number id) -> :nil | num

the stream a chunk of the movi is of, the two digits it begins with
```

### avi-odml

```code
(avi-odml stream track) -> :t | :nil

the OpenDML index of a stream: its indx, of the ix chunks each of a
part of it, each a base and the chunks from it, a top bit set not a key
```

### avi-odml?

```code
(avi-odml? track) -> :t | :nil

has a stream an OpenDML index, an indx of indexes of chunks
```

### avi-packet

```code
(avi-packet reel n) -> :nil | str

the bytes of frame n of the pictures, as the file has them
```

### avi-rbsp

```code
(avi-rbsp str at end) -> str

a NAL's bytes after its header, each 3 that follows two 0s taken out
```

### avi-read

```code
(avi-read stream at len) -> str

bytes of the file, as many as it has from at
```

### avi-reel

```code
(avi-reel stream) -> :nil | reel

the reel of the first stream of pictures of an AVI, every stream in
:tracks. Its chunks from the OpenDML index if there is one, else idx1,
else as the movi are read. Each a time a frame of its own, the rate
its strh says, in the order they are in the file
```

### avi-scan

```code
(avi-scan stream movis tracks)

the chunks of each movi read one after another, as there is no index
to say: a file cut short, or one whose index is lost. A LIST rec in it
is gone in to. Which are keys is not said: of a picture of one frame
each, Motion JPEG, all are, of the rest what their own bytes say
```

### avi-scan-keys

```code
(avi-scan-keys stream track) -> (n ...)

the keys of a stream of pictures from their own bytes: of Motion JPEG
each, of MPEG-4 a VOP coded as I, of H.264 an IDR slice, of DivX 3 a
frame whose first bit is 0
```

### avi-se

```code
(avi-se reader) -> num
```

### avi-sps

```code
(avi-sps rbsp) -> (frame_bits poc_type poc_bits frame_mbs_only)

of an SPS what a slice header's widths are
```

### avi-start-code

```code
(avi-start-code str byte) -> :nil | index

where a start code, 00 00 01 and that byte, is first in a str
```

### avi-stream

```code
(avi-stream str body stop) -> track

a strl: its strh, strf and indx, what kind, its codec as an mp4's,
its size or its sound
```

### avi-tops

```code
(avi-tops stream end) -> ((id at stop kind) ...)

the chunks at the top of each RIFF of the file, where each begins in
the file, its hdrl and the movi of each read later as they are wanted
```

### avi-track-stream

```code
(avi-track-stream reel track) -> stream

all of a stream's chunks, one after the other, as a stream of its own
that can be seeked, wherever they are in the file: a new one each time
```

### avi-ts-fill

```code
(avi-ts-fill st) -> :nil | str

what is next, the rest of the chunk it is in, no more than 64KB
```

### avi-ts-piece

```code
(avi-ts-piece st at) -> index

the chunk that byte at of the stream is in, by halves of the starts
```

### avi-ts-seek

```code
(avi-ts-seek st offset whence) -> pos
```

### avi-ue

```code
(avi-ue reader) -> num

a number of Exp-Golomb: so many 0s, a 1, and that many bits
```

### avi-word

```code
(avi-word str at) -> num

four bytes, low first
```

### backdrop-lines

```code
(backdrop-lines origin gap extent) -> (pos ...)

where the lines of a ruling fall across an extent of pixels, 0 up to
it: one is at origin, and the rest are gap apart each way from it.
origin and gap can be whole numbers or have parts, a line is on the
pixel nearest where it falls. None if they are too close to tell apart
```

### ball-rows

```code
(ball-rows centre radius mat4x4_obj mat4x4_frust height) -> (y y1)

the rows of a frame of that height a ball may be on, as it is seen.
All of them if it is at the eye or behind it. The ball is as big as
the most the matrix of its object stretches anything
```

### base64-decode

```code
(base64-decode text) -> :nil | str

:nil if what is left at the end is not whole bytes
```

### base64-encode

```code
(base64-encode data) -> str
```

### bindings-default

```code
(bindings-default) -> bindings

how a board starts out. A pen draws, and rubs out with its other end
or with the button on its side held, Chris: "I frequently had left
pen bound to draw and right pen button bound to erase !" The mouse
draws with its left button, moves things with its right and the
board itself with its middle. A finger moves things. A main button
holds the board's first pen, black, a second button its second,
white, which is the fill of what the device draws, Chris: "I would
have left/right default to black/white"
```

### bit-mask

```code
(bit-mask mask ...) -> val
```

### bitcnt

```code
(bitcnt n) -> num bits
```

### bits-at

```code
(bits-at reader) -> num
```

### bits-huff

```code
(bits-huff reader table) -> index

the native gives where it ends in the low 32 bits and the index above
```

### bits-of

```code
(bits-of str) -> reader
```

### bits-seek

```code
(bits-seek reader at) -> reader
```

### bits-skip

```code
(bits-skip reader n) -> reader
```

### bits-str

```code
(bits-str reader) -> str
```

### bn-add

```code
(bn-add a b) -> nums
```

### bn-addmod

```code
(bn-addmod a b modulus) -> nums

a and b each less than the modulus
```

### bn-bit

```code
(bn-bit a i) -> 0 | 1
```

### bn-bits

```code
(bn-bits a) -> num

how many bits a has, to its top bit that is set
```

### bn-bytes

```code
(bn-bytes a len) -> str

len bytes of a, the top byte first, 0s in front as need be
```

### bn-cmp

```code
(bn-cmp a b) -> -1 | 0 | 1
```

### bn-from-bytes

```code
(bn-from-bytes data) -> nums

a number from its bytes, the top byte first, as DER and TLS have them
```

### bn-from-num

```code
(bn-from-num n) -> nums
```

### bn-invmod

```code
(bn-invmod a modulus) -> nums

1 over a, mod a prime: a to the prime less 2
```

### bn-mod

```code
(bn-mod x modulus) -> nums
```

### bn-modulus

```code
(bn-modulus m) -> modulus

what (bn-mod) takes a number mod: m, and m moved up till the top bit
of its top limb is set, and by how much
```

### bn-mul

```code
(bn-mul a b) -> nums

each limb of the product a dot of a part of a and a part of b turned
round, then the carries
```

### bn-mulmod

```code
(bn-mulmod a b modulus) -> nums
```

### bn-norm

```code
(bn-norm a [from]) -> a

each limb back to 26 bits, the carry, or the borrow, taken on up from
limb from, and new limbs at the top for what is over. A number that
has gone under 0 is an error
```

### bn-pad

```code
(bn-pad a n) -> nums

a with limbs of 0 at the top, to n limbs
```

### bn-powmod

```code
(bn-powmod a e modulus) -> nums

a to the power e, mod. e a big number, from its top bit down
```

### bn-reduce

```code
(bn-reduce x up) -> nums

x mod up, the top bit of up's top limb set. A digit of the quotient
at a time, from x's top two limbs and up's top limb and 1, so never
too big, and what it times up is taken from x
```

### bn-shl

```code
(bn-shl a s) -> nums

a moved up s bits, s less than 26
```

### bn-shr

```code
(bn-shr a s) -> nums

a moved down s bits, s less than 26, what falls off gone
```

### bn-sub

```code
(bn-sub a b) -> nums

a less b, b not more than a
```

### bn-submod

```code
(bn-submod a b modulus) -> nums

a and b each less than the modulus
```

### bn-trim

```code
(bn-trim a) -> nums

without the limbs of 0 at the top, one left at least
```

### bn-zero?

```code
(bn-zero? a) -> :t | :nil
```

### bn-zeros

```code
(bn-zeros n) -> nums

n limbs of 0
```

### board-bite

```code
(board-bite doc swept [lines but tol user]) -> :nil | (made gone swaps)

user, whose eraser it is: a shape another made is left as it is, but
by the teacher's, +board_teacher, or with no user said. Chris: "students
could not erase things the teacher or the other student had written"
swaps is what stands in the place of each thing that was bitten,
((id (shape ...)) ...), for whoever had hold of it
tol, how near a line a point of a thing may be and be left out, see
(board-bite-shape)
but, the ids of things that are left as they are whatever covers
them: what somebody has hold of is theirs till they let it go
with lines :t a line, a thing part of which can be rubbed out as a
line, (cwb-rub?), is left as it is, it is cut another way
what swept covers, paths in the space of the document, taken out of
every thing of the layers that can be changed. The ids of the shapes
that were made, and of those they stand in the place of, :nil if
nothing was touched. A group whose things have all gone goes. A list
of what is left to do, no function here calls itself
```

### board-bite-shape

```code
(board-bite-shape item full m swept [tol]) -> :nil | (shape ...)

a shape with what swept covers taken out of it. swept is paths in
the space of the document, full the matrix that puts the shape there,
m the one that puts the space the shape is in there. :nil if it is
not touched. What is to stand in its place, in the space it is in:
nothing if all of it went, or all but a sliver, and the shape is then
out of the document.

What a line drawn by hand is drawn with has a great many points, ten
for each of its own, the round of every join: a long squiggle has
twenty thousand, and all that is done here is by how many there are.
So those that are within tol of the line between their neighbours
are left out first, a quarter of a pixel if not given, which is not
seen and is most of them
```

### board-copy-items

```code
(board-copy-items items) -> items

items that can be changed and leave those they were copied from as
they were. What a shape is flattened to goes with it, it is the same
```

### board-covered?

```code
(board-covered? doc ids) -> :t | :nil

is anything that is not one of these in front of one of them, and
over it, of the things of the document that are not in a group
```

### board-ghost-line

```code
(board-ghost-line paths zoom col) -> (shape shape)

a closed line of what is someone else's, a hold, a finger: of a color
with white dashes along it, 6 on and 6 off on the screen whatever the
zoom, so that it is seen over anything, dark or light
```

### board-hull

```code
(board-hull item) -> :nil | path

the hull of all a shape draws, in its own space, a path with no dent
in it that the shape is inside. Kept with what else is kept of the
shape, (cwb-flat), and worked out only when first asked for
```

### board-item-corners

```code
(board-item-corners item) -> :nil | (x y x y x y x y)

the four corners of the box of an item, (board-item-frame), where
they are in the space it is in. :nil if it draws nothing
```

### board-item-frame

```code
(board-item-frame item) -> :nil | (matrix (x y x1 y1))

the box of an item, and the matrix that puts it where it is: its own
box and its own matrix, turned and sized as it is. A box is always a
rectangle: a place has no slant, lib/cwb/doc.inc. :nil if it draws
nothing
```

### board-may-rub?

```code
(board-may-rub? user item) -> :t | :nil

may that user rub out or delete what that item is: the teacher's, or
one with no user said, takes anything, any other only what it made.
Chris: "students could not erase things the teacher or the other
student had written", and of a delete, the same
```

### board-none?

```code
(board-none? ink) -> :t | :nil

an ink that is wholly see through is none, whatever its colour
```

### board-pen-d

```code
(board-pen-d points) -> d

the path of a line drawn by hand, points is x y x y ... It goes
from the first to the last, and between is curved: each point is
where the line turns toward, and it passes half way between each two
```

### board-rub-group

```code
(board-rub-group doc group x y r) -> :nil | items

the items of a group with what of its lines is within r of a point,
of the space the group is in, rubbed out. :nil if none was touched.
What is in it that is not a line, or is a group itself, is left
```

### board-rub-look

```code
(board-rub-look paths zoom) -> shape

what an eraser is shown as, as far as it reaches, or the area it will
rub out: pale, with a grey line of dashes round it, 8 on and 4 off on
the screen whatever the zoom, a look nothing else has
```

### board-rubs?

```code
(board-rubs? bindings event mode) -> :t | :nil

does the pointer rub out: an eraser, or bound to, by its tool or by
its mode, the board's mode if it has none. Along an instrument's edge
what rubs out rubs out and all else draws, a finger too, Chris: "you
can hold an instrument with 2 fingers and draw your third or forth down
the edges, and they WILL draw !"
```

### board-snap

```code
(board-snap v step) -> v

to the nearest step, as it is if the step is 0
```

### board-tap?

```code
(board-tap? board state x y) -> :t | :nil

did a contact lift as a tap: soon, and near where it landed. A board
that is told no time, (:tick), goes by the distance alone
```

### board-theirs?

```code
(board-theirs? board id user) -> :t | :nil

held by a user other than that one. What no one holds, anyone may
take
```

### board-tool-of

```code
(board-tool-of board event) -> tool

what a pointer is bound to do: its tool, :hand :eraser :pan, or a pen
of what its mode draws, the board's if it has none, and select mode the
hand, so a mouse alone can do everything
```

### boot-files-file

```code
(boot-files-file cpu abi) -> path
```

### boot-funcs

```code
(boot-funcs funcs [all]) -> funcs

those of these functions an install's boot image holds, every one of
them with all
```

### boot-id

```code
(boot-id) -> hex | ""

the id of the boot image this node's machine has, "" if it was made
before there was one
```

### boot-id-file

```code
(boot-id-file cpu abi) -> path
```

### boot-id-of

```code
(boot-id-of files) -> hex

the id of those source files, whatever order they are given in. The
name of each and the hash of what is in it, all hashed
```

### boot-id-tree

```code
(boot-id-tree) -> :nil | hex

the id of the files this node's boot image was made from, as the tree
has them now, :nil if it was made before their names were kept. Not
the boot image's own id, it was made from other source than is here
```

### bot-breathe

```code
(bot-breathe now)

(task-breathe) of a task that is a bot, given the time: every bot of
the node rests at once, from the time it is due till +bot_rest after,
and one that comes to it in that time rests to its end
and only if an app of the node is ready, there is nobody to rest for
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

### byte-swap

```code
(byte-swap num width) -> num

the low width bytes of a number the other way about, the high first,
of 2, 4 and 8 by shifts, of other widths a byte at a time
```

### byte-to-hex-str

```code
(byte-to-hex-str num) -> str
```

### call-ask-up

```code
as (call-ask), the service of this desktop started first if it has

none: it is there only once a call is started or joined, a desktop
that never calls has none
```

### call-headphones

```code
(call-headphones on)

with headphones what is heard is not heard by the microphone, so all
may talk at once: half duplex off
```

### call-join

```code
(call-join id)

the call of that id, hex, (call-list)
```

### call-leave

```code
(call-leave)
```

### call-list

```code
(call-list) -> ((id name) ...)

the calls of the mesh
```

### call-mute

```code
(call-mute on)
```

### call-name

```code
a comma is not of a name, the mesh keeps one after one
```

### call-play

```code
(call-play path)

the sound file at path sent to the call in place of the microphone
till it ends, "" to stop it, a note played to the others
```

### call-put-away

```code
(call-put-away)

the app that showed the call is closed: out of it, unless another app
shows it as its own. A live microphone is never with nothing on the
screen to say so
```

### call-record

```code
(call-record on)

this desktop is recording the call, or not: all in it are told
```

### call-service

```code
(call-service) -> :nil | net_id

the voice service of this desktop, on its node, or any on the machine
```

### call-start

```code
(call-start name)

a new call, of that name, that this desktop is in
```

### call-state-read

```code
(call-state-read msg) -> :nil | state

what a watcher is sent: an Emap of :id, hex or :nil if in no call,
:name, :muted, :recording, :headphones, :inbox, where this one hears,
:members, a list of (name inbox rec) of all in it, rec 1 for one that
records, and :talking, the inboxes of those talking now
```

### call-tap

```code
(call-tap mbox)

mbox is sent all that is said and heard in the call, to be mixed,
lib/audio/voice.inc (voice-mix); :nil for none
```

### call-unwatch

```code
(call-unwatch mbox)
```

### call-user

```code
(call-user) -> name

who is asking, as the others in a call see them: the user of the app,
a service has none
```

### call-watch

```code
(call-watch mbox [shows])

mbox is sent the state of the call each time it changes, (call-state-read),
and now. Again within +call_lease to go on being. shows, if the app shows
the call as its own, a Whiteboard in its session's call: then the call
is not left when another that showed it is put away, (call-put-away)
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

### canvas-key

```code
(canvas-key canvas) -> 0 | key

the key of the shared memory the pixels of a canvas are in, 0 if
they are its own
```

### canvas-load

```code
(canvas-load file flags [swap_mode stream]) -> :nil | canvas

the picture of a file, its kind by its name, read from the stream if
given, as for an object of a store, lib/files/names.inc
```

### canvas-loader

```code
(canvas-loader ext info load [save])

files whose names end ext are read by these, (info stream) and
(load stream), and written by (save canvas stream type ...) if given
```

### canvas-loader-of

```code
(canvas-loader-of file) -> :nil | (ext info load save)
```

### canvas-mesh-create

```code
(canvas-mesh-create verts) -> 0 | mesh

the vertices of a mesh, kept on the GPU, from the bytes of a reals,
the attrs of a vertex one after another
```

### canvas-mesh-destroy

```code
(canvas-mesh-destroy mesh) -> mesh
```

### canvas-pair-create

```code
(canvas-pair-create vertex fragment layout) -> 0 | pair

a vertex shader and a pixel shader that draw triangles, from the text
of their stages. The layout is a byte for how many attrs a vertex
has, a byte for the cull, 0 none, 1 what faces away, 2 what faces us,
then a byte for the floats of each attr
```

### canvas-save

```code
(canvas-save canvas file type [optionals...]) -> :nil | canvas
```

### canvas-shade

```code
(canvas-shade col) -> col

a little darker, a quarter of the way to black, the edge of a thing
that is in shadow
```

### canvas-shader-create

```code
(canvas-shader-create vertex fragment) -> 0 | shader

a shader from the text of its vertex and fragment stages
```

### canvas-shader-destroy

```code
(canvas-shader-destroy shader) -> shader

a shader, or a pair
```

### canvas-shader-format

```code
(canvas-shader-format) -> 0 | 1 | 2

the shading language the host GUI driver takes for a shader,
0 if it can not draw one, 1 for MSL, 2 for SPIR-V
```

### canvas-shared

```code
(canvas-shared width height scale [key]) -> :nil | canvas

a canvas with its pixels in shared memory, that the nodes of this
machine can all draw on. With no key the pixels are made, and
(canvas-key) is the key of them. Given that key, and the same size,
a task on another node has a canvas on the same pixels. :nil if the
host has no shared memory, or there are no pixels of that key.
```

### canvas-tint

```code
(canvas-tint col) -> col

a little brighter, a quarter of the way to white, the edge of a thing
that the light is on
```

### cell-rows

```code
(cell-rows rows cols cell) -> (cells w h)

cells of a size, each row of (what val) starting a new line and going
on to the next after cols of them, from 0 0
```

### chacha20

```code
(chacha20 key nonce counter data) -> str
```

### char-be

```code
(char-be num width) -> str

a number as width bytes, the high byte first; (char) has the low first,
and makes them, of 1 to 8
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

### clamp

```code
(clamp x lo hi) -> x

x, no lower than lo and no higher than hi
```

### clip-get-rpc

```code
(clip-get-rpc) -> string
```

### clip-put-rpc

```code
(clip-put-rpc string)
```

### clip-service

```code
(clip-service) -> :nil | net_id

the clipboard service to use. Each desktop has one of its own, on its
node, it talks to the clipboard of the host through the GUI driver of
the node. An app with a window is on its desktop's node and has that
one, so a desktop that quits does not take the clipboard of another.
On a host with a clipboard of its own they are all that one clipboard.
A task on a node with no desktop has any one there is on the machine
```

### code-be

```code
(code-be str width [idx]) -> num

a number of width bytes from idx, the high byte first, as a file or a
protocol of the net has them; (code) has the low first, and reads them,
of 1 to 8
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
The forms of a block go onto a list. An if and a for have lists of
their own, for the blocks in them, and the rest of a block can go
inside the arms of an if that leaves it. Each is a thing to do, a
block, its flag and the list its forms go onto, on a list of those
still to do. No function here calls itself
```

### cpu-edge-cut

```code
where the edge from vertex a, in front of the near plane, to vertex b,

behind it, crosses the plane, every number of the vertex
```

### cpu-exits?

```code
does this block hold a return, or a break of the loop it is in ? The

blocks in it are asked in turn, from a list of those still to ask
```

### cpu-fold

```code
run it now if all the args are constants
```

### cpu-funcs

```code
-> the lambda of main, or of the function named

the constants worked out, and each function a lambda
```

### cpu-loop-return?

```code
does this block hold a return from inside a loop ?
```

### cpu-mat-mul

```code
a matrix times a matrix, each row of the one dotted with each

column of the other
```

### cpu-mat-vec

```code
a matrix times a vec4, each row of the matrix dotted with it
```

### cpu-mat-vec3

```code
the 3 by 3 of a matrix times a vec3
```

### cpu-near-cut

```code
(cpu-near-cut (v0 v1 v2)) -> ((v0 v1 v2) ...)

a triangle cut by the near plane, where z is -w, as the native code
cuts it. None, the triangle, a smaller one, or two
```

### cpu-op

```code
a built in op
```

### cpu-pow

```code
whole part of the power by squaring, the rest by repeated roots
```

### cpu-share

```code
that much of a 32 bit argb pixel, a share of 0 to 256
```

### cpu-texture

```code
the color of a picture at a place. tex is (width height pixels), of

(shader-texture). The place is 0 to 1 across and 0 to 1 down, from
the top left, and goes round again outside that. It is the one pixel
the place is in, no pixel is mixed with the next. One with no width
is no picture, and is nothing
```

### cpu-zero

```code
what a value of the type is before it is set
```

### crc32

```code
(crc32 data [crc]) -> crc
```

### create

```code
a child is started, the word that it has comes to the task mailbox.

If the children are to be away from this node, and there is another,
it is on one of the others, each in turn
```

### create-word

```code
tail is what follows the word, a space, or nothing for a part of a

word that goes on in the next line. With a link, where it goes, the
word is a Link, and is connected to the :link_event of the Md, if
whoever made the Md gave it one
```

### crypto-same?

```code
(crypto-same? a b) -> :nil | :t
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

### cwb-add

```code
(cwb-add doc item [index]) -> item

put an item on top of a layer, default the top one
```

### cwb-adopt

```code
(cwb-adopt doc item) -> item

give an item, and each item in it if it is a group, an id of this
document, if it has none
a list that grows as it is gone along, so ids are in the order of
the items, a group and then what is in it
```

### cwb-bounds

```code
(cwb-bounds items [m]) -> (x y x1 y1) | :nil

the box round all that the items draw, :nil if they draw nothing
```

### cwb-d

```code
(cwb-d [cmd | num] ...) -> d

a path from its commands and numbers, (cwb-d "M" 0 0 "L" 10 5 "Z")
```

### cwb-d-arc

```code
(cwb-d-arc cx cy r a0 a1 [pie]) -> d

part of a circle about a point, from one angle round to another the
way the angle grows, in radians from the x axis. A slice of pie, with
the two lines to the middle, if pie. Once round or more is a circle.

It is written as arcs of no more than a third of a turn each. The A
of a path says where an arc ends and not where its middle is, that
is worked out from the two ends, and when they are close together,
an arc most of the way round, or across from each other, half way,
there is little to work it out from: the arc came off its circle
```

### cwb-d-ellipse

```code
(cwb-d-ellipse cx cy rx [ry]) -> d

an ellipse about a point, a circle if only one radius is given
```

### cwb-d-line

```code
(cwb-d-line x y x1 y1) -> d
```

### cwb-d-paths

```code
(cwb-d-paths paths [closed]) -> d

a line through the points of each path in turn, each closed if said,
the dashes of (path-dash) say
```

### cwb-d-points

```code
(cwb-d-points points [closed]) -> d

a line through each point in turn, points is x y x y ..., back to
the first if closed
```

### cwb-d-rect

```code
(cwb-d-rect x y x1 y1 [r]) -> d

a box from one corner to the other, its corners round by r if given
```

### cwb-doc

```code
(cwb-doc [width height]) -> doc

a new document, of one empty layer. The background is a colour, 0
for none, what is behind shows. The style is what the app draws
behind it to work on, :plain :grid :lines or :axis, with :grid the
gap, it is not part of the picture. Nor is :view, how it was last
looked at, (x y zoom turn): the point of it at the top left of what
showed it, how big, and how many quarters turned. The things of a document are anywhere, what
shows it is a view of some of that, and :nil is from 0 0 at 1
```

### cwb-draw

```code
(cwb-draw canvas doc [m clip skip]) -> count

draw every layer that is not hidden, the first at the back. skip is
ids, of items of the layers, that are not drawn, those being moved
about, which are drawn over the rest
```

### cwb-draw-items

```code
(cwb-draw-items canvas items m [clip]) -> count

draw items on a canvas, by a matrix, :nil for as they are. With a
clip, a box in the canvas's space, a shape that is nowhere in it is
not drawn. How many shapes were drawn
```

### cwb-each

```code
(cwb-each doc fnc)

call (fnc item layer_index holder) for every item of the document,
those in groups too, the holder is the list it is in
```

### cwb-find

```code
(cwb-find doc id) -> :nil | (item layer_index holder)

an item by its id, with the layer it is in and the list that holds it
```

### cwb-fit

```code
(cwb-fit doc [pad]) -> :nil | (dx dy)

make the document the size of what is in it: every item of every
layer is moved, all by the same, so that the box round them all is
pad in from the top left, default 16, and the width and height are
that box and pad all round. How far they were moved, :nil for a
document with nothing in it, which is left as it is
```

### cwb-flat

```code
(cwb-flat shape) -> (fills strokes (x y x1 y1) | :nil)

there may be more after those three, kept with them: take it apart
with &ignore last. (board-hull) keeps the hull of a shape there
the polygons a shape is drawn with, those of its fill and those of
its stroke, and the box they are all in, in its own space. :nil for
the box of a shape that draws nothing. Worked out once and kept
```

### cwb-get

```code
(cwb-get item key) -> value
```

### cwb-group

```code
(cwb-group items [key val] ...) -> group
```

### cwb-hit

```code
(cwb-hit doc x y [tol m]) -> :nil | (item layer_index shape)

the item of a layer a point is on, the one on top, in a layer that is
not hidden or locked. The item is one of the layer's own, a group if
the shape that was hit is in one. m is the matrix the document is
seen by, if the point is in the space of a view of it
```

### cwb-id-set

```code
(cwb-id-set ids) -> ids | set

ids as what is quick to ask of: the list if it is short, or a set of
them. One that is a set already is that set
a set says it is a list too, so it is asked if it is a set, first
```

### cwb-id?

```code
(cwb-id? ids id) -> :nil | found

is an id one of these, a list or what (cwb-id-set) made of one
```

### cwb-in

```code
(cwb-in data) -> :nil | doc

a document from the tree of one, (cwb-out), of this version. Anything
else is not a document
```

### cwb-in-box

```code
(cwb-in-box doc box [m]) -> ((item layer_index) ...)

the items of the layers that are wholly in a box, as a drag round
them has it, not those of a layer that is hidden or locked
```

### cwb-inv

```code
(cwb-inv item) -> matrix | :nil

the inverse of an item's matrix, kept with it
```

### cwb-item

```code
(cwb-item type key_vals) -> item
```

### cwb-item-in

```code
(cwb-item-in tree) -> item

an item from what a file has of it. A key that is not known is kept,
in others, to be written back as it was.
Each item is put in its place in the list that is to hold it, which
is made first, with a place for each
```

### cwb-item-out

```code
(cwb-item-out item) -> tree

an item as it is in a file: its type, then each thing it has that is
not what it would have anyway, a key and its value. A group has its
items the same way. What is left to do is a list
```

### cwb-items

```code
(cwb-items doc [index]) -> items

the items of a layer, default the top one, the list itself
```

### cwb-layer

```code
(cwb-layer doc [index]) -> (name flags items)

a layer, default the top one
```

### cwb-load

```code
(cwb-load stream) -> :nil | doc

a document from a .cwb file
a file that is not a tree at all throws as it is read, where there
are errors to throw
```

### cwb-mat-paths

```code
(cwb-mat-paths m paths) -> paths

new paths, each moved by the matrix, the same ones if it is :nil
```

### cwb-mat-place

```code
(cwb-mat-place m) -> (place slant)

a matrix as a place, and the slant that is left over, the part of it a
place can not say: m is the place's matrix by (1 slant 0 0 1 0). It is
0.0 for a matrix made of moves, turns and sizes alone
```

### cwb-new-id

```code
(cwb-new-id doc) -> id
```

### cwb-num

```code
(cwb-num n) -> str

a number as a path has it, no more of it than there is
```

### cwb-out

```code
(cwb-out doc) -> emap

a document as the tree a .cwb file is, to be saved by itself or as a
part of something else that is
```

### cwb-outline

```code
(cwb-outline shape) -> ((closed path) ...)

the lines of a shape, each open or closed, in its own space. The
letters of a :text are closed. A line of less than two points is
left out
```

### cwb-paper

```code
(cwb-paper canvas width height back style gap [y y1]) -> canvas

on a canvas that is width by height: back, a colour, 0 for plain
paper, and the lines of the style, gap apart. Only rows y up to y1 if
given. It is all boxes, so that a clip, if the canvas has one, holds
```

### cwb-pick

```code
(cwb-pick items ids [out]) -> items

those of the items whose id is one of the ids, in the order they are
in, or with out those whose id is not
```

### cwb-place

```code
(cwb-place item) -> place
```

### cwb-place!

```code
(cwb-place! item place) -> item

put an item in a place, and work out its matrix and the inverse of that
```

### cwb-place-mat

```code
(cwb-place-mat place) -> matrix | :nil

the matrix that does what a place says: sized, turned, then moved
```

### cwb-remove

```code
(cwb-remove doc id) -> :nil | item

take an item out of the document, a group goes with all in it
```

### cwb-remove-ids

```code
(cwb-remove-ids doc ids) -> doc

take many items out at once. Those that are items of a layer, as
what is selected is, go in one pass of each layer. Any that are not,
inside a group, are then taken out one by one
```

### cwb-rub

```code
(cwb-rub item x y r) -> :nil | items

an item that (cwb-rub?) with what of it is within r of a point, of
the space it is in, rubbed out. The lines that are left, each a new
item like it, none if all of it went. :nil if none of it was that
near. An end that was its own keeps how that end was drawn, an arrow
say, and an end that was cut is round
```

### cwb-rub-points

```code
(cwb-rub-points p cx cy r) -> :nil | ((x y x y ...) ...)

a line through points, x y x y ..., with what of it is within r of a
point taken out. What is left, each a line of two points or more,
cut where it meets the circle, to the nearest sixteenth, a root is
not exact and a path is text. :nil if none of it was within r
```

### cwb-rub?

```code
(cwb-rub? item) -> :t | :nil

can part of it be rubbed out: a line, one that is drawn and not
filled, and has an end. A box, a filled thing, words and a group can
not, each is all there or not there
```

### cwb-save

```code
(cwb-save doc stream) -> stream
```

### cwb-set

```code
(cwb-set item [key val] ...) -> item

set what an item has. What it was flattened to is let go of, unless
all that was set was its place, its name, its props or its user,
which do not change what it is flattened to. :place is a place, :m a
matrix, which is made a place, (cwb-mat-place), and its slant, if it
has any, left out: nothing is slanted
```

### cwb-shape

```code
(cwb-shape d [key val] ...) -> shape

a shape of that path, black, 2 wide and not filled unless it is said.
The keys are :fill :stroke, each a colour, 0 for none, :width, :rule,
:nonzero or :evenodd, :join, :miter :bevel or :round, :cap1 and :cap2,
its two ends, :butt :square :tri :arrow or :round, :m, :name, :props
```

### cwb-shape-hit?

```code
(cwb-shape-hit? shape m x y [tol]) -> :t | :nil

is a point on what a shape draws, its fill or its stroke, or within
tol of it, default 0
```

### cwb-shape-touch?

```code
(cwb-shape-touch? shape m box) -> :t | :nil

does what a shape draws, its fill or its stroke, touch a box of the
space m puts it in: is in it, crosses it, or has it inside. Not the
box round the shape: a line that goes corner to corner of its box is
not touched by a box in one of the other corners
```

### cwb-slot

```code
(cwb-slot key) -> index
```

### cwb-text

```code
(cwb-text text x y [key val] ...) -> shape

words, their left end and the line they sit on at that point. :font
and :font_size say which letters, they are filled, black unless it is said
```

### cwb-text-mid

```code
(cwb-text-mid text x y [key val] ...) -> shape

words with their middle at a point, to label a box by its middle say
```

### cwb-touch-box

```code
(cwb-touch-box doc box [m]) -> ((item layer_index) ...)

the items of the layers that a box touches, as a box dragged out to
select has it: an item any shape of which is in it, crosses it or has
it inside. Not those of a layer that is hidden or locked
```

### cwb-unslant

```code
(cwb-unslant item m) -> items

put an item where a matrix of what holds it says, as an ungroup puts
what was in a group: the group's matrix and the item's together. Most
are a place, and the item is put there, and is all there is. One that
slants, an item turned in a group that was sized more one way than the
other, is a place and a slant, and a place can not slant: the slant is
put into what it is. A group's is put into its items, the same way, and
a shape is made what it draws, its fill and its line, each a polygon,
slanted, as a bite makes it. Those new ones have no id. No function
here calls itself: what is still to do is a list, each an item, its
matrix and the list what it makes goes into
```

### cwb-walk

```code
(cwb-walk items m fnc)

call (fnc shape matrix) for each shape of the items, and of the
groups in them, the one at the back first. The matrix is the one that
puts the shape where it is in the space the items are in, given one
for that space, or :nil. A list of what is left to do, no function
here calls itself
```

### database-stale?

```code
the time of a file is to the second. A source written in the second

the cache was made in is taken to be newer than it: trace -w writes
sources just after it reads them, and the cache then said, to the
next that asked, what they had been
```

### date

```code
(date [secs]) -> (sec min hour day month year dotw)
```

### date-dos

```code
(date-dos stamp) -> (time date)

a (date) as MS-DOS keeps it, as zips and exFAT do: two seconds at a
time, months from 1, years from 1980, none before
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

### deflate-compress

```code
(deflate-compress in_stream out_stream [zlib]) -> :t

the bytes of in as DEFLATE, raw, or with zlib's head and sum round it.
32KB a block, of the fixed codes, a match looked for by native code at
the place the 3 bytes from each were last, as far back as 32KB
```

### der-at

```code
(der-at data at) -> :nil | (tag start end head)

the thing of DER at a place: its tag, where what is in it starts and
ends, and where its head starts, at
```

### der-bytes

```code
(der-bytes data item) -> str

what is in a thing, without its head
```

### der-in

```code
(der-in data item) -> ((tag start end head) ...)

the things inside a thing
```

### der-kids

```code
(der-kids data from to) -> ((tag start end head) ...)

the things one after the other from one place to another
```

### der-oid

```code
(der-oid bytes) -> str

an object identifier as it is written, 1.2.840.10045.2.1
```

### der-whole

```code
(der-whole data item) -> str

a thing with its head, as it was signed
```

### destroy

```code
a child is told to go, the job it had goes back on the queue
```

### dia-arrow

```code
(dia-arrow x y x1 y1 [heads]) -> group

a line from a point to a point, an arrow at its far end, or with
heads :none no arrow, :both one at each
```

### dia-box

```code
(dia-box text w h [fill size bold]) -> (group width height)

a box of a size that is given, with words in the middle of it. text
is a str, or a list of them for lines one under another, the first
bold if bold is :first
```

### dia-card

```code
(dia-card title under sections [fill]) -> (group width height)

a card: a band with a title and a line of smaller words under it,
then sections, each a heading and its rows, ((heading (row ...)) ...).
A section of many rows is set in columns. Its top left is at 0 0
```

### dia-class

```code
(dia-class name under chain kids sections) -> doc

the diagram of a class: what it comes of, from the first to it,
along the top, its card, name under and sections as (dia-card), and
below that the classes that come of it. chain and kids are names
```

### dia-doc

```code
(dia-doc parts [pad]) -> doc

a document of parts, each a group or a shape, as big as they are and
pad round them, default 12. Nothing behind them, the page shows. It is
one picture, so its parts are one group, as a board inserting it or
opening it has it, and taken apart by an ungroup
```

### dia-elbow

```code
(dia-elbow x y x1 y1 [heads]) -> shape

from a point across, down or up, and across to a point, as the
branches of a tree are drawn
```

### dia-head

```code
(dia-head x y x1 y1) -> shape

the head of an arrow that comes from a point and ends at another, a
filled triangle big enough to be seen at the width a diagram's lines are
```

### dia-hierarchy

```code
(dia-hierarchy nodes [alone]) -> doc

the tree of nodes, ((name parent) ...), and under it, in rows, those
that come of nothing and have nothing come of them, with alone, a
line of words, over them. They are not in the tree, a tree of one each
```

### dia-join

```code
(dia-join scene from to [label heads bend]) -> scene

a line from one named box to another, with an arrow at the far end,
heads :none or :both for none or two. It leaves and arrives at the
sides that face each other. label, words at its middle. bend :elbow
for a line that goes across, then up or down, then across
```

### dia-move

```code
(dia-move items x y) -> group

items as one thing, put at a point
```

### dia-pill

```code
(dia-pill text [size bold fill]) -> (group width height)

words in a box with round corners, its top left at 0 0
```

### dia-put

```code
(dia-put scene name part x y) -> scene

put a part, (group width height), with its top left at a point. With
a name it can be joined to, :nil for one that is only there
```

### dia-row

```code
(dia-row pills gap max_width) -> (group width height)

pills, each (group width height), set left to right, onto another
line when they would be wider than max_width. Top left at 0 0
```

### dia-say

```code
(dia-say scene text x y [size bold color]) -> scene

words on the scene, their left end and the line they sit on at a point
```

### dia-scene

```code
(dia-scene) -> scene

nothing yet: the lines, the boxes, where each box that has a name
is, and what lies under it all
```

### dia-scene-doc

```code
(dia-scene-doc scene [pad]) -> doc

the document of a scene, its lines under its boxes
```

### dia-side

```code
(dia-side box way) -> (x y)

the middle of a side of a box, (x y w h): :left :right :top :bottom
```

### dia-tree

```code
(dia-tree nodes [here]) -> (group width height)

a tree, its root at the left and what comes of each thing to the
right of it. nodes is ((name parent) ...), parent :nil or a name not
among them for a root. here, a name, is marked. Top left at 0 0
```

### dia-under

```code
(dia-under scene part x y) -> scene

put a part under everything, the lines too: a box that others are
in, a node with its tasks, which a line to one of them crosses
```

### dia-width

```code
(dia-width text size [bold]) -> width

how wide words are in letters of that size
```

### dia-words

```code
(dia-words text x y size [bold color]) -> shape

words, their left end at x and the line they sit on at y
```

### diagram

```code
(diagram name doc)

what a script of docs/diagrams/ saves its scene with, beside itself by
its own name, as make docs runs it
```

### diagram-save

```code
(diagram-save doc file)

a diagram saved, a document of a whiteboard
```

### diff-hunk

```code
one change, the lines of a that go and the lines of b that come, from

after line ha of a and line hb of b. The ironed path has them in
blocks, so they are written as blocks, a run of lines deleted, a run
added, or the one changed for the other
```

### diff-range

```code
a line, or the first and last of a run of them
```

### dir-list

```code
(dir-list path) -> (names kinds)

what is in a folder, each name and what the host says it is, "4" a
folder, "8" a file, as two lists, "." and ".." among them. Nothing for
a folder that is not there
```

### each-mergeable

```code
(each-mergeable lambda seq) -> seq
```

### ec-add

```code
(ec-add p1 p2 fm) -> :nil | pt
```

### ec-affine

```code
(ec-affine pt fm) -> :nil | (x y)
```

### ec-dbl

```code
(ec-dbl pt fm) -> :nil | pt

twice a point, :nil the point at infinity
```

### ec-mul

```code
(ec-mul k pt fm) -> :nil | pt

a point times a number, a bit at a time from the top, giving way to
the other tasks of the node
```

### ec-on-curve?

```code
(ec-on-curve? x y curve) -> :t | :nil

y^2 = x^3 - 3x + b, and each less than the prime
```

### ec-secret

```code
(ec-secret curve secret) -> nums

a secret of random bytes as a number from 1 to n - 1, mod n, and 1
for the one in 2 to the 256 that is 0
```

### ec-small

```code
(ec-small a k fm) -> nums

a times a small number, mod
```

### ecdh-public

```code
(ecdh-public curve secret) -> str
```

### ecdh-shared

```code
(ecdh-shared curve secret public) -> :nil | str
```

### ecdsa-curve

```code
(ecdsa-curve p n b x y len) -> curve

a curve from its numbers in hex: the prime, the order of the base
point, b, the base point, and how many bytes a number of it is
```

### ecdsa-verify

```code
(ecdsa-verify curve public digest r s) -> :t | :nil
```

### ed-add

```code
(ed-add p q) -> p

the point p and the point q, added, into p. A point is 4 numbers
```

### ed-car

```code
(ed-car o) -> o

each part down to 16 bits, what is over going on to the next, and
what is over the top coming back in at the bottom 38 times
```

### ed-copy

```code
(ed-copy a) -> a new number of the field, the same as a
```

### ed-decode-neg

```code
(ed-decode-neg bytes) -> :nil | point

the point of 32 bytes, with its x the other way, which is what a
check wants. :nil if they are not a point of the curve
```

### ed-encode

```code
(ed-encode p) -> str

a point as 32 bytes, its y, with whether its x is odd in the top bit
```

### ed-inv

```code
(ed-inv a) -> 1 over a, a new number
```

### ed-mod-l

```code
(ed-mod-l x) -> str

a list of 64 numbers, the bytes of a number, the low one first, less
every multiple of the order of the base point that will come off it,
as 32 bytes. The list is changed
```

### ed-mul-ref

```code
(ed-mul-ref o a b) -> o

what (ed-mul) does, in Lisp, to check it by
```

### ed-odd?

```code
(ed-odd? a) -> 0 | 1
```

### ed-pack

```code
(ed-pack n) -> str

a number of the field as its 32 bytes, the low byte first, and less
than the prime, the prime is taken off twice if it will come off
```

### ed-point

```code
(ed-point x y) -> a point, of its x and y
```

### ed-pow2523

```code
(ed-pow2523 a) -> a to the power of (p-5)/8, a new number
```

### ed-reduce

```code
(ed-reduce hash) -> str

the 64 bytes of a hash as a number, less the order of the base point
as often as it will come off, 32 bytes
```

### ed-scalarmult

```code
(ed-scalarmult q scalar) -> point

the point q added to itself the number of times the 32 bytes of the
scalar say, the low byte first. q is not changed
```

### ed-secret

```code
(ed-secret seed) -> (scalar prefix)

the number a seed stands for, and the other half of its hash
```

### ed-unpack

```code
(ed-unpack bytes) -> a number of the field, from its 32 bytes
```

### ed25519-public

```code
(ed25519-public seed) -> str

the public key of a secret seed of 32 bytes, 32 bytes
```

### ed25519-sign

```code
(ed25519-sign seed message) -> str

the signature of a message under a secret seed, 64 bytes
```

### ed25519-verify

```code
(ed25519-verify public message signature) -> :nil | :t

is it the signature of this message, by the holder of the secret
that this is the public key of ?
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

### ensure-lock-service

```code
(ensure-lock-service) -> netid | :nil

the service, waited for if it is not yet heard of, as it may not be
in the first moments of a session. For two seconds, it was half of
one, which a slow machine with a lot to do ran past now and then
```

### ensure-net-service

```code
(ensure-net-service) -> netid | :nil

the Net service of this machine. It is waited for, a desktop or a
terminal starts one as it comes up. A node that is neither has none,
so one is started, or a link from there would go round the service,
and round the key of the machine with it
```

### env-props

```code
(env-props) -> ((key val) ...)

the properties of views in the user's *env*, usr/env.inc, the plain
names and their variants, those the screen has. The screen draws
itself by its own :paper and :ink, so a window chooses its own from
:paper_window and :ink_window. Each task's own, *env_props*
```

### env-root

```code
(env-root) -> view

the user's properties on a view of this task's own, the root of each
window it makes till it is shown, (window-attach). The task's own as
usr/env.inc makes *env_root* in each task: gui/lisp.inc is in the
shared root of a GUI's node, and a name is found where it is used
```

### env-theme

```code
(env-theme name) -> ((old new) ...)

the symbol fonts of the user's *env* are those of the theme, each the
size it was, and so on this task's root. Those that changed, the font
that was and the one that is
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

what is in a directory, a list of its own
```

### exfat-dir-put

```code
entries to a directory, from an index on. They can cross from one

cluster of the directory to the next, so they go an entry at a time
```

### exfat-dir-read

```code
(exfat-dir-read vol entry) -> (entries index)

a directory as it was last read, or read now. Every name looked for
on the way to a file was a read of each directory on the way, and of
every entry in it, a store of blocks has 256 in one
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

the room there is left, the clusters that are not in use. Counted
the first time it is asked, and kept as clusters are marked: it is
asked with every put to a store, and the count was a look at every
bit of the bitmap
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

### exfat-key

```code
a name as the volume sees it, with no regard to case
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
every file that is found, saved or removed comes by here, and many
of them in one batch touch the device not at all: it gives way here too
```

### exfat-pending-over

```code
(exfat-pending-over vol offset len) -> vol

what waits is written now if any of it is of those bytes: what is read
of them is what was written, and a write of them again is the last
```

### exfat-pending-write

```code
(exfat-pending-write vol) -> vol

the writes of whole blocks that wait, to the device, in the order
they are on it, those that are next to each other as one
```

### exfat-raw-read

```code
whole blocks from the device, :nil if they are not all there. Each

time the device is gone to, the task gives way first: a volume at
work is a great deal of reading and writing, and the node is not its
alone
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
the time now, as a directory entry keeps it, MS-DOS's date over its time
```

### exfat-store

```code
data to clusters, a run is one write. What is left of the last block

is made zero. The rest of the last cluster is not written, nobody is
given it to read, and a directory is always whole clusters
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
every file and directory under a path, a directory before what is in it
```

### exfat-write

```code
(exfat-write vol offset data) -> vol

bytes to the device. A run of whole blocks goes straight to it. The
rest, the ends of a long write and all of a short one, change the
blocks that are kept, a block that is only part written is read first.
Between an (exfat-begin) and its (exfat-end) the run waits, as it is:
the files written one after another are next to each other, and go
to the device as one write, not one each, which a flash stick is slow
at. A write of what waits writes that first
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

### fat32-chain

```code
(fat32-chain vol start) -> (cluster ...)

the clusters of a file or a directory. A chain that goes round on
itself is cut off at the size of the volume
```

### fat32-data

```code
(fat32-data vol start len [keep]) -> str

the bytes of a file or a directory, a run of clusters is one read.
A len of :nil is all of the chain, a directory has no length. keep is
for a directory, its blocks are kept
```

### fat32-dir

```code
(fat32-dir vol entry) -> ((name dir size start no_fat index count) ...)

what is in a directory
```

### fat32-entries

```code
(fat32-entries vol start) -> ((name dir size start no_fat index count) ...)

the files and directories of a directory. The parts of a long name
come before the entry of their file, the last part first, and are
its name if they are all there and their checksum is that of its
8 and 3 name. Then that is its name.
```

### fat32-find

```code
(fat32-find vol path) -> :nil | (name dir size start no_fat index count)
```

### fat32-list

```code
(fat32-list vol path) -> :nil | ((name dir size start no_fat index count) ...)
```

### fat32-load

```code
(fat32-load vol path) -> :nil | str
```

### fat32-mount

```code
(fat32-mount path | stream) -> :nil | vol

a path is a file of the host that is the image of a disk. A stream
can be a memory stream. A volume that is FAT12 or FAT16 is not taken.
```

### fat32-next

```code
the cluster that follows this one, the top 4 bits are not part of it.

Every file has a chain, so it is read from the block as it is kept,
an entry of the table never lies over the end of a block
```

### fat32-path

```code
(fat32-path vol path) -> :nil | (root ... entry)

the entries from the root down to a path
```

### fat32-root

```code
the root as an entry
```

### fat32-shift

```code
the power of 2 that n is, :nil if it is not one
```

### fat32-short

```code
the 8 and 3 name of an entry. It is held in upper case, with a bit

each for a name and an end that are to be shown in lower. A character
past 127 is of a code page that is not known here, it is taken as
Latin 1
```

### fat32-walk

```code
(fat32-walk vol path fnc) -> vol

(fnc path entry)
every file and directory under a path, a directory before what is in it
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

### fixed??

```code
(fixed?? form) -> :t | :nil
```

### fixeds?

```code
(fixeds? form) -> :t | :nil
```

### fixeds??

```code
(fixeds?? form) -> :t | :nil
```

### flac-bits

```code
(flac-bits reader n) -> num

n bits, 0 to 32, high first
```

### flac-codec

```code
(flac-codec reel [scale]) -> codec

what shows a frame: its samples, the left above the right, on a canvas
of its own; a scale is not taken
```

### flac-crc8

```code
(flac-crc8 str from to) -> num
```

### flac-find

```code
(flac-find reel at) -> :nil | (at frame)

the first header from at on, of 2 bytes of sync and its CRC-8 good
```

### flac-frame

```code
(flac-frame info str) -> :nil | (chans bytes)

the frame at the start of str: its header, a subframe a channel, a
side of one bit more, then the channels made left and right; the
bytes, the frame aligned to a byte and its CRC-16
```

### flac-head

```code
(flac-head info str) -> :nil | head

the header of a frame at the start of str, checked by its CRC-8: its
block size, sample size, channel assignment, its frame or sample
number and which, and the bits of it, by their codes or of STREAMINFO
of no more than 16 bytes, the least frame of silence 15, the reader's
bytes past them 0 and the CRC-8 to say
```

### flac-index

```code
(flac-index reel) -> reel

of a FLAC of blocks of sizes that vary, where each frame is, found the
once from the first, each next the one of its sample and its size on
```

### flac-info

```code
(flac-info stream) -> :nil | info

of a file's start, after an ID3v2 tag if it has one: STREAMINFO's block
sizes, frame sizes, rate, channels, bits a sample and samples, and the
points of a SEEKTABLE, (sample offset) each, the rest stepped over.
:start is where the first frame is
```

### flac-locate

```code
(flac-locate reel n) -> :nil | at

where frame n begins: where one read ended if it was n - 1, else from
where the bytes say it should be, back if a later frame is found there,
then on header by header to it
```

### flac-native

```code
(flac-native key) -> function
```

### flac-next

```code
(flac-next reel at n) -> :nil | (at frame)

the header of frame n from at on: the sync and the CRC-8 of a header
are in a frame's bytes now and then, those of another number passed
```

### flac-number

```code
(flac-number reel head) -> num

the frame a header is of, or of blocks of sizes that vary its sample,
which is what such a frame's header has
```

### flac-packet

```code
(flac-packet reel n) -> :nil | str

frame n, to the header of frame n + 1; the ends of the last four kept
```

### flac-pcm

```code
(flac-pcm info chans) -> (left right)

a frame's channels as reals at the scale of 16 bits, the first two, the
one of one on both
```

### flac-picture

```code
(flac-picture codec n) -> canvas
```

### flac-reel

```code
(flac-reel stream) -> :nil | reel

a file of FLAC as a reel of its frames, each found as it is wanted, of
STREAMINFO's block size, :rate of them a second. Its :tracks the one of
sound, of codec "flac", its pictures the samples of each frame, as an
MP3's
```

### flac-residual

```code
(flac-residual reader samples order n)

the residual of a subframe of n, from order on: its partitions each
of a Rice parameter, of 4 bits or 5, or the escape and its raw bits
```

### flac-signed

```code
(flac-signed reader n) -> num

n bits of two's complement
```

### flac-subframe

```code
(flac-subframe reader n bps) -> nums

a channel's n samples of bps bits: one number, the samples as they
are, a fixed prediction, or an LPC one, then any bits wasted put back.
One the format forbids, an order past its samples, a precision of 16, a
shift down by less than 0, a reserved type, is heard as nothing: a
release build does not stop at a throw, the error a value that would
go on into the natives
```

### flac-unary

```code
(flac-unary reader) -> num

the 0s before a 1
```

### flatten

```code
(flatten list) -> list
```

### flm-add

```code
(flm-add film canvas) -> film

the next frame of the film, what is on the canvas now. It must be
the size of the first. The pixmap of the canvas is left as argb.
```

### flm-bits

```code
the bits a pixel of a format takes in the stream
```

### flm-close

```code
(flm-close film) -> frames

no more frames. The feed is flushed and let go, which is the end of
it for the encoder, and when it has written the last of the film it
says how many frames it made, which is the result
```

### flm-codec

```code
(flm-codec reel [scale]) -> codec

what makes frames of the packets of a film, on a canvas of its own
the native of a delta frame is taken as the codec is made, not as this
is read: this is read by every task that loads a picture, and one that
named a function the system had not been built with yet never got as
far as building it, gui/pixmap/flm.vp
a scale only if it is of the whole size: a frame is drawn on the one
before by its runs, as wide as the film
```

### flm-convert

```code
(flm-convert in out [rate]) -> :nil | frames

a film of version 1 or 2 read, and written to out as this version: a
whole first frame and then each frame after it, found by its runs.
Those have no packets, a frame ends where its runs have covered every
pixel. :nil if in is not such a film, or of a format whose pixels are
not whole bytes
```

### flm-decode

```code
(flm-decode codec packet index) -> :nil | canvas

the frame of a packet on the codec's canvas: packet 0 is a whole frame,
each after it is drawn on the frame before it
```

### flm-encode

```code
the encoder task. The feed is the size of a frame, then frame after

frame of pixels, till it ends
```

### flm-encode-frame

```code
the next frame of the film, a packet written to its stream
```

### flm-encoder

```code
the state of a film being written to a stream
```

### flm-frame

```code
(flm-frame codec n) -> :nil | canvas

frame n, each packet after the frame given last, or after the first
if n is before it
```

### flm-head

```code
(flm-head stream) -> :nil | (version flags type width height rate)

what the header of a film says, rate in frames a second
```

### flm-open

```code
(flm-open stream format [rate]) -> film

a film to be written to a stream, format is the bits of a pixel,
1, 8, 12, 15, 16, 24 or 32, and rate the frames a second, 30 if not
said. The encoder is started, and waits for frames
```

### flm-packet

```code
(flm-packet reel index) -> :nil | str

the bytes of a packet of the film
```

### flm-packet-write

```code
a packet, its length and its bytes
```

### flm-reel

```code
(flm-reel stream) -> :nil | reel

a film to be read from a stream that can seek: its header, and where
each packet is and how long, found by their lengths. :nil if it is no
film of this version, :version says which it is
```

### flm-stage

```code
the source of the encoder task, it says where its feed is, makes the

film, and says how many frames there were
```

### float-time

```code
(float-time [smooth]) -> (sec min hour)
```

### flush-bits

```code
(flush-bits stream (array bit_pool bit_pool_size))
```

### folder-mode?

```code
(folder-mode? mode) -> :nil | :t

is a mode, the third of what (pii-fstat) says, a folder's
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

### format-group

```code
(format-group prefix indices) -> (str ...)

runs of indices as the prefix and their ends, "r" (1 2 3 5) -> ("r1-r3" "r5")
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

### fs-walk

```code
(fs-walk vol path fnc lister) -> vol

(fnc path entry) (lister vol path) -> :nil | entries
every file and directory under a path, a directory before what is in
it, of any file system whose lister gives what is in a directory, as
exfat-list and fat32-list do. It keeps a list of the directories still
to do, it does not call itself, so the depth of the tree costs no stack.
fnc is called from inside it, so the names it uses are not these: vol
path fnc lister todo dirs entry child
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

### gcm-hash

```code
(gcm-hash state h data) -> state

the data taken into the hash, the last block made up with zeros
```

### gcm-key

```code
(gcm-key key) -> (keys h)
```

### gcm-open

```code
(gcm-open gkey nonce aad sealed) -> :nil | str
```

### gcm-seal

```code
(gcm-seal gkey nonce aad data) -> str
```

### gcm-tag

```code
(gcm-tag gkey j0 aad cipher) -> str
```

### gen-norms

```code
(gen-norms verts tris) -> (norms new_tris)
```

### get-cstr

```code
(get-cstr str idx) -> str
```

### glsl-block

```code
the lines of a block. An if and a for have blocks in them: what is

still to come is a list, the next thing last, a statement and how far
in it is, or a line that closes what was opened before it
```

### glsl-decls

```code
the inputs as uniforms, the constants, the globals and the functions

of a program, onto the lines of whoever calls
```

### going

```code
a child has said it has gone, :t if msg is that. The job it had is

put back on the queue, not counted as tried, and it is started again
```

### gui-at-rpc

```code
(gui-at-rpc x y) -> :nil | str

the title of the window at x y on the screen, the one in front there,
"" if no window is, :nil if the GUI does not answer
```

### gui-grab-rpc

```code
(gui-grab-rpc [title area]) -> :nil | why | (w h pixels)

what is on the screen now, of the window whose title has title in it,
or of all of it, or of an area of it, (x y w h), or "mouse w h" an area
that big round the mouse, read by the GUI from
its back buffer: its width, its height and its pixels, 32 bits each,
blue green red alpha
```

### gui-input-rpc

```code
(gui-input-rpc events) -> :nil | :t

events, a str of +gui_event structs one after another, given to the GUI
as if from the host's mouse and keyboard, in order, before it answers
```

### gui-locate-rpc

```code
(gui-locate-rpc [title key value front]) -> :nil | why | (x y w h sx sy sw sh)

where on the screen the window whose title has title in it is, or the
first in it whose key is value if a key is given, as (ui-find) finds it,
and the part of it that can be seen, why if none of it can. With front
the window is put in front of the others first
```

### gui-owners-rpc

```code
(gui-owners-rpc) -> :nil | (owner ...)

the mailbox of the task that has each window of the screen, hex, the
front one first. A test can tell from it which apps have a window
```

### gui-rpc

```code
(gui-rpc view cmd) -> :nil | view
```

### gui-theme-rpc

```code
(gui-theme-rpc name) -> :nil | name

tell the GUI the theme of the desktop is now this one, it tells every
window, lib/theme/theme.inc
```

### gui-tree-rpc

```code
(gui-tree-rpc [title key value depth keys]) -> :nil | why | text

the tree of widgets of the window whose title has title in it, or of
all of the screen, as (ui-save) writes it: of the first in it whose key
is value if a key is given, depth levels of children, :nil for all, and
only the properties keys, a list, :nil for all
```

### gui-wait

```code
(gui-wait ask secs) -> answer

(ask) again each 100ms, for as many seconds, till it is not that there
is no such window, an app just opened has not made it yet
```

### handler

```code
(handler state page line) -> state
```

### hash-tree

```code
(hash-tree things) -> tree

the tree of things, each (path hash [meta]). A map, the path of a
folder, "" the top, to (hash kids), the kids in the order of their
names. A tree of nothing has a top, with nothing in it
```

### hash-tree-compare

```code
(hash-tree-compare mine theirs) -> (differ meta gone into only_mine only_theirs)

one folder of mine against the same folder of theirs, both as kids.
The names of: things I have that they have not, or not the same.
Things that are the same but for the meta, each (name meta) with
mine. Things they have and I have not. Folders we both have that are
not the same, to go into. Folders only I have, and folders only they
have. A name that is a thing on one side and a folder on the other is
both a thing and a folder that one has and the other has not
```

### hash-tree-kids

```code
(hash-tree-kids tree folder) -> kids

what is in a folder, nothing if there is no such folder
```

### hash-tree-read

```code
(hash-tree-read text) -> kids

a folder from its text. A name may have spaces in it, it is last
```

### hash-tree-root

```code
(hash-tree-root tree) -> hash

the hash of the top, which is of all of it
```

### hash-tree-text

```code
(hash-tree-text kids) -> str

a folder as text, a line each, in the order of their names. It is
what a folder's hash is the hash of, and what goes in a message
```

### hash-tree-under

```code
(hash-tree-under tree folder) -> paths

the path of every thing in a folder and in the folders below it
```

### header-pass

```code
what a function says of itself in the comments after its first line:

inputs, outputs, each a line for a register, and trashes
```

### held-mouse-id

```code
the view the mouse is on. While a button is held that is the view it

went down on, a window being dragged can lag behind the pointer, and
to look under the pointer again would let go of it
```

### herd-want

```code
how many workers a herd of these nodes is to have
```

### hex-decode-stream

```code
(hex-decode-stream in_stream out_stream [chunk_size flags])
```

### hex-encode-stream

```code
(hex-encode-stream in_stream out_stream [chunk_size flags])
```

### hkdf-expand

```code
(hkdf-expand prk info length) -> str
```

### hkdf-extract

```code
(hkdf-extract salt secret) -> str
```

### hmac-sha256

```code
(hmac-sha256 key data) -> str

the hash of a str with a key, HMAC of RFC 2104, 32 bytes. Only one
who has the key can make it, or check it. A key longer than a block
is hashed first, and one that is shorter is made up with 0.
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

### huff-table

```code
(huff-table codes) -> nums

levels of 256, each by the next 8 bits of a code: a code that ends in
a level fills each entry its bits begin, what it stands for by 256 and
how many of the 8 it is; a longer one's entry is the next level, at
-entry. A list of the levels yet to make, each the entry of the one
before to link to it, its codes and the bits of them taken already
```

### huffman-book

```code
(huffman-book token_bits freqs) -> book

a code of the tokens as often as freqs has them, and of the end. Each
is taken as once more than it was, so a token not seen has a code too
```

### huffman-codes

```code
(huffman-codes lengths) -> codes

the code of each symbol, its bits the other way round, to be written
with (write-bits) of its length
```

### huffman-compress

```code
(huffman-compress in_stream out_stream [token_bits block])

the tokens a block of them at a time, default 65536, each with a code
of its own. Each block a bit of 1, its code's lengths, its tokens and
the end of it, and the stream a bit of 0 after the last
```

### huffman-compress-static

```code
(huffman-compress-static in_stream out_stream book) -> :t | :nil

:nil if a token has no code in the book, the stream is coded up to it
```

### huffman-decompress

```code
(huffman-decompress in_stream out_stream) -> :t | :nil

:nil if the stream stops before its end, or is not one, what was got
by then is written
```

### huffman-decompress-static

```code
(huffman-decompress-static in_stream out_stream book) -> :t | :nil
```

### huffman-freqs

```code
(huffman-freqs in_stream [token_bits freqs]) -> freqs

how often each token is in the stream, added to freqs if given, a
nums of 2^token_bits
```

### huffman-lengths

```code
(huffman-lengths freqs limit) -> lengths

freqs a nums of how often each symbol is, lengths a nums of the same
length, none more than limit, which must be enough for the symbols
```

### huffman-limit

```code
the longest a code may be, enough for every symbol, and no less than 15
```

### huffman-read-book

```code
(huffman-read-book in_stream) -> :nil | book
```

### huffman-read-lengths

```code
-> :nil | lengths
```

### huffman-table

```code
(huffman-table lengths) -> (counts symbols)

to read a code: how many there are of each length, and the symbols in
the order of their codes
```

### huffman-write-book

```code
(huffman-write-book out_stream book)
```

### idle

```code
every child that is up and has no job is given one
```

### image-funcs

```code
(image-funcs funcs cpu) -> funcs

those of these functions the boot image of a CPU holds, besides the
loader, the statics and the kernel, and what they call, which every
image has. The VP64 one is what an install runs, and what a node of a
CPU with no translator runs: it holds what an install binds before it
has built anything. A native one is made by an install, after it has
built the rest, so it holds only the loader, and what it calls, and the
loader binds the rest as it is asked for: what runs while it loads must
not be in a block it is writing, docs/vm/vp_functions.md
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

### inflate-decompress

```code
(inflate-decompress in_stream out_stream [zlib]) -> :nil | :t

the bytes a DEFLATE stream is, raw, or with zlib's head and sum round
it. :nil if it is not one, or stops before its end, what was got by
then is written. A block of codes is read by native code, and what it
makes kept in a ring of the last 32KB, at is how many bytes have been
```

### inflate-dynamic

```code
(inflate-dynamic in bits) -> :nil | (literals distances)

the codes a block of its own codes says, at its head
```

### inflate-fixed

```code
(inflate-fixed) -> (literals distances)

the codes of a block of fixed codes
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

### job-child

```code
(job-child timeout fn_work)

the loop of a child of a farm. Each job that comes is given to
fn_work, (fn_work msg mbox), with the mailbox it came to, till the
child is told to go, an empty message, or has had nothing for
timeout. Going of its own accord, a child that has had a job tells
its farm, so a job sent to it as it went is not lost
```

### join

```code
(join seqs seq [mode]) -> seq
```

### jpg-bit

```code
(jpg-bit reader) -> 0 | 1

the next bit of the packed data, the high bit of each byte first. A
0xFF is followed by a 0 that is not data; anything else after it is
a marker, and from there on there are only 0 bits. The reader is the
stream and (byte bits marker), a nums the native block reads too
```

### jpg-code

```code
(jpg-code table) -> (maxcode valptr mincode values)

the canonical code of a table, as the standard's F.2.2.3 has it
```

### jpg-decode

```code
(jpg-decode in out info)

a baseline scan, from where the stream is, a row of MCUs at a time,
each row of pixels written blue green red alpha
```

### jpg-decode-symbol

```code
(jpg-decode-symbol reader table) -> value
```

### jpg-extend

```code
a number of n reader, as the standard has the negative ones
```

### jpg-geometry

```code
(jpg-geometry width height comps) -> (hmax vmax mcus_x mcus_y)
```

### jpg-head

```code
(jpg-head stream) -> :nil | emap

what comes before the first scan: :width :height, :comps ((id h v
table) ...), :progressive, :quant (64 nums) by table, :dc :ac (counts
values) by table, :restart, :scan and :spectral. The stream is left at
the start of the packed data
```

### jpg-next-marker

```code
(jpg-next-marker reader in) -> :nil | marker

the marker after a scan, which the scan may have come to
```

### jpg-progressive

```code
(jpg-progressive in out info)

the scans, from where the stream is, then the pixels a row of MCUs at
a time, each row of pixels written blue green red alpha
```

### jpg-restart

```code
a restart: to the next byte, past the marker, which may not have been

come to yet
```

### jpg-rows

```code
(jpg-rows out samplings planes my width height hmax vmax mcus_x)

the rows of pixels of a row of MCUs, those inside the picture, from
the plane of each component, (h v) its sampling, of mcus_x h 8 by v 8,
each row native
```

### jpg-scan

```code
(jpg-scan reader in head grids width height hmax vmax mcus_x mcus_y)

a scan of a progressive JPEG, its numbers into the grids
```

### jpg-segment

```code
(jpg-segment out kind data) -> :t | :nil

a marker's segment read into out, :nil if it ends the head: a scan, or
a kind of frame not read. A scan's components are (id dc ac), and how
it is cut, :spectral, (start end high low): the numbers of a block it
has, from start to end in zig zag order, and of their bits, high the
last bit sent before, low the one now
```

### jpg-segments

```code
(jpg-segments stream out) -> :nil | out

the markers and their segments, from where the stream is, till a scan,
the stream left at the start of its packed data. :nil if it ends, or
the file does, before
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

### lerp

```code
vertex kd is where the edge from vertex ka, in front of the near

plane, to vertex kb, behind it, crosses the plane
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

### link-text

```code
(link-text line) -> line

each link of the line, [text](target) or ![text](target), is made
the words " <l:target> text </l> ", for (format-words) to make the
text a Link. One with no text, no target, or a space in its target
is left as it is written. A word is drawn with a space after it, so
where there is none in the text, before a link or after, it is said
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

### lock-claim-rpc

```code
(lock-claim-rpc key [mode timeout_us]) -> :t | :nil
```

### lock-history-rpc

```code
(lock-history-rpc [timeout_us]) -> list | :nil
```

### lock-release-rpc

```code
(lock-release-rpc key [timeout_us]) -> :t | :nil
```

### lock-service

```code
(lock-service) -> netid | :nil
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

### loops

```code
(loops obj [kids]) -> ((holder key) ...)

each place under obj that holds obj or something between it and
obj: the holder, an environment or a list, and the key or the index
it is at. What is under a thing is its properties, if it is an
environment, its elements, if it is a list, and what kids gives of it,
(kids thing) -> things, the children of a view say. A function is not
looked in, nor what was looked in already by another way down
```

### lz4-compress

```code
(lz4-compress in_stream out_stream [window_size])

window_size is the most of a block, it is made one of the four a
frame can say, the least that holds it
```

### lz4-decompress

```code
(lz4-decompress in_stream out_stream [window_size]) -> :t | :nil

:nil if it is not a frame of version 01, or a block is more than the
frame said a block could be, what was got by then is written. A
block's sum, the content's size and a dictionary's id are passed
over, not looked at
```

### macro-func?

```code
(macro-func? form) -> :t | :nil
```

### macro?

```code
(macro? form) -> :t | :nil
```

### mail-entry-id

```code
(mail-entry-id entry) -> net_id

the mailbox of an entry as (mail-enquire) gives them, name,id,info
```

### mail-first

```code
(mail-first name) -> :nil | net_id

the mailbox of the first said as name, (mail-declare), wherever it is
```

### mail-here

```code
(mail-here name) -> :nil | net_id

one said as name, (mail-declare), on this node, whose task is still
there: one that died without (mail-forget) is still said, its mailbox
is gone. Each desktop is a node, so this is the one of this desktop,
the guard of a service or an app there is to be one of
```

### mail-lowest

```code
(mail-lowest name) -> :nil | net_id

the lowest mailbox said as name. A service that is to be one may be
started twice at once, by two that each found none: every task that
sees both picks the same this way, the one that stays, (mail-single)
```

### mail-read-timeout

```code
(mail-read-timeout mbox [timeout_us]) -> msg | :nil
```

### mail-service

```code
(mail-service name) -> :nil | net_id

the service said as name to use: the one on this node, the desktop's,
if it is still there, else one on another node. A task on a node with
no desktop has any one on the machine. One on this node that is not
here is dead; one on another can not be asked, (mail-validate) knows
only the mailboxes of the node it is asked on
```

### mail-single

```code
(mail-single name mbox) -> :nil | net_id

:nil if the service said as name with this mailbox is the one, else
the one it is to give way to, the lowest. The names come to each node
by its pings, so a service checks each time it wakes; one that has to
give way stops saying its name, sends on what still comes to it, and
goes. Only for a service a task may use any one of: two networks on
one machine each have a Net service of their own
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

### md-anchor

```code
(md-anchor text) -> name

the name a heading is found by, as the place of a link, file.md#name.
The text of the heading in lower case, each space a -, and all but
letters, digits, - and _ left out, with where a link in it goes to.
"A heading with [a link](x.md) in it!" is a-heading-with-a-link-in-it
```

### md-link-file

```code
(md-link-file file target) -> path

the file a link of a document is to. It is from the folder the
document is in, unless it starts at the root with a /. Each .. is a
folder up, each . is no move, and what is after a # is a place in
the file, not part of its name
```

### mdct-i0

```code
(mdct-i0 x) -> real

the Bessel function I0, its series, the terms till they are nothing
```

### mdct-imdct

```code
(mdct-imdct dct4 tables lines n) -> reals

the 2n samples of the IMDCT of n lines: their DCT-IV y, by 2/N, 1/n,
as y from n/2, then -y backwards, then -y to n/2
```

### mdct-kbd

```code
(mdct-kbd n alpha) -> reals

the rising half, n, of the Kaiser-Bessel derived window of 2n
```

### mdct-pi

```code
(mdct-pi) -> real
```

### mdct-sine

```code
(mdct-sine n) -> reals

the rising half, n, of the sine window of 2n
```

### mdct-tables

```code
(mdct-tables n) -> (tw rev)

what media_dct4 takes for a DCT-IV of n: the twiddles before,
e^(-i pi (4j+1)/4n), after, e^(-i pi k/n), of the FFT of m, n/2,
e^(-2 pi i k/m), each cos and sin, and the bit reversal of m
```

### mesh-ball

```code
(mesh-ball mesh) -> (centre radius)

a ball that the mesh is inside, the middle of its box and how far
the furthest vertex is from there
```

### mesh-corners

```code
(mesh-corners mesh) -> str

a mesh as the shaders want it. A face is lit flat, so a vertex that
faces share is a vertex of its own for each of them, where it is and
then the normal of that face, 7 numbers. The triangles are then just
the vertices in the order they come. It is the bytes of the numbers,
as they go in a message to a child that draws.
```

### mesh-corners-smooth

```code
(mesh-corners-smooth mesh) -> str

a mesh as the shaders want it, lit smooth. The normal at a vertex is
that of all the faces that share it, taken together, so a face shades
from corner to corner and a ball of faces is round
```

### min-length

```code
(min-length list) -> min
```

### mkv-asc

```code
(mkv-asc name rate channels) -> str

an AudioSpecificConfig of AAC-LC of a rate and channels, or if the
name ends /SBR, of type 5, SBR on a core of half the rate
```

### mkv-block

```code
(mkv-block str at end start tracks key)

a block: its track, its time from the cluster's start, its keys, and
its frames into its track's tables, each frame of a laced block where
it is in the file, str of the block from start
```

### mkv-children

```code
(mkv-children str at end) -> ((id body end) ...)

the elements from at to end of a str
```

### mkv-cluster

```code
(mkv-cluster stream at end tracks) -> at

a Cluster's time and its blocks, read a window of a MB at a time, an
element longer than that read whole: a Cluster of no size, of a
recording, ends where the next element of the top is, and that may be
the file's end. Its end
```

### mkv-element

```code
(mkv-element str body stop start tracks id)

an element of a Cluster, of a str from start: its time, or a block
```

### mkv-head

```code
(mkv-head stream at) -> :nil | (id size data_at)

the element at at of a stream: its id, its size, -1 not known, and
where what it holds begins
```

### mkv-laced

```code
(mkv-laced str at end lacing) -> ((at size) ...)

the frames of a block from at, its lacing: none, Xiph's, fixed, or
EBML's, each where it begins and how long
```

### mkv-reel

```code
(mkv-reel stream) -> :nil | reel

the reel of the first video track of a Matroska file, every track in
:tracks: its EBML header, then its Segment's Info, Tracks and
Clusters, the rest stepped over
```

### mkv-track

```code
(mkv-track str at end) -> track

a TrackEntry: its number, kind as an mp4's, its codec named as an mp4
names it, its setup, size or sound
```

### mkv-vint

```code
(mkv-vint str at) -> (value bytes marked)

a number of 1 to 8 bytes at at, its first byte's leading 0s one less
than its bytes: its value without the length bit, how long it is, and
its value with it, as an id is. All ones is a size not known, -1
```

### month-of-the-year

```code
(month-of-the-year month) -> str
```

### mp3-alias

```code
the butterflies between each two subbands of long blocks, of a mixed

one the first two alone, of short none, native, lib/media/lisp.vp
media_mp3_alias
```

### mp3-band

```code
a band's lines requantised, |x|^4/3 by 2^(e/4), signed, native,

lib/media/lisp.vp media_dequant
```

### mp3-before

```code
(mp3-before size) -> frames

how many frames of a size are decoded first, before one, for the
reservoir to hold the 511 bytes a frame may begin back
```

### mp3-codec

```code
(mp3-codec reel [scale]) -> codec

what shows a frame: its samples, the left above the right, on a
canvas of its own, by a decoder of its own; a scale is not taken
```

### mp3-decoder

```code
(mp3-decoder) -> decoder

what goes from frame to frame: the reservoir of main data, of each
channel its scale factors, the halves of its IMDCT and its v, and the
books made native tables, made the once by (mp3-tables)
```

### mp3-frame

```code
(mp3-frame decoder frame) -> (left right)

a frame's samples, its main data from the reservoir and itself, each
granule's channels read, the pair's middle and side made left and
right, each to samples; nothing heard of a frame whose main data
begins before what the reservoir has
```

### mp3-frames

```code
(mp3-frames str) -> ((at size) ...)

the frames of the bytes of a file, after an ID3v2 tag if it has one,
each found where the one before ends, or the next header after
```

### mp3-granule

```code
(mp3-granule reader lsf sri) -> granule

of a channel of a granule, its side information: the bits of its scale
factors and lines, the big values, the gain, the scale factors' code,
the block type, mixed or not, the tables of its regions and the pairs
of each, the gains of short windows, and the flags after
big values of no more than the 288 pairs of the 576 lines: a frame that
says more is wrong, and the native would write past them
```

### mp3-guess

```code
where frame n should begin: by the table of a first frame of Xing,

between two of its hundredths, else of a varying bit rate by the
bytes a frame of them all, else by the bit rate of the first
```

### mp3-header

```code
(mp3-header str at) -> :nil | header

the 4 bytes at at, if they are a header of layer III: MPEG-1, 2 or 2.5,
the rate and its index of the band tables, the mode and its extension,
the channels, granules and samples, and the bytes of the frame
```

### mp3-id3

```code
(mp3-id3 str) -> bytes

the bytes of an ID3v2 tag at the start, and its footer, 0 if none
```

### mp3-imdct

```code
(mp3-imdct lines granule overlap) -> (reals ...)

each subband's 18 samples: the IMDCT of 36 of its 18 lines, or of a
short block three of 12 of its 6 a window, by the window of its block
type, the two lowest of a mixed block of any type the plain window's;
the first half on what the last left, the second half left for the
next; of an odd subband the odd samples the other sign, by its windows
```

### mp3-intensity

```code
(mp3-intensity left right granule sf header)

of a pair of intensity stereo, from the top band down, each band the
right has nothing in shares the left's by the right's scale factor, its
position, those below it of middle and side if the frame says; a short
block each window its own way, the last band by the scale factor of
the band before
```

### mp3-lines

```code
(mp3-lines decoder reader granule end) -> nums

the 576 lines of a channel's granule, quantised and signed, by the
native reader, lib/media/lisp.vp media_mp3_lines: the big values by
pairs, each region by its book, 15 the more bits of its table and then
the sign; then quads of 1 and 0 till the end of the part
```

### mp3-packet

```code
(mp3-packet reel n) -> :nil | str

frame n: where one read ends if it was n - 1, else the first found
from a little before where it should be. The ends of the last four
read are kept, the sound and the picture each read one after another
```

### mp3-pi

```code
(mp3-pi) -> real

pi, by Newton from 3, sin(x) to 0
```

### mp3-picture

```code
(mp3-picture codec n) -> canvas

frame n's samples drawn: after the one before, else with those before
it decoded first, for the reservoir and the overlap
```

### mp3-positions

```code
(mp3-positions lsf sfc) -> ((kl kr) ...)

the shares of left and right of each position of intensity stereo: of
MPEG-1 tan(pos pi/12) to 1, of MPEG-2 by 2^-1/4 or 2^-1/2 a step, the
scalefac_compress's last bit, to the left at an odd position, the right
at an even
```

### mp3-reel

```code
(mp3-reel stream) -> :nil | reel

a file of MP3 as a reel of its frames, each found as it is wanted, not
all now: a book of 40 hours is 2GB. They are counted by a first frame
of Xing or Info if it has one, else by the bit rate, :frames, and a
frame a packet, :rate of them a second. Its :tracks the one of sound,
of codec "mp3", and its pictures those of lib/media/mp3.inc's codec,
:width by :height, the samples of each frame
```

### mp3-reorder

```code
a short block's lines from band, window, line to band, line, window
```

### mp3-requant

```code
(mp3-requant granule lines sf sri) -> reals

each line by the gain of its band: the global gain, less its scale
factor and the pretab by the scale, of a short window its gain too
```

### mp3-rows

```code
(mp3-rows rows cols f) -> reals

a matrix of (f row col), its rows one after another, as the native
media_matvec takes it
```

### mp3-samples

```code
frame n's samples, :nil of one not there or not read
```

### mp3-scales

```code
(mp3-scales reader granule gr scfsi prev) -> (sf ...)

the scale factors of MPEG-1: of short blocks 3 a band, of a mixed one
the 8 long first, of long the 4 groups of bands, any the first granule
gives the second by the scfsi; each so many bits by its slen
```

### mp3-scales-lsf

```code
(mp3-scales-lsf reader granule header ch) -> (sf ...)

the scale factors of MPEG-2 and 2.5: the 9 bits of the scalefac_compress
split to the lengths of 4 parts, of the right of intensity stereo its
own way, as many in each part as the table has, 40 in all; of 500 or
more the pretab used
```

### mp3-side

```code
(mp3-side reader header) -> (main_data_begin scfsi granules)

the side information: where the main data begins, back in the
reservoir, the scale factors each channel shares between its two
granules, and each granule's channels
```

### mp3-split

```code
(mp3-split sf n1 n2 n3) -> (slen ...)

the 4 lengths of MPEG-2's scale factors in one number, the last by n3,
then n2 and n1, each of 0 a length of 0
```

### mp3-sync

```code
(mp3-sync str at) -> :nil | (at header)

the first frame from at on: a header, and one of the same rate where
it ends, if that is in the str, a header alone may be of a frame's
bytes
```

### mp3-synth

```code
(mp3-synth subbands v pcm at) -> v

the polyphase filterbank: for each of the 18 times, the 32 subbands'
samples into the 64 of the matrix, on the front of the 1024 of v, and
of the window D over 16 of them 32 samples, into pcm from at
```

### mp3-tables

```code
(mp3-tables) -> tables

made the once, as the first decoder is: the starts of the long bands
by the rate, |x|^4/3 to 8206, the butterflies, the cosines of the IMDCT
of 36 and of 12, the windows of the 4 block types and of the short, the
matrix of the filterbank and its window of 512
```

### mp3-unit

```code
the bytes of a frame of a constant bit rate, by the rate
```

### mp3-xing

```code
(mp3-xing str at header) -> :nil | (frames bytes toc vbr)

what a first frame of Xing or Info, of nothing heard, says of those
after it: how many, their bytes, and where each hundredth of them
begins, by 1/256 of the bytes, :nil of each it does not say, and if
their bit rate varies, Xing's. A table is taken only if it rises,
some coders leave it all 0, and not that of Info, of a constant bit
rate, the rate is nearer
```

### mp4-boxes

```code
(mp4-boxes str at end) -> ((name body end) ...)

the boxes from at to end of a str: each name, where what is in it
starts, and where it ends
```

### mp4-close

```code
(mp4-close codec) -> :nil
```

### mp4-codec

```code
(mp4-codec reel [scale]) -> :nil | codec

the host's decoder of the video track's codec, on a canvas of its own,
:nil if it has none. Its native is taken as the codec is made, not as
this is read, as flm-codec does
```

### mp4-config

```code
(mp4-config str at end) -> ((name str) ...)
```

### mp4-edits

```code
(mp4-edits str trak track) -> track

where the track begins, by its edit list, edts elst: :start the time,
of its own, that is shown first, and :delay how long, of its own time,
nothing of it is shown before that, an edit of no media first. What a
sample is shown at, of the film's time, is its shown less start, and
delay more. Each edit is its length, in the film's time, mvhd's, the
time of the track it starts at, -1 for none, and a rate. Only the
first edit with media is taken, as a player of a film from start to
end has it
```

### mp4-feed

```code
the packets fed to the host till the picture shown nth is out on the

canvas. One before the last given, or after the next key, the host
starts again from the key before its packet; one after it, before the
next key, is fed on to, which is no more work
```

### mp4-frame

```code
(mp4-frame codec n) -> :nil | canvas

the picture shown nth on the canvas, as it is if it is there already
```

### mp4-in

```code
(mp4-in str box path) -> :nil | box

the first box down a path of names, in a box
```

### mp4-moov

```code
(mp4-moov stream) -> :nil | str

the moov box of a file, what is in it, its other boxes stepped over
```

### mp4-n

```code
a number of so many bytes, high first
```

### mp4-order

```code
(mp4-order reel) -> (packet ...)

the packet shown nth, worked out once and kept in the reel, a codec
made again at another scale has it: the sort of the frames of a film
of two hours is half a second. With no ctts they are shown as decoded
```

### mp4-packet

```code
(mp4-packet reel index) -> :nil | str

the bytes of a sample of the video track, or as the reel's :packet_fn
gives them, the frames of a reel of another kind re-framed for the host
```

### mp4-reel

```code
(mp4-reel stream) -> :nil | reel

the reel of the first video track of a file, every track in :tracks
```

### mp4-rescale

```code
(mp4-rescale codec scale) -> codec

the scale is only the canvas's: the host's decoder is kept as it is,
and a canvas of the scale given the picture the old one had. A codec
made again would decode from the key before the frame, seconds of it
```

### mp4-table

```code
(mp4-table str box head width) -> ((n ...) ...)

the entries of a table box: after its version and flags, a count, and
the entries, each so many 4 byte numbers, head more bytes before them
```

### mp4-tables

```code
(mp4-tables track) -> track

the tables of a track's samples, if they have not been read yet: where
each sample is in the file, how long, when it is decoded and shown,
and which are keys
```

### mp4-tables-read

```code
the samples: their sizes
```

### mp4-track

```code
(mp4-track str trak [film_scale]) -> track

what a track is, its kind, codec, size or sound and setup, how many
samples it has and where it begins
```

### msafe?

```code
(msafe? o) -> :t | :nil
```

### msl-block

```code
the lines of a block. An if and a for have blocks in them: what is

still to come is a list, the next thing last, a statement and how far
in it is, or a line that closes what was opened before it
```

### msl-inputs

```code
the inputs block, packed types and pad words give the std140 offsets.

A matrix is on a 16 byte boundary as it is, and is its columns
```

### msl-name

```code
a name is given a trailing _ so it can not be a word of MSL, half say
```

### msl-set-inputs

```code
the members that are inputs, from the block
```

### msl-set-textures

```code
the members that are textures, from the parameters
```

### msl-struct

```code
the shader as a struct. The inputs, the globals, the attrs and the

varyings are members, the constants are members with a value, and the
functions are methods
```

### msl-texture-params

```code
the textures of a pixel shader as they come to its fragment function,

each a texture and a sampler at the same number, -> text to go after
the other parameters
```

### mtl-read

```code
(mtl-read stream) -> fmap

each material by its name, an emap: :colour (r g b), :seen 0 to 1,
and :map, the file of its picture, if it has one
```

### must-break?

```code
(must-break? rule elems) -> :t | :nil

is the form broken however short, as it has more than the one of its
last arguments
```

### name-folder?

```code
(name-folder? name) -> :nil | :t

a folder of the host, or of the store, one there is an object in
```

### name-in-store

```code
(name-in-store name) -> :nil | path

the path in the store of a name of it, "" for its top
```

### name-list

```code
(name-list folder) -> ((name ...) (type ...))

what is in a folder, as (pii-dirlist) says it, unzipped: each name, and
its type, "4" a folder, "8" a file
```

### name-size

```code
(name-size name) -> :nil | bytes

how long the thing named is, :nil if there is none, or it is a folder
```

### name-stream

```code
(name-stream name [mode]) -> :nil | stream

the thing named as a stream, mode as (file-stream) has it
```

### name-there?

```code
(name-there? name) -> :nil | :t

a file, an object, or a folder of either, of that name
```

### name-whole-stream

```code
(name-whole-stream name) -> :nil | stream

the thing named as a stream any task of this node can read: a file's
own, or all of an object, read now, as a string stream
```

### neg?

```code
(neg? num) -> :t | :nil
```

### nempty?

```code
(nempty? form) -> :t | :nil
```

### net-accept-rpc

```code
(net-accept-rpc offer_msg) -> (client_in client_out) | :nil
```

### net-beacon-rpc

```code
(net-beacon-rpc port) -> status | :nil
```

### net-close-rpc

```code
(net-close-rpc (client_in client_out))
```

### net-discover-rpc

```code
(net-discover-rpc [mesh_only]) -> status | :nil

the service makes a mesh with the others it can reach, and listens
for their beacons. With mesh_only :t it does not listen, it has only
what the other services tell it
```

### net-link-rpc

```code
(net-link-rpc target) -> status | :nil
```

### net-listen-rpc

```code
(net-listen-rpc port accept_mbox) -> listener_handle | :nil
```

### net-open-rpc

```code
(net-open-rpc host port) -> (client_in client_out) | :nil
```

### net-peers-rpc

```code
(net-peers-rpc) -> :nil | ((system_id ip port linked build link) ...)

what the Net service of this machine knows of the others, from their
beacons and from each other. linked is "linked" or "unlinked", build
is "same", "differs" or "unknown", the boot image they run as against
this one's, and link "same", "unknown", or the version of the link
they speak if it is another. :nil if the service does not answer
```

### net-quiet

```code
(net-quiet [delay_us] [stable_count] [last_cnt]) -> (node_id ...)

wait till the number of nodes known has stayed the same for a while.
A network that never settles must not hang the caller, so it gives
up, with what it has, after 20 times as long as a settled one takes.
```

### net-refused

```code
(net-refused where head)

a door tells the Net service of this machine it closed on another
machine for its link: where, host:port, or "dialled in", and the first
bytes of its hello
```

### net-refused-rpc

```code
(net-refused-rpc) -> :nil | ((where head seconds) ...)

the machines a door closed on lately, for speaking another link: where,
the first bytes of the hello it sent, and how long ago. The last 8, of
the last 10 minutes
```

### net-service

```code
(net-service) -> netid | :nil
```

### net-what-is

```code
(net-what-is system) -> str

what a machine of the mesh says it is, arm64/Darwin, asked of one of
its nodes. ? if none of its nodes is seen
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

### node-name

```code
(node-name node file) -> :nil | str

the name a node gives: a symbol, with no ' before it, a string, with
no quotes, or a (path-to-absolute "./name"), from where the file is
```

### node-net

```code
(node-net shape [cnt fronts kind script id sid]) -> (pid ...)

make this node node 0 of a network of a shape, (node-shape) has them
and what cnt is, and wait for the nodes to be seen. It is how a
session starts, from one node, and how a network is added to one
that is running, hung from this node. The first fronts of them,
default none, are of kind and run script, as (node-start) has it,
the other desktops of a session. With an id, a name for these nodes,
they are noted under it, to be stopped as one, (node-stop). With a
sid, a system id, they are a system of their own: this node is not
one of the shape, the whole of it is new nodes with that id, and
one link from here to the first of them is the way in.
```

### node-nets

```code
(node-nets) -> ((id shape total (pid ...) (link ...) file sid) ...)

the networks added to this machine's sessions by name, (node-net)
with an id, each with the processes of its nodes, the names of its
links, the note it is in, and its system id, :nil if it has this
machine's. Whichever node of the machine started them
```

### node-regs

```code
(node-regs node) -> "none" | str

the registers a (dec-method) says, a list of them, or :nil for none
```

### node-shape

```code
(node-shape shape [cnt]) -> (total pairs)

the links of a network of a shape, each a list of two node numbers.
:full, every node to every other, :ring, :star, node 0 in the middle,
:tree, two below each, :mesh, a square that wraps round, and :cube. cnt
is the number of nodes, the width for a :mesh or a :cube. Not given, or
0, it is sized to this machine, the most that is no more than one for
each processor. No more than 64 nodes, 32 for :full.
```

### node-spawn

```code
(node-spawn [num kind script]) -> (pid ...)

start num more nodes on this machine, default 1, each linked to this
node and to each other. kind and script are as (node-start) has them,
so a GUI node can be added to a TUI network, and a TUI node to a GUI
one.
```

### node-start

```code
(node-start total pairs [fronts kind script net sid]) -> (pid ...)

this node is node 0 of total, start the others, 1 on, linked as pairs
has them, each a list of two node numbers. A pid of -1 is a node the
host could not start. The first fronts of them, all if not given, are
of kind, the host program, :gui or :tui, this node's own if not given,
and run script. A node with a script is a front of its session, a way
in to it, a desktop is service/gui/app.lisp. net is a line to head
the note of them with, so they can be found and stopped as one,
(node-nets) and (node-stop). sid is a system id for them, in place
of this machine's, 16 characters, the kernel's -sid option.
```

### node-stop

```code
(node-stop id) -> :nil | num

stop the nodes of a network that was added by name, all of them, and
clear away its links and the note of it. :nil if there is none of
that name, else how many nodes are to go. Each node of this machine
is sent a task that exits it if it is one of them, not at once: the one
that asks may itself be a node of the network, and is given the time
to say what it did
```

### node-wait

```code
(node-wait want)

wait till that many nodes are seen, so what is started next can
spread over them, and not for ever
```

### nodes-to-ask

```code
(nodes-to-ask refused) -> nodes

the nodes there are, less those that could not start the task. If
that is every one of them, the nodes of this machine: a task that no
node can start is asked for there, as it was, and not of nobody
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

### num??

```code
(num?? form) -> :t | :nil
```

### nums?

```code
(nums? form) -> :t | :nil
```

### nums??

```code
(nums?? form) -> :t | :nil
```

### obj-args

```code
(obj-args field [offset]) -> args

convert to obj-get/obj-set common args
```

### obj-corner

```code
(obj-corner text counts) -> (position place normal)

a corner of a face, a, a/b, a//c or a/b/c. split would lose the empty
part of a//c, so the slashes are found
```

### obj-fit

```code
(obj-fit obj) -> mat4x4

the matrix that puts the whole of it in a ball of radius 1 about 0
```

### obj-flat

```code
(obj-flat obj corners [matrix]) -> reals

the triangles of a group as a shader is given them, a corner eight
numbers: where it is, three, after matrix if one is given, the place
on the picture, two, across and down, and its normal, three. A place
of an .obj goes up from the bottom, a picture's down from the top, so
down is 1 less it. A corner with no place is at 0 0, and a triangle
whose corners have no normal is given the one of its face
```

### obj-index

```code
(obj-index text count) -> index

a number of a corner as an index from 0, -1 if there is none
```

### obj-normal

```code
(obj-normal p0 p1 p2) -> reals

the normal of a triangle, the way it faces when its corners go round
counter clockwise
one with no area faces up
```

### obj-read

```code
(obj-read stream) -> emap

:positions reals, three a position, :places reals, two a place,
:normals reals, three a normal, :groups ((material nums) ...), the
nums three a corner and nine a triangle, in the order the materials
were first used, and :libraries (name ...), the .mtl files
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

### one

```code
a block of the scan, at row br and column bc of the grid
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

### opt-pair

```code
(opt-pair 'opt_var sep) -> args

the arg split at the first sep, (key value), so the value may hold
one, text== is ("text" "="). With no sep in it, (key)
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

### options-input

```code
(options-input args fn) -> :nil | (fn stream)

fn of the file named after the command, read under a lock, or of
stdin if none is named. The file is let go before the lock is. The
names here are long, fn is called with them about it
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

### page-scale

```code
(page-scale size) -> size

a size by the zoom of the app's *window*
```

### pairs-value?

```code
(pairs-value? rule j) -> :t | :nil

is argument j of a form, with this rule, the value of a binding
```

### pal-icon

```code
(pal-icon what val) -> shapes

what is drawn on a cell, about 0 0, in a box of 20 or so
```

### pal-ink-cell

```code
(pal-ink-cell x0 y0 x1 y1 ink) -> shapes
```

### pal-line

```code
(pal-line d [key val] ...) -> shape

a line of an icon, round at its ends and its corners unless it is said
```

### pal-tip

```code
(pal-tip what val) -> :nil | str

the name of a cell, as the toolbars' tips say it. A colour or a pen has
none, as an ink of the bar has none
```

### palette-enable

```code
(palette-enable board [colors widths]) -> board

from now a hand that taps where there is nothing, with nothing
selected, opens a palette there, its own. With the colours and the
widths given, or its own twelve and four
```

### palette-float

```code
(palette-float board x y) -> palette

a floating menu about a point, as many as are wanted, from a button of
a toolbar. It stays till its cross is tapped, and a tap on it binds the
device that tapped, see (. palette :pick) and (. board :pick)
```

### palette-open

```code
(palette-open board x y [kind id]) -> palette

a menu on the board, about a point, for the pointer of that kind and
id, default the mouse. One that pointer had open shuts. It is about the
point, wherever that is: under the hand that asked for it
```

### palettes

```code
(palettes board) -> palettes

those that are open on a board
```

### parse-text

```code
links, if the links of the text are to be made Links. They are not for

an Md that was given no :link_event, nothing would follow one, and it
is shown as it is written, with where it goes to be read
```

### path-angle

```code
(path-angle x y) -> angle

the angle of a vector, from the x axis round towards the y axis, in
radians, more than -pi and no more than pi. 0.0 for no vector at all.
A first guess good to a part in 300, then stepped twice toward where
the vector turned back by it lies along the x axis
```

### path-dash

```code
(path-dash src dash gap [closed]) -> (path ...)

a line as dashes, each dash long with a gap after it, from its start,
each its own path to stroke. Closed, the line goes round back to its
start too. A dash that ends on a corner ends there, and none is of no
length
```

### path-dash-to

```code
(path-dash-to cur x y) -> cur

a point on the end of a dash, unless it is the one already there
```

### path-dir

```code
(path-dir path) -> str

the folders of a path, apps/x/y.wav -> apps/x, "" if it has none
```

### path-ext

```code
(path-ext path) -> str

the extension of a file's name, with its dot, y.wav -> .wav, "" if it
has none. A dot it starts with is not one, .gitignore has none
```

### path-gen-earc

```code
(path-gen-earc x1 y1 rx ry phi large sweep x2 y2 dst) -> dst

the arc of an ellipse from one point to another, as the A of an SVG
path has it: the two radii, the turn of the ellipse in radians, the
larger of the two arcs if large is not 0, drawn the way the angle
grows if sweep is not 0. The first point is taken to be in dst, the
rest are added, the last is the end point exactly. Radii too small
to reach are made as big as is needed, and with no radius, or no
distance to go, it is a line
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

### path-name

```code
(path-name path) -> str

the name of a file, the path less its folders, apps/x/y.wav -> y.wav
```

### path-smooth

```code
(path-smooth src) -> dst
```

### path-stem

```code
(path-stem path) -> str

the name of a file less its extension, apps/x/y.wav -> y
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

### pbkdf2-bytes

```code
(pbkdf2-bytes state) -> str

the 8 numbers of a state as the 32 bytes of the hash, each number the
top byte first
```

### pbkdf2-sha256

```code
(pbkdf2-sha256 password salt count size) -> str
```

### period-end

```code
the frames since the last IDR, each its place by its count
```

### pii-kill

```code
(pii-kill pid) -> :t | :nil

end a process that can not be asked to go, a node that does not
answer. :t if it was told to end or was not there. The host has to be
one that can, it says how new it is in the fourth word of (pii-host),
and with one that is older nothing is done. The native function is
looked up here, when it is called, and not in class/lisp/root.inc: a
boot image of before it was written still starts
```

### pipe-farm

```code
(pipe-farm jobs [retry_timeout told]) -> ((job result) ...)

run pipe farm and collect output. A job that is not answered within
retry_timeout, in microseconds, a minute if not given, is given to a
new worker, and so is one whose node has gone. A job that has been
given out three times with no answer stops the farm, and it and any
that are still out have no result. With told the farm says how it
goes as it goes, (told :start job) and (told :done job result), see
(Monitored), and (told :tick) each second, so it can say what is late
```

### pipe-run

```code
(pipe-run cmdline [outfun])
```

### pipe-split

```code
(pipe-split cmdline) -> ((mode cmd) ...)
```

### pipe-task

```code
(pipe-task file) -> form

a command is started as a form that loads its file, not as the file,
so that a load that throws can be caught there and the pipe told,
(import-cmd) in class/lisp/root.inc
```

### pixmap-key

```code
(pixmap-key pixmap) -> 0 | key

the key of the shared memory its pixels are in, 0 if they are its own
```

### place-angle

```code
(place-angle place a cx cy) -> place

turned about a point of what holds it till its angle is a
```

### place-by-mat

```code
(place-by-mat place m) -> place

moved by a matrix: by as much, if it only moves, or else to where the
matrix and the place's together put it, what of that a place can say
```

### place-move

```code
(place-move place dx dy) -> place
```

### place-pair

```code
(place-pair place a0 b0 a1 b1) -> place

moved, turned and sized the same both ways, as two points (x y) at a0
and b0 are taken to a1 and b1, as two fingers on a thing take it
```

### place-scale

```code
(place-scale place fx fy ax ay) -> place

sized along its own sides by fx and fy, about a point of its own space,
which stays where it is
```

### place-turn

```code
(place-turn place da cx cy) -> place

turned by da about a point of what holds it. Where it goes is worked
out in reals: a fixed cos and sin are a turn that is a little too
small, and that, a thousand times, is a thing that creeps
```

### pmap?

```code
(pmap? form) -> :t | :nil
```

### png-channels

```code
(png-channels colour) -> num
```

### png-chunk

```code
(png-chunk stream) -> :nil | (type data)

the next chunk, its sum read and not looked at
```

### png-chunk-head

```code
(png-chunk-head stream) -> :nil | (type len)

the head of the next chunk, its data not read
```

### png-chunk-write

```code
(png-chunk-write stream kind data)

a chunk, its length, its kind, the data and the CRC-32 of the two
```

### png-chunks

```code
(png-chunks in out)

a stage, the zlib of the rows as IDAT chunks, then the IEND
```

### png-head

```code
(png-head stream) -> :nil | (width height depth colour interlace)
```

### png-idat

```code
(png-idat in out len)

the data of the IDAT chunks, one after the other, the first of them
begun, len of it to come. It stops at a chunk that is not one
```

### png-rows

```code
(png-rows in out width height)

a stage, the rows of 32 bit pixels, blue green red alpha, as rows of a
PNG of colour 6, each its filter, 0, and red green blue alpha: the
swap png-pixels does of 8 bits is its own way back
```

### png-unfilter

```code
(png-unfilter in out width height depth colour palette clear)

the rows inflated, each its filter undone and its pixels written as
32 bits, blue green red alpha, as a pixmap reads them
```

### poly-area

```code
(poly-area paths) -> fixed

the area of a list of paths, one that goes round the other way taken
off, so what (poly-op) gives has the area that is filled. Not less
than 0. Each path is summed about its first point, and the sum kept
in two parts, so nothing is lost and nothing runs out of room
```

### poly-band-ref

```code
(poly-band-ref edges active y0 y1 mode out) -> :nil | out

the pieces of the answer in a band, from the left, put in out, 6
numbers each: its left at the top and at the bottom, its right at
each, the number of its left edge and of its right.

active is the edges that are in the band, the place of each in
edges. Those that end at its top are taken out of it, and it is put
in order across. With +poly_by_middle in the mode
that is by the middle of each, for a band none cross in. Without, it
is by the top, and if any two cross in the band nothing is made and
:nil comes back: the band is to be cut where they cross.

This is the reference, in Lisp, and is not what is used: the native
(poly-band) below does the very same, and a test holds the two
together on every machine, a band at a time.
```

### poly-cuts-ref

```code
(poly-cuts-ref edges active ya yb cuts) -> cuts

the ys within a band at which two of its edges cross, put on the end
of cuts. As (poly-band) left them: active in order across the top,
and each edge with where it is at the top and at the bottom.
The reference, as (poly-band-ref) is
```

### poly-edges

```code
(poly-edges raw set edges ys) -> edges

the edges of a set, each (ytop ybot xtop xbot wind set), those that
lie flat left out, and every y a point is at
```

### poly-edges-in

```code
(poly-edges-in raw x0 y0 x1 y1) -> nums

the edges of raw paths that have any part in a box, or on it, 4
numbers each, where one starts and where it ends
```

### poly-fixed

```code
(poly-fixed ((x y ...) ...)) -> paths
```

### poly-hit-ref?

```code
(poly-hit-ref? a b [mode_a mode_b]) -> :t | :nil

do two sets of polygons touch: have they any point that is of both,
an edge or a corner that only meets the other counts. a and b are
lists of paths, of any shape, with holes, as (poly-op) takes them,
and a mode is the winding rule a set is filled by, as there.

It is told the cheap way first. Their boxes are apart and they do
not. Then an edge of the one meets an edge of the other, and only the
edges that are in the box both boxes share are looked at, in whole
numbers, so it is exact.
If none do, no line of either is on a line of the other, and each
path is all in the other set or all out of it: one point of it says.

This is the reference, in Lisp, and is not what is used: the native
(poly-hit?) below tells the same, and a test holds the two together
```

### poly-hit?

```code
(poly-hit? a b [mode_a mode_b]) -> :t | :nil

do two sets of polygons touch, as (poly-hit-ref?) above tells it, in
native code. Not a hull is made for it: an edge of the one that has
no part in the box of the other is passed over, and that is enough
```

### poly-hull

```code
(poly-hull paths) -> path

the least path with no dent in it that has every point of the paths
in it or on it. Its points go round the one way, whatever the paths
do. Points that are all in one line give the two ends of it
```

### poly-lines-ref

```code
(poly-lines-ref out y0 y1 above below segs openl openr) -> below

the lines of the pieces of a band, as (poly-band) gave them, put on
segs, 5 numbers each, from x y to x y and the number of the edge it
is along, -1 for one that goes across: down the left of each piece,
up its right, and across at the top of the band where it or the band
above, above, u v u v ..., has a piece and the other has none.

A side that goes on from where the same side of the same edge ended
in the band above is not a new line, that one is made longer: openl
and openr have, for each edge, where in segs its last left and its
last right are, -1 for none.

below is made what is along the bottom, for the band below.
The reference, as (poly-band-ref) is
```

### poly-num

```code
(poly-num raw) -> fixed

the fixed number that is held as this whole number, its whole part
and its fraction made apart, so a big one has the room
```

### poly-op

```code
(poly-op op a b [mode_a mode_b]) -> paths

op is :union, :intersect, :difference, a and not b, or :xor.
a and b are lists of paths, either may be empty. A mode is the
winding rule a set is filled by, as (:fpoly) has them, 0 odd even,
the default, or 1 non zero
```

### poly-raw

```code
(poly-raw paths) -> ((x y ...) ...)

the paths with each number as the whole number it is held as
```

### poly-rings-ref

```code
(poly-rings-ref segs) -> ((x y ...) ...)

the lines, 5 numbers each, joined end to start into rings. Lines that
follow one another with the one number are one line
```

### poly-scan-ref

```code
(poly-scan-ref edges ys openl openr mode segs) -> segs

every band, from the top to the bottom, its lines put on segs
```

### poly-segs

```code
(poly-segs op a b [mode_a mode_b]) -> segs

the lines of what (poly-op) gives, 5 numbers each, not yet joined
into rings: where one starts and ends, and the edge it is of
op is :union, :intersect, :difference, a and not b, or :xor.
a and b are lists of paths, either may be empty. A mode is the
winding rule a set is filled by, as (:fpoly) has them, 0 odd even,
the default, or 1 non zero
```

### poly-sweep

```code
(poly-sweep shape points) -> paths

what a shape covers as it is moved along a line. shape is a path
about 0 0 with no dent in it, points the line it goes along, x y
x y. At each point is the shape, each two that follow one another
are joined by the hull of them both, and all of the hulls are made
one
```

### poly1305

```code
(poly1305 key data) -> str

the tag of a str, 16 bytes
```

### poly1305-add

```code
(poly1305-add ctx data) -> ctx

more of what the tag is of
```

### poly1305-end

```code
(poly1305-end ctx) -> str

the tag of all that was added, 16 bytes. The ctx is done with.
```

### poly1305-start

```code
(poly1305-start key) -> ctx

a tag with nothing in it yet. The state the native code keeps, the
bytes that do not yet make a block, and the half of the key that
is added at the end
```

### pos?

```code
(pos? num) -> :t | :nil
```

### ps-analysis

```code
(ps-analysis ps slots is34) -> ((re im) ...)

the 71 or 91 hybrid bands of the 38 slots of a channel: the lowest 3 or
5 bands split by the hybrid filters, each with 6 slots before, the rest
as they are
```

### ps-apply

```code
(ps-apply ps slots top) -> (left right)

the 32 QMF slots of each side, of the 38 of the one, top the band above
the last SBR made, the delays above it let go
```

### ps-bad?

```code
(ps-bad? ps e) -> :nil | :t

an intensity or a coherence of an envelope out of its range
```

### ps-decorrelate

```code
(ps-decorrelate ps bands is34) -> ((re im) ...)

the second channel's bands, each unlike the first's: by its parameter
band's transients a gain, then the lowest by all-pass filters of 3
links of their delays, those above by a delay of 14 slots, the top by 1
```

### ps-filter

```code
(ps-filter re im filter n) -> ((re im) ...)

n sub-subbands of a band of 44 slots, 6 before, by a complex filter
of 13 taps, symmetric, the 7 of each in the table
```

### ps-filter2

```code
(ps-filter2 re im g reverse) -> ((re im) (re im))

a band of 44 slots two, by a real filter of 13 taps of which only the
centre and the odd are not 0
```

### ps-huff

```code
(ps-huff ps reader book) -> value
```

### ps-map

```code
(ps-map to from spec) -> to

each band of the spec the mean of its bands of from, of numbers as C
divides them, to 0, of reals by the count
```

### ps-new

```code
(ps-new) -> ps
```

### ps-par

```code
(ps-par ps reader par num book e dt mask)

the parameters of an envelope, from those of the one before in time,
or each from the band below in frequency, kept to 3 bits if masked
```

### ps-read

```code
(ps-read ps reader bits) -> bits

the PS of an extension of bits, read by a reader of its own: its header
if it has one, its envelopes' borders, the intensities and coherences
of each, and of its extension the phases. How many bits it took, all
of them if it is wrong, and that wrong frame's parameters left none
```

### ps-remap

```code
(ps-remap par num is34 full) -> nums

an envelope's parameters of num bands as those of 34 or 20
```

### ps-series

```code
(ps-series slots q n) -> (re im)

band q's first n slots, of slots of 64 bands each
```

### ps-stereo

```code
(ps-stereo ps left right is34)

each band's two of the one and the unlike, mixed by its parameter
band's matrix of each envelope, interpolated over the envelope's
slots from the one before, its phases too if they were sent
```

### ps-synthesis

```code
(ps-synthesis bands is34) -> ((re im) ...)

the 32 slots of 64 QMF bands of the hybrid bands, those split summed
```

### ps-tables

```code
(ps-tables)

the books made native tables and the rest reals, made the once
```

### pset?

```code
(pset? form) -> :t | :nil
```

### ptr-at

```code
(ptr-at what key) -> :nil | index

where the value of key is in what a rule binds, keys and values in
turn. A key is looked for only where keys are: a value can be a
keyword that is a key's name too, (:tool :pen :pen 3)
```

### ptr-contact

```code
(ptr-contact id) -> num

which contact of its device a pointer is
```

### ptr-device

```code
(ptr-device id) -> device

the device of a pointer's id, the id of its contact 0
```

### ptr-event

```code
(ptr-event id kind buttons x y [pressure time]) -> event

kind is :mouse :pen :eraser or :touch. buttons is those held, 0 for a
pointer that is only over the board, or has just come up. pressure is
0.0 to 1.0, default 1.0 if a button is held. time in microseconds
```

### ptr-main

```code
(ptr-main event) -> event

the event as its main role, which is what a device's pen and width
are read by
```

### ptr-part

```code
(ptr-part event) -> :stroke | :fill | :both | :ink

what of a thing a pen given by this press colours: the main button the
outline, the second the fill, Chris: "left button bound pen to outline
and the right button bound pen to fill"; both, both; a finger, one pen,
what the thing shows
```

### ptr-roles

```code
(ptr-roles kind buttons) -> (buttons ...)

the buttons of the rules of a device a press is, :nil for a finger
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

### rack-fresh

```code
(rack-fresh phases [user gui asker]) -> str

on this machine, a new session for each phase, a list of command
lines, one after another, and what they said. A session is started by
the host, so it is on the boot image as it is on disk now, which a
phase before it may have made. With a user the session is theirs, and
not whoever last signed on to the machine, as run.sh -u has it. With
gui the session is on the GUI host, a desktop, its GUI up before the
command lines are run, as run.sh -f has it. With asker, a mailbox,
what a session says is mailed to it as it comes, "LINE text", and
if its node has gone the session is let go, and stops
the form has no space or tab in it, a node's args are split at those
```

### rack-run

```code
(rack-run cmdline [make_first leave_out gone user tree gui all skip only said]) -> (line ...)

make every machine that takes a sync the same as this one, then run
the command line on each, and on this one. With make_first, make,
make all boot and make apps are run first, in a session before. leave_out is the
machines not to run on, as (sync-services) names them, they are still
made the same. gone is paths to remove on the others. tree is the one
that is sent, default the system's own, the test of this sends a small
one. With gui the session is a desktop, see rack-fresh. cmdline may be
several, with a ; between, run one after another. With all every line a
session said comes back, not only those of how it went. skip is the
machines not to touch at all, not made the same and not run on: one
that is not ours, in the mesh as a guest. only, if given, is the
machines to touch, the start of a system id each, and no other: our
own rig, with guests in the mesh of any kind. said, a function, is
given what each session says as it says it, so a run that hangs shows
where. The lines are SYNC and GONE
for each other machine, SKIP for each skipped, RAN for each machine,
and DONE
```

### rack-session-clear

```code
(rack-session-clear)

the nodes a session started stopped, and its note, and the note of
every node it names, and of those they name: a node of the session
that started nodes of its own left one, a test that starts a node which
starts six more, and nothing else clears it away. The files of the
links they name go with them. A list of those still to look at, this
does not call itself
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

### reals??

```code
(reals?? form) -> :t | :nil
```

### reel-clock

```code
(reel-clock seconds) -> str
```

### reel-close

```code
(reel-close codec) -> :nil

let go of what the codec holds that is not in Lisp, a host decoder
```

### reel-codec

```code
(reel-codec reel [scale]) -> :nil | codec

:nil if there is none here for what the reel holds. With a scale, 2
3 or 4, the frames are shown that much smaller, the codec's canvas is
made with that scale and its :swap makes them so. :scale of the codec
is the one it took, a kind may take only some
```

### reel-frame

```code
(reel-frame codec n) -> :nil | canvas

frame n, in the order they are shown, on the codec's canvas
```

### reel-kind

```code
(reel-kind ext open packet codec frame [close])

files whose names end ext are reels: (open stream) -> :nil | reel,
(packet reel n), (codec reel scale), (frame codec n), (close codec). The
kinds are of the shared root of a GUI's node, gui/canvas/lisp.inc
imports them, and one of a name already there is replaced: a kind
added by a task would hold that task's functions, and so the task,
after it had gone
```

### reel-open

```code
(reel-open file [stream]) -> :nil | reel

the reel of a file, its kind by its name, read from the stream if
given, as for an object of a store, lib/files/names.inc
```

### reel-packet

```code
(reel-packet reel n) -> :nil | str
```

### reel-pcm

```code
(reel-pcm chans [scale]) -> (left right)

the native taken as it is used: a system not yet built with it can
still import this, the kinds are of a desktop's shared root
```

### reel-rescale

```code
(reel-rescale codec scale) -> :nil | codec

the codec shown at another scale, if its kind can: its decoder as it
is, nothing decoded again, on a canvas of the scale with the picture it
had. :nil if it can not, and a codec of the scale is to be made
```

### reel-wave

```code
(reel-wave canvas w h pcm) -> canvas

the picture of a reel of sound, a frame's samples, pcm the left and the
right as reals at the scale of 16 bits, or :nil for none: the left above
the right, a column of two pixels a sample, joined to the one before
```

### reel-whole

```code
(reel-whole str at bytes) -> num
```

### reflow

```code
(reflow words line_width [indent tab_width]) -> lines
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

### rle-compress

```code
(rle-compress in_stream out_stream [token_bits run_bits])
```

### rle-decompress

```code
(rle-decompress in_stream out_stream [token_bits run_bits max_tokens])
```

### ror-sum

```code
(ror-sum data bits [skip]) -> num

the checksum of the FAT file systems: the sum turned right a bit and
each byte added, bits wide, the bytes at the indices of skip left out.
exFAT's entries and its boot sectors, FAT32's long names
```

### rpad

```code
(rpad form width [str]) -> str
```

### rsa-hash

```code
(rsa-hash hash_name data) -> :nil | str
```

### rsa-mgf1

```code
(rsa-mgf1 hash_name seed len) -> str

the hash of the seed and a count, again and again, len bytes of it
```

### rsa-open

```code
(rsa-open n e signature) -> :nil | (bits em)

the signature to the power e mod n, as bytes as long as n, and how
many bits n has. :nil if the signature is not less than n
```

### rsa-prefix

```code
(rsa-prefix hash_name) -> :nil | str

the DER of the name of a hash, the DigestInfo the digest goes after
```

### rsa-verify-pkcs1

```code
(rsa-verify-pkcs1 n e hash_name digest signature) -> :t | :nil
```

### rsa-verify-pss

```code
(rsa-verify-pss n e hash_name digest signature) -> :t | :nil
```

### rsa-xor

```code
(rsa-xor a b) -> str
```

### rule

```code
a rule is kept as (weights low one fill pairs clauses), all worked out

here, the once, so that applying a rule is only a matter of looking
```

### s-arc

```code
(s-arc cx cy r a0 a1 [steps]) -> items

part of a circle, a stroke, from one angle to another in degrees,
anticlockwise as they grow
```

### s-arc-arrow

```code
(s-arc-arrow cx cy r a0 a1 [h]) -> items

an arc with a head on its end
```

### s-arrow

```code
(s-arrow x y x1 y1 [h]) -> items

a line with a head on its far end
```

### s-box

```code
(s-box x y x1 y1) -> items

a rectangle, filled, its corners as round as a stroke's
```

### s-corners

```code
(s-corners [a b l]) -> items

the four corners of a frame
```

### s-deg

```code
an angle in degrees as the system has angles
```

### s-disc

```code
(s-disc cx cy r) -> items

a circle, filled
```

### s-fill

```code
(s-fill x y x y ...) -> items

a shape, filled, its corners a little round
```

### s-flip

```code
(s-flip items) -> items

top for bottom
```

### s-head

```code
(s-head x y dx dy [h]) -> items

the head of an arrow, at x y, for a line that comes in along dx dy
```

### s-line

```code
(s-line x y x y ...) -> items

a stroke from point to point
```

### s-loop

```code
(s-loop x y x y ...) -> items

a stroke that comes back to where it began
```

### s-map

```code
(s-map items fx fy [fr]) -> items

every point of a thing moved, x by fx and y by fy, and a radius by fr
```

### s-mirror

```code
(s-mirror items) -> items

left for right
```

### s-ring

```code
(s-ring cx cy r) -> items

a circle, its line
```

### s-rrect

```code
(s-rrect x y x1 y1 [r]) -> items

a rectangle with round corners, its line
```

### s-small

```code
(s-small items [s]) -> items

a thing made smaller, kept to the top left, its strokes as thick as
ever. It leaves room for a badge
```

### s-turn

```code
(s-turn items) -> items

across for down, a thing that lies along is made to stand
```

### sbr-analysis

```code
(sbr-analysis data samples) -> ((re im) ...)

the 32 slots of 32 bands of 1024 samples: the newest 320 by the
window, its 5 parts of 64 summed, u, and band k 2 the sum of u n e^(i
pi (k + 1/2)(2n - 1/2)/64). Of n and 63 - n together that is 2 e^(-i 3
pi (k + 1/2)/128) by the DCT-IV of 32 of u n - u 63-n, re, and the
DST-IV of u n + u 63-n, im, a DCT-IV of it backwards by -1 to the k
```

### sbr-apply

```code
(sbr-apply sbr kind chans [alone]) -> (reals ...)

the 2048 samples of each of an element's channels of 1024, its kind
0 a channel or 1 a pair: each to the QMF bank's bands, its high band
made by the data read, and to samples at twice the rate; with PS, of
a channel alone in its block, the two of it
```

### sbr-assemble

```code
(sbr-assemble sbr data high gains)

the high band of each slot, its gains smoothed over the slots before
unless at a transient, its noise, or its sinusoid, in the slots of
this frame's Y: each slot's bands of the high band made a vector by
the gather, and all by vectors of the bands
```

### sbr-bands

```code
(sbr-bands start stop n) -> nums

the widths of n bands from start to stop, each the same ratio wider
```

### sbr-channel

```code
(sbr-channel) -> data

what a channel keeps: of the frame before its grid and the last of its
envelope and noise, its filters and the states of its QMF banks
```

### sbr-chirp

```code
(sbr-chirp sbr data)

each noise band's chirp, of how hard it is to be filtered, now and in
the frame before, smoothed
```

### sbr-data

```code
(sbr-data sbr reader kind) -> :nil | :t

a frame's data of a channel or a pair, coupled or not, and its
extensions stepped over
```

### sbr-dequant

```code
(sbr-dequant sbr)

the envelopes and noise floors as levels: of a coupled pair the first
the level and the second the balance, made each side's
```

### sbr-derived

```code
(sbr-derived sbr) -> :nil | :t

the high and low tables, the noise floor's and the limiter's, and the
patches, of the master table and where the high band begins
```

### sbr-dtdf

```code
(sbr-dtdf reader data)

each envelope and noise floor coded in time or in frequency
```

### sbr-envelope

```code
(sbr-envelope sbr reader data channel) -> :nil | :t

each envelope's scale factors, the first of a channel coded in
frequency from a start, else each from the one before it, in time
from the last envelope, its table of the other resolution mapped
```

### sbr-estimate

```code
(sbr-estimate sbr data high) -> (reals ...)

of each envelope the energy of the high band made, each band's, or
over each band of its table, by the slots of the envelope
```

### sbr-gains

```code
(sbr-gains sbr data maps energies) -> ((gain noise sine) ...)

of each envelope each band's gain, noise and sinusoid, each limiter
band's gains kept to its limit and its energy made what was sent,
native, lib/media/lisp.vp media_sbr_gains
```

### sbr-grid

```code
(sbr-grid sbr reader data) -> :nil | :t

the envelopes in time of a frame of 16 slots, the borders of each and
of the noise floors, and which is the one a transient starts
```

### sbr-grid-copy

```code
(sbr-grid-copy to from)

the second channel of a coupled pair has the first's grid, its own of
the frame before kept
```

### sbr-harmonics

```code
(sbr-harmonics sbr reader data)

which high bands have a sinusoid added
```

### sbr-header

```code
(sbr-header sbr reader)

a header: the spectrum, a reset if that is not as it was, and the
limiter, its bands made again if they are another number
```

### sbr-high

```code
(sbr-high sbr data low alphas) -> ((re im) ...) | :nil

the high band, the bands above kx each a band of a patch filtered by
its prediction and its noise band's chirp, over the slots of the
envelopes
```

### sbr-huff

```code
(sbr-huff sbr reader book) -> value

a code of one of the 10 books, what it stands for
```

### sbr-inverse

```code
(sbr-inverse sbr low) -> (alpha0 alpha1)

of each band below k0 the two terms of its prediction, by its
autocorrelation, each (re im), none of a band too tonal to predict
```

### sbr-invf

```code
(sbr-invf sbr reader data)

how hard each noise band is filtered, the last kept
```

### sbr-limiter

```code
(sbr-limiter sbr)

the bands of the limiter: of the low table and the borders of the
patches, those closer than its bands an octave merged, a patch's
border kept over another
```

### sbr-log2

```code
(sbr-log2 real) -> real

of 1 or more, by halving the step to it, pow being there and log not
```

### sbr-low

```code
(sbr-low sbr data) -> ((re im) ...)

the low band, each of 32 bands its 40 slots: 8 of the frame before and
the 32 of this one, below kx and kx before
```

### sbr-mapping

```code
(sbr-mapping sbr data) -> :nil | ((orig noise sines) ...)

of each envelope, each band of the high band its level, its noise
floor's, and if a sinusoid is in its band, of those added; which are
added, of the frame before too, kept
```

### sbr-master

```code
(sbr-master sbr) -> :nil | :t

the master table, the borders of the bands of the high band, k0 to k2
```

### sbr-new

```code
(sbr-new rate [ps]) -> sbr

of an element of a core of a rate, the SBR at twice it, till its first
header only the banks; with ps, parametric stereo makes two of its one
```

### sbr-noise

```code
(sbr-noise sbr reader data channel) -> :nil | :t

each noise floor's levels, as the envelopes are
```

### sbr-off

```code
(sbr-off sbr)

back to the banks alone, as when a frame's data is wrong
```

### sbr-patches

```code
(sbr-patches sbr) -> :nil | :t

the patches, each the bands below it that a run of bands above is
copied from, to the top of the high band
```

### sbr-read

```code
(sbr-read sbr reader crc bytes kind) -> bits

the SBR of a fill element of bytes, the reader past its 4 bits of
type, of the element of a kind before it, 0 a channel, 1 a pair: its
header if it has one, the tables made again if that says, and its
data. How many bits it took, a wrong frame turns SBR off
```

### sbr-round

```code
(sbr-round real) -> num
```

### sbr-sums

```code
(sbr-sums start widths) -> nums

the borders of bands of widths from start
```

### sbr-synthesis

```code
(sbr-synthesis data slots) -> reals

the 2048 samples of the 32 slots of 64 bands: each slot's 128 of its
bands newest in v, its 10 parts by the window, summed. Of the sum of
the re by cos and the im by sin of pi (k + 1/2)(2n - 255)/128, over
64, a and b, v n is a - b, v 127 - n is -a - b. The 255 is 2 pi (k +
1/2) less than (n + 1/2)(k + 1/2), so a is the DCT-IV of 64 of the re
over -64, b the DST-IV of the im, a DCT-IV of it backwards by -1 to
the n, over -64. The window's sum native, lib/media/lisp.vp
media_window, as MP3's
```

### sbr-tables

```code
(sbr-tables)

the books made native tables, the window, the noise and what the
banks take, made the once. The banks are DCT-IVs, of 32 and of 64,
(sbr-analysis) and (sbr-synthesis) say how, and the analysis's
twiddle after, 2 e^(-i 3 pi (k + 1/2)/128), its cos and its sin
```

### sbr-x

```code
(sbr-x sbr data low) -> ((re im) ...)

the 38 slots of 64 bands, 32 to the synthesis and the 6 after to the
hybrid filters of PS: the low band, and the high band made, of the
frame before for the slots its last envelope runs into this one
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

### scatter-place

```code
(scatter-place place [index val] ...) -> place

a new place, as that one with those parts set
```

### scene-order

```code
the triangles of count vertices taken three at a time
```

### scene-pipeline

```code
the two shaders a scene is drawn with, assembled the first time. Or

those of an object that has two of its own, their files
```

### screen-children

```code
(screen-children) -> (view ...)

the views of the screen, the windows, the mouse, the wallpaper. Not one
that stands where a view was hidden, (. view :hide), in gui/view: a
plain View, hidden, there till the frame is drawn so what was under the
view is drawn again, and freed with the frame. It is no window and has
no owner of its own, to be closed, told of a theme or a new size
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
statements in a scope of their own. A block can have blocks in it, and

they are done here one at a time, with a list as the stack of those
that are part done, as (flatten) does it. A frame is a block: its
forms, how far through them it is, its statements so far, how long
syms was when it began, and what is to be made of it when it is done
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

### sh-expr

```code
an expression, checked and typed. One can have others in it to any

depth, and no function here calls itself: what is still to be done is
a list, the next thing last, an expression or the making of one whose
args are done, and what has been made so far is another
```

### sh-float-to-real

```code
a real from the bits of a 32 bit float
```

### sh-fold

```code
(sh-fold node fnc) -> value

what a back end makes of an expression, from the bottom up. fnc is
(lambda (node args) ...) -> value, called for each node with what was
made of those under it, in order. A back end's own function for an
expression then calls nothing that calls it, the walk is here, with a
list as the stack of what is still to do, as (flatten) does it
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

### sh-pixel-only

```code
for a back end that has not got the vertex stage yet. It has only

pixel shaders with no varyings and no matrix in them
```

### sh-real-to-float

```code
the bits of a 32 bit float, from a real, a fixed or an int
```

### sh-returns?

```code
does every path through the block end at a return ? The arms of an

if that is last are blocks to ask it of in turn, on a list
```

### sh-stmt

```code
a statement, checked and typed, onto out. One that has blocks in it

does not do them, a function that calls itself has no stack to spare.
It leaves each on frames for (sh-block) to do next, with what is to
be made of it when it is done
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
An arm can be an if again. Each form that is to be looked at is a
place, a list and where in it, on a list of those still to do
```

### sha256

```code
(sha256 data) -> str

the hash of a str, 32 bytes
```

### sha256-add

```code
(sha256-add ctx data) -> ctx

more of what is being hashed
```

### sha256-end

```code
(sha256-end ctx) -> str

the hash of all that was added, 32 bytes. The ctx is done with.
```

### sha256-start

```code
(sha256-start) -> ctx

a hash with nothing in it yet. The state, the first 32 bits of the
fractions of the square roots of the first 8 primes, the bytes that
do not yet make a block, and how many bytes there have been.
```

### sha384

```code
(sha384 data) -> str

the hash of a str, 48 bytes
```

### sha384-end

```code
(sha384-end ctx) -> str

the hash, the first 48 bytes of what SHA-512 ends with
```

### sha384-start

```code
(sha384-start) -> ctx

SHA-384 is SHA-512 from another state, the first 64 bits of the
fractions of the square roots of the 9th to the 16th primes, added
to as SHA-512 is, (sha512-add)
```

### sha512

```code
(sha512 data) -> str

the hash of a str, 64 bytes
```

### sha512-add

```code
(sha512-add ctx data) -> ctx

more of what is being hashed
```

### sha512-end

```code
(sha512-end ctx) -> str

the hash of all that was added, 64 bytes. The ctx is done with.
```

### sha512-start

```code
(sha512-start) -> ctx

a hash with nothing in it yet. The state, the first 64 bits of the
fractions of the square roots of the first 8 primes, the bytes that
do not yet make a block, and how many bytes there have been.
```

### shader-attrs

```code
(shader-attrs program) -> ((name type) ...)

what a vertex shader reads of each vertex
```

### shader-compile

```code
(shader-compile forms [head_only]) -> program

with head_only only what a caller of a ready made function needs is
taken, the inputs, the attrs and varyings, the kind, and what each
function gives and takes. No body is read or checked, and the consts
and globals are :lazy. (shader-full) reads the rest when it is wanted.
```

### shader-cpu

```code
(shader-cpu program) -> lambda

(lambda x y x1 y1 input ... varying ... texture ...) -> (vec4 ...)
the lambda shades the pixels of the tile, row by row, the
centre of pixel x y is at frag coord x + 0.5, y + 0.5. A pixel
shader with varyings is given a value for each, after the inputs,
and every pixel of the tile has those. And then each of its
textures, as (shader-texture) gives one.
```

### shader-cpu-args

```code
(shader-cpu-args program [((name val) ...)]) -> (val ...)

the input args for the lambda, defaults for those not given, then a
value for each varying of a pixel shader, 0 for those not given, then
each texture of it, the pixmap given under its name as
(shader-texture) has it, or as it is if that is how it was given, and
no picture at all for one not given
```

### shader-cpu-func

```code
(shader-cpu-func program name) -> lambda

(lambda arg ...) -> value
a function of a file of functions, as a Lisp lambda. It is what the
native code of (shader-vp-func) is checked by. A float is a real, a
vector a reals, and a matrix a reals of 16, a row at a time.
```

### shader-cpu-tris

```code
(shader-cpu-tris vertex pixel verts tris width height [vvals pvals cull tri_size x y x1 y1]) -> str

triangles drawn with a vertex shader and a pixel shader, in Lisp, a
pixel at a time. It is slow, and is what the native code is checked
by. The args are those of (shader-vp-draw-tris), with the size of the
frame in place of a pixmap. The result is the pixels, row by row, the
top row first, each a 32 bit argb, 0 where nothing was drawn.
```

### shader-cpu-vertex

```code
(shader-cpu-vertex program) -> lambda

(lambda verts input ...) -> ((position varying ...) ...)
the lambda places each vertex of a list. A vertex is a list of its
attrs, in the order the shader has them. For each it gives where the
vertex is, a vec4, as main gave it, then what main left each varying
as, in the order the shader has them. A varying that is not set is 0.
```

### shader-dim

```code
(shader-dim type) -> :nil | 2 | 3 | 4
```

### shader-full

```code
(shader-full program) -> program

a program with all of it there. One that was loaded with only its
head has the rest read and checked now, into the same program
```

### shader-func

```code
(shader-func program name) -> (name type ((name type) ...))

a function of a program that Lisp can call
```

### shader-funcs

```code
(shader-funcs program) -> ((name type ((name type) ...)) ...)

the functions of a program, what each gives and takes
```

### shader-glsl

```code
(shader-glsl program) -> str
```

### shader-glsl-pair

```code
(shader-glsl-pair vertex pixel) -> (vertex_text fragment_text)

a vertex shader and a pixel shader that go together, to draw
triangles with, as a vertex shader and a fragment shader of GLSL.

The vertex shader has each attr of a vertex as an attribute, and each
of its varyings as a varying, of the same names, and its inputs as
uniforms. A varying it does not set is 0. Where it puts a vertex is
gl_Position as it is, z of -1 to 1 in view is what GL has.

The fragment shader has the varyings the pixel shader reads, by name,
and its inputs as uniforms. The frag coord is GL's own, y up. A pixel
whose alpha is under 1 in 255 is not drawn. The color leaves with its
alpha multiplied in, for a target that is blended that way.

An input the two both have, of the one name, is the one uniform, as
GL links them, and both are given the same precision so that it can.
```

### shader-gui

```code
(shader-gui program) -> :nil | shader

a shader the GPU can draw into a canvas, with (. canvas :shade shader
block), where block is from (shader-pack). :nil if this host can not.
The driver builds it in its own time, till then :shade draws nothing.
```

### shader-gui-frame

```code
(shader-gui-frame canvas draws) -> :nil | :error | canvas

a frame of triangles drawn by the GPU into the texture of a canvas,
with a depth buffer. draws is a list of (pair mesh vblock pblock
[textures]), a pair, a mesh, the blocks of (shader-pack) for the two
shaders of the pair, and a list of the textures of its pixel shader,
in the order it has them, each of (shader-gui-texture), as many as it
has. :nil is the GPU still busy with the last frame, or the
driver still building a pair, nothing was drawn, try again. :error is
a pair the driver could not build.
```

### shader-gui-mesh

```code
(shader-gui-mesh verts) -> :nil | mesh

the vertices of a mesh, kept on the GPU, to be drawn with a pair
whose vertex shader has those attrs. verts is the bytes of
(shader-verts-str). :nil if this host can not. It is let go of with
(canvas-mesh-destroy).
```

### shader-gui-pair

```code
(shader-gui-pair vertex pixel [cull]) -> :nil | pair

a vertex shader and a pixel shader that the GPU can draw triangles
into a canvas with, (shader-gui-frame). :nil if this host can not.
With cull a triangle that faces away is left out, :front those that
face us. The driver builds it in its own time. It is let go of with
(canvas-shader-destroy).
and after the attrs, how many textures the pixel shader has
```

### shader-gui-pair-text

```code
-> (vertex_stage fragment_stage)

the two stages of a pair, kept, both made if either is not there.
The fragment stage is written last, so if it is there both are
```

### shader-gui-sweep

```code
what the GUI back ends keep of another make goes, the first time
```

### shader-gui-texture

```code
(shader-gui-texture canvas) -> :nil | texture

the picture of a canvas as the GPU has it, to give a pair whose pixel
shader has a texture, (shader-gui-frame). The canvas must have been
shown, (. canvas :swap), and is to be kept for as long as it is drawn
with. :nil if it has none
```

### shader-kept-text

```code
(shader-kept-text name lambda) -> str

what a back end makes of a program, text or a module, kept in a
file of that name beside the native code, and made only if it is
not there
```

### shader-key

```code
(shader-key text) -> str

what a source is known by, a hash of it. The native code made from a
source is kept under its key, so a source that has been met before
is found again without a line of it being read
```

### shader-layout

```code
(shader-layout program) -> (size (name type offset) ...)
```

### shader-load

```code
(shader-load file) -> program

a program from a file. Only its head is read, and a hash of the file
taken, which is what its native code is kept under. The rest is read
and checked when a back end has to make something from it,
(shader-full), so a file whose native code is there already costs a
hash and a few lines
```

### shader-make

```code
(shader-make files) -> str

which make of a back end this is, a hash of the files it is written
in, 8 hex digits. It is part of the name of all that a back end
keeps, so that a change to the back end is a change of name, and
what the old one made is not found. Worked out once by a task.
```

### shader-mesh-send

```code
(shader-mesh-send msg verts)

answer a child that asked for a mesh, msg is what came to the ask
mailbox, verts the vertices of the mesh as a str, (shader-verts-str)
```

### shader-msl

```code
(shader-msl program) -> str

the entry point is fragment_main
```

### shader-msl-pair

```code
(shader-msl-pair vertex pixel) -> (vertex_text fragment_text)

a vertex shader and a pixel shader that go together, to draw
triangles with. The entry points are vertex_main and fragment_main.

The vertex function takes the attrs of a vertex as [[attribute(n)]],
in the order the shader has them, and its inputs at [[buffer(0)]].
What it hands on is each of its varyings, [[user(locn n)]]. Where it
puts a vertex has z of -1 to 1 in view, a GPU of this kind has 0 to
1, so z is moved as it leaves.

The fragment function takes the varyings the pixel shader reads, by
the place each has in the vertex shader's list, its inputs at
[[buffer(0)]], and the size of the target at [[buffer(1)]], from
which, and where the pixel is, comes the frag coord, y up. A pixel
whose alpha is under 1 in 255 is not drawn. The color leaves with its
alpha multiplied in, for a target that is blended that way.
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

### shader-pair

```code
(shader-pair vertex pixel) -> (vertex pixel)

a vertex shader and a pixel shader that go together. Every varying
the pixel shader reads must be one the vertex shader has, of the same
type. The vertex shader may have more, they are not used.
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

### shader-size

```code
(shader-size type) -> 1 | 2 | 3 | 4 | 16

how many numbers a value of the type is
```

### shader-spirv

```code
(shader-spirv program) -> str

the entry point is fragment_main
```

### shader-spirv-pair

```code
(shader-spirv-pair vertex pixel) -> (vertex_module fragment_module)

a vertex shader and a pixel shader that go together, to draw
triangles with. The entry points are vertex_main and fragment_main.

The vertex module takes the attrs of a vertex at locations, in the
order the shader has them, and its inputs as a block, set 1 binding
0. It hands on each of its varyings, at a location. Where it puts a
vertex has z of -1 to 1 in view, a GPU of this kind has 0 to 1, so z
is moved as it leaves.

The fragment module takes the varyings the pixel shader reads, at
the place each has in the vertex shader's list, its inputs as a
block, set 3 binding 0, and the size of the target, set 3 binding 1,
from which, and where the pixel is, comes the frag coord, y up. A
pixel whose alpha is under 1 in 255 is not drawn. The color leaves
with its alpha multiplied in, for a target that is blended that way.
```

### shader-spirv-vertex

```code
(shader-spirv-vertex) -> str

the vertex shader that goes with every fragment shader, one triangle
that covers the target. Its uniform, set 1 binding 0, is the size of
the target, and it gives each pixel its frag coord, with y up. The
entry point is vertex_main.
```

### shader-stage

```code
(shader-stage program) -> :pixel | :vertex | :func
```

### shader-strip

```code
(shader-strip vfile pfile ask shared width height y y1 cull draws) -> job

the job for rows y to y1 of a frame of that width and height. ask
is the mailbox a child asks for a mesh at, shared the key of the
pixels, (canvas-key). draws is a list of (mesh vblock pblock [y y1
tfiles]), the number of a mesh, the blocks of (shader-pack) for the
two shaders, the rows of the frame the mesh may be on, if the app
knows, all of them if not, and a list of the textures of the pixel
shader, if it has any, in the order it has them, each of
(shader-strip-texture).
```

### shader-strip-texture

```code
(shader-strip-texture canvas) -> :nil | str

a picture as a strip names it, for a canvas whose pixels are in
shared memory, (canvas-shared). :nil if they are not, the children
of a farm can not reach them
```

### shader-sweep

```code
(shader-sweep letters makes) -> :t

what a back end keeps in lib/gpu/jit/ is named a letter of its own,
its make, 8 hex digits, and then the program. Kept by another make
it is never found again, so it goes: every file of these letters
whose make is not one of these is removed. Once for a task for each
set of makes, the first time it is asked
```

### shader-texture

```code
(shader-texture pixmap) -> (width height pixels)

a picture as the Lisp back end reads one, its pixels a str, row by
row, the top row first, each a 32 bit argb as the pixmap has them
```

### shader-textures

```code
(shader-textures program) -> ((name) ...)

the pictures a pixel shader reads from, in the order it has them
```

### shader-tile

```code
(shader-tile file inputs x y x1 y1 width height shared) -> job

the job for a tile of a frame of that width and height, inputs is the
block from (shader-pack), shared the key of the pixels, (canvas-key)
```

### shader-tile-show

```code
(shader-tile-show canvas msg) -> canvas

the answer to a tile. If the child could not reach the pixels of the
canvas they came with the answer, and are put there
```

### shader-unpack

```code
(shader-unpack program block) -> ((name val) ...)

a float comes back as a real, a vector as a reals
```

### shader-varyings

```code
(shader-varyings program) -> ((name type) ...)

what a vertex shader sets for the pixel shader, or a pixel shader reads
```

### shader-verts-str

```code
(shader-verts-str reals) -> str

the bytes of a reals of vertices, to send in a message
```

### shader-vp

```code
(shader-vp program) -> (shade frame_size)

the native function for a pixel shader. It is assembled if this is
the first time this CPU has been given this program.
```

### shader-vp-argb

```code
(shader-vp-argb native frame x y x1 y1 [height]) -> str

the pixels of a tile, row by row, each a 32 bit argb. If the height
of the frame is given then row 0 is the top row, as a canvas has it.
```

### shader-vp-depth

```code
(shader-vp-depth width height) -> depth

a depth buffer for a pixmap of this size, with nothing in it, all of
it as far away as can be. A new one is made for each frame, and it is
a copy of one that is kept, the one copy of the bytes. What is kept is
all but the last 4 bytes, so that joining them on is always a new
str, the one that is kept is never the one that is handed out
```

### shader-vp-draw

```code
(shader-vp-draw native frame pixmap x y x1 y1 [height alpha]) -> :nil | pixmap

a tile drawn straight onto a 32 bit pixmap, where it belongs on it,
with no copy of the pixels in between. :nil if the tile is not all
inside the pixmap. The pixels are full on, so they are right for a
pixmap that is premultiplied as well as one that is not. With alpha
the alpha of a pixel is the one main gave, for a shader that is to be
seen through. A pixmap is premultiplied, so such a shader gives its
color times its alpha.
```

### shader-vp-draw-tris

```code
(shader-vp-draw-tris pipeline verts tris pixmap depth [vvals pvals cull tri_size x y x1 y1 depth_y]) -> pixmap

draw triangles on a 32 bit pixmap. verts is a reals, the attrs of a
vertex one after another, vertex after vertex, or the bytes of one
in a str. tris is a nums, three
numbers of vertices for each triangle, the first three of every
tri_size, 3 if not given. vvals and pvals are the inputs of the two
shaders. Where a vertex shader puts a vertex, x and y of -1 to 1 are
the edges of the pixmap, y up, and z of -1 to 1 is in view, nearest
first. With cull a triangle whose vertices go round clockwise as seen
is left out, it faces away. A cull of :front leaves out those that go
round the other way, for a mesh made the other way round, or a view
that is turned over. Only the pixels of x y x1 y1 are drawn,
all of the pixmap if they are not given, so that a frame can be drawn
a part at a time, or by several tasks. A task that draws rows y to y1
only can have a depth buffer of just those rows, and says so with a
depth_y of y, the row its depth buffer starts at. A pixel whose alpha
is 0 is not drawn, and leaves the depth buffer alone. One that is
full on is written. One between goes over what is there, which is
taken to have had its alpha multiplied in, and its depth is kept as
any other, so what is see through is to be drawn after what is not,
the furthest first.
A triangle that the near plane goes through, where z is -w, is cut by
it, and what is in front is drawn.
```

### shader-vp-fill

```code
(shader-vp-fill program) -> (fill frame_size params vary_slots)

the native function for a pixel shader that fills triangles with it
```

### shader-vp-frame

```code
(shader-vp-frame program native [((name val) ...)]) -> frame

the frame the native function works in, with the inputs set
```

### shader-vp-func

```code
(shader-vp-func program name) -> func

a function of a file of functions, as a native function Lisp calls,
(func arg ...) -> value. A :float is a real, an :int a num, a vector
a reals of its size, a :mat4 a reals of 16, a row at a time, and
what comes back is one of those, made new. It is assembled the first
time this CPU meets it, and kept.
```

### shader-vp-pipeline

```code
(shader-vp-pipeline vertex pixel) -> pipeline

a vertex shader and a pixel shader as native code, to draw triangles
with. Each is a function of its own, so a vertex shader is assembled
once however many pixel shaders it is used with, and a pixel shader
once however many vertex shaders. What ties the two is where in a
placed vertex each varying of the pixel shader is.
```

### shader-vp-pixels

```code
(shader-vp-pixels native frame x y x1 y1) -> (vec4 ...)

the pixels of a tile, row by row, each a reals of 4
```

### shader-vp-place

```code
(shader-vp-place native frame verts) -> reals

place the vertices of a reals, the attrs of one after another, vertex
after vertex. The result is a reals, for each vertex where it is, 4
numbers, then its varyings. The vertices can be a str, the bytes of
such a reals, 8 to a number, as (shader-verts-str) makes, which is
how they are when they have come in a message.
```

### shader-vp-vertex

```code
(shader-vp-vertex program) -> (place frame_size attr_slots out_slots)

the native function for a vertex shader, it places vertices. A vertex
is attr_slots numbers, its attrs one after another, and what comes
out for it is out_slots numbers, where it is, 4, then its varyings.
```

### share-acked

```code
(share-acked board key) -> board

that one has what it was last sent, the next can go
```

### share-add

```code
(share-add board key) -> board

a board to send to, that has nothing yet: the first it is sent is all
```

### share-apply

```code
(share-apply board text from) -> :t | :nil

make the document what a delta from that one says: the items it puts,
in the place of those of their ids or on top, those it says are gone
taken out, and those it knows in its order, in the places they are
in. What this board has that it does not know stays where it is. What
that one was drawing is drawn here. What changed is noted for the
rest, but for that one: it has it. Was it a delta
```

### share-delta

```code
(share-delta board key) -> :nil | text

what that one is to be sent now, if it is not waiting on the last, and
there is anything to send. It is then waiting, (share-acked)
```

### share-drop

```code
(share-drop board key) -> board

one no longer sent to, and what it was drawing goes
```

### share-held-for

```code
(share-held-for board key) -> ((id who) ...)

what is held by any but that one: what it is to be told is taken, and
the number of the board that has it, as a pointer has, for its color
```

### share-id-base

```code
(share-id-base board base) -> board

a board that has joined gives new things ids from base on, so no two
boards give the same id to two things
```

### share-ids

```code
(share-ids doc) -> ids

the ids of the document's items, the back one first
```

### share-note

```code
(share-note board) -> board

what has changed since this was last asked is told to each it sends to
```

### share-pointers

```code
(share-pointers board key) -> ((name x y kind down who) ...)

where this board's pointers are, those that have moved of late, and
those of the boards that send here but the one it is to go to, which
has its own. Names are this board's own ids, and as they came to it.
who is the number of the board a pointer is of, the host 0 and each
that joined the next, by where its users begin, for its color
```

### share-self-holds

```code
(share-self-holds board) -> board

what this board's hands hold now, and what its users have selected, is
its own, if no one else has it: a thing selected here is not to be
taken there, "own only", as it is not by another user here
```

### share-start

```code
(share-start board [host]) -> board

from now what changes in the document is noted for those it is shared
with. The host's, the teacher's, says who has hold of what
```

### share-stop

```code
(share-stop board) -> board
```

### share-temps

```code
(share-temps board key) -> ((name shape) ...)

what is being drawn here, and on the boards that send here, but for
the one it is to go to: it has its own
```

### share-tops

```code
(share-tops doc ids) -> ids

the items of the document, not those in a group, that are or have
in them one of these
```

### short-to-hex-str

```code
(short-to-hex-str num) -> str
```

### shuffle

```code
(shuffle list [start end]) -> list
```

### sign-extend

```code
(sign-extend v bits) -> num

v, the low bits of a number of two's complement, as that number
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

### sound-add

```code
(sound-add datas) -> data

pieces added, sample by sample, as long as the first, kept in range: a
mix. One shorter than the first is quiet after its end
```

### sound-from-file

```code
(sound-from-file in out path [ctl])

the samples of a sound file, by its format, or till ctl is sent anything
```

### sound-gain

```code
(sound-gain in out percent)

louder or quieter, 100 as it is
```

### sound-info

```code
(sound-info file) -> :nil | (rate channels bits bytes)

what a sound file is, and how many bytes of samples it has
```

### sound-level

```code
(sound-level in out who)

each piece passed on, and how loud it was sent to who, a number
```

### sound-loader

```code
(sound-loader ext info head tail)

files whose names end ext are read and written by these
```

### sound-loader-of

```code
(sound-loader-of file) -> :nil | (ext info head tail)
```

### sound-lowpass

```code
(sound-lowpass in out hz)

what is above hz let through less, a pole of its own
```

### sound-map

```code
(sound-map data fn) -> data

each sample of the piece made new, (fn sample) -> sample, kept in range
```

### sound-mic

```code
(sound-mic in out ctl)

what the microphone hears, in the order it was heard, till ctl is sent
anything. A piece that is not there when two after it are is lost
```

### sound-order

```code
(sound-order seq) -> order

the place in what comes: the number of the piece to be had next, and
what has come before it is wanted
```

### sound-peak

```code
(sound-peak data) -> num

the loudest of a piece, every other sample looked at
```

### sound-pieces

```code
(sound-pieces in fn)

each piece of the in stream, to its end, given to fn
```

### sound-sample

```code
(sound-sample data index) -> num

the 16 bit sample at index of samples, signed
```

### sound-seconds

```code
(sound-seconds file) -> num

how long a sound file is, in whole seconds
```

### sound-speaker

```code
(sound-speaker in out)

played on the Audio service, at the pace it is heard, 100ms ahead
```

### sound-stage

```code
(sound-stage fn [arg ...]) -> stage

a stage of this module
```

### sound-stopped?

```code
(sound-stopped? ctl) -> :t | :nil

has a source been told to stop, ctl sent anything, if it has a ctl
```

### sound-take

```code
(sound-take order seq data) -> datas

a piece come, and what can be had now, in order: one that is not there
when more than two after it are is lost, and one that comes after
that is let go
```

### sound-to-file

```code
(sound-to-file in out path)

the samples to a sound file by its format, written as they come, its
length put in at the end
```

### sound-tone

```code
(sound-tone in out hz ms [level ctl])

a tone of hz a second for ms, at level of 32767, or till ctl is sent
anything
```

### sound-write

```code
(sound-write out data)

a piece passed on now: a stream holds what is written till it has some
6KB, nearly ten pieces, 190ms of a voice before any of it went on
```

### source-ask

```code
(source-ask svc path) -> :nil | text

a file's text from a @Source, :nil if it has none it will give, or it
says nothing in the time. A new mailbox each time: an answer that comes
late is to a mailbox nobody reads
```

### source-caching?

```code
(source-caching?) -> :t | :nil

may translations be kept on this system's disk: not if the system says
not, *env_source_cache* :nil, for its security or as it has no disk of
its own, and then every one is made each time it is wanted
```

### source-ffi-form

```code
(source-ffi-form path more) -> form

what (ffi path ...) is in a task sent from another system: the native
made ready first, then bound as ever
```

### source-here

```code
(source-here) -> netid

this system's @Source, started if it has none, as a node that only
runs what it is sent may not
```

### source-links

```code
(source-links fn) -> (name ...)

the functions a native function links to, by the paths in it
```

### source-mode

```code
(source-mode) -> str

the build of this node, which VP it wants
```

### source-native

```code
(source-native path)

a native function of the system that sent this task, and all it calls
that this node has not the same of, made ready by this system's own
@Source, which keeps a translator for this CPU, and placed in the
task's function server: in memory, none of it kept in a file of the
task's. What the node has the same VP of is server zero's, nothing is
placed for it
```

### source-pack

```code
(source-pack entries) -> str

files, (path tag text) each, one after another: its path, its tag and
the length of its text on lines of their own, then the text
```

### source-service

```code
(source-service) -> :nil | netid

the @Source of this system
```

### source-split

```code
(source-split text) -> (native ...)

native functions one after another, each as long as its header says
```

### source-stream

```code
(source-stream sid svc path) -> :nil | stream

a file of a system as a stream, by the system id in hex and its
@Source: a file stream on that system. On another the @Source of this
one is asked, once, for the file and all it imports, all the way down:
it has them of that system, asked once for all the tasks here. Each is
L, the copy here is the same, as what the two systems have in common
is, T and the text, or n. What this task is given it keeps, and its
imports after are read from that
```

### source-task

```code
(source-task path) -> task

what to open, with (open-task) and the rest, for a Lisp file of this
system to run on any node of the mesh: it and what it imports are read
from this system, and need not be where it runs. A task that was sent
from another system sends its tasks as of that one, which has its
files. The path itself if there is no @Source, read where it runs as
any task is
```

### source-unpack

```code
(source-unpack str) -> ((path tag text) ...)
```

### split-cells

```code
the cells of a row. One with nothing in it is a cell, it keeps what

comes after it in its own column
```

### spv-block

```code
the statements of a block, up to the one that leaves it. An if and a

for have blocks in them, and no function here calls itself: what is
still to be done is a list, the next thing last, a statement, or what
comes after a block that was put there before it. A statement that
leaves, a break or a return, has the rest of its own block taken off
```

### spv-block-var

```code
a uniform block of these members, -> (var_id ptr_type_id ...) the

pointer types are those of the members. A matrix is a column at a
time, each 16 bytes on from the last
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

### spv-port

```code
a variable that comes in to the module, class 1, or goes out, class

3, at a location, decor 30, or as a built in, decor 11. A shader's
name for it, if it has one, reads and sets it as it is
```

### spv-program

```code
(spv-program program) -> block

the inputs block of a program, a variable for each of its inputs,
constants and globals, and its functions
```

### spv-ptr

```code
a pointer type, class is 1 input, 2 uniform, 3 output, 6 private, 7 function
```

### spv-splat

```code
a float that goes with a vector is made a vector
```

### spv-start

```code
at the start of the entry point the inputs are copied from the block,

and the constants and the globals are set
```

### spv-str

```code
the words of a string, it ends with a zero byte
```

### spv-tex-type

```code
the id of the type of a picture that is read with a place, a 2D image

of floats and how it is read, as the one thing
```

### spv-texture

```code
a picture of a pixel shader, a variable of the module, at set 2, at

the binding that is its number, as SDL's GPU interface wants it
```

### spv-type

```code
the id of a type, made the first time it is asked for. A vector is of

floats, and a matrix of vectors, its columns, and those are made here
if they are not there, this does not call itself for them. The ids
are given out in the order they always were, the float, then this
type, then the vector a matrix is of
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

### stage

```code
(stage imports fn [arg ...]) -> stage

imports a path or a list of them
```

### stage-form

```code
the source of a stage's task. in and out are each a form, done the

mailbox told it has finished, if any. No function of this module is
called in a backquote: it is looked for as the form is made, when the
module's names are gone
```

### stage-in

```code
an in stream whose mailbox the stage gives the one before it
```

### stage-mbox

```code
a mailbox as a form
```

### stages-load

```code
(stages-load stream pixmap type stages) -> :t | :nil
```

### stages-run

```code
(stages-run stages [done]) -> stages

a pipeline whose ends are stages too: the first a source, made with no
in stream, :nil, the last a sink, with no out stream, :nil. Sound is
so, a microphone at one end, a speaker or a file at the other. It runs
by itself, done, if given, is sent :t when the sink has finished
```

### stages-save

```code
(stages-save pixmap type stream stages) -> :t
```

### start

```code
start a child, where the farm says
```

### stdio-get-args

```code
(stdio-get-args stdio) -> cmd_line
```

### stop

```code
stop a child
```

### storage-ask

```code
(storage-ask svc type [body wait]) -> :nil | answer

one thing asked of a service, and what it says. :nil if it says
nothing in the time, seconds. A new mailbox each time: an answer that
comes late is to a mailbox nobody reads
```

### storage-block

```code
(storage-block name [svc]) -> :nil | data

a block by its name, from the service if it has it, and if not from
another member that has. The asker goes to the other member, a
service never asks another and waits: two that did, each of the
other, would wait for ever
```

### storage-block-at

```code
(storage-block-at name svc) -> :nil | data

a block by its name, if that service has it. It is checked against
its name here too, what came may not be what was sent
```

### storage-block-retry

```code
(storage-block-retry name svc secs) -> :nil | data

a block by its name, of any member that has it, asked again each
second till there is one, or for secs seconds
```

### storage-check

```code
(storage-check [mend svc]) -> :nil | (name ...)

the blocks that are not what their names say, in hex. With mend they
are let go of
```

### storage-claim

```code
(storage-claim image) -> :nil | :t

is the store in that file this process's to serve. It is if no
process that is still there has said it is its own. It says so, in
a file beside the image, waits a moment, and looks again: of two
that say so at once the last to write is what both read, so one of
them goes on and the other does not
```

### storage-copies

```code
(storage-copies path [copies svc]) -> :nil | num | (:ok entry) | (:none :nil) | (:lost :nil)

how many members are to keep the blocks of the object a path is, the
dial: more for what must not be lost, or is read a great deal, fewer
for what can be had again. 0 is as many as the store says. It is a
write of the path, and the members come to it as they hear. Without
copies, how many it is now, :nil if there is no such object
```

### storage-cut

```code
(storage-cut members [svc]) -> :nil | :t

for a test: the member is as if it could not reach those members,
its tender does not look at them. None, and it can reach all again
```

### storage-delete

```code
(storage-delete path [from svc]) -> (:ok entry) | (:stale entry) | (:none :nil) | (:lost :nil)

there is no object of that name from now, provided it is from
```

### storage-from

```code
what a path is said to be now, as it is sent: ? not said, - none
```

### storage-get

```code
(storage-get path [svc]) -> :nil | data

all of an object. :nil if there is none, or a block of it can not be had
```

### storage-info

```code
(storage-info [svc]) -> :nil | emap

what the store is: :id :member :members :block_size :copies :objects
:blocks :free
```

### storage-info-read

```code
(storage-info-read answer) -> :nil | emap

what a service said of its store, as (storage-info) gives it: for
what asked with (storage-send), of several at once
```

### storage-join

```code
(storage-join [image size id copies force wait]) -> :nil | netid

the store of this machine is started, in this session: the one in
that file, storage/store.img if it is not said, made if there is
none, of so many MB. Its id is what is said, or the mesh's own,
(storage-key-id), or store. :nil if another process has it, or it
could not be started. wait is as (storage-start) has it
```

### storage-key-id

```code
(storage-key-id [file]) -> :nil | id

the id of the store of the mesh this machine is of: made from its
key, so machines given the same key are of the same store with
nothing said, and not the key, nor anything the key can be had from.
:nil if the machine has no key
```

### storage-list

```code
(storage-list [prefix svc]) -> :nil | (entry ...)

the paths that begin so, in order
```

### storage-manifest

```code
the blocks of the object a path is, (bytes names), :nil if there is none
```

### storage-member

```code
(storage-member [file]) -> :nil | emap

what this machine was told when it was made a member: :image, :size
in MB, :id and :copies if they were said, and :travels, :t if it is
a machine that is taken away from the others, a laptop. :nil if it
never was a member
```

### storage-member-forget

```code
(storage-member-forget [file])

this machine is not started as a member by itself any more. Its
store is left as it is
```

### storage-member-save

```code
(storage-member-save image size [id copies file travels])

this machine is a member, of the store in that file, from now on
```

### storage-member-travels

```code
(storage-member-travels travels [file]) -> :nil | :t

this machine is one that is taken away from the others, or is not
now. :nil if it is no member
```

### storage-members

```code
(storage-members [change svc]) -> :nil | (member ...)

who the members of the store are meant to be. With a change, "+id"
a machine is joined to it, "-id" one is retired: it is told to this
member, and the others hear of it as they meet
```

### storage-pace

```code
(storage-pace tick grace [svc]) -> :nil | :t

how often the member's tender looks, 0 for only when told, and how
long a member is not heard from before what it kept is kept by
others, from now on
```

### storage-pin

```code
(storage-pin prefix [off svc]) -> :nil | (prefix ...)

this system holds all of every object whose path begins so, or with
off no longer does. What is pinned now
```

### storage-pinned-by

```code
(storage-pinned-by path [svc]) -> :nil | (member ...)

the members that have that path pinned, that hold all of its object,
as far as this system knows. Every member is told what each has pinned
```

### storage-pins-missing

```code
(storage-pins-missing [svc]) -> :nil | (name ...)

the blocks of what is pinned that this system has not got yet, in
hex. None, and it is all here
```

### storage-put

```code
(storage-put path data [from svc]) -> (:ok entry) | (:stale entry) | (:full :nil) | (:lost :nil)

a path is set to an object of this data, as (storage-put-stream)
```

### storage-put-file

```code
(storage-put-file path file [from svc]) -> (:ok entry) | (:stale entry) | (:full :nil) | (:lost :nil) | (:none :nil)

a path is set to an object of what is in a file of the host, of any
length. :none if there is no such file
```

### storage-put-stream

```code
(storage-put-stream path stream [from svc]) -> (:ok entry) | (:stale entry) | (:full :nil) | (:lost :nil)

a path is set to an object of what is read from a stream, to its
end, provided it is now the object from: its name, "" for none, and
not given, not to say. :stale with what it is if it is not that. It
is read a few blocks at a time, so what is put can be of any length,
and only the blocks the service has not got are sent
```

### storage-read

```code
(storage-read path offset len [svc]) -> :nil | data

part of an object, only the blocks that part is in are asked for
```

### storage-record

```code
(storage-record rec member kind [what]) -> rec

a thing done, kept to be sent, if there is a recorder
```

### storage-record-ask

```code
(storage-record-ask svc mbox [stop]) -> :nil | :t

a member is to tell that mailbox what it does, for so long, or with
stop, not to. With :nil for the mailbox, it is to tell nobody
```

### storage-record-members

```code
(storage-record-members [group]) -> (netid ...)

the services of every member of a store of that group that is there,
on every machine, *Storage if not given
```

### storage-record-name

```code
a block as a record names it, enough of it to be told from the rest,

- for none, a path deleted
```

### storage-record-new

```code
(storage-record-new) -> rec

what a task of a member keeps of its recorders: each, (mbox until),
and the lines it has still to send
```

### storage-record-run

```code
(storage-record-run write until [group stop]) -> lines

what every member of a group does, each line given to write as it
comes, till the time is until, (pii-time), or a mail is on the stop
mailbox. Each member is asked again each second, and one that has
come since is asked then too. write is given :nil when nothing has
come for a tenth of a second, for one that holds lines back to send a
few at a time. How many lines there were. Its names
are its own, record_..., so that write finds its own
```

### storage-record-seed

```code
(storage-record-seed write [group]) -> lines

what every member of a group has now, as the lines a recording of it
from the start would have left, each given to write: who each sees,
what each has each path as, the blocks of each object, and which of
them each keeps. So a recording begun now has what came before. How
many lines there were. Its names are its own, seed_..., so that write
finds its own
```

### storage-record-send

```code
(storage-record-send rec) -> rec

what was kept is sent, in one mail
```

### storage-record-to

```code
(storage-record-to rec text) -> rec

a recorder, the mailbox in hex, is told from now, or for longer. -
and a mailbox, that one is not, and - alone, none is
```

### storage-recording?

```code
(storage-recording? rec) -> :nil | :t

is a recorder told, those whose time is up are forgotten
```

### storage-rename

```code
(storage-rename from to [was svc]) -> (:ok entry) | (:stale entry) | (:none :nil) | (:lost :nil)

the path to is given the object the path from is, and from is then no
more, in one step: nothing is copied, a path is only the name of the
object it is. Provided to is now the object was, "" for none, :nil not
to say, :stale with what it is if it is not. :none if from is not.
What a program does that is to come through anything: write a path of
its own, see that all went as it should, and rename it to where it is
to be, and who reads that has the old object or the new, whole
```

### storage-resize

```code
(storage-resize megabytes [svc]) -> :nil | path

the store of this member put on a volume of that many MB, a copy on
this machine, and the file it is in now: storage/store.8192MB.img for
storage/store.img. A machine whose member it is starts it from there
from now on. :nil if it is that size already, or can not be made so,
and it is as it was. The member answers nothing while it copies
```

### storage-resized-name

```code
(storage-resized-name image megabytes) -> path

where a store of that file is put when it is made that size: beside
it, the size in the name, storage/store.8192MB.img
```

### storage-restore

```code
(storage-restore name [prefix svc]) -> :nil | (restored newer)

the objects whose paths begin so are as they were in the snapshot,
all of it if not said, how many were set back, and how many paths
there are now under the prefix that the snapshot has not, which are
left. :nil if there is no such snapshot
```

### storage-result

```code
(storage-result answer) -> (:ok entry) | (:stale entry) | (:missing names) | (:full :nil) | (:none :nil) | (:lost :nil)

what the service said of a write, as (store-commit) gives it. :lost
if it said nothing
```

### storage-retire

```code
(storage-retire start [svc]) -> (:ok member ...) | (:none) | (:many id ...) | :nil

the member whose id starts so retired, case aside, as (storage-service-on)
takes the start of a system id. Only if one member's id starts so:
:none if none does, and :many and theirs if more than one, none retired
```

### storage-same-file?

```code
(storage-same-file? path file [svc]) -> :nil | bytes

an object, read as a stream, is what is in a file of the host, byte
for byte and no longer. How long that is
```

### storage-seen

```code
(storage-seen path [svc]) -> :nil | (seen members)

how many of the members are known to have what a path is now, and
how many members there are: seen by 4 of 5
```

### storage-send

```code
(storage-send svc type [body]) -> mbox

one thing asked of a service and not waited for. Where what it says
will be, to be read when it is wanted: several can be asked at once,
of several services, and be on their way together
```

### storage-service

```code
(storage-service [name]) -> :nil | netid

the Storage service of this system, or one of another name, a test's
```

### storage-service-on

```code
(storage-service-on system) -> :nil | netid

the Storage service of another machine, by the start of its system id
in hex, as mesh lists it: what is in its store looked at, put or got,
as a client, this machine no member of it
```

### storage-settle

```code
(storage-settle [svc wait]) -> :nil | (met fetched dropped)

the member's tender is told to look now, and what it did is waited
for: how many members it compared notes with and found something
new, how many blocks it fetched, and how many it let go of
```

### storage-snapshot

```code
(storage-snapshot name [prefix svc]) -> :nil | count

a snapshot of the objects whose paths begin so, all if not said, kept
as snapshots/name/ and their paths, how many: no block is copied, see
lib/storage/store.inc. :nil if the name is not one a snapshot may have,
not empty, no / or space
```

### storage-snapshot-drop

```code
(storage-snapshot-drop name [svc]) -> :nil | count

a snapshot is gone, how many paths it had. Its blocks are let go of by
a sweep, if nothing else is made of them
```

### storage-snapshots

```code
(storage-snapshots [svc]) -> :nil | ((name count) ...)

the snapshots there are, and how many objects each is of
```

### storage-start

```code
(storage-start image [:size bytes :id str :name str :block bytes :force :t

	:group str :member str :tick us :grace us :copies num :node node :wait s]) -> :nil | netid
the service is started on this node, or that one, to keep the store that is in
that file of the host. If there is none there and a size is given,
one is made of that many bytes, the store id, and this system its
member. :nil if there is no store and none could be made. What is
there and is not a store is left as it is, unless force: then it is
made one, and what was there is gone, a disk that is to be a store.
If the service of that name is there already, it is that one.

name is what it is known by on this system, @Storage. group is what
the members of a store find each other by, over every machine,
*Storage. member is who it is, this system: another only for a test
of several members on the one machine. tick is how often its tender
looks, 0 for only when it is told to, and grace how long a member is
not heard from before what it kept is kept by others.

wait is how long it is given to say it is up, 30 s, or :t, as long
as it takes while its task is there, or its node, on another: a big
store is minutes opening, and one given up on comes up all the same.

node is the node it is started on, this one if not said. Another
was tried, so that the hashing of a block and its writing were done
beside what the asker does: each block's crossing of the link cost
more than the second core gave, a copy into a store fell 61 to 50
MB/s, a read from it 116 to 60
```

### storage-stat

```code
(storage-stat path [svc]) -> :nil | entry

what a path is now, :nil if there is no object of that name
```

### storage-state

```code
(storage-state [svc]) -> :nil | emap

what a member is, as its tender and other members ask it: :member
:store :digest :members :retired :cut :tender
```

### storage-state-read

```code
(storage-state-read answer) -> :nil | emap

what a service said of itself, as (storage-state) gives it
```

### storage-stop

```code
(storage-stop [svc]) -> :nil | :t

the service puts what it has on the volume and goes
```

### storage-stream

```code
(storage-stream path [mode svc ahead]) -> :nil | stream

an object as a stream, as a file is, for what knows nothing of a
store, mode as (file-stream) has it.

+file_open_read, or no mode: to read, (read-line) (read-blk)
(read-char) (stream-seek) and the rest. It has a block of the object
at a time, and the next few on their way, ahead, 4 if not said: so
the object can be of any size. :nil if there is no object of that
path.

+file_open_write: to write a new object, of nothing to begin with.
+file_open_append: to write on the end of the object that is there,
or a new one if there is none. The path is set to what was written
when the stream is flushed, (stream-flush), and when it is let go
of, as a file is: not before, and whoever sets it last has it. One
opened to write and let go of with nothing written is an object of
nothing, as a file would be. :nil if there is no service
```

### storage-stream-ask

```code
(storage-stream-ask st at) -> mbox

a block is asked for, and not waited for: of the service if it has
it, and if not of a member that keeps it, one after another of them
as the blocks go, so that what is ahead comes from several at once.
Where the answer will be
```

### storage-stream-fill

```code
(storage-stream-fill st) -> :nil | data | (:lost hex)

the next block. The blocks after it are asked for as this one is
read, Chris: "We could implament read ahead at the point of
consumtion ?", so that they are here, or on their way, when wanted.
One that did not come, or is not what its name says, is had the slow
way, from any member that has it, again and again for a while, a link
that dropped is made again in a few seconds. One that can not be had
is the stream's fault, (:lost hex) with its name, so that a reader can
know it is not the end of the object, (stream-fault)
```

### storage-stream-land

```code
(storage-stream-land st most) -> st

the answers to the blocks on their way, the oldest first, till no more
than most are. One not put as it should be, and the path is never set
```

### storage-stream-next

```code
(storage-stream-next st) -> :nil | data | (:lost hex)

the next block, had of the service, (storage-stream-fill)
```

### storage-stream-put

```code
(storage-stream-put st data flush) -> :nil | (:ok entry) | (:stale entry) | (:full :nil) | (:lost :nil)

more of an object that is written, and if it is a flush. Each whole
block is put as there is one, so what is written can be of any
length. At a flush the path is set to the object of the blocks so
far and what is left over, a last short block: till then it is what
it was, and one who reads it sees the old or the new and never a
part. What is left over is kept, a stream can be written on after
a flush, and the next sets the path again
```

### storage-stream-seek

```code
(storage-stream-seek st offset whence) -> pos

it is read from there from now: from the start, from where it is, or
from the end. What was asked for ahead is not waited for, its
answers are to mailboxes nobody reads, and what is ahead of the new
place is asked for at the next read. That read waits for a block,
Chris: "a seek, pays the cost, but it allways does in any streaming
object"
```

### storage-sweep

```code
(storage-sweep [svc]) -> :nil | num

the blocks no object is made of are let go of, how many
```

### storage-unclaim

```code
(storage-unclaim image)

the store in that file is nobody's to serve, if it was this process's
```

### storage-user-pin

```code
(storage-user-pin [svc file]) -> :nil | prefix

on a machine that travels, the area of the user of this session,
usr/<user>/, is pinned here, so that when it is taken away it has all
of it. The user is the session's own, run.sh -u name, or the one the
machine has, usr/current. :nil if it does not travel, or there is no
user, or no service
```

### storage-wait

```code
(storage-wait path [wait want svc]) -> :nil | (seen members)

wait till most of the members have what a path is now, or as many as
want, and not longer than wait, seconds, 10. :nil if they have not by
then: a member that is cut off from most of the others has that
every time, and that is how a writer knows.

A write is taken by the one member whoever can be reached. One that
most members have keeps its path when members that were apart meet,
(store-keeps), so a writer that must not have its write set aside
waits for this, and takes :nil as not written
```

### storage-walk

```code
(storage-walk path state visit [at wait svc]) -> :nil | (state trail)

visit is '(lambda (state data at count) ... (list state next))
a gopher is sent to an object. It has what it was given, state, and
a function, visit, and goes to the
blocks of the object one at a time, the first, or at: the function is
given what it has, the block, which block it is and how many there
are, and gives what it has now and the block to go to next, :nil when
it is done. What it has then comes straight back here, with its
trail, ((member at) ...), the members it was at in the order it went
to them and the block it came to each for: where
a block it wants is not kept by the member it is at, it is sent on,
as it is, to one that keeps it, and carries on there.
:nil if there is no such object, a block it went to could not be had,
the function threw, or nothing came back in the time, seconds.

visit is the function written out, quoted, not a function: it is
sent as text, with what it has, and that is all there is of the
gopher, so it can be sent on to wherever the next block is. What it
has must print as it reads, numbers, strs, lists of them.

Each member has a task that runs the gophers that are at it, a block
of each in turn, ./gopher.lisp. A function has a second for a block:
it is made to give way and to look at the clock each time it goes
round, and one that has had its second is done with, :nil comes back.

The rules of ChrysaLisp are the rules of a visit: it does not call
itself. It goes round with a loop, or by asking for the block again
```

### storage-walk-answer

```code
(storage-walk-answer mbox [wait]) -> :nil | (state trail)

what a gopher sent with (storage-walk-send) came back with, as
(storage-walk), waited for so many seconds at most, or with :t till
it comes
```

### storage-walk-send

```code
(storage-walk-send path state visit [at svc]) -> :nil | mbox

a gopher sent, as (storage-walk), and not waited for: where what it
comes back with will be, for (storage-walk-answer). So many can be
out at once
```

### storage-where

```code
(storage-where name [svc]) -> (netid ...)

the other members a block of that name may be had from, those that
are to keep it first. The service says who, it does not ask them
```

### store-at

```code
(store-at store) -> str

where its count of changes is now, to give back to (store-since)
```

### store-block-drop

```code
(store-block-drop store name) -> :nil | :t
```

### store-block-file

```code
(store-block-file store name) -> path

where on the volume a block of that name is kept, or is to be put
```

### store-block-get

```code
(store-block-get store name) -> :nil | data

the block of that name. :nil if it is not kept here, and :nil if what
is kept is not it: it has gone wrong, and is no use to anyone
one that is not kept is known not to be, with no look at the volume:
a member is asked for many it has not got, by those looking for one
```

### store-block-has?

```code
(store-block-has? store name) -> :nil | :t

is a block of that name kept here. The names of what is kept are
held, read from the volume once as it is opened: to look one up on
the volume is to read two directories of it
```

### store-block-put

```code
(store-block-put store data) -> :nil | name

a block of this data is kept, and its name. If it is kept already
there is no more to do. :nil if there is no room for it
```

### store-blocks

```code
(store-blocks store) -> (name ...)

every block that is kept here, whatever is in it, as the volume has it
```

### store-blocks-found

```code
(store-blocks-found store) -> ((name folder) ...)

every block that is kept here, whatever is in it, as the volume has
it, and the folder it is in
```

### store-check

```code
(store-check store [mend]) -> (name ...)

the blocks kept here that are not what their names say, gone wrong
on the disk. With mend they are let go of, to be had again from
another member that has them right
```

### store-close

```code
(store-close store) -> :nil

all that it has is on the device
```

### store-commit

```code
(store-commit store path names size [from]) -> (:ok entry) | (:stale entry) | (:full :nil) | (:missing names)

a path is set to the object of these blocks, which are kept here
already, provided it is now the object from: "" for none, :nil not
to say. :stale with what it is if it is not. :missing with those of
the blocks that are not kept here
```

### store-copies

```code
(store-copies store path [copies]) -> :nil | num | (:ok entry) | (:none :nil)

how many members are to keep the blocks of the object a path is: the
dial. As many as the store says, unless it is set, to more for what
must not be lost or is read a great deal, to fewer for what is big
and can be had again. 0 sets it back to what the store says.

It is a write of the path like any other, by this member, and the
others hear of it as they do of any: then those that are now to keep
its blocks fetch them, and those that no longer are let go of them.
Without copies, what it is, :nil if there is no such object
```

### store-count

```code
a store counts the changes to its path map from when it was opened,

and has for each path the count at its last change, so that another
member can be told only what is new since it last asked,
(store-since). The counts are of this opening of it, which has a
name, :epoch: one who knew the counts of another is told everything
```

### store-cut

```code
(store-cut store data) -> (block ...)

an object as the blocks it is kept in, each the block size but the
last. One of nothing is no blocks
```

### store-delete

```code
(store-delete store path [from]) -> (:ok entry) | (:stale entry) | (:none :nil)

there is no object of that name from now, provided it is from, as
(store-commit). Its blocks are there till they are swept
```

### store-digest

```code
(store-digest store) -> hash

the path map and the members, as one name: two members with the same
have nothing to tell each other. It is asked for by every member each
time it looks, and is of every path there is, so it is held till
something changes, (store-save)
```

### store-entries

```code
(store-entries store) -> (entry ...)

every entry of the path map, those deleted too, what is told to
another member
```

### store-entry-line

```code
the path is last, it may have spaces in it

how many copies, if it is said, is after the deleted, 0/5: an entry
written before there was such a thing reads as not said
```

### store-find

```code
(store-find store path) -> :nil | entry

what a path is now, one that is deleted too
```

### store-fold

```code
(store-fold store path state step) -> :nil | (state)

(step state data index) -> state
an object read a block at a time, in order, each given to a function
with what the last one gave: what it gives at the end, as (store-walk).
A name is found where a function is called from, not where it is
written: what (store-walk) calls its function must not be step, or
the step below would be this very lambda
```

### store-folder

```code
(store-folder store) -> path

the folder blocks are put in now, the next once one has so many
```

### store-get

```code
(store-get store path) -> :nil | data

all of an object
```

### store-held

```code
(store-held store) -> (name ...)

the names of the blocks kept here, as they are held: what
(store-block-has?) goes by. No reading of the volume
```

### store-holders

```code
(store-holders store name [among copies]) -> (member ...)

the members that are to keep a block of that name, as many as there
are to be copies, the store's if it is not said, of the members, or
of those given, the ones that are there. Each member is given a
number made from its id and the name, and those with the greatest
keep it: so every member works out the same, nobody has to be asked,
and when a member comes or goes only the blocks that are its share
move

What was worked out is kept, the members in the block's order, for
as long as it is asked of the same members: a member asks it of
every block it knows of each time it looks to its share, and it is a
hash and a sort for each
```

### store-info

```code
(store-info store) -> (objects blocks free)

how many objects, how many blocks kept, and the room left in bytes
counted, not listed and put in order to be counted: it is asked often
```

### store-join

```code
(store-join store members [retired later]) -> :nil | :t

these are members too, and those are retired, as another member has
it. Both only ever grow, so two that are put together are the same
whichever way round, and a member that is retired stays retired. Was
there anything new
```

### store-journal-add

```code
(store-journal-add store entries) -> :nil | :t

entries of the path map are on the volume, at the end of the journal,
as the one write. :nil if there is no journal or no room in it for
them all, and the map is to be written
```

### store-journal-open

```code
the journal of a store that has just had its map read: what it has

that is later than the map is taken, and it is added to from there.
One that has none is given one. How many entries were taken
```

### store-journal-reset

```code
it has nothing in it, as it does when the map has just been written
```

### store-journal-write

```code
bytes of the journal, where they are on the volume
```

### store-keeps

```code
(store-keeps store a b) -> a | b

of two entries for a path that were written apart, the one that
keeps it. One that most members have, over one that they have not.
Else the one written more times, and else the one whose object has
the greater name: that is only so that every member chooses the same
```

### store-later

```code
the path map or the members are not as they were, and it is not

written yet: what was learnt from another member can be learnt
again, so whoever keeps the store writes it when it has a moment,
(store-save), and till then :dirty says there is something to write
```

### store-lines

```code
(store-lines text) -> (line ...)
```

### store-list

```code
(store-list store [prefix deleted]) -> (entry ...)

the paths that begin so, in order, with those deleted if asked
```

### store-make

```code
(store-make path | stream size id member [block_size label copies]) -> :nil | :t

a new store with nothing in it, on a new volume of that many bytes.
id is the store's, any str with no space in it, member the id of the
member whose it is, the system id of its machine in hex. A path is a
new file of the host, a stream is written where it is and must be
that long. copies is how many members keep a block, 3
```

### store-manifest

```code
(store-manifest store hash) -> :nil | (size names)

the object of that name, if its manifest is kept here. One that has
been read is held: a name is of the one thing for ever, and what an
object is made of is asked again and again
```

### store-manifest-text

```code
(store-manifest-text size names) -> data

what is kept for an object: how long it is, and its blocks in order
```

### store-map-insert

```code
an entry goes in the path map, in place of what that path was. How

many objects there are is kept as it goes, it is asked with every
put, and to count them was a walk of the map each time
```

### store-meet

```code
(store-meet store entries [later]) -> (changed ...)

what another member has in its path map is set against what this one
has. The paths that are not as they were here, their entries now.

An entry of theirs that comes after mine is taken, one that comes
before is not, mine will be taken there. The same entry: each now
knows the other has it. Two written apart: one keeps the path,
(store-keeps), with the counts of both, so that it comes after both
and whoever meets it next takes it; the other, if it is an object and
not a delete, is kept under another name, (store-other-name). Both
members come to the same, whichever does this. With later it is not
written to the volume now, (store-later)
```

### store-members

```code
(store-members store) -> (member ...)

who the members are meant to be, in order: every one that was ever
joined and has not been retired, away or not
```

### store-most?

```code
(store-most? store entry) -> :nil | :t

do most of the members have this entry, as far as is known here. At
most one of two entries written apart can be so
```

### store-now

```code
(store-now store path) -> hash

the name of the object a path is now, "" if there is none: what a
writer says it is setting it from
```

### store-open

```code
(store-open path | stream member) -> :nil | store

a store that is there. :nil if the volume is not one. It is opened by
the member it was made for, or last opened by. Opened by another, a
machine given a new id, a disk moved to another machine, that member
takes the place of the one it was, in the members too: there is not
one more member, and (get :was store) is who it was
```

### store-other-name

```code
(store-other-name entry) -> path

the name an entry is kept under when another keeps its path: the
path, and the start of the name of its object. Every member makes
the same name of it
```

### store-pin

```code
(store-pin store prefix [off]) -> store

this member holds all of every object whose path begins so, wherever
else it is kept, and does not let go of it. With off it no longer
does
```

### store-pinned-by

```code
(store-pinned-by store path) -> (member ...)

the members that hold all of the object of that path, as far as is
known here: it is under what they have pinned. Not one that is retired
```

### store-pinned-lines

```code
(store-pinned-lines store) -> (line ...)

what every member has pinned, as lines, member count prefix: a line a
prefix, the prefix last, it may have spaces in it. A member that has
none pinned now is a line with no prefix, so that is known too
```

### store-pinned-read

```code
(store-pinned-read store lines) -> :nil | :t

lines of (store-pinned-lines), as another member has them or as they
were kept: of each member, what is of a greater count than is had
here is taken. If it is of this member, its pins are that now: an
image put back from an older copy. Was anything new
```

### store-pinned-take

```code
(store-pinned-take store lines [later]) -> :nil | :t

what another member says is pinned, taken in and kept
```

### store-pinned?

```code
(store-pinned? store path) -> :nil | :t
```

### store-pins-missing

```code
(store-pins-missing store) -> (name ...)

the blocks of what is pinned that are not kept here yet, in hex.
None, and what is pinned is all here: the machine can be taken away
```

### store-plan

```code
(store-plan store among) -> plan

what this member is to keep among those members, as it is now
```

### store-plan-dropped

```code
a block that is no longer kept
```

### store-plan-held

```code
a block that is kept now
```

### store-plan-object

```code
the object of a path, into what is wanted and what is others'. If

its manifest is not here, what it is made of is known when it comes
```

### store-put

```code
(store-put store path data [from]) -> (:ok entry) | (:stale entry) | (:full :nil)

a path is set to an object of this data, provided it is now from, as
(store-commit). Nothing is kept if it is not
```

### store-rank

```code
(store-rank name among copies) -> (member ...)

of those members, the ones that are to keep a block of that name, as
many as there are to be copies. It is worked out of the name and who
they are and nothing else: anything that knows the two knows who
keeps a block, a gopher too, and has nobody to ask.

The members are put in an order that is the block's own, and the
first so many keep it. So to ask for more copies adds members to
those that keep it and moves it from none, and to ask for fewer
takes the last of them away
```

### store-read

```code
(store-read store path offset len) -> :nil | data

part of an object, only the blocks that part is in are read
```

### store-rename

```code
(store-rename store from to [was]) -> (:ok entry) | (:stale entry) | (:none :nil)

the path to is set to the object from is, with its copies, and from is
then no more, provided to is now the object was, as (store-commit). The
two in one call: a service, one task, answers nothing between them
```

### store-resize

```code
(store-resize store path size) -> :nil | store

the store moved to a new volume of that many bytes, made there: every
file of this one copied to it as it is, the blocks above all, and
then the path map, the members and the pins written there as the
store has them, which a store that was full may not have had room to
write, and a journal of the new volume's own size. A copy on the one
machine, no member asked for anything: bigger, or smaller if what
there is fits. :nil if it can not be made or does not fit, and the
store is on the volume it was on, as it was. The old volume is left
as it was
the names the function given to (exfat-walk) uses are not names it
has, vol and path, it is called from inside it and would have those
```

### store-restore

```code
(store-restore store name [prefix]) -> :nil | (restored newer)

the objects whose paths begin so are as they were in the snapshot,
all of it if not said: each path that is not as it was is set back,
and one deleted since is had again. How many were set back, and how
many paths there are now under the prefix that the snapshot has not,
which are left. :nil if there is no such snapshot
```

### store-save

```code
(store-save store) -> store

the path map, the members and the pins, as they are now. Whatever
changes any of them ends here, so what was worked out from them, the
digest, is not held any longer
```

### store-seen

```code
(store-seen a b) -> (member ...)

the members that are in either list, once each, in order
```

### store-set

```code
the one way a path is changed here. from is what the writer says it

is now, :nil if it does not say. How many copies there are to be of
it is of the path, and stays as it is unless it is given
```

### store-share

```code
(store-share store among) -> (wants extras)

this member's share, among the members that are there: the blocks it
is to keep and has not got, names, and those it has and is not to
keep, each (name holders), the members that are to.

It is to keep every manifest, a member has to know what an object is
made of to look after any of it. Of the blocks of an object, those
whose keepers it is one of, (store-holders), and all of them if its
path is pinned. How many keepers a block has is its object's to say,
(store-copies), and one that two objects are made of has as many as
the one that asks for more. A block that no object is made of is
neither, that is for (store-sweep)

What it is to keep is worked out the once, (store-plan), and kept
till the path map, the pins or who is there change. What it wants
and what is others' are kept as blocks come and go: a tender asks
this many times a look, and to work it out each time was a walk of
every block of every object
```

### store-since

```code
(store-since store [at]) -> (entry ...)

the entries of the path map, those deleted too, that are not as they
were at that count, as (store-at) gave it. All of them if it is not
given, or is of another opening of the store
```

### store-snapshot

```code
(store-snapshot store name [prefix]) -> :nil | count

a snapshot of the objects whose paths begin so, all if not said, how
many. Not of the paths of other snapshots. :nil if the name is not one
a snapshot may have
```

### store-snapshot-drop

```code
(store-snapshot-drop store name) -> count

a snapshot is gone, its paths deleted, how many. Its blocks are let go
of by a sweep, if nothing else is made of them
```

### store-snapshot-name?

```code
(store-snapshot-name? name) -> :nil | :t

a name a snapshot may have: not empty, and no / or space in it
```

### store-snapshots

```code
(store-snapshots store) -> ((name count) ...)

the snapshots there are, and how many objects each is of
```

### store-stat

```code
(store-stat store path) -> :nil | entry

what a path is now, :nil if there is no object of that name
```

### store-sweep

```code
(store-sweep store) -> num

the blocks that no object is made of are let go of, how many. Not
while an object is half written: its blocks are kept before its path
is set
```

### store-text

```code
(store-text lines) -> text
```

### store-touch

```code
a path is not as it was
```

### store-vv-bump

```code
(store-vv-bump vv member) -> vv

one more write by a member. A vv is ((member count) ...), in the
order of the members' ids, so two that say the same are the same
```

### store-vv-cmp

```code
(store-vv-cmp a b) -> :same | :after | :before | :apart

a against b: after it, it has as many or more of every member's
writes, before it, or apart, each has some the other has not
```

### store-vv-count

```code
(store-vv-count vv member) -> num
```

### store-vv-merge

```code
(store-vv-merge a b) -> vv

the most of each member's writes that either has
```

### store-vv-total

```code
(store-vv-total vv) -> num

how many times the path has been written, as far as it knows
```

### store-walk

```code
(store-walk store path state visit [at]) -> :nil | (state)

(visit state data at count) -> (state next)
an object read a block at a time, where the reading says to go. The
function is given what it had, a block, which block it is and how
many there are, and gives what it has now and the block to read
next, :nil when it is done. It starts at the first, or at at. What
it has at the end is given in a list, so that an end of :nil can be
told from :nil: there is no such object, or a block it went to is
not kept here. A block there is not, past the end, is the end.

This is how a search is sent to the data: what it has is small, and
with the function is all there is of it, so it can be carried on by
whoever has the next block
```

### store-wanted

```code
(store-wanted store [prefix]) -> (held missing)

the blocks the objects of these paths are made of, as two sets of
names in hex: those kept here, and those that are not. An object
whose manifest is not kept here is one block missing, what its other
blocks are is not known
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

### str??

```code
(str?? form) -> :t | :nil
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

### stripes-apply

```code
(stripes-apply doc items text) -> :nil | version

make a copy of a document what the text of a delta says it is. items
is the copy's items by id, an Fmap, those that are not sent again are
kept, with what they are flattened to. The version it is then at
```

### stripes-delta

```code
(stripes-delta doc log have version) -> text

what a copy of the document at version have needs to be the document
as it is, at version: the layers, as the ids of their items, and the
items that changed, or all of them
```

### stripes-full?

```code
(stripes-full? log have version) -> :t | :nil

does a copy at version have need the whole document to be at
version. log is ((version changes) ...), the newest last, changes is
ids or :all. It does if what happened since have is not all in the
log, or any of it was :all
```

### substr

```code
(substr text substr) -> matches
```

### sv-apply

```code
a built in op whose args are all worked out before it, they are in a,

b and c, as many as it has
```

### sv-arith

```code
one step of + - * /, by the types of the two sides
```

### sv-at

```code
-> (register offset)

where a slot of the frame is
```

### sv-block

```code
code for the statements of a block. An if and a for have blocks in

them, and no function here calls itself: what is still to be done is
a list, the next thing last, a statement or what is to come after a
block that was put there before it
```

### sv-branch

```code
code to jump to the label if the bool is as sense says
```

### sv-calls?

```code
does this expression call a function of the shader ? Walked with a

list as the stack of what is still to be looked at
```

### sv-code-text

```code
the code so far as lines of VP source
```

### sv-const

```code
a float constant, to a new register
```

### sv-define

```code
a new variable, set to the value of an expression
```

### sv-emit

```code
a call of the system can lose the registers things are reached by
```

### sv-expr

```code
code for an expression, the registers that hold the value
```

### sv-fill-code

```code
the code that walks the triangles, to the code list. The slots are

those of the bindings in (sv-fill-source)
```

### sv-fill-source

```code
-> (text frame_size params vary_slots)

the VP source of the native function that fills triangles with a
pixel shader. params is where in the frame the caller's numbers go,
(ntris stride tstride vw vh cx0 cy0 cx1 cy1 cull dy vary), vary is the
first of a slot for each number of the varyings, where in a placed
vertex that number is, in bytes
```

### sv-floor

```code
floor of a register, in place
```

### sv-func-source

```code
-> text

the VP source of the native function for a function of a file of
functions. It is called from Lisp as any native function is. Its
frame is on the stack. A number arg is copied to it. A reals arg is
read where it is, by a register, the first three of them, if the
function never sets it, else it is copied too. What it gives is a
new real, num or reals, and a reals is made first and written
straight into.
```

### sv-keep

```code
(sv-keep name func info) -> (func ~info)

keep the numbers that go with a native function just made
```

### sv-kept

```code
(sv-kept name) -> :nil | (func info)

the native function of that name if it is there from before, and
the numbers that were kept with it. Nothing of the program is read
to find out. The numbers are written last, after the function is
whole, so if they are there it is
```

### sv-ld

```code
a float from a slot of the frame, or from where that slot really is
```

### sv-leaf

```code
a value with nothing under it, the registers that hold it
```

### sv-lin

```code
(sv-lin items) -> steps

lay out what is to be done, in order. What is still to be laid out is
on a stack, a list, and the next thing is the last of it
```

### sv-lin-branch

```code
the steps of a jump to a label if a bool is as sense says. The label

is in a cell, a list of it, it may not be made till the steps are done
```

### sv-lin-expr

```code
the steps of an expression, those under it still to be laid out
```

### sv-mat

```code
code for an expression that is a matrix, where in the frame it is. A

variable is where it is kept. A product is worked out into new slots,
each number of it a row of the one by a column of the other. With
into, the last product of a chain is worked out there, a place that
is no part of what it is made from.
A product can be of products. It is laid out as steps first, a list
as the stack of what is still to be laid out, and the steps are then
done with a list of where each matrix so far is
```

### sv-mat-copy

```code
a matrix from one place in the frame to another
```

### sv-mat-vec

```code
a matrix times a vector that is in registers, to new registers. A

vec3 is by the 3 by 3 of the matrix
```

### sv-name

```code
the name of the native function of a program, from the hash of its

source, and which make of this back end it is
```

### sv-native

```code
-> :nil | func

the native function of this name, assembled from the text if this
CPU has not got it yet
```

### sv-place-again

```code
as (shader-vp-place), for a result that is used at once and let go

of, as a draw does. The reals the last such call of that size gave
is used again, every number of it is written, so there is no new one
to make and clear for each object of each frame
```

### sv-pow

```code
x to the power y, both kept, to a new register
```

### sv-program

```code
-> (main_param_offsets main_ret_offset), or those of the entry named

the code of a program, after its inputs have been given their slots.
A label that sets its constants and globals, then each function at a
label of its own. The labels start with the prefix, so that the two
programs of a pair can be in the one native function.
```

### sv-reals

```code
a reals of n numbers, all 0
```

### sv-run

```code
(sv-run steps) -> values

do the steps, the code comes out as they are done. The values are a
stack, a list, each the registers a value is in
```

### sv-sets?

```code
does any statement of a block set this variable. The block is walked

with a list as the stack of what is still to be looked at
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

### sv-tex-coord

```code
a place of 0 to 1 that goes round again, in a float register, as

which pixel of that many it is, to a new int register. limit is
where in the frame the many is. The float register is used up
```

### sv-tex-slots

```code
room in the frame for the textures of a pixel shader, after its

inputs: for each where its pixels are, how wide and how high it is,
and how many bytes a row of it is, 8 bytes each. -> where each begins
```

### sv-texture

```code
the color of a picture at a place, four new float registers. at is

where in the frame the picture is said, uv the two registers of the
place, which are used up. The one pixel the place is in. A picture
of no width is none, and is nothing
```

### sv-var

```code
the frame offset of a variable, the last one of that name declared
```

### sv-vertex-source

```code
-> (text frame_size attr_slots out_slots)

the VP source of the native function that places vertices
```

### swap

```code
(swap list idx idx) -> list
```

### swatch-colors

```code
(swatch-colors) -> (argb ...)

row by row: light to dark, a hue each 30 degrees across, then grey
```

### swatch-tip

```code
(swatch-tip what val [long]) -> :nil | str

what a cell is, in words, short as the toolbars' tips are; a colour
has none, as an ink of the bar has none
```

### swatches-on

```code
(swatches-on board) -> (swatches ...)

those open on the board
```

### sym-code-hex

```code
a code as four hex digits, the high byte first
```

### sym-font

```code
(sym-font symbols radius joint cap) -> str

the whole of a .ctf, of symbols each (name items), in the order they
are given, from +sf_base
```

### sym-glyph

```code
(sym-glyph code paths) -> str

a glyph of a .ctf, docs/ai_digest/ctf_command.md. Its left edge is at
0, and its advance is how wide its ink is
```

### sym-names-text

```code
lib/consts/symbols.inc, the name of each symbol and its code
```

### sym-paths

```code
(sym-paths items radius joint cap) -> paths

the outlines of a thing, at the size they are worked in. radius is
half the weight of a stroke, on the grid
```

### sym-wind

```code
(sym-wind paths) -> paths

the outlines of one item, all turned if they must be so that the
biggest goes round the one way. A glyph is filled by the non zero
rule, and two outlines that go round opposite ways cut a hole where
they overlap, a disc on the end of a stroke did
```

### sym?

```code
(sym? form) -> :t | :nil
```

### sync-clean

```code
(sync-clean root [kept top]) -> :nil | (commit time top)

the waypoint a tree is at, if it is exactly that and changed in
nothing since, :nil if it has none or is not. top, if it is known
```

### sync-from

```code
(sync-from node said root kept me [named herd_only]) -> :rig | :herd | :no

what a machine may do here, by who it is and the waypoint it says it
is at: anything, if it is this machine or one named, unless it asks
only as the herd; as the herd, if this tree has no .git and is at a
waypoint and it is at a newer one, or this tree is already exactly the
one at its waypoint, and only the waypoint is to be noted; or nothing
```

### sync-hear

```code
(sync-hear reply_mbox [wait]) -> :nil | (status data)

what it says back, :nil if it does not in the time
```

### sync-ignored?

```code
(sync-ignored? rules segs is_dir) -> :nil | :t

is a path, as its parts, left out ? The folders above it are taken
to have been asked of already, as a walk down the tree does
```

### sync-inside?

```code
(sync-inside? root path [real]) -> :nil | :t

is a path, safe as written, under the root as the host has it ? Every
folder on the way has to be a folder, and the file a file, or not
there yet. A link is neither, so a link in the tree to somewhere else
is never gone through, and nothing is written or removed beyond it.
real is a set of the folders found to be so, to ask the host the once
```

### sync-kept

```code
(sync-kept root) -> :nil | path

the file the hashes of a tree are kept in from one asking to the next,
with what else is built: the system's own tree only
```

### sync-kind

```code
(sync-kind folder entry) -> :nil | kind

what the host says an entry of a folder is, "4" a folder, "8" a file,
anything else something else, a link for one. :nil if it is not there
```

### sync-list

```code
(sync-list root rules [kept live]) -> ((path size hash mode) ...)

the files of a tree, with the size and the SHA-256 of each. With a
file to keep them in, a hash is not worked out again for a file whose
time and size are as they were. One changed in the last two seconds is
always hashed, its time may not change if it is changed again.
live is a map to hold them in between one call and the next, for a
task that stays, a service. The file is then read the once, and is
what is there when the task starts again
```

### sync-named

```code
(sync-named [file]) -> (system_id ...)

the machines named in this machine's sync_from, or that file, each the
32 hex digits of a system id on a line of its own
```

### sync-no-modes?

```code
(sync-no-modes? machine) -> :nil | :t

is either end a host with no modes for its files, this one or the one
named, "cpu/os" as a service says it: then none are sent or asked for
```

### sync-of?

```code
(sync-of? node systems) -> :t | :nil

is a node one of the machines of those system ids
```

### sync-own-rules

```code
(sync-own-rules root) -> rules

the rules of a tree, its own .gitignore, and what is the machine's own
and not the tree's: its waypoint, who it takes a sync from, its key
and its keyring
```

### sync-prune

```code
(sync-prune root path) -> count

the folders above a path just removed that it left empty, removed from
the bottom up, never the root. One with anything at all in it is left,
what the rules leave out too, so a folder of obj/ stays. git does the
same, a tree has no empty folder, and none is sent
```

### sync-push

```code
(sync-push svc root rules_text [check remove kept no_modes herd_only])

	-> :nil | :old | (sent bytes removed failed send gone remoded)
make the tree a sync service has the same as the one under root. With
check nothing is changed there. With remove, what is there and not
here is removed. send and gone are the paths that differ, and the ones
only there. A file is given the mode it has here, who may read, write
and run it, and one that is the same but for its mode has that set,
unless no_modes, for a host that has no such thing. :nil if the
service does not answer, :old if it is from before there were trees,
:refused if it takes nothing from this one, (Who takes a sync). With
herd_only it is asked to take it only as the herd would, not as one of
the machines it names
```

### sync-root

```code
(sync-root svc rules_text [no_modes wait]) -> :nil | :old | hash

the top of the tree a sync service has, by those rules. It works it
out when asked, and keeps it to be asked of its folders. :nil if it
does not answer, :old if it is a sync from before there were trees
```

### sync-rules

```code
(sync-rules text) -> rules

the rules of a .gitignore, each (anchored dir_only segments). Those
that take back a rule, !, are not known and are left out. The .git
folder is never listed
```

### sync-rules-text

```code
(sync-rules-text root) -> str

the rules of a tree as its .gitignore has them, "" for none
```

### sync-safe?

```code
(sync-safe? path) -> :nil | :t

a path the service will write to, by how it is written. Under its
root, and not up out of it. No .., no start at the top of the host's
files or of a drive, no ~, and no character below a space. Nor what
is the machine's own, whatever the rules of one that sends say: its
key in use and its keyring, who it takes a sync from, its waypoint
```

### sync-seg?

```code
(sync-seg? pat seg) -> :nil | :t

does one part of a path match one part of a rule, which may have a *
```

### sync-services

```code
(sync-services [name]) -> ((mbox system_id machine root) ...)

the machines that will take a sync, as their services say
```

### sync-tell

```code
(sync-tell svc reply_mbox kind data [at total mode])

a message to a sync service
```

### sync-top

```code
(sync-top root [kept]) -> hash

the top of the tree of hashes of a tree, by its own rules
```

### sync-tree

```code
(sync-tree root rules [kept no_modes live]) -> tree

the tree of hashes of the files under a root. The mode of a file is
part of it, unless no_modes, for a host that has no such thing. kept
and live are as (sync-list) has them
```

### sync-walk

```code
(sync-walk root rules) -> paths

every file under the root that the rules do not leave out, each a
path from the root, in order
```

### sync-waypoint

```code
(sync-waypoint root) -> :nil | (commit time top)

the waypoint a tree was last at, as its file waypoint says: the commit,
when it was made, and the top of the tree of hashes it had then
```

### sync-waypoint-text

```code
(sync-waypoint-text commit time top) -> str
```

### task-band

```code
(task-band :bot | :app) -> :t

the band the task runs in. A bot is a task that works by itself for
others, a Storage service, its tender, a gopher: one that is ready
runs before any app, and after the GUI. sys/task/class.inc, where the
numbers 4 and 5 are tk_bot_priority and tk_app_priority. The function
is looked up when it is called, not in root.inc, so a boot image of
before it was written still starts
```

### task-breathe

```code
(task-breathe)

(task-slice), and for a bot, every so often, +bot_work, a rest of all
the bots of the node, +bot_rest, so the apps of the node have a turn:
a bot gives way only to its own band and those before it, and one at
work for a second held every app of its node for that second.
Measured, 20 gophers that never came back, the apps waited 9.1 s
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

### text-most

```code
(text-most text) -> text

the text, or its first +text_most bytes, cut between characters
```

### texture-metrics

```code
(texture-metrics texture) -> (handle width height)
```

### texture-sized

```code
(texture-sized texture) -> (texture width height)

what a view's :ctx_blit is given, the texture itself, which the view's
list of draws keeps while it has it, and its size
```

### theme-current

```code
(theme-current home) -> name

the theme a user has, home is usr/<user>/. The first if none was
ever chosen, or if what the file says is not a theme. The file is the
name and no more, it is read by every app as it starts, and there is
nothing in it to go wrong
```

### theme-file

```code
(theme-file name) -> symbols_file

the font of a theme, that of the first if there is none of that name
```

### theme-save

```code
(theme-save home name)

the theme a user has from now on
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

### tls-certificates

```code
(tls-certificates msg [v12]) -> (cert ...)

the certificates of a Certificate message, read, :nil for one that
does not read. Of 1.2 there is no context before them and nothing
after each
what the server sent, so no index past what is there
```

### tls-client-hello

```code
(tls-client-hello host public random) -> str
```

### tls-close

```code
(tls-close conn)

a close told to the server, and the connection let go of
```

### tls-fill

```code
(tls-fill state) -> :nil | str

what the server says next, a record of it. What it says about
itself after the handshake, a ticket, is read past
```

### tls-handshake

```code
(tls-handshake state host) -> :nil | :t
```

### tls-handshake12

```code
(tls-handshake12 state host transcript secret crandom sh) -> :nil | :t

the rest of a handshake of TLS 1.2, RFC 5246 7.3, after the
ServerHello: the server's Certificate, its share signed, perhaps a
request for one of this side, which is sent none, and its done. Then
this side's share, the keys, a change of cipher spec and this side's
Finished, and the server's two, checked
```

### tls-handshake13

```code
(tls-handshake13 state host transcript secret share) -> :nil | :t

the rest of a handshake of TLS 1.3, after the ServerHello
```

### tls-keys

```code
(tls-keys secret) -> (key iv)
```

### tls-label

```code
(tls-label secret label context len) -> str

HKDF-Expand-Label of RFC 8446 7.1
```

### tls-message

```code
(tls-message state) -> :nil | (type whole)

the next handshake message, from as many records as it takes, the
whole of it with its head, as the transcript has it
```

### tls-nonce

```code
(tls-nonce iv seq) -> str

the iv with the number of the record in its last 8 bytes, xor
```

### tls-open

```code
(tls-open host port [unchecked]) -> :nil | (in out state)

a connection that is private between here and the host, which has
shown it is the host. unchecked :t, and who answers is not asked to
show it. :nil if not, (tls-why) says why
```

### tls-open-record

```code
(tls-open-record state) -> :nil | (type data)

the next record that is not a change of cipher spec, opened with
the keys of the other side if it is sealed
```

### tls-open12

```code
(tls-open12 state type body) -> :nil | data

a record of TLS 1.2 opened with the keys of the other side
```

### tls-prf

```code
(tls-prf secret label seed len) -> str

the PRF of TLS 1.2, RFC 5246 5, P_SHA256
```

### tls-record-read

```code
(tls-record-read state) -> :nil | (type body)
```

### tls-scheme

```code
(tls-scheme scheme) -> :nil | (way hash_name)

a signature scheme of TLS 1.3 as lib/crypto/x509.inc has it
```

### tls-seal

```code
(tls-seal state type data)

data of a type, sealed with the keys of this side, as a record
```

### tls-server-hello

```code
(tls-server-hello hello) -> :nil | (version suite random share ems)

what the server took of what was offered: TLS 1.3 and the one suite,
its X25519 share, or TLS 1.2 and a suite of it, and whether it makes
the keys of the whole handshake, ems. A HelloRetryRequest, a random of
a set value, is not taken: there is no other group to offer it. Nor a
server of 1.2 whose random says it has 1.3
```

### tls-server-kx

```code
(tls-server-kx msg cert crandom srandom) -> :nil | (group public)

the server's share of a ServerKeyExchange of 1.2, X25519 or P-256, if
it is signed, with both randoms, by the key of its certificate. cert
:nil, and it is not checked
```

### tls-vec

```code
(tls-vec len_bytes data) -> str

data after its length, as a vector of TLS is
```

### tls-verify

```code
(tls-verify cert msg transcript) -> :t | :nil

the server's CertificateVerify, a signature of the handshake so far by
the key of its certificate, RFC 8446 4.4.3
```

### tls-why

```code
(tls-why) -> :nil | str

why the last (tls-open) of this task gave :nil
```

### tls-write

```code
(tls-write state data flush)
```

### tok-rule

```code
(tok-rule tokens body i n prule pargc) -> token

the token that opens a form, with the rule of the form, whether the
form must be broken, and its operator, kept on the end of it. They are
worked out the once, the first time the token is come to.
```

### tokenize-line

```code
(tokenize-line line [state]) -> (token_list state_list state)

a line as its tokens, and what each of them is: :text :symbol :keysym
:number :comment :string1 :string2. state is what the line before
ended in, :text if it is not given, and the state that comes back is
what this one ends in, to give with the next: it is not :text when a
string is still open
```

### tool-arc-points

```code
(tool-arc-points out cx cy r a0 a1 seg) -> out

points round part of a circle, from one angle to the other, whichever
way that is, no further apart than seg, pushed on a list as x y
```

### tool-area-d

```code
(tool-area-d shape) -> d

the outline of that, to be drawn
```

### tool-catch

```code
(tool-catch edge band) -> (points ...)

the outlines of where an edge catches a pen: every point no more than
the band from it. Each a closed line, x y x y ...
```

### tool-glyph

```code
(tool-glyph what [mode]) -> shapes

the sign of a part of an instrument, about 0 0, some 20 across: an
arrow that goes round for :turn, one with two heads for :size, a
cross for :close. For :mode, in a ring, what the instrument is set
to draw: an arc, a slice of pie, or a circle, either of those filled
```

### tool-magnet

```code
(tool-magnet angle) -> angle

an angle, in radians, or the one that matters that it is near
by 180 and then over pi, and back by pi and then over 180: a degree
in radians is too small a number to be held well
```

### tool-rub-area

```code
(tool-rub-area shape) -> (path ...)
```

### tool-ticks

```code
(tool-ticks x0 y0 x1 y1 nx ny step [small mid big]) -> d

marks along a line, every step from its start, each a little line
the way nx ny points, which is one long. Every fifth is longer and
every tenth longer still
```

### toolbar-states

```code
(toolbar-states toolbar states)

each button of a toolbar lit, or not, by its state in a list
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

### tree-form?

```code
(tree-form? node [head]) -> :nil | nodes

what is in a form, if it is one, and, given a head, if the first
thing in it is that symbol, "defun" say
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

### tree-read

```code
(tree-read stream) -> (nodes open stray)

the nodes of a file, how many brackets were not closed, 0 for one
that is whole, and how many closed nothing: those are left out
```

### tree-said

```code
(tree-said node nodes at [most]) -> (comment_node ...)

what is said of a form, node, which is nodes at at: the comments
on the lines after its first. Of one that is all on a line, those on
the lines after it, the next things in nodes. Of one that goes on,
the first comments in it, if no more than most things are before
them, default 3, its head, a name and what it takes, and they are
not on its first line, a note on the end of that is not of it
```

### tree-save

```code
(tree-save stream tree [key_filters]) -> tree | :nil
```

### tree-text

```code
(tree-text node) -> str

a node as it would be written on one line, comments and empty lines
left out. A list of what is left to write, it does not call itself
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

### ui-find

```code
(ui-find view key value) -> :nil | view

the first of view and those in it, depth first, whose key is value, a
:type by its name, Button or :Button, any other as it prints
```

### ui-merge-props

```code
(ui-merge-props props) -> props
```

### ui-save

```code
(ui-save stream view [depth keys]) -> tree | :nil

the tree of a view and those in it, each its :type, its :bounds in
its parent, and its properties: atoms, flat sequences of atoms, fonts
as (name pixels). depth the levels of children saved, :nil for all, a
view at the last has :children_count in place of its children, as has
a subtree in it, the child of a Scroll, a page of the Docs app say,
unless it is the view saved. keys the properties saved, :nil for all.
A symbol not a plain :keyword is saved as its text, as the words of an Md
are symbols, with spaces and brackets in them, that a tree writes bare
and could not read back
```

### ui-save-atom

```code
(ui-save-atom atom) -> atom
```

### ui-symbols

```code
(ui-symbols symbols) -> strs

what a bar's buttons show, as text. A symbol of the symbol font by its
name, +sym_undo, lib/consts/symbols.inc, or by its code, and a str is
itself. A name is in a list that is not run, so it is looked up here
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

### unified-pass

```code
Each file is read as a tree, lib/text/tree.inc, and every form of it

looked at in the order they are written, those inside others too, a
(def-func) in an (errorcase) say. A list of where it is in each list
of nodes it is part way through, each (nodes at)
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

### url-split

```code
the folder and the start of a name, a path from / of the host's as it is
```

### usort

```code
(usort list [fcmp start end]) -> list
```

### vector-angle-wrap

```code
(vector-angle-wrap a) -> a

an angle as one of more than -pi and no more than pi
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

### vector-box-join-2d

```code
(vector-box-join-2d a b) -> box | :nil

the box round two boxes, either can be :nil
```

### vector-collide-boxes-2d

```code
(vector-collide-boxes-2d a b) -> :t | :nil
```

### vector-collide-line-box-2d

```code
(vector-collide-line-box-2d x0 y0 x1 y1 bx by bx1 by1) -> :t | :nil

does a line from one point to another touch a box, be in it, cross
it or end in it. The line is cut to each side of the box in turn, and
touches if anything of it is left
```

### vector-face-normal

```code
(vector-face-normal p0 p1 p2 none) -> reals

the normal of a triangle of reals, the way it faces when its corners go
round counter clockwise, none if it has no area and faces no way
```

### vector-nearest-on-line-2d

```code
(vector-nearest-on-line-2d x y x0 y0 x1 y1) -> (px py t)

the point of a line nearest a point, and how far along it that is, 0 to 1
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

### view-loops

```code
(view-loops view) -> ((holder key) ...)

(loops) of a tree of views, a view's children under it. An app's
window should have none
check off, View? is the GUI's, there when there are views to look at
```

### view-resolve

```code
(view-resolve view) -> view

each view of it, it too, given a variant for a plain property has the
variant's value, found from where it is. Which variant it was is
kept, :variants, so a new theme or user can be found again by it
```

### view-variant

```code
(view-variant view key variant) -> view

a plain property of a view made by hand, not by a ui- form, given its
variant's value, the user's, as a ui- form gives it, and which variant
it was kept, :variants, so a theme or a user finds it again by it,
(view-resolve). (view-variant label :font :font_small)
```

### voice-duplex

```code
(voice-duplex voice on)

half duplex, on to begin with: one heard of late is not spoken over.
Off with headphones, where nothing heard is heard by the microphone
```

### voice-hear

```code
(voice-hear in out inbox ctl said talk)

a stage that is both ends: what comes to inbox played, each that sends
on a stream of its own, in the order it was said, till ctl is sent e.
p and a tap's mailbox hex, or p alone, as voice-send. said, the ctl of
the voice-send beside it, is told h as another is heard, half duplex.
talk, if not :nil, is told T and who, hex, as each is heard
```

### voice-inbox

```code
(voice-inbox voice) -> hex

where what is to be heard here is sent, to tell the others
```

### voice-mix

```code
(voice-mix in out tap own ctl)

a source: what comes to tap made one sound, till ctl is sent anything.
What is said here, from own, hex, is the clock: as each piece of it
comes, it is written with the next of each other's added in. One
further ahead than +voice_mix_ahead has its oldest let go
```

### voice-mute

```code
(voice-mute voice on)

none of what is said here sent, while on
```

### voice-piece

```code
(voice-piece from seq data) -> str
```

### voice-play

```code
(voice-play voice path)

the sound file at path sent in place of the microphone till it ends,
"" to stop it. 16000 a second, mono, 16 bit, as the microphone is
```

### voice-send

```code
(voice-send in out ctl from talk)

a sink: each piece mailed, numbered, to each that is to hear it. ctl
is sent t and who that is, the mailboxes hex, a comma between; m1 or
m0, muted or not; p and a tap's mailbox hex, or p alone for none; d1
or d0, half duplex or not, on to begin with; h, another was heard; f
and the path of a sound file, f alone for none: what it has is sent
in place of what the microphone hears, a piece of it for each piece
of the microphone, so at the pace it is heard, till it ends.

Only speech is sent, the gate: quiet, nothing goes, no hiss and no
traffic. And half duplex: one heard of late is not spoken over, unless
this one was speaking first, so what another says, played from the
speaker here and heard by the microphone, is not sent back to them.
Headphones have no need of it, d0. talk, if not :nil, is told T and
from, hex, while this one is sent, who is talking
```

### voice-stage

```code
(voice-stage fn [arg ...]) -> stage

a stage of this module
```

### voice-start

```code
(voice-start [talk]) -> voice

the microphone sent to none yet, and what is sent here heard. talk, a
mailbox, is told T and an inbox, hex, as each in it talks, this one too
```

### voice-stop

```code
(voice-stop voice)

neither said nor heard any more
```

### voice-tap

```code
(voice-tap voice mbox)

mbox sent all that is said and heard, to be mixed, (voice-mix); :nil
for none
```

### voice-to

```code
(voice-to voice to)

who hears what is said here now: their inboxes, hex
```

### wheel-push?

```code
(wheel-push? dx dy) -> :t | :nil

a turn not running on from the one before: the other way, or bigger
```

### wheel-view

```code
(wheel-view view) -> view

what takes a turn over view, the nearest up with a :mouse_wheel, else it
```

### window-attach

```code
(window-attach window) -> window

a window is put on the user's root of its own task as it is made, so
what it does not set it finds there as it is laid out, and its
variants are found. Shown, it goes onto the screen, which has the same
properties. Not the screen before then: a window an app made and did
not show would be left on it when the app has gone, and its class
with the app, the login app's of a session of a user say
```

### within-compile-env

```code
(within-compile-env lambda)
```

### word-parts

```code
(word-parts word font room) -> (part ...)

a word too long for a line, a file path or a link, as the parts that
do fit. It is cut after a / _ - . or ) where it can be, and where a
run between two of those is still too long, after any character
```

### x25519

```code
(x25519 secret point) -> str

the secret, 32 bytes, times the point, 32 bytes, the u of a point
of the curve, the low byte first
```

### x25519-public

```code
(x25519-public secret) -> str

the public half of a secret, to give the other side
```

### x25519-swap

```code
p and q swapped if bit is 1
```

### x509-alg

```code
(x509-alg oid) -> :nil | (scheme hash_name)

how a certificate is signed, from the identifier of the way
```

### x509-bits

```code
(x509-bits data item) -> str

what a BIT STRING has, after the byte of how many bits are not used
```

### x509-chain-check

```code
(x509-chain-check certs host [roots now]) -> :nil | why not

:nil if the first of certs is for the host, each is issued by the
next, up to one issued by a root, or a root, and each is in date
```

### x509-digits

```code
(x509-digits text from to) -> num
```

### x509-extensions

```code
(x509-extensions cert data item) -> cert

the names it is for, and whether it may issue others
```

### x509-host?

```code
(x509-host? cert host) -> :t | :nil

is the certificate for the host, by one of its names, a name that
starts *. for any one name more in front of the rest
```

### x509-key

```code
(x509-key cert data spki) -> cert

the kind of key, :rsa :p256 :p384 :ed25519, and the key. Of RSA, its
n and e
```

### x509-parse

```code
(x509-parse der) -> :nil | cert

:der :tbs, what was signed, :sig_alg :sig :issuer :subject, as
their DER, :from :until in seconds, :key_kind :key :names :ca
```

### x509-pem

```code
(x509-pem stream) -> (der ...)

the certificates of a PEM file, each between its BEGIN and END lines
```

### x509-roots

```code
(x509-roots) -> (cert ...)

the roots the host keeps, read the first time they are wanted
```

### x509-sig-check

```code
(x509-sig-check kind key scheme hash_name data signature) -> :t | :nil

a signature of data by a key, signed one of the ways, scheme :pkcs1,
:pss, :ecdsa, :ed25519, with a hash, as a certificate or TLS has it
```

### x509-signed?

```code
(x509-signed? cert issuer) -> :t | :nil

is cert issued by issuer: named so, and its signature that of the
issuer's key
```

### x509-time

```code
(x509-time data item) -> seconds

a time of UTCTime, YYMMDDHHMMSSZ, the years 1950 to 2049, or of
GeneralizedTime, YYYYMMDDHHMMSSZ, as seconds from 1970
0, long ago, if it is too short to be a time
```

### zip

```code
(zip seq ...) -> seq
```

### zip-end

```code
(zip-end data) -> :nil | at

where the record that ends the zip is, looked for from the end back
```

### zip-find

```code
(zip-find entries name) -> :nil | entry

the one of that name, or the one whose name ends so, a file in a
folder of the zip given by its own name
```

### zip-list

```code
(zip-list data) -> :nil | ((name method packed full at crc) ...)

what is in a zip, its directory. A folder is a name that ends in /.
:nil if it is not a zip
```

### zip-read

```code
(zip-read data entry out) -> :nil | :t

what one thing of a zip is, to a stream. :nil if it was packed some
other way, or is not all there
```

### zip-write

```code
(zip-write out files [stamp]) -> :nil | count

a zip of files, ((name data) ...), to a stream: each packed by DEFLATE,
or stored as it is when that is no smaller, a name that ends in / a
folder, then the directory and the record that ends it. stamp is the
time each is said to be of, (date), now if not given. :nil if there
are more than 65535 or it would be past 4GB, which want ZIP64
```

