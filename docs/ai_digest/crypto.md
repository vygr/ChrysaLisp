# Hashes And Ciphers

`lib/crypto/` is where the hash and cipher primitives go, for the storage
service, `docs/ai_digest/storage_service.md`, and one day for TLS. A block of
data at a time is too much work for Lisp, so the part that is done for every
byte is native code, `lib/crypto/lisp.vp`, and the part that is done once,
the padding and the like, is Lisp.

There is a hash, SHA-256, with HMAC on it, and a cipher that guards what it
hides, ChaCha20 with Poly1305.

| | native code | on one core of an Apple M4 Max |
|---|---|---|
| SHA-256 | 2,848 bytes | 253MB a second |
| ChaCha20 | 2,200 bytes | 441MB a second |
| Poly1305 | 888 bytes | 2,778MB a second |
| seal, the two together | | 378MB a second |

The sizes are those of ARM64. None of it uses the instructions a CPU may
have for this, it is the same VP on every CPU, and all of it is in the boot
image, as the native code of every library is.

## SHA-256

The hash of FIPS 180-4. 32 bytes that stand for any amount of data, such
that no two different inputs have ever been found with the same hash. It is
what would name a block of the store by its content.

```lisp
(import "lib/crypto/sha256.inc")

(sha256 "abc")
```

`(sha256 data)` is the hash of a str, as a str of 32 bytes.

For what does not come all at once, a file read a part at a time.

```lisp
(defq ctx (sha256-start))
(while (defq part (read-blk stream 65536))
	(sha256-add ctx part))
(defq hash (sha256-end ctx))
```

* `(sha256-start) -> ctx`, a hash with nothing in it yet.
* `(sha256-add ctx data) -> ctx`, more of what is being hashed, of any
  length. What does not make a whole block of 64 bytes is kept for the next
  call.
* `(sha256-end ctx) -> str`, the 32 bytes. The ctx is done with.

The parts can be of any size, the hash is the same as that of the whole.

### With a key

```lisp
(hmac-sha256 key data) -> str
```

HMAC of RFC 2104, the hash of a str with a key, 32 bytes, that only one who
has the key can make or check. It is SHA-256 twice over, and is all Lisp.

### A key from a password

```lisp
(import "lib/crypto/pbkdf2.inc")

(defq salt (random-bytes 16)
	key (pbkdf2-sha256 password salt 100000 32))
```

`(pbkdf2-sha256 password salt count size) -> str`, PBKDF2 of RFC 8018 with
the HMAC above, `size` bytes to use as a key. The same four give the same
key on any machine.

A password is short and can be guessed, so the key is made slow to work
out, `count` times round. That costs the one who knows the password once,
and one who is guessing it every guess. 100,000 times round is 0.48 seconds
on an Apple M4 Max. The salt is not secret, it is kept with what the key is
for, and is there so the same password is not the same key twice, and so a
table of guesses made for one is no use for another.

The hash of the key with each of the two pads of HMAC is the same every
time round, so it is done the once, and each time round is two calls of the
native code and no more. That is near 4 times quicker than calling
`(hmac-sha256)` each time. It calls `(task-slice)` every 1,024 times round.

It is all Lisp but the hash. It does not make guessing dear in memory, as
scrypt and Argon2 do, only in time.

### The native code

```lisp
(sha256-blocks state data offset count) -> state
```

`count` blocks of 64 bytes, from that offset of `data`, are taken into the
state, which is 8 numbers of 32 bits in a str of 32 bytes, and is changed
where it is. It takes an offset so that a long str is hashed with no copy of
any of it. It refuses a state that is not 32 bytes, and blocks that are not
all inside the data.

`(sha256-add)` gives it 1,024 blocks at a time, 64KB, and calls
`(task-slice)` between, so a long hash does not keep the other tasks of its
node waiting.

A number of 32 bits is kept in a 64 bit register with nothing above it. VP
has no rotate, so a rotate is done with a copy of the number above itself,
which, moved down n bits, has the number rotated right by n as its low 32
bits. What gets above 32 bits in a sum does no harm, and is cut off when the
sum is kept. The 8 working numbers stay in registers for all 64 rounds, which
are written out 8 at a time, after which each number is back in the register
it started in.

It hashes 253MB a second on one core of an Apple M4 Max, with none of the
SHA instructions a CPU may have.

### Tests

`tests/crypto/test_sha256.lisp`. The answers of the standard, nothing, `abc`,
the two longer examples and a million of the letter a. Every length about
the edges of a block, where the padding changes. A str added a part at a
time, in parts of nine sizes. What the native code refuses. And for HMAC
the test cases of RFC 4231. The answers it is checked against were made with
Python's `hashlib` and `hmac`.

`tests/crypto/test_pbkdf2.lisp`. The test cases that go round with RFC 6070,
for SHA-256, a key of more than one block of the hash, a password longer
than a block, and that the size asked for is the size had.

## ChaCha20 With Poly1305

The AEAD of RFC 8439, and what to use to hide data. It hides it, and it
guards it, what is opened is what was sealed or it does not open.

```lisp
(import "lib/crypto/aead.inc")

(defq sealed (aead-seal key nonce aad data))
(defq data (aead-open key nonce aad sealed))
```

* `(aead-seal key nonce aad data) -> str`, the data encrypted, with a tag
  of 16 bytes after it, so 16 bytes longer.
* `(aead-open key nonce aad sealed) -> :nil | str`, the data, or `:nil` if
  the tag is not right. That is when any bit of what was sealed, of the aad,
  of the key or of the nonce is not what it was sealed with, or it is cut
  short.

The key is a str of 32 bytes, the nonce one of 12. **A nonce must never be
used twice with a key**, two things sealed with the same pair give each
other away, and the key of the tag with them. A counter is a good nonce. So
is the name of the thing, if a thing is only ever sealed once.

`aad` is data that goes with it, guarded but not hidden, the name of a
block, a header, or `""`. It is not in what comes back from a seal, the one
who opens has to have it.

It is the two below put together as the RFC has it. The key of the tag is
the start of block 0 of the cipher's stream, so it is new for each nonce,
and the data is encrypted from block 1. The tag is of the aad, then what was
encrypted, each made up with 0 to a whole 16 bytes, then how long each was.
A tag is checked a byte at a time with every byte looked at, right or
wrong.

### ChaCha20

```lisp
(import "lib/crypto/chacha20.inc")

(chacha20 key nonce counter data) -> str
```

The data, each byte xored with the next byte of a stream that the key, the
nonce and the counter make. Done again with the same three it gives the data
back. The counter is the number of the first block of 64 bytes of the
stream, so `(chacha20 key nonce 3 ...)` is the stream from 192 bytes in.

It hides and does not guard. A bit changed on the way is a bit changed when
it is decrypted, and nothing says so. Use the seal.

```lisp
(chacha20-xor key nonce counter data out offset length) -> out
```

The native code. `length` bytes of `data`, from that offset, go to the same
place in `out`, which can be the data itself. It is given 64KB at a time,
with `(task-slice)` between. A part of a block at the end is done on the
stack and copied out.

The block is 16 numbers of 32 bits. VP has 15 registers, so 12 are in
registers through the 20 rounds and the four of the third row are on the
stack, the one a quarter round uses loaded for it and stored after. A rotate
left is two shifts, an or, and an and.

### Poly1305

```lisp
(import "lib/crypto/poly1305.inc")

(poly1305 key data) -> str
```

A tag of 16 bytes for a str, that only one who has the key could have made.
The key is 32 bytes and is for the one message, a key used for two lets it
be worked out, which is why the seal makes one for each nonce.
`(poly1305-start key)`, `(poly1305-add ctx data)` and `(poly1305-end ctx)`
are for what comes a part at a time.

```lisp
(poly1305-blocks state data offset count top) -> state
```

The native code. The tag is a sum, of each 16 bytes as a number, times a
part of the key, and on, all less a multiple of the prime 2 to the 130 less
5. The sum is a number of 130 bits, kept as five of 26, so that a product of
two of them, and five such added, fits in 64 bits, VP has no way to get at
the top half of a product. What would go above 130 bits comes back in at the
bottom times 5. `top` is 1 for whole blocks and 0 for a last block that has
had its own top bit put in, which is done in Lisp, as is the last of the
arithmetic, done once.

### Tests

`tests/crypto/test_chacha20.lisp`, `test_poly1305.lisp` and `test_aead.lisp`.
The three examples of RFC 8439. Every length about the edges of the blocks
of both, with aad of four lengths. More than the native code is given at
once. Keys and data of all ones, where the sums of Poly1305 are biggest. A
part of a str, to another, and onto itself. And that nothing opens with a
bit changed, a byte short, or a byte too many. The answers are from a Python
of the RFC written for the job, which gives the RFC's own.

## Random Bytes

```lisp
(import "lib/crypto/random.inc")

(defq key (random-bytes 32) nonce (random-bytes 12))
```

`(random-bytes size) -> str`, that many bytes that can not be guessed, from
the host's own source, `/dev/urandom` on a Mac and on Linux, `RtlGenRandom`
on Windows. It is what a key is made from. A nonce of 12 bytes made this way
is safe for as many messages as anyone will send with one key, though a
counter is surer, it can not come round twice.

`(random num)`, of the language, is not this. It gives numbers from a seed,
quick, and good for a game or a test, and whoever knows the seed knows them
all.

The Windows host program used `rand()` for these, seeded from the time and
its process number, which would not have done for a key. It asks the system
now. That is built here for Windows and links, and has not been run there.

## Not here yet

* A check for a CPU that can not load a number from an address that is not
  a multiple of its size. The native code loads 4 and 8 bytes at a time
  from wherever in a str it is told to start.
* Arithmetic on a field, for error correction and for signatures.
