# Hashes And Ciphers

`lib/crypto/` is where the hash and cipher primitives go, for the storage
service, `docs/ai_digest/storage_service.md`, and one day for TLS. A block of
data at a time is too much work for Lisp, so the part that is done for every
byte is native code, `lib/crypto/lisp.vp`, and the part that is done once,
the padding and the like, is Lisp.

There is a hash so far, SHA-256, and HMAC on it. No cipher yet.

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

The function is 2,848 bytes on ARM64, and is in the boot image, as the
native code of every library is. It hashes 253MB a second on one core of an
Apple M4 Max. It uses none of the SHA instructions a CPU may have, it
is the same VP on every CPU.

### Tests

`tests/crypto/test_sha256.lisp`. The answers of the standard, nothing, `abc`,
the two longer examples and a million of the letter a. Every length about
the edges of a block, where the padding changes. A str added a part at a
time, in parts of nine sizes. What the native code refuses. And for HMAC
the test cases of RFC 4231. The answers it is checked against were made with
Python's `hashlib` and `hmac`.

## Not here yet

* A cipher. ChaCha20 with Poly1305 is the one meant, add, rotate and xor,
  with no tables and nothing of any one CPU.
* Arithmetic on a field, for error correction and for signatures.
