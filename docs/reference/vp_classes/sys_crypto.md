# :sys_crypto

```image
docs/reference/vp_classes/sys_crypto.cwb
```

## VP methods

### :aead_open -> sys/crypto/aead_open

```code
inputs
:r0 = key, 32 bytes (ptr)
:r1 = nonce, 12 bytes (ptr)
:r2 = in, length + +aead_tag_size bytes, the tag last (ptr)
:r3 = out, length bytes (ptr)
:r4 = length, not counting the tag (bytes)
outputs
:r0 = 1 if the tag is right and out has the data, else 0
trashes
:r0-:r14
info
the tag worked out, and every byte of it looked at, right or wrong;
only if it is right is the data decrypted. out can be in.
```

### :aead_seal -> sys/crypto/aead_seal

```code
inputs
:r0 = key, 32 bytes (ptr)
:r1 = nonce, 12 bytes (ptr)
:r2 = in (ptr)
:r3 = out, length + +aead_tag_size bytes (ptr)
:r4 = length (bytes)
trashes
:r0-:r14
info
the data encrypted, and its tag after it, ChaCha20-Poly1305 of RFC
8439 with no aad. out can be in, with room for the tag.
```

### :chacha20 -> sys/crypto/chacha20

```code
inputs
:r0 = key, 32 bytes (ptr)
:r1 = nonce, 12 bytes (ptr)
:r2 = counter, the block of 64 bytes in starts at (uint)
:r3 = in (ptr)
:r4 = out (ptr)
:r5 = length (bytes)
trashes
:r0-:r14
info
length bytes of in to out, each xored with the next byte of the key
stream of ChaCha20, RFC 8439. out can be in.
```

### :poly1305_blocks -> sys/crypto/poly1305_blocks

```code
inputs
:r0 = state (ptr)
:r1 = data (ptr)
:r2 = count of blocks of 16 bytes
:r3 = top, 1 for whole blocks, 0 for a last that has had its own put in
trashes
:r0-:r14
info
the blocks taken into the sum of Poly1305, RFC 8439
```

### :poly1305_finish -> sys/crypto/poly1305_finish

```code
inputs
:r0 = state (ptr)
:r1 = s, the second half of the key, 16 bytes (ptr)
:r2 = tag out, 16 bytes (ptr)
trashes
:r3-:r14
info
the tag of all the blocks taken in: h mod the prime, and s added
```

### :poly1305_init -> sys/crypto/poly1305_init

```code
inputs
:r0 = key, 32 bytes, its first half r (ptr)
:r1 = state, +poly1305_state_size bytes (ptr)
trashes
:r2-:r9
info
a tag with nothing in it yet: h 0, r its bits cleared as RFC 8439 has
it, as five numbers of 26 bits, and the top four of r times 5
```

