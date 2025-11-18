# Bloom Filter Library and Demo

A space-efficient probabilistic data structure for set membership testing in ChrysaLisp.

## What is a Bloom Filter?

A Bloom filter is a probabilistic data structure that efficiently tests whether an element is a member of a set. It's space-efficient but allows for false positives (saying an element is in the set when it's not) while guaranteeing no false negatives (if it says an element is not in the set, it definitely isn't).

### Key Characteristics

- **Space Efficient**: Uses a bit array, much smaller than storing actual elements
- **Fast Operations**: O(k) time for both insertions and lookups, where k is the number of hash functions
- **Probabilistic**: Can have false positives but NEVER false negatives
- **No Deletion**: Standard Bloom filters don't support element removal (by design)

## Library Location

The Bloom filter implementation is located at:
```
lib/collections/bloom.inc
```

## Usage

### Creating a Bloom Filter

```lisp
(import "lib/collections/bloom.inc")

; Create with default parameters (1024 bits, 3 hash functions)
(defq bf (Bloom))

; Create with custom size and hash function count
(defq bf (Bloom 2048 5))  ; 2048 bits, 5 hash functions
```

### Basic Operations

```lisp
; Add elements
(. bf :add "apple")
(. bf :add "banana")
(. bf :add "cherry")

; Check membership
(if (. bf :contains? "apple")
    (print "apple might be in the set")
    (print "apple is definitely NOT in the set"))

; Check if empty
(if (. bf :empty?)
    (print "Filter is empty"))

; Clear all elements
(. bf :clear)

; Copy a Bloom filter
(defq bf2 (. bf :copy))
```

### API Reference

#### Constructor
- `(Bloom [size] [hash_count])` - Create a new Bloom filter
  - `size`: Bit array size (default: 1024)
  - `hash_count`: Number of hash functions (default: 3)

#### Methods
- `:add key` - Add an element to the filter
- `:insert key` - Alias for `:add` (Set interface compatibility)
- `:contains? key` - Check if element might be in set (returns `:t` or `:nil`)
- `:find key` - Return key if possibly in set, `:nil` if definitely not
- `:clear` - Reset the filter to empty state
- `:empty` - Alias for `:clear` (Set interface compatibility)
- `:empty?` - Check if filter is empty
- `:size` - Return the bit array size
- `:item_count` - Return the number of items added (approximate)
- `:hash_count` - Return the number of hash functions
- `:false_positive_rate` - Estimate the current false positive probability
- `:optimal_hash_count expected_items` - Calculate optimal hash function count
- `:copy` - Create a shallow copy of the filter

## Running the Demo

To run the demonstration:

```bash
./run.sh apps/bloom_demo/app.lisp
```

Or within ChrysaLisp:
```lisp
(import "apps/bloom_demo/app.lisp")
```

## Demo Features

The demo application showcases:

1. **Basic Operations** - Creating filters, adding elements, checking membership
2. **False Positive Demonstration** - Shows how false positives occur
3. **Space Efficiency Comparison** - Compares memory usage with regular sets
4. **Optimal Parameters** - Demonstrates parameter calculation
5. **Clear and Reuse** - Shows how to reset and reuse filters
6. **Copy Operation** - Demonstrates filter copying
7. **Use Case: Spell Checker** - Practical application example

## When to Use Bloom Filters

### Good Use Cases

- **Caching**: Check if an expensive computation result might be cached
- **Spell Checking**: Quick dictionary lookup for common words
- **URL Filtering**: Check if a URL is in a blacklist
- **Database Query Optimization**: Avoid expensive lookups for non-existent keys
- **Network Applications**: Reduce network lookups by filtering locally
- **Duplicate Detection**: Quick first-pass filter for duplicate elements

### When NOT to Use

- When false positives are unacceptable
- When you need to delete elements
- When the dataset is very small (overhead not worth it)
- When you need exact membership information

## Performance Considerations

### Optimal Parameter Selection

The false positive rate depends on three factors:
- `m`: Bit array size
- `n`: Number of elements inserted
- `k`: Number of hash functions

**False Positive Rate Formula**: `(1 - e^(-kn/m))^k`

**Optimal Hash Functions**: `k = (m/n) * ln(2) ≈ 0.693 * (m/n)`

### Example Configurations

| Elements | Bit Size | Hash Functions | FP Rate |
|----------|----------|----------------|---------|
| 100      | 1024     | 7              | ~0.01%  |
| 1000     | 8192     | 5              | ~0.1%   |
| 10000    | 65536    | 4              | ~1%     |

## Implementation Details

### Bit Array Storage

The filter uses a list of 64-bit integers to represent the bit array:
- Each integer represents 64 bits
- Bit operations: `logior` (OR), `logand` (AND), `<<` (shift)

### Hash Functions

Multiple hash functions are simulated by mixing the base hash with different seeds:
```lisp
(logxor (hash key) (<< seed 13) (>> seed 7))
```

This approach provides good distribution while being computationally efficient.

### Space Efficiency

For 1000 elements with 1% false positive rate:
- Bloom filter: ~1.2 KB (9600 bits)
- Regular set: ~8-32 KB (depending on element size)

**Savings**: ~85-95% memory reduction

## Extending the Library

The Bloom filter class extends the `Set` base class and can be used anywhere a Set is expected. To add new functionality:

```lisp
(defmethod :union (other_bloom)
    ; Combine two Bloom filters (OR operation on bit arrays)
    ; Implementation here...
)
```

## Mathematical Background

### Probability Analysis

Given:
- `m` = number of bits
- `n` = number of elements
- `k` = number of hash functions

After inserting `n` elements, the probability that a specific bit is still 0:
```
P(bit = 0) = (1 - 1/m)^(kn) ≈ e^(-kn/m)
```

False positive probability:
```
P(false positive) = (1 - e^(-kn/m))^k
```

## References

- [Bloom, Burton H. (1970). "Space/time trade-offs in hash coding with allowable errors"](https://dl.acm.org/doi/10.1145/362686.362692)
- [Wikipedia: Bloom Filter](https://en.wikipedia.org/wiki/Bloom_filter)

## License

This implementation follows the ChrysaLisp project license.

## Author

Created as a demonstration of space-efficient data structures in ChrysaLisp.
