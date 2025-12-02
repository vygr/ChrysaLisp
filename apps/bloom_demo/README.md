# Bloom Filter Library and Demo

A comprehensive suite of space-efficient probabilistic data structures for set membership testing in ChrysaLisp.

**NEW:** Now includes Counting Bloom Filters (with deletion), Scalable Bloom Filters (auto-growing), and full serialization support!

## What is a Bloom Filter?

A Bloom filter is a probabilistic data structure that efficiently tests whether an element is a member of a set. It's space-efficient but allows for false positives (saying an element is in the set when it's not) while guaranteeing no false negatives (if it says an element is not in the set, it definitely isn't).

### Key Characteristics

- **Space Efficient**: Uses a bit array, much smaller than storing actual elements
- **Fast Operations**: O(k) time for both insertions and lookups, where k is the number of hash functions
- **Probabilistic**: Can have false positives but NEVER false negatives
- **No Deletion**: Standard Bloom filters don't support element removal (by design)

## Library Locations

Three Bloom filter variants are available:

```
lib/collections/bloom.inc           - Standard Bloom Filter
lib/collections/counting_bloom.inc  - Counting Bloom Filter (supports deletion)
lib/collections/scalable_bloom.inc  - Scalable Bloom Filter (auto-grows)
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
- `:save stream` - Serialize filter to a stream
- `:load stream` - Deserialize filter from a stream
- `:save_to_file filename` - Save filter to a file
- `:load_from_file filename` - Load filter from a file

## Advanced Variants

### Counting Bloom Filter

Supports element deletion by using counters instead of bits.

```lisp
(import "lib/collections/counting_bloom.inc")

; Create with size, hash functions, and max counter value
(defq cbf (CountingBloom 512 3 15))

; Add elements
(. cbf :add "apple")
(. cbf :add "banana")

; Remove elements - THE KEY FEATURE!
(. cbf :remove "banana")
(. cbf :erase "apple")  ; Alias for :remove

; Get counter statistics
(bind '(min max avg) (. cbf :counter_stats))
```

**Constructor:**
- `(CountingBloom [size] [hash_count] [max_count])` - Create a counting filter
  - `size`: Counter array size (default: 1024)
  - `hash_count`: Number of hash functions (default: 3)
  - `max_count`: Maximum counter value (default: 15)

**Additional Methods:**
- `:remove key` - Remove an element from the filter
- `:erase key` - Alias for `:remove` (Set interface)
- `:counter_stats` - Return `(min max avg)` counter statistics
- `:save stream` / `:load stream` - Serialization support
- `:save_to_file filename` / `:load_from_file filename` - File I/O

**Trade-offs:**
- ✅ Supports deletion (unique feature)
- ❌ Uses more memory (~64x more than standard Bloom filter)
- ❌ Counter overflow possible with max_count limit

### Scalable Bloom Filter

Automatically grows by adding new internal Bloom filters as needed.

```lisp
(import "lib/collections/scalable_bloom.inc")

; Create with initial size, target FP rate, and growth factor
(defq sbf (ScalableBloom 128 0.01 2))

; Add unlimited items - it grows automatically!
(each (lambda (i)
    (. sbf :add (cat "item_" i)))
    (range 0 10000))

; Check stats
(bind '(filter_count total_size count capacity) (. sbf :stats))
(print (cat "Internal filters: " filter_count))
```

**Constructor:**
- `(ScalableBloom [initial_size] [target_fp_rate] [growth_factor])` - Create scalable filter
  - `initial_size`: Size of first filter (default: 1024)
  - `target_fp_rate`: Target false positive rate (default: 0.01 = 1%)
  - `growth_factor`: Growth multiplier for new filters (default: 2)

**Additional Methods:**
- `:filter_count` - Return number of internal Bloom filters
- `:capacity` - Return total capacity before next growth
- `:stats` - Return `(filter_count total_size count capacity)`
- `:save stream` / `:load stream` - Serialization support
- `:save_to_file filename` / `:load_from_file filename` - File I/O

**Trade-offs:**
- ✅ Handles unlimited items automatically
- ✅ Maintains target false positive rate
- ❌ No deletion support
- ❌ Multiple internal filters add complexity

## Serialization

All three Bloom filter variants support saving and loading:

```lisp
; Save to stream
(defq stream (file-stream "myfilter.dat" +file_open_write))
(. bloom_filter :save stream)
(. stream :close)

; Load from stream
(defq stream (file-stream "myfilter.dat"))
(defq loaded_filter (Bloom))
(. loaded_filter :load stream)
(. stream :close)

; Convenience methods
(. bloom_filter :save_to_file "myfilter.dat")
(. bloom_filter :load_from_file "myfilter.dat")
```

**File Formats:**
- Standard Bloom: Magic `0x424C4F4D` ("BLOM")
- Counting Bloom: Magic `0x43424C4D` ("CBLM")
- Scalable Bloom: Magic `0x53424C4D` ("SBLM")

**Use Cases:**
- Cache warm-up: Persist filters between sessions
- Checkpoint/restore: Save filter state for recovery
- Sharing: Distribute pre-built filters
- Storage: Archive historical filters

## Running the Demos

### Basic Demo

```bash
./run.sh apps/bloom_demo/app.lisp
```

### Advanced Features Demo

```bash
./run.sh apps/bloom_demo/advanced_demo.lisp
```

### Unit Tests

```bash
./run.sh apps/bloom_demo/test.lisp
```

Or within ChrysaLisp:
```lisp
(import "apps/bloom_demo/app.lisp")
(import "apps/bloom_demo/advanced_demo.lisp")
```

## Demo Features

### Basic Demo (`app.lisp`)

1. **Basic Operations** - Creating filters, adding elements, checking membership
2. **False Positive Demonstration** - Shows how false positives occur
3. **Space Efficiency Comparison** - Compares memory usage with regular sets
4. **Optimal Parameters** - Demonstrates parameter calculation
5. **Clear and Reuse** - Shows how to reset and reuse filters
6. **Copy Operation** - Demonstrates filter copying
7. **Use Case: Spell Checker** - Practical application example

### Advanced Demo (`advanced_demo.lisp`)

1. **Counting Bloom Filter** - Deletion support demonstration
2. **Scalable Bloom Filter** - Dynamic growth in action
3. **Serialization** - Save/load filters to/from disk
4. **Comparison** - Side-by-side feature comparison

## Choosing the Right Variant

| Feature | Standard | Counting | Scalable |
|---------|----------|----------|----------|
| **Deletion Support** | ❌ No | ✅ Yes | ❌ No |
| **Auto-Growth** | ❌ No | ❌ No | ✅ Yes |
| **Memory Efficiency** | ✅ Best | ⚠️ ~64x more | ✅ Good |
| **False Positives** | ✅ Predictable | ✅ Predictable | ✅ Managed |
| **Serialization** | ✅ Yes | ✅ Yes | ✅ Yes |
| **Best For** | Fixed datasets | Mutable sets | Growing datasets |

**Quick Decision Guide:**
- Fixed size, known dataset → **Standard Bloom**
- Need to delete items → **Counting Bloom**
- Unknown/growing size → **Scalable Bloom**

## When to Use Bloom Filters

### Good Use Cases

- **Caching**: Check if an expensive computation result might be cached
- **Spell Checking**: Quick dictionary lookup for common words
- **URL Filtering**: Check if a URL is in a blacklist
- **Database Query Optimization**: Avoid expensive lookups for non-existent keys
- **Network Applications**: Reduce network lookups by filtering locally
- **Duplicate Detection**: Quick first-pass filter for duplicate elements

### When NOT to Use

- When false positives are unacceptable (all variants)
- When you need to delete elements (unless using Counting Bloom)
- When the dataset is very small (overhead not worth it)
- When you need exact membership information (all variants)
- When you need to iterate over elements (all variants)

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
