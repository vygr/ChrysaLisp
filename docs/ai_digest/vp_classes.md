# VP Class Inheritance Hierarchy

This diagram shows the inheritance relationships between the core classes
defined in `class/`. Indentation indicates that a class inherits from the one
above it.

```
obj
 |
 +-> error
 |
 +-> dim
 |
 +-> hset
 |    |
 |    +-> hmap
 |
 +-> lisp
 |
 +-> num
 |    |
 |    +-> fixed
 |    |    |
 |    |    +-> real
 |    |
 |    +-> func
 |
 +-> seq
 |    |
 |    +-> array
 |    |    |
 |    |    +-> list
 |    |    |
 |    |    +-> nums
 |    |         |
 |    |         +-> fixeds
 |    |              |
 |    |              +-> reals
 |    |
 |    +-> str
 |         |
 |         +-> node
 |         |
 |         +-> netid
 |         |
 |         +-> sym
 |
 +-> stream
 |    |
 |    +-> fstream
 |    |
 |    +-> in
 |    |
 |    +-> out
 |    |
 |    +-> sstream
 |
 +-> stdio
```

## Class Descriptions

### 1. Root and Abstract Classes

**`obj`**

*   **Inherits From:** None (Root Class)

*   **Purpose:** The fundamental base class for all objects in the system. It
    provides the core mechanisms for reference counting, type introspection, and
    polymorphism. Every other class ultimately inherits from `obj`.

*   **Key Methods:**

    * `:ref` / `:deref`: The core of the reference counting memory management
        system.

    * `:init` / `:deinit`: The object constructor and destructor logic.
        `:deinit` is `virtual`, allowing subclasses to clean up their specific
        resources.

    * `:type`: A `virtual` method that returns a `list` of symbols representing
        the object's class hierarchy (e.g., `(:real :fixed :num :obj)`).

    * `:hash`: A `virtual` method for generating a hash value, essential for use
        in `hset` and `hmap`.

    * `:print`: A `virtual` method for generating a string representation of the
        object.

**`seq`** (Sequence)

*   **Inherits From:** `obj`

*   **Purpose:** An **abstract base class** that defines the common interface
    for all sequence-like data structures (strings, arrays, lists). It ensures
    that different kinds of sequences can be treated polymorphically by
    functions that expect to read elements by index or get a length.

*   **Key Methods:** All methods are `virtual`, meaning they are intended to be
    implemented by subclasses.

    * `:get_length`: Returns the number of elements in the sequence.

    * `:ref_elem`: Retrieves an element at a given index.

    * `:slice`: Returns a new sequence containing a sub-section of the original.

    * `:cat`: Concatenates a list of sequences into a new one.

    * `:find`: Searches for an element within the sequence.

**`stream`**

*   **Inherits From:** `obj`

*   **Purpose:** An **abstract base class** for all I/O streams. It defines a
    standard API for reading from and writing to various data sources (files,
    memory, network mailboxes). It manages an internal buffer to improve
    performance.

*   **Key Methods:**

    * `:read` / `:write`: Read/write blocks of data.

    * `:read_char` / `:write_char`: Handle single bytes.

    * `:flush`: A `virtual` method to commit any buffered data to the underlying
        source.

    * `:read_next` / `:write_next`: `virtual` methods that subclasses must
        implement to get more data from or send data to the underlying source
        when the buffer is exhausted/full.

### 2. Concrete Data Structures

**`array`**

*   **Inherits From:** `seq`

*   **Purpose:** A concrete, general-purpose dynamic array for storing raw
    `long` values (which can be numbers or pointers). It implements the `seq`
    interface and manages its own capacity, reallocating memory as needed. It is
    the foundation for more specialized collections like `list` and `nums`.

*   **Key Methods:**

    * `:set_cap`: Manages the allocated capacity of the array.

    * `:push_back` / `:pop_back`: Adds/removes elements from the end.

    * `:get_length`: Returns the number of elements currently in the array.

    * `:get_elem`: Retrieves an element at a specific index.

    * `:erase`: Removes an element using a fast swap-and-pop technique.

**`list`**

*   **Inherits From:** `array`

*   **Purpose:** The primary general-purpose container for storing collections
    of **other objects**. It specializes `array` by making its lifecycle methods
    object-aware.

*   **Key Methods:**

    * `:deinit` / `:clear`: Overridden to call `obj:deref` on every object it
        contains, ensuring proper reference-counted memory management.

    * `:set_elem`: Overridden to `deref` the old object at the target index
        before replacing it.

    * `:ref_elem`: Returns an element and increments its reference count.

    * It inherits `:push_back`, `:pop_back`, etc., but because it stores object
        pointers, these methods effectively manage lists of heterogeneous
        objects.

**`str`** (String)

*   **Inherits From:** `seq`

*   **Purpose:** The fundamental, immutable string type. It is a vector of bytes
    with associated length and hash-caching fields.

*   **Key Methods:**

    * `:create_from_cstr`, `:create_from_file`, `:create_from_long`:
        Constructors for various sources.

    * `:append`, `:cat`: Creates new strings by concatenation.

    * `:compare`, `:same`, `:starts_with`: String comparison utilities.

    * `:split`: Tokenizes a string into a `list` of new strings.

    * `:hash`: A fast, cached string hashing function.

**`sym`** (Symbol)

*   **Inherits From:** `str`

*   **Purpose:** The Lisp symbolic atom. It inherits all the properties of a
    string but adds the crucial concept of **interning**. Interned symbols are
    guaranteed to have a single, unique object instance for any given character
    sequence, allowing for fast pointer comparisons instead of slow string
    comparisons.

*   **Key Methods:**

    * `:intern`, `:intern_cstr`, `:intern_str`: The core methods that find or
      create the unique instance of a symbol in a global hash set
      (`statics_sym_intern`).

**`node`**

*   **Inherits From:** `str`

*   **Purpose:** A specialized string used to represent the unique node ID of
    a ChrysaLisp kernel instance.

*   **Key Methods:**
    * `:hash`: Overridden to compute the hash based on the 128-bit `node_id`
        stored within its string data, rather than the entire string content.

**`netid`**

*   **Inherits From:** `str`

*   **Purpose:** A specialized string used to represent the unique network ID of
    a temporary ChrysaLisp mailbox instance.

### 3. Numerical Types

**`num`**

*   **Inherits From:** `obj`

*   **Purpose:** A simple object wrapper for a standard `long` integer. It
    serves as the base class for more complex number types like `fixed` and
    `real`.

*   **Key Methods:**

    * `:create`: Creates a `num` object from a raw `long`.

    * Arithmetic methods (`:add`, `:sub`, `:mul`, etc.): These methods uniquely
        operate on a `list` of `num` objects, performing a reduction (e.g.,
        `(+)` sums the list) and returning a *new* `num` object with the result.

**`fixed`**

*   **Inherits From:** `num`

*   **Purpose:** A fixed-point number type. It stores its value as a scaled
    integer.

*   **Key Methods:**

    * Overrides arithmetic methods (`:mul`, `:div`) to implement correct
        fixed-point logic (e.g., shifting after multiplication).

    * Overrides `:print` to display the number with a decimal point.

    * Adds fixed-point specific methods like `:frac` and `:floor`.

**`real`**

*   **Inherits From:** `fixed`

*   **Purpose:** A software-implemented floating-point number type.

*   **Key Methods:**

    * Overrides all arithmetic and mathematical methods (`:add`, `:sub`, `:sin`,
        `:sqrt`, etc.) to delegate the calculations to the `sys_math` library,
        which contains the low-level floating-point routines.

### 4. Vectorized Numerical Types

**`nums`**

*   **Inherits From:** `array`

*   **Purpose:** A high-performance vector of `long` integers. It is designed
    for SIMD-like, element-wise operations.

*   **Key Methods:**

    * Arithmetic methods (`:add`, `:sub`, `:mul`, `:div`): These methods take
        two source `nums` vectors and perform the operation on each
        corresponding pair of elements, storing the result in a destination
        vector. They are implemented using optimized `vec-loop` macros.

    * `:dot`: Computes the dot product.

    * `:sum`: Computes the sum of all elements.

**`fixeds`**

*   **Inherits From:** `nums`

*   **Purpose:** A high-performance vector of fixed-point numbers.

*   **Key Methods:**

    * Overrides arithmetic methods like `:mul` and `:div` to apply fixed-point
        logic (e.g., bit-shifting) to each element-wise operation within the
        vector loop.

**`reals`**

*   **Inherits From:** `fixeds`

*   **Purpose:** A high-performance vector of software-implemented
    floating-point numbers.

*   **Key Methods:**

    * Overrides all arithmetic methods to call the corresponding `sys_math`
        routine (e.g., `:r_add`, `:r_mul`) for each element pair inside the
        vector loop.

### 5. I/O Stream Implementations

**`fstream`** (File Stream)

*   **Inherits From:** `stream`

*   **Purpose:** Implements the `stream` API for reading from and writing to
    filesystem files.

*   **Key Methods:**

    * `:init`: Opens a file using `host_os:pii_open`.

    * `:deinit`: Flushes data and closes the file handle.

    * `:read_next`: Fills the buffer by reading a block from the file.

    * `:write_next`: Flushes the buffer by writing its contents to the file.

**`sstream`** (String Stream)

*   **Inherits From:** `stream`

*   **Purpose:** Implements the `stream` API for reading from and writing to an
    in-memory `str` object. This is extremely useful for building strings
    piece-by-piece.

*   **Key Methods:**

    * `:init`: Takes a `str` object to use as its backing buffer.

    * `:ref_string` / `:claim_string`: Methods to get the underlying `str`
        object.

    * `:write_next`: When the buffer is full, it automatically allocates a new,
        larger `str` object and copies the old content over, making it
        dynamically resizable.

**`in` / `out`**

*   **Inherits From:** `stream`

*   **Purpose:** These two classes work together to provide a reliable,
    sequenced, stream-like interface over the OS's unreliable, message-passing
    mail system. `in` receives messages and reassembles them in order, while
    `out` sends them.

*   **Key Methods:**

    * `in:next_msg`: The core logic for handling out-of-order message fragments
        and ensuring the application only sees a contiguous stream of data.

    * `out:flush`: Bundles the contents of its write buffer into a `stream_msg`
        and sends it via `sys_mail:send`, including sequence numbers.

### 6. System and Application-Level Classes

**`lisp`**

*   **Inherits From:** `obj`

*   **Purpose:** Represents an instance of the Lisp interpreter. It holds the
    execution context for a task, including its environment (`hmap`), stack, and
    references to core symbols and functions.

*   **Key Methods:**

    * `:init`: Sets up the Lisp environment, either by creating the root
        environment or linking to the shared one.

    * `:repl_eval`, `:repl_apply`: The core of the evaluator and function
        application logic.

    * `:read`: The S-expression parser.

    * `:repl_error`: The error handling and reporting mechanism.

**`func`** (Function)

*   **Inherits From:** `num`

*   **Purpose:** Represents a callable function. Its underlying value is a raw
    pointer to the function's executable entry point. The Lisp interpreter uses
    the `func` vtable to distinguish it from a plain `num`.

*   **Key Methods:**

    * `:print`: Cleverly extracts the function's symbolic path for a readable
        representation (e.g., `#<sys/task/sleep>`).

**`hset`** / **`hmap`** (Hash Set / Hash Map)

*   **Inherits From:** `obj` / `hset`

*   **Purpose:** The fundamental associative data structures. `hset` provides
    the core hashing and bucketing logic. `hmap` extends `hset` to store
    key-value pairs and adds a `:parent` pointer to model lexical scope, making
    it the central component of the Lisp environment.

*   **Key Methods:**

    * `hmap:find`: The hyper-optimized O(1) lookup mechanism that uses the
        `str_hashslot` cache on symbols.

    * `hmap:search`: Traverses the `:parent` chain to find a binding in an outer
        scope.

    * `hmap:insert` / `:set`: Adds or updates a key-value pair.

**`stdio`**

*   **Inherits From:** `obj`

*   **Purpose:** A container object that holds a task's three standard I/O
    streams (`stdin`, `stdout`, `stderr`), which are instances of `in` and
    `out`.

*   **Key Methods:**

    * `:init`: Creates the `in` and `out` streams based on the mailboxes
        provided in the task's initial creation message.

**`dim`** (Dimension)

*   **Inherits From:** `obj`

*   **Purpose:** Represents a multi-dimensional array or tensor.

*   **Key Methods:**

    * `:init`: Takes a `nums` object specifying the dimensions (e.g.,
      `(nums 10 20)`) and a flat `array` object containing the data. Lisp
      functions like `dim-get` and `dim-set` use the dimension info to calculate
      the correct index into the flat data array.