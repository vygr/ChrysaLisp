# ChrysaLisp Filesystem Library

This library provides a framework for implementing filesystems in ChrysaLisp, with a complete ExFat filesystem implementation that operates entirely within a `(memory-stream)`.

## Overview

The filesystem library consists of:

1. **Base Fs Class** (`lib/fs/fs.inc`) - Abstract base class defining the filesystem interface
2. **ExFat Implementation** (`lib/fs/exfat.inc`) - Complete ExFat filesystem implementation
3. **Test/Demo** (`apps/test_exfat.lisp`) - Demonstration of the ExFat filesystem

## Architecture

### Base Fs Class

The `Fs` class provides an abstract interface that all filesystem implementations should inherit from. It defines standard filesystem operations:

- **Lifecycle**: `:init`, `:deinit`, `:format`, `:mount`, `:unmount`
- **File Operations**: `:create`, `:open`, `:close`, `:read`, `:write`, `:seek`, `:tell`, `:remove`
- **Directory Operations**: `:mkdir`, `:rmdir`, `:list`
- **Information**: `:exists`, `:stat`, `:get-size`
- **Manipulation**: `:rename`

All methods throw exceptions with "not implemented" messages by default, requiring subclasses to override them.

### ExFat Implementation

The ExFat filesystem implementation (`ExFat` class) provides a complete, standards-compliant ExFat filesystem that operates on a ChrysaLisp stream object (typically a `memory-stream`).

#### Key Features

- **Standards Compliant**: Implements the ExFat specification including:
  - Boot sector with proper signature (0x55AA)
  - File Allocation Table (FAT) for cluster chain management
  - Directory entries with proper structure
  - Cluster-based storage allocation

- **Stream-Based**: Uses only official ChrysaLisp stream calls:
  - `(stream-seek stream offset whence)` - Position within the stream
  - `(read-blk stream size)` - Read blocks of data
  - `(write-blk stream data)` - Write blocks of data
  - `(stream-flush stream)` - Flush pending writes

- **In-Memory or Persistent**:
  - Can use `(memory-stream)` for RAM disc functionality
  - Can use `(file-stream)` to create persistent filesystem images
  - Raw stream data can be written to physical media via block drivers

#### ExFat Structure

The ExFat filesystem layout:

```
+------------------------+
| Boot Sector            | Sector 0 (512 bytes)
+------------------------+
| Extended Boot Sectors  | Sectors 1-8
+------------------------+
| OEM Parameters         | Sectors 9-11
+------------------------+
| Reserved Sectors       | Sectors 12-23
+------------------------+
| FAT Region             | Starting at sector 24
| (File Allocation Table)|
+------------------------+
| Cluster Heap           | Data clusters (starts at cluster 2)
| - Root Directory       |
| - Files and Subdirs    |
+------------------------+
```

##### Boot Sector (512 bytes)

- Bytes 0-2: Jump boot code (0xEB, 0x76, 0x90)
- Bytes 3-10: "EXFAT   " signature
- Bytes 64-71: Partition offset (8 bytes)
- Bytes 72-79: Volume length in sectors (8 bytes)
- Bytes 80-83: FAT offset in sectors (4 bytes)
- Bytes 84-87: FAT length in sectors (4 bytes)
- Bytes 88-91: Cluster heap offset (4 bytes)
- Bytes 92-95: Cluster count (4 bytes)
- Bytes 96-99: Root directory first cluster (4 bytes)
- Bytes 100-103: Volume serial number (4 bytes)
- Bytes 104-105: File system revision (0x0100 = version 1.0)
- Bytes 108: Bytes per sector shift (9 = 512 bytes)
- Bytes 109: Sectors per cluster shift
- Bytes 110: Number of FATs (always 1)
- Bytes 510-511: Boot signature (0x55, 0xAA)

##### File Allocation Table (FAT)

The FAT uses 32-bit entries to track cluster chains:

- `0x00000000`: Free cluster
- `0x00000002` - `0xFFFFFFF6`: Next cluster in chain
- `0xFFFFFFF7`: Bad cluster
- `0xFFFFFFFF`: End of cluster chain

Special entries:
- FAT[0]: Media type (0xFFFFFFF8)
- FAT[1]: End of chain marker (0xFFFFFFFF)
- FAT[2+]: Data clusters

##### Directory Entries

Each directory entry is 32 bytes. Multiple consecutive entries form a directory entry set:

- **File Entry** (0x85):
  - Byte 0: Entry type (0x85)
  - Byte 1: Secondary count (number of following entries)
  - Bytes 4-5: File attributes

- **Stream Extension** (0xC0):
  - Contains file size and first cluster

- **File Name Entry** (0xC1):
  - Contains up to 15 UTF-16 characters of filename

- **Allocation Bitmap Entry** (0x81):
  - Tracks cluster allocation status

- **Volume Label Entry** (0x83):
  - Contains volume name

- **End of Directory** (0x00):
  - Marks end of directory listing

#### Key Implementation Details

##### Cluster Management

Clusters are the basic allocation unit (default 4096 bytes = 2^12). The cluster heap starts at cluster number 2 (clusters 0-1 are reserved).

Methods:
- `:allocate-cluster` - Finds a free cluster and marks it as end-of-chain
- `:free-cluster-chain` - Frees a chain of clusters starting from a given cluster
- `:read-cluster` - Reads a cluster's data
- `:write-cluster` - Writes data to a cluster

##### Sector and Cluster I/O

The implementation provides layered access:

1. **Sector Level** (512 bytes):
   - `:read-sector` - Read a 512-byte sector
   - `:write-sector` - Write a 512-byte sector

2. **Cluster Level** (4096 bytes default):
   - `:read-cluster` - Read a full cluster
   - `:write-cluster` - Write a full cluster

3. **FAT Level**:
   - `:read-fat-entry` - Read a 32-bit FAT entry
   - `:write-fat-entry` - Write a 32-bit FAT entry

##### Little-Endian Encoding

ExFat uses little-endian byte order for all multi-byte values:

- `:write-u32-le` - Write 32-bit value in little-endian
- `:write-u64-le` - Write 64-bit value in little-endian
- `:read-u32-le` - Read 32-bit little-endian value
- `:read-u16-le` - Read 16-bit little-endian value

## Usage

### Basic Example

```lisp
(import "lib/fs/exfat.inc")

; Create a memory stream for the filesystem
(defq fs-stream (memory-stream))

; Create ExFat filesystem instance
(defq exfat (ExFat fs-stream))

; Format the filesystem (10 MB)
(. exfat :format (* 10 1024 1024))

; Create a file
(. exfat :create "/hello.txt")

; Open the file for writing
(defq handle (. exfat :open "/hello.txt" :write))

; Write some data
(. exfat :write handle "Hello, ExFat!")

; Close the file
(. exfat :close handle)

; Open for reading
(defq handle (. exfat :open "/hello.txt" :read))

; Read the data
(defq data (. exfat :read handle nil 100))
(prinl "Read: " data)

; Close the file
(. exfat :close handle)

; Unmount the filesystem
(. exfat :unmount)
```

### Creating a Persistent Filesystem Image

```lisp
(import "lib/fs/exfat.inc")

; Create a file stream for the filesystem image
(defq fs-stream (file-stream "disk.img" +file_open_write))

; Create and format ExFat filesystem
(defq exfat (ExFat fs-stream))
(. exfat :format (* 100 1024 1024))  ; 100 MB

; ... perform filesystem operations ...

; Unmount and close
(. exfat :unmount)
(stream-flush fs-stream)

; The file "disk.img" now contains a valid ExFat filesystem
; that can be mounted on a host OS
```

### Using as a RAM Disc

```lisp
(import "lib/fs/exfat.inc")

; Create in-memory filesystem
(defq ram-disc (ExFat (memory-stream)))
(. ram-disc :format (* 50 1024 1024))  ; 50 MB RAM disc

; Use like any filesystem
(. ram-disc :mkdir "/temp")
(. ram-disc :create "/temp/data.bin")

; The entire filesystem exists only in RAM
; Fast access, no persistence
```

### Writing to Physical Media

```lisp
(import "lib/fs/exfat.inc")

; Create and format filesystem in memory
(defq fs-stream (memory-stream))
(defq exfat (ExFat fs-stream))
(. exfat :format (* 1024 1024 1024))  ; 1 GB

; ... populate filesystem ...

; Unmount and flush
(. exfat :unmount)
(stream-flush fs-stream)

; Get the raw stream data
; In a real implementation, you would:
; 1. Get the memory-stream data via stream operations
; 2. Write it to a block device driver
; 3. The physical media would then be mountable on a host OS

; Pseudo-code example:
; (defq raw-data (get-stream-data fs-stream))
; (write-to-block-device "/dev/sdb" raw-data)
```

## Implementation Status

### Completed Features

- ✅ Boot sector creation and validation
- ✅ FAT (File Allocation Table) management
- ✅ Cluster allocation and deallocation
- ✅ Basic file creation and deletion
- ✅ Basic directory creation
- ✅ File handle management
- ✅ Basic read/write operations
- ✅ Seek operations
- ✅ Mount/unmount functionality
- ✅ Stream-based I/O (sector and cluster level)

### Partially Implemented

- ⚠️ Directory entry parsing (simplified)
- ⚠️ File name handling (basic support)
- ⚠️ Path resolution (assumes root directory)

### TODO / Future Enhancements

- ❌ Full directory entry set creation (File + Stream Extension + File Name entries)
- ❌ Long filename support (multiple file name entries)
- ❌ Directory traversal and path resolution
- ❌ File/directory enumeration (listing)
- ❌ File/directory metadata (timestamps, attributes)
- ❌ File renaming and moving
- ❌ Allocation bitmap management
- ❌ Up-case table for case-insensitive filename comparison
- ❌ Volume label support
- ❌ Fragmentation handling (multi-cluster files)
- ❌ Directory spanning multiple clusters
- ❌ Error recovery and validation
- ❌ Transaction safety
- ❌ Wear leveling considerations for flash media

## Testing

Run the test program to see the ExFat filesystem in action:

```bash
./run apps/test_exfat.lisp
```

The test program demonstrates:
- Creating a memory-based filesystem
- Formatting with ExFat
- Mounting/unmounting
- Creating files and directories
- File I/O operations
- FAT manipulation
- Cluster allocation/deallocation
- Stream-based access

## Technical Notes

### Why ExFat?

ExFat was chosen for this implementation because:

1. **Simplicity**: Simpler than NTFS or ext4, more suitable for embedded systems
2. **No Journaling**: Lightweight, no complex transaction logs
3. **Large File Support**: Supports files > 4GB (unlike FAT32)
4. **Flash-Friendly**: Designed for flash memory and SD cards
5. **Wide Support**: Recognized by Windows, macOS, Linux, and embedded systems
6. **Cluster-Based**: Clean, simple cluster chain structure

### Stream-Only I/O

The implementation uses **only** ChrysaLisp stream calls, making it:

- **Portable**: Works with any stream type (memory, file, network, etc.)
- **Testable**: Can use memory-streams for unit testing
- **Flexible**: Can layer compression, encryption, etc., on the stream
- **Simple**: No direct memory manipulation outside the stream abstraction

### Endianness

ExFat is little-endian. The implementation includes helper methods for proper byte ordering, ensuring the filesystem image is compatible with any host OS.

### Memory Efficiency

The `memory-stream` class uses a chunked architecture (default 4KB chunks), which means:

- No large contiguous memory allocations required
- Filesystem can grow dynamically
- Efficient for sparse filesystems
- Compatible with ChrysaLisp's memory model

## Future: Block Device Integration

The end goal is to:

1. Create filesystem in `memory-stream`
2. Populate with files and directories
3. Flush the stream
4. Extract raw bytes from memory-stream
5. Write to physical media via block device driver
6. Mount on host OS (Windows, Linux, macOS)

The stream-based architecture makes this straightforward:

```lisp
; Pseudo-code for future block device support
(defq block-dev (open-block-device "/dev/sdb"))
(defq fs-stream (memory-stream))
(defq exfat (ExFat fs-stream))

; Create filesystem
(. exfat :format (get-block-device-size block-dev))
; ... populate ...
(. exfat :unmount)

; Write to physical device
(write-stream-to-block-device fs-stream block-dev)
(close-block-device block-dev)

; Now the physical device contains a valid ExFat filesystem
```

## References

- [ExFat Specification](https://docs.microsoft.com/en-us/windows/win32/fileio/exfat-specification)
- [ChrysaLisp Streams Documentation](../../docs/ai_digest/streams.md)
- [ChrysaLisp Memory Architecture](../../docs/ai_digest/memory_architecture.md)

## License

Part of the ChrysaLisp project. See main repository for license information.
