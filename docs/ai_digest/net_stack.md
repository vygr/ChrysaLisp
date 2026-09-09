# The ChrysaLisp Network Stack: URL, JSON, and HTTP/1.1

This document provides a comprehensive technical overview of the modern network
stack and data interchange libraries located in `lib/net/`, together with the
`cmd/nettest.lisp` command-line utility.

The network stack bridges ChrysaLisp's message-based distributed actor
architecture with standard Internet protocols. Built entirely in idiomatic
ChrysaLisp, these libraries adhere strictly to the system's core design tenets:
vectorized string manipulation via atomic `splice`, iterative and stack-based
parsing to honor bounded thread stacks, zero-copy stream processing, and deep
integration with native Virtual Processor (VP) assembly primitives.

## 1. Architectural Overview & Design Philosophy

Traditional Lisp network implementations often rely on recursive-descent parsers
and character-by-character string accumulation. In ChrysaLisp, such patterns are
expressly avoided:

1. **Vectorized String Manipulation:** String transformations (such as percent-
   encoding, URL decoding, and JSON string escaping) avoid repeated memory
   allocation. Instead, they pre-classify character ranges using `char-class`
   tables (`bskip`, `rbskip`, `bfind`), compute coordinate spans into an integer
   vector (`nums`), and execute the transformation in a single atomic pass
   using the native `splice` primitive.

2. **Stack-Based, Non-Recursive Parsers:** ChrysaLisp tasks operate with small,
   fixed stack limits optimized for cooperative scheduling. Both the JSON parser
   (`json-parse`) and serializer (`json-stringify`) use explicit Lisp heap stacks
   to manage nested arrays and objects, eliminating the danger of call-stack
   overflows on deeply nested structures.

3. **Native VP Engine Integration:** Rather than implementing character escape
   decoding entirely in interpreted Lisp, the JSON reader scans raw escape
   boundaries and delegates decoding directly to the native `(:str :unescape)`
   VP method. This provides native C-speed parsing for Unicode code points
   (`\uXXXX`), 4-byte UTF-16 surrogate pairs (e.g., emojis), and standard control
   escapes (`\b`, `\f`, `\n`, `\r`, `\t`, `\"`, `\\`, `\/`).

4. **Stream-Centric I/O:** HTTP requests and responses operate over ChrysaLisp
   `stream` abstractions (`in-stream`, `out-stream`, `memory-stream`, `fstream`).
   Response payloads can stream directly into memory or destination file streams
   on disk with constant memory overhead.

## 2. The URL Codec (`lib/net/url.inc`)

The URL library implements RFC 3986 URI parsing, query parameter manipulation,
and high-speed percent-encoding/decoding.

### 2.1. Vectorized Percent-Encoding (`url-encode`)

The encoder uses a 769-byte precomputed lookup table (`+url_rep_table`) where
byte 0 contains `"+"` and bytes 1..768 contain the pre-rendered tokens `"%00"`
through `"%FF"`.

```vdu
(defq +url_unreserved (char-class "A-Za-z0-9_.~-")
	+cls_pct (char-class "%") +cls_pct_plus (char-class "%+"))

(defq +url_rep_table (cat "+" (apply (const cat)
	(map (# (cat "%" (hex-encode (char %0)))) (range 0 256)))))
```

* **Fast-Path Verification:** Before allocating index buffers, `(rbskip
  +url_unreserved s -1)` scans the input. If all characters are unreserved,
  the input string is returned immediately without copying.

* **Atomic Splicing:** When escaping is required, an index vector (`idxs`) is
  populated with source spans from the input string interleaved with target
  slices into `+url_rep_table`. A single `(splice s +url_rep_table idxs)` call
  assembles the encoded result.

* **Query Flag:** When `query_flag` is `:t`, spaces are encoded as `"+"` (offset
  `0..1` of `+url_rep_table`) rather than `"%20"`.

### 2.2. Vectorized Percent-Decoding (`url-decode`)

Decoding mirrors this technique against a precomputed 256-byte table containing
all single byte values (`+all_chars`):

* Fast-path scanning via `(rbskipn cls s -1)`.

* Direct 2-byte hex decoding using the native `hex-decode` primitive for `"%XX"`
  sequences.

* Conversion of `"+"` to space when `query_flag` is active.

### 2.3. Query String Handling

* **`url-query-parse (q_str)`**: Parses key-value pairs separated by `"&"` into
  a persistent hash map (`pmap`). Keys are automatically converted to keyword
  symbols (e.g., `"foo=bar"` becomes `:foo "bar"`). Keys and values are fully
  percent-decoded.

* **`url-query-format (q)`**: Formats a `pmap`, `Fmap`, `Lmap`, or association
  list into a valid `application/x-www-form-urlencoded` query string. Keyword
  colons are stripped before encoding.

### 2.4. URL Decomposition and Assembly

* **`url-parse (url_str)`**: Parses a URL string into a normalized `pmap`
  containing:

  * `:scheme`: Lowercase scheme (e.g., `"http"`, `"https"`, `"gemini"`).

  * `:user`: Decoded user authentication credentials.

  * `:host`: Hostname or IPv6 address literal (brackets stripped).

  * `:port`: Integer port number (defaults to `(url-scheme-port scheme)` if omitted).

  * `:path`: Hierarchical path (defaults to `"/"`).

  * `:query`: Raw query string.

  * `:params`: Pre-parsed `pmap` of query parameters.

  * `:fragment`: Anchor fragment without `"#"` prefix.

* **`url-format (u)`**: Reconstructs a canonical URL string from a parsed map,
  correctly quoting IPv6 host literals (`[::1]`) and omitting default ports.

* **`url-path-query (u)`**: Returns the path and query formatted for HTTP
  request lines (e.g., `"/api/v1/query?msg=hello"`).

## 3. The JSON Interchange Engine (`lib/net/json.inc`)

The JSON module provides high-performance serialization and deserialization
between JSON text and native ChrysaLisp data structures.

### 3.1. Type Mapping

* **Object (`{...}`):** Persistent hash map (`pmap`) with keyword keys (`:key`).

* **Array (`[...]`):** Sequence `list`.

* **String (`"..."`):** ChrysaLisp string (`str`).

* **Integer (`42`):** Integer (`num`).

* **Decimal (`3.14`):** 48.16 Fixed-point number (`fixed`).

* **Exponent (`1e-4`):** IEEE 64-bit double float (`real`).

* **Boolean (`true`/`false`):** `:t` / `:nil`.

* **Null (`null`):** `:nil`.

### 3.2. Streaming Parser (`json-parse`)

The parser processes an input string or open `stream`:

1. **Stack-Based Context:** An explicit `stack` list maintains open containers
   (`:object` and `:array`). Each frame tracks the container type, the
   accumulated collection, the expected next state (e.g., `:expect_colon`,
   `:expect_comma_or_end`), and the pending object key.

2. **Native Escape Delegation:** In `json-read-string`, the stream scans
   characters up to the closing quote (`"\q"`). When escape sequences (`"\\"`)
   are detected, the escape sequence is preserved in the intermediate stream
   buffer. Once closed, the accumulated string is dispatched in a single call to
   the native VP method:

```vdu
(unescape (str ss))
```

   Because `(:str :unescape)` natively implements hex parsing and UTF-8 encoding
   for `\uXXXX` (including 4-byte surrogate pairs `\uD800..\uDBFF\uDC00..\uDFFF`
   and standard `\b`, `\f`, `\n`, `\r`, `\t`, `\q`, `\\`), unescaping happens
   at bare-metal speed without creating temporary Lisp objects for every
   character.

3. **Number Recognition:** Numbers are scanned into a local buffer. Integers and
   fixed-point numbers resolve via `str-to-num`, while numbers containing `"e"`
   or `"E"` resolve via `str-to-real`.

### 3.3. Iterative Serializer (`json-stringify`)

`json-stringify` serializes any ChrysaLisp data structure into compact JSON
without recursion:

* **Container Traversal:** An explicit stack walks nested `pmap` and `list`
  structures. At each node, items are partitioned, converted to key/value pairs,
  and joined with commas.

* **Scalar Serialization:**

  * Floats use `real-to-str` to produce standards-compliant decimal strings.

  * Fixed-point numbers and integers convert directly via `str`.

  * `:nil` and `:t` map to `"null"` and `"true"`.

  * Strings are processed through `json-escape-str`, which uses `(escape s)` and
    `splice` to escape quotes and control characters.

* **Tree Shims:** Convenience functions `json-to-tre` and `json-from-tre` provide
  symmetric conversion interfaces for tree-processing pipelines.

## 4. The HTTP/1.1 Client (`lib/net/http.inc`)

The HTTP client provides a robust, connection-pooled HTTP/1.1 implementation
built on top of ChrysaLisp's network daemon (`service/net/app.inc`).

### 4.1. Network Daemon Integration

ChrysaLisp isolates raw TCP socket management within the Net service.
`lib/net/http.inc` interacts with this service through message-based Remote
Procedure Calls (RPC):

```code
+-------------+   RPC Connect   +--------------------+
| HTTP Client | --------------> | Net Service Daemon |
|             | <-------------- |                    |
+-------------+  in/out streams +--------------------+
       |                                  |
       |  HTTP/1.1 Request / Response     |
       +--------------------------------->| Remote Server
```

* `(net-service)` discovers the active service mailbox using `(mail-enquire "*Net,")`.

* `(net-open-rpc host port)` establishes a TCP connection, returning a pair of
  connected streams: `(client_in client_out)`.

* `(net-close-rpc conn)` terminates the connection and releases socket resources.

### 4.2. Connection Pooling (`+http_pool`)

To eliminate TCP handshake latency on repeated requests, `http.inc` maintains a
private, host-keyed connection pool (`+http_pool`):

1. **Checkout (`http-pool-checkout`):**

   * Retrieves idle sockets keyed by `"host:port"`.

   * Validates socket liveness using `(mail-validate (in-mbox in))`.

   * Polls for unexpected incoming data or remote closures via
     `(mail-poll (list (in-mbox in)))`. If the server sent a FIN or RST packet
     while idle, the stale connection is discarded and closed immediately.

   * If no valid pooled connection exists, a new socket is opened via
     `net-open-rpc`.

2. **Checkin (`http-pool-checkin`):**

   * Inspects the response's `Connection` header and transport framing.

   * If the server specified `Connection: close` or if transport framing could
     not be verified, the socket is closed via `net-close-rpc`.

   * If the stream is active and properly framed (via `Content-Length` or
     `chunked` transfer encoding), the connection is returned to the pool for
     subsequent requests.

3. **Automatic Stale-Socket Retry:**

   * If a reused connection drops on the initial read (server closed the
     keep-alive socket race), `http-request` detects `from_pool`, closes the
     broken connection, and transparently retries the request once on a fresh
     connection.

4. **Pool Teardown (`http-pool-clear`):**

   * Closes and flushes all idle sockets in the pool.

### 4.3. Transport Framing & Body Handling (`http-read-body`)

The client automatically detects and handles all standard HTTP/1.1 payload
framing mechanisms:

1. **Chunked Transfer Encoding:**

   * Reads chunk size lines in hexadecimal.

   * Strips optional chunk extensions (e.g., `1a0;ext=val`).

   * Streams chunk payloads in `+http_buf_size` (4096-byte) blocks.

   * Consumes terminating CRLFs and trailers until the zero-length chunk.

2. **Content-Length Framing:**

   * Reads exactly the specified number of bytes, preventing hangs on keep-alive
     connections.

3. **Connection Close Framing:**

   * Reads until stream EOF when no explicit length or chunking is declared.

4. **Zero-Copy Disk Streaming (`dest_stream`):**

   * By default, response bodies accumulate in a `memory-stream`.

   * Callers can optionally pass an open destination stream (such as an
     `fstream` targeting a file on disk). The payload is written directly to the
     destination without buffering the complete response in RAM.

### 4.4. The HTTP Request API

```vdu
(http-get url [headers dest_stream])
(http-post url body [headers dest_stream])
(http-head url [headers])
(http-request method url [headers body dest_stream])
```

Responses are returned as a persistent hash map (`pmap`):

```vdu
(pmap
	:status  200
	:reason  "OK"
	:proto   "HTTP/1.1"
	:headers (pmap :content-type "text/html" :content-length "1234" ...)
	:body    <stream>)
```

The helper `(http-body-str resp)` extracts the entire body stream as a single
string and resets the stream position to 0.

## 5. The `nettest` CLI Command (`cmd/nettest.lisp`)

`cmd/nettest.lisp` serves as an interactive diagnostic utility and reference
implementation demonstrating the complete network stack.

### 5.1. Command-Line Syntax

```
Usage: nettest [options] [url|host] [port]

    options:
        -h --help: this help info.

    Simple HTTP / Net service test.
    Examples:
        nettest http://example.com/
        nettest http://httpbin.org/get?msg=hello+world
```

### 5.2. Execution Flow

1. Parses command-line arguments using `lib/options/options.inc`.

2. Parses the destination URL via `url-parse` and applies optional port
   overrides.

3. Issues an HTTP GET request using `http-get`.

4. Pretty-prints the HTTP status code, status message, protocol version, and all
   received response headers.

5. Iterates through the response body stream using `lines!` to print the content
   directly to standard output.

## 6. Practical Examples & Idioms

### 6.1. Fetching and Parsing JSON from an API

```vdu
(import "lib/net/http.inc")
(import "lib/net/json.inc")

(defun fetch-user (user_id)
	(defq url (cat "http://api.example.com/users/" (str user_id)))
	(when (defq resp (http-get url (pmap :accept "application/json")))
		(when (= (pfind resp :status) 200)
			(json-parse (http-body-str resp)))))
```

### 6.2. Posting JSON Data

```vdu
(defun update-status (status_msg)
	(defq payload (json-stringify (pmap :status status_msg :active :t)))
	(http-post "http://api.example.com/status" payload
		(pmap :content-type "application/json")))
```

### 6.3. Streaming a Large File to Disk

```vdu
(defun download-file (url local_path)
	(when (defq out (file-stream local_path +file_open_write))
		(defq resp (http-get url (pmap) out))
		(stream-close out)
		(= (pfind resp :status) 200)))
```

### 6.4. Safe Parameter Encoding

```vdu
(import "lib/net/url.inc")

(defun make-search-url (query page)
	(url-format (pmap
		:scheme "http"
		:host "search.example.com"
		:path "/search"
		:params (pmap :q query :p (str page)))))
```

## 7. Summary

The ChrysaLisp network libraries demonstrate how standard internet protocols can
be cleanly expressed within a modern Lisp operating environment. By pairing
native assembly acceleration (`splice`, `(:str :unescape)`) with non-recursive
heap-stack parsers and connection pooling, ChrysaLisp achieves robust,
high-throughput network I/O with minimal memory footprints and predictable
execution times.
